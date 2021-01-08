#Looking for a method to fit groundfish harvest and effort compostion data for reporting
#would like to detect a trend to composition data
library(magrittr)
library(ggplot2)

#stat_areas
stat_areas <- 
  read.csv(".\\Lingcod report_03-17\\stat_areas.csv", na.strings = "", stringsAsFactors = FALSE) %>%
  setNames(c("stat_area", "CFunit", "SFUnit", "area"))

#Harvest data
H0 <- 
  readxl::read_excel("H:\\My Documents\\Southcentral halibut and groundfish\\Lingcod report_03-17\\Ling_harvest.xls", 
                     range = "Ling_harvest!A2:E4054",
                     col_names = c("fleet", "port", "year", "stat_area", "H"), 
                     col_types = c(rep("text", 3), "numeric", "numeric")) %>%
  dplyr::left_join(stat_areas, by = "stat_area") %>%
  dplyr::mutate(year = as.numeric(year))

# note all missing areas are zerro harvest 
table(H0$area, H0$fleet, useNA = "always")
H0[is.na(H0$area), ]
H0 <- H0[!is.na(H0$area), ] #4068 before, 4057 after

#Note some areas with no harvest or sparse sampling
aggregate(H ~ port + area, H0, sum)
H0[H0$port == "Valdez" & H0$area == "Resurrection Bay", ]
H0 <- H0[!(H0$port == "Valdez" & H0$area == "Resurrection Bay"), ] #4057 before, 4056 after
H0[H0$port == "Cordova", ]
H0 <- H0[H0$port != "Cordova", ] #4056 before, 4045 after

#Note Military and Unknown fleets
#Unknown fleet has zero harvest
aggregate(H ~ port + fleet, H0, sum)
table(H0$fleet)
H0 <- H0[H0$fleet != "Unknown", ] #4045 before, 4039 after
#Assume Military is Guided (check this with Martin)
H0$fleet <- ifelse(H0$fleet == "Military", "Charter", H0$fleet)

H <- 
  H0 %>%
  dplyr::group_by(port, year, fleet, area) %>% 
  dplyr::summarise(H = sum(H)) %>%
  dplyr::ungroup()

#some varibles needed in the regression
H$yearc <- H$year - mean(H$year)

make_jagsdat <- function(dat, port){
  data <- dat[dat$port == port, c("yearc", "fleet", "area", "H")] %>%
            dplyr::arrange(area, fleet, yearc)
  data_full <- expand.grid(list(yearc = unique(dat$yearc[dat$port == port]), 
                                fleet = unique(dat$fleet[dat$port == port]), 
                                area = unique(dat$area[dat$port == port]),
                                value = 0)) %>%
    dplyr::left_join(data, by = c("yearc", "fleet", "area")) %>%
    dplyr::mutate(H = ifelse(is.na(H), value, H))
  
  count = array(data_full$H,
                dim = c(length(unique(data_full$yearc)),
                        2,
                        length(unique(data_full$area))))
  
  list(
    count = count,
    yearc = unique(data$yearc),
    A = length(unique(data$area)),
    Y = length(unique(data$yearc)),
    M = apply(count, c(1,2), sum))
}

ports <- unique(H$port)
jags_dat <- lapply(ports, make_jagsdat, dat = H)

#jags w overdisersion on each observation
#area mean
writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + re[y,f,a]
      }
    }
  }
}
", con="model_alpha.txt")
modfile_alpha <- 'model_alpha.txt'

parameters_alpha <- c("alpha", "sd")
ni <- 1E4; nb <- ni/3; nc <- 3; nt <- 20
post_alpha <- lapply(jags_dat, 
                      function(x){
                        OLREjags_re <- jagsUI::jags(x, 
                                                    parameters.to.save = parameters_alpha, 
                                                    model.file = modfile_alpha, 
                                                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)})
saveRDS(post_alpha, ".\\Lingcod report_03-17\\post_alphaH.rds")

#fleet/area mean
writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  for (a in 1:A){beta[1,a] <- 0 } #area baseline
  beta[2, 1] <- 0 ; #fleet baseline
  for (a in 2:A){ beta[2,a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + re[y,f,a]
      }
    }
  }
}
", con="model_beta.txt")
modfile_beta <- 'model_beta.txt'

parameters_beta <- c("alpha", "beta", "sd")
ni <- 1E5; nb <- ni/3; nc <- 3; nt <- 20
post_beta <- lapply(jags_dat, 
                      function(x){
                        OLREjags_re <- jagsUI::jags(x, 
                                                    parameters.to.save = parameters_beta, 
                                                    model.file = modfile_beta, 
                                                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)})
saveRDS(post_beta, ".\\Lingcod report_03-17\\post_betaH.rds")

#area slopes
writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  for (a in 1:A){beta[1,a] <- 0 } #area baseline
  beta[2, 1] <- 0 ; #fleet baseline
  for (a in 2:A){ beta[2,a] ~ dnorm(0, 0.0001)}

  epsilon[1] <- 0 #area baseline
  for (a in 2:A){epsilon[a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + epsilon[a]*yearc[y] + re[y,f,a]
      }
    }
  }
}
", con="model_epsilon.txt")
modfile_epsilon <- 'model_epsilon.txt'

parameters_epsilon <- c("alpha", "beta", "epsilon", "sd")
ni <- 1E5; nb <- ni/3; nc <- 3; nt <- 20
post_epsilon <- lapply(jags_dat, 
                      function(x){
                        OLREjags_re <- jagsUI::jags(x, 
                                                    parameters.to.save = parameters_epsilon, 
                                                    model.file = modfile_epsilon, 
                                                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)})
saveRDS(post_epsilon, ".\\Lingcod report_03-17\\post_epsilonH.rds")

writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  for (a in 1:A){beta[1,a] <- 0 } #area baseline
  beta[2, 1] <- 0 ; #fleet baseline
  for (a in 2:A){ beta[2,a] ~ dnorm(0, 0.0001)}

  # gamma[1] <- 0 #area baseline
  # for (a in 2:A){gamma[a] ~ dnorm(0, 0.0001)}
  
  epsilon[1] <- 0 #area baseline
  for (a in 2:A){epsilon[a] ~ dnorm(0, 0.0001)}
  
  for (a in 1:A){gamma[1,a] <- 0 } #area baseline
  gamma[2,1] <- 0 ; #fleet baseline
  for (a in 2:A){ gamma[2,a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + epsilon[a]*yearc[y] + gamma[f,a]*yearc[y] + re[y,f,a]
      }
    }
  }
}
", con="model_gamma.txt")
modfile_gamma <- 'model_gamma.txt'

parameters_gamma <- c("alpha", "beta", "epsilon", "gamma", "sd")
post_gamma <- lapply(jags_dat, 
                   function(x){
                     OLREjags_re <- jagsUI::jags(x, 
                                                 parameters.to.save = parameters_gamma, 
                                                 model.file = modfile_gamma, 
                                                 n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)})
saveRDS(post_gamma, ".\\Lingcod report_03-17\\post_gammaH.rds")

ports
lapply(post_alpha, print)
lapply(post_beta, print)
lapply(post_epsilon, print)
lapply(post_gamma, print)

DIC <- 
  data.frame(alpha = sapply(post_alpha, function(x) x$DIC),
             beta = sapply(post_beta, function(x) x$DIC),
             epsilon = sapply(post_epsilon, function(x) x$DIC),
             gamma = sapply(post_gamma, function(x) x$DIC))
rownames(DIC) <- ports
DIC

deviance <- 
  data.frame(alpha = sapply(post_alpha, function(x) x$mean$deviance),
             beta = sapply(post_beta, function(x) x$mean$deviance),
             epsilon = sapply(post_epsilon, function(x) x$mean$deviance),
             gamma = sapply(post_gamma, function(x) x$mean$deviance))
rownames(deviance) <- ports
deviance

#Homer
plot_post(post_alpha[[1]], H, ports[1]) # Run without fleet (ther is no private data to speak of)
plot_post(post_beta[[1]], H, ports[1])
plot_post(post_epsilon[[1]], H, ports[1]) #Lowest DIC and best
plot_post(post_gamma[[1]], H, ports[1])
jags_dat[[1]]$M
#jagsUI::traceplot(post_epsilon[[1]])

#Kodiak
plot_post(post_alpha[[2]], H, ports[2]) 
plot_post(post_beta[[2]], H, ports[2])
plot_post(post_epsilon[[2]], H, ports[2]) #Lowest DIC and best
plot_post(post_gamma[[2]], H, ports[2])
jags_dat[[2]]$M
#jagsUI::traceplot(post_epsilon[[2]])

#Seward
plot_post(post_alpha[[3]], H, ports[3]) #Lowest DIC? Try dropping Eastern PWS
plot_post(post_beta[[3]], H, ports[3])
plot_post(post_epsilon[[3]], H, ports[3])
plot_post(post_gamma[[3]], H, ports[3])
jags_dat[[3]]$M
jags_dat[[3]]$count[,,5]
#Try dropping Eastern PWS
Jags_datSeward <- jags_dat[[3]]
Jags_datSeward$M
Jags_datSeward$M <- Jags_datSeward$M - Jags_datSeward$count[,,5]
Jags_datSeward$A <- 4
Jags_datSeward$count <- Jags_datSeward$count[,,1:4]
Jags_datSeward #OK
#Try dropping Eastern PWS
post_alphaSeward <- OLREjags_re <- jagsUI::jags(Jags_datSeward, 
                                                parameters.to.save = parameters_alpha, 
                                                model.file = modfile_alpha, 
                                                n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
post_betaSeward <- OLREjags_re <- jagsUI::jags(Jags_datSeward, 
                                               parameters.to.save = parameters_beta, 
                                               model.file = modfile_beta, 
                                               n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
post_epsilonSeward <- OLREjags_re <- jagsUI::jags(Jags_datSeward, 
                                                  parameters.to.save = parameters_epsilon, 
                                                  model.file = modfile_epsilon, 
                                                  n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
post_gammaSeward <- OLREjags_re <- jagsUI::jags(Jags_datSeward, 
                                                parameters.to.save = parameters_gamma, 
                                                model.file = modfile_gamma, 
                                                n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
post_alphaSeward$DIC
post_betaSeward$DIC
post_epsilonSeward$DIC
post_gammaSeward$DIC
#Try repor tperiod only
Jags_datSewrecent <- make_jagsdat(H[H$year %in% 1996:2017, ], "Seward")
#Try dropping Eastern PWS
post_alphaSewrecent  <- OLREjags_re <- jagsUI::jags(Jags_datSewrecent , 
                                                parameters.to.save = parameters_alpha, 
                                                model.file = modfile_alpha, 
                                                n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
post_betaSewrecent  <- OLREjags_re <- jagsUI::jags(Jags_datSewrecent , 
                                               parameters.to.save = parameters_beta, 
                                               model.file = modfile_beta, 
                                               n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
post_epsilonSewrecent  <- OLREjags_re <- jagsUI::jags(Jags_datSewrecent , 
                                                  parameters.to.save = parameters_epsilon, 
                                                  model.file = modfile_epsilon, 
                                                  n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
post_gammaSewrecent  <- OLREjags_re <- jagsUI::jags(Jags_datSewrecent , 
                                                parameters.to.save = parameters_gamma, 
                                                model.file = modfile_gamma, 
                                                n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
post_alphaSewrecent$DIC; post_alphaSewrecent$mean$deviance
post_betaSewrecent$DIC; post_betaSewrecent$mean$deviance
post_epsilonSewrecent$DIC; post_epsilonSewrecent$mean$deviance
post_gammaSewrecent$DIC; post_gammaSewrecent$mean$deviance
plot_post(post_alphaSewrecent, H[H$year %in% 1996:2017, ], ports[3]) #Lowest DIC? Try dropping Eastern PWS
plot_post(post_betaSewrecent, H[H$year %in% 1996:2017, ], ports[3])
plot_post(post_epsilonSewrecent, H[H$year %in% 1996:2017, ], ports[3])
plot_post(post_gammaSewrecent, H[H$year %in% 1996:2017, ], ports[3])


#Valdez
plot_post(post_alpha[[4]], H, ports[4]) #lowest DIC. Run without fleet
plot_post(post_beta[[4]], H, ports[4])
plot_post(post_epsilon[[4]], H, ports[4])
plot_post(post_gamma[[4]], H, ports[4])
jags_dat[[4]]$M
#jagsUI::traceplot(post_alpha[[4]])

#Whittier
plot_post(post_alpha[[5]], H, ports[5])
plot_post(post_beta[[5]], H, ports[5])
plot_post(post_epsilon[[5]], H, ports[5]) #best
plot_post(post_gamma[[5]], H, ports[5])
jags_dat[[5]]$M
#jagsUI::traceplot(post_epsilon[[5]])


plot_post <- function(post, dat, plotport){
  data <- dat[dat$port == plotport, c("yearc", "fleet", "area", "H")] %>%
    dplyr::arrange(area, fleet, yearc)
  data_full <- expand.grid(list(yearc = unique(dat$yearc[dat$port == plotport]), 
                                fleet = unique(dat$fleet[dat$port == plotport]), 
                                area = unique(dat$area[dat$port == plotport]),
                                value = 0)) %>%
    dplyr::left_join(data, by = c("yearc", "fleet", "area")) %>%
    dplyr::mutate(H = ifelse(is.na(H), value, H),
                  area = factor(area, levels = unique(dat$area[dat$port == plotport])))
  
  x <- 
    data.frame(
      area = 1,
      charter = rep(1:0, each = length(unique(data_full$yearc))),
      private = rep(0:1, each = length(unique(data_full$yearc))),
      yearc = rep(min(data_full$yearc):max(data_full$yearc), times = 2),
      yearccharter = c(min(data_full$yearc):max(data_full$yearc), rep(0, length(unique(data_full$yearc)))),
      yearcprivate = c(rep(0, length(unique(data_full$yearc))), min(data_full$yearc):max(data_full$yearc))) %>%
    dplyr::arrange(area, private, yearc) %>%
    as.matrix()
  
  b <- 
    data.frame(
      intercept = post$mean$alpha,
      fleet = if(!is.null(post$mean$beta)){t(post$mean$beta)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2),
      year = if(!is.null(post$mean$epsilon)){post$mean$epsilon} else rep(0, length(post$mean$alpha)),
      year = if(!is.null(post$mean$gamma)){t(post$mean$gamma)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2)) %>%
    as.matrix() %>%
    t()

  p <- as.data.frame(exp(x%*%b)/apply(exp(x%*%b), 1, sum)) %>% setNames(unique(dat$area[dat$port == plotport]));
  p$fleet <- rep(c("Charter", "Private"), each = length(unique(data_full$yearc)))
  p$yearc <- rep(min(data_full$yearc):max(data_full$yearc), times = 2)

  p <- 
    tidyr::pivot_longer(p, -c(fleet, yearc), names_to = "area") %>%
    dplyr::mutate(area = factor(area, levels = rev(unique(dat$area[dat$port == plotport])))) %>%
    dplyr::arrange(fleet, yearc, area) %>% 
    dplyr::group_by(fleet, yearc) %>% 
    dplyr::mutate(pct = cumsum(value))
  p$area <- factor(p$area, levels = unique(dat$area[dat$port == plotport]))
  
  out <- data_full %>%
    ggplot(aes(x = yearc, weight = H, fill = area)) +
    geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
    geom_line(data = p, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, size = 1.1) +
    #  geom_line(data = p2, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, linetype = 1) +
    facet_grid(. ~ fleet) +
    scale_y_continuous(name = "Percent") +
    scale_x_continuous(name = "Year", breaks = seq(min(dat$yearc), max(dat$yearc), 3), labels = seq(min(dat$year), max(dat$year), 3)) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")
  
  return(out)
}
