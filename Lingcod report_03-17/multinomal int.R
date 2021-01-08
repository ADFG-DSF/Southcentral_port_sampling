#Looking for a method to fit groundfish harvest and effort compostion data for reporting
#would like to detect a trend to composition data
library(magrittr)
library(ggplot2)
int_boat <- readRDS(".\\Data\\int_boat.rds") 

# Boat scale distribution of Lingcod harvest, total effort and CPUE by target species and fleet.
# Note there are negligable trips harvesting lingcode but those trip do well
plottext <- 
  expand.grid(target = levels(int_boat$target),
              fleet = c("Charter", "Private"),
              name = c("H", "E", "cpue"))
plottext$name <- 
  factor(plottext$name, 
         levels = c("H", "E", "cpue"),
         labels = c("Harvest", "Effort", "CPUE"))
plottext$n <- 
  c(format(as.vector(table(int_boat$target[!is.na(int_boat$H)], int_boat$fleet[!is.na(int_boat$H)])), trim = TRUE, big.mark = ","),
    rep(format(as.vector(table(int_boat$target[!is.na(int_boat$E)], int_boat$fleet[!is.na(int_boat$E)])), trim = TRUE, big.mark = ","), 2))
int_boat %>%
  dplyr::mutate(cpue = H/E) %>%
  dplyr::select(fleet, target, H, E, cpue) %>%
  tidyr::pivot_longer(c(H, E, cpue)) %>%
  dplyr::mutate(name = factor(name, 
                              levels = c("H", "E", "cpue"),
                              labels = c("Harvest", "Effort", "CPUE"))) %>%
  ggplot(aes(x = target, y = value)) +
  geom_boxplot() + #varwidth = TRUE
  geom_text(data = plottext, aes(target, Inf, label = n), vjust = 1) +
#  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_grid(name ~ fleet, scales = "free_y") +
  scale_x_discrete(name = "Target Species") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=30,hjust=1))

#More lingcod than halibut on a halibut trip
sum(int_boat$H > 0 & int_boat$hH <= int_boat$H & int_boat$target == "Halibut", na.rm = TRUE) / 
  sum(int_boat$target == "Halibut", na.rm = TRUE)
#int_boat[int_boat$H > 0 & int_boat$hH <= int_boat$H & int_boat$target == "Halibut", ]
table(int_boat$port[int_boat$H > 0 & int_boat$hH <= int_boat$H & int_boat$target == "Halibut"])

#More lingcod than bottomfish on a bottomfish trip
int_boat$bH <- 
  ifelse(is.na(int_boat$hH), 0, int_boat$hH) +
  ifelse(is.na(int_boat$pH), 0, int_boat$pH) + 
  ifelse(is.na(int_boat$npH), 0, int_boat$npH) +
  ifelse(is.na(int_boat$yH), 0, int_boat$yH) 
sum(int_boat$H > 0 & 
       int_boat$bH <= int_boat$H & 
       int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon"), na.rm = TRUE) / 
  sum(int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon"), na.rm = TRUE)
int_boat[int_boat$H > 0 & 
           int_boat$bH <= int_boat$H & 
           int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon"), ] %>% print(n = 200)
table(int_boat$port[int_boat$H > 0 & 
                      int_boat$bH <= int_boat$H & 
                      int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon")], useNA = "ifany")

#More than 2 lingcod by target species
sum(int_boat$H > 2 & int_boat$target == "Halibut", na.rm = TRUE)
sum(int_boat$target == "Halibut", na.rm = TRUE)
(tab2l <- table(int_boat$H > 2, int_boat$target))
tab2l[2,]/apply(tab2l, 2, sum)


#More rockfish than halibut on a halibut trip
int_boat$rH <- 
  ifelse(is.na(int_boat$pH), 0, int_boat$pH) + 
  ifelse(is.na(int_boat$npH), 0, int_boat$npH) +
  ifelse(is.na(int_boat$yH), 0, int_boat$yH) 
sum(int_boat$hH > 0 & 
       int_boat$rH >= int_boat$hH & 
       int_boat$target == "Halibut", na.rm = TRUE) /
  sum(int_boat$target == "Halibut", na.rm = TRUE)


dat_H2 <- int_boat %>% dplyr::filter(target == "Lingcod" | H >= 2)
table(dat_H2$target)
aggregate(H ~ target, dat_H2, mean)
aggregate(E ~ target, dat_H2, mean)


# Sum across areas to plot harvest composition by target for each port and fleet
int_target <- 
  dplyr::arrange(int_boat, year, port, fleet, area, target) %>%
  dplyr::group_by(year, port, fleet, area, target) %>%
  dplyr::summarise(H = sum(H), E = sum(E)) %>%
  dplyr::ungroup()
# lingcod effort is mostly bycatch
table(int_target$target, int_target$H > 0, int_target$port)
#fill data gaps

  # dplyr::mutate(H = ifelse(is.na(H), value, H),
  #               target2 = ifelse(target %in% c("S", "O", "R"), "O", target))
#Lingcod harvest by target species
lapply(unique(int_target$port), function(x){
  dat <- int_target[int_target$port == x, ]
  expand.grid(list(year = unique(dat$year),
                   port = unique(dat$port),
                   fleet = unique(dat$fleet), 
                   target = unique(dat$target),
                   value = 0))}) %>%
do.call(rbind, .) %>%
dplyr::left_join(int_target, by = c("year", "port", "fleet", "target")) %>%
  ggplot(aes(x = year, weight = H, fill = target)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  facet_grid(port ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(1993, 20017, 3)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")



# Boat scale distribution of Lingcod harvest, total effort and CPUE by target species and fleet.
# Note there are negligable trips harvesting lingcode but those trip do well
plottext <- 
  expand.grid(target = levels(int_boat$target),
              fleet = c("Charter", "Private"),
              name = c("H", "E", "cpue"))
plottext$name <- 
  factor(plottext$name, 
         levels = c("H", "E", "cpue"),
         labels = c("Harvest", "Effort", "CPUE"))
plottext$n <- 
  c(format(as.vector(table(int_target$target[!is.na(int_target$H)], int_target$fleet[!is.na(int_target$H)])), trim = TRUE, big.mark = ","),
    rep(format(as.vector(table(int_target$target[!is.na(int_target$E)], int_target$fleet[!is.na(int_target$E)])), trim = TRUE, big.mark = ","), 2))
int_target %>%
  dplyr::mutate(cpue = H/E) %>%
  dplyr::select(fleet, target, H, E, cpue) %>%
  tidyr::pivot_longer(c(H, E, cpue)) %>%
  dplyr::mutate(name = factor(name, 
                              levels = c("H", "E", "cpue"),
                              labels = c("Harvest", "Effort", "CPUE"))) %>%
  ggplot(aes(x = target, y = value)) +
  geom_boxplot() + #varwidth = TRUE
  geom_text(data = plottext, aes(target, Inf, label = n), vjust = 1) +
  #  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_grid(name ~ fleet, scales = "free_y") +
  scale_x_discrete(name = "Target Species") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=30,hjust=1))









int_area <- 
  int_target %>%
  dplyr::group_by(port, year, fleet, area) %>% 
  dplyr::summarise(H = sum(H), E = sum(E)) %>%
  dplyr::ungroup()
ports <- unique(int_area$port)
aggregate(E ~ year + port, int_area, sum)
jags_dat <- lapply(ports, make_jagsdat, dat = int_area[int_area$year >= 2000, ], stat = "E")

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

parameters_alpha <- c("alpha", "sd", "re")
ni <- 1E4; nb <- ni/3; nc <- 3; nt <- 20
post_alpha <- lapply(jags_dat, 
                      function(x){
                        OLREjags_re <- jagsUI::jags(x, 
                                                    parameters.to.save = parameters_alpha, 
                                                    model.file = modfile_alpha, 
                                                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)})

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

parameters_beta <- c("alpha", "beta", "re", "sd")
post_beta <- lapply(jags_dat, 
                      function(x){
                        OLREjags_re <- jagsUI::jags(x, 
                                                    parameters.to.save = parameters_beta, 
                                                    model.file = modfile_beta, 
                                                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)})

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

parameters_epsilon <- c("alpha", "beta", "epsilon", "re", "sd")
post_epsilon <- lapply(jags_dat, 
                      function(x){
                        OLREjags_re <- jagsUI::jags(x, 
                                                    parameters.to.save = parameters_epsilon, 
                                                    model.file = modfile_epsilon, 
                                                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)})

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

parameters_gamma <- c("alpha", "beta", "epsilon", "gamma", "sd", "re")
post_gamma <- lapply(jags_dat, 
                   function(x){
                     OLREjags_re <- jagsUI::jags(x, 
                                                 parameters.to.save = parameters_gamma, 
                                                 model.file = modfile_gamma, 
                                                 n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)})
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

sd <- 
  data.frame(alpha = sapply(post_alpha, function(x) x$mean$sd),
             beta = sapply(post_beta, function(x) x$mean$sd),
             epsilon = sapply(post_epsilon, function(x) x$mean$sd),
             gamma = sapply(post_gamma, function(x) x$mean$sd))
rownames(sd) <- ports
sd

#Homer
plot_post(post_alpha[[1]], int_area[int_area$year >= 2000, ], ports[1])
plot_post(post_beta[[1]], int_area[int_area$year >= 2000, ], ports[1])
plot_post(post_epsilon[[1]], int_area[int_area$year >= 2000, ], ports[1]) #Lowest DIC and best
plot_post(post_gamma[[1]], int_area[int_area$year >= 2000, ], ports[1])
jags_dat[[1]]$M
#jagsUI::traceplot(post_epsilon[[1]])

#Kodiak
plot_post(post_alpha[[2]], int_area[int_area$year >= 2000, ], ports[2]) 
plot_post(post_beta[[2]], int_area[int_area$year >= 2000, ], ports[2])
plot_post(post_epsilon[[2]], int_area[int_area$year >= 2000, ], ports[2]) #Lowest DIC and best
plot_post(post_gamma[[2]], int_area[int_area$year >= 2000, ], ports[2])
jags_dat[[2]]$M
#jagsUI::traceplot(post_epsilon[[2]])

#Seward
plot_post(post_alpha[[3]], int_area[int_area$year >= 2000, ], ports[3]) #Lowest DIC? Try dropping Eastern PWS
plot_post(post_beta[[3]], int_area[int_area$year >= 2000, ], ports[3])
plot_post(post_epsilon[[3]], int_area[int_area$year >= 2000, ], ports[3])
plot_post(post_gamma[[3]], int_area[int_area$year >= 2000, ], ports[3])
jags_dat[[3]]$M
jags_dat[[3]]$count[,,5]

#Valdez
plot_post(post_alpha[[4]], int_area[int_area$year >= 2000, ], ports[4]) #lowest DIC. Run without fleet
plot_post(post_beta[[4]], int_area[int_area$year >= 2000, ], ports[4])
plot_post(post_epsilon[[4]], int_area[int_area$year >= 2000, ], ports[4])
plot_post(post_gamma[[4]], int_area[int_area$year >= 2000, ], ports[4])
jags_dat[[4]]$M
#jagsUI::traceplot(post_alpha[[4]])

#Whittier
plot_post(post_alpha[[5]], int_area[int_area$year >= 2000, ], ports[5])
plot_post(post_beta[[5]], int_area[int_area$year >= 2000, ], ports[5])
plot_post(post_epsilon[[5]], int_area[int_area$year >= 2000, ], ports[5]) #best
plot_post(post_gamma[[5]], int_area[int_area$year >= 2000, ], ports[5])
jags_dat[[5]]$M
#jagsUI::traceplot(post_epsilon[[5]])





ll <- rep(NA, dim(p)[1])
for(i in 1:dim(p)[1]) {ll[i] <- dmultinom(rbind(jags_dat[[1]]$count[,1,], jags_dat[[1]]$count[,2,])[i, ], 
                                                 prob = as.matrix(p[i, 1:5]), 
                                                 log = TRUE)}
-2*sum(ll)

y <- rbind(jags_dat[[1]]$count[,1,], jags_dat[[1]]$count[,2,])
-2*sum(lgamma(apply(y, 1, sum) + 1) - lgamma(y + 1)[,1] - lgamma(y + 1)[,2] - lgamma(y + 1)[,3] - lgamma(y + 1)[,4] - lgamma(y + 1)[,5] + y[,1] * log(p[,1]) + y[,2] * log(p[,2]) + y[,3] * log(p[,3]) + y[,4] * log(p[,4]) + y[,5] * log(p[,5]))




get_devdata <- function(dat){rbind(dat$count[,1,], dat$count[,2,])}
dev_dat <- lapply(jags_dat, get_devdata)

get_deviance <- function(dat, preds){
  ll <- rep(NA, dim(preds[[1]])[1])
  for(i in 1:dim(preds[[1]])[1]) {ll[i] <- dmultinom(dat[i, ], 
                                                prob = as.matrix(preds[[1]][i,]), 
                                                log = TRUE)}
  -2*sum(ll)
}
preds_alpha <-
  mapply(p_post, 
         post = post_alpha, 
         plotport = ports, 
         MoreArgs = list(dat = int_area[int_area$year >= 2000, ]), 
         SIMPLIFY = FALSE)
preds_beta <-
  mapply(p_post, 
         post = post_beta, 
         plotport = ports, 
         MoreArgs = list(dat = int_area[int_area$year >= 2000, ]), 
         SIMPLIFY = FALSE)
preds_epsilon <-
  mapply(p_post, 
         post = post_epsilon, 
         plotport = ports, 
         MoreArgs = list(dat = int_area[int_area$year >= 2000, ]), 
         SIMPLIFY = FALSE)
preds_gamma <-
  mapply(p_post, 
         post = post_gamma, 
         plotport = ports, 
         MoreArgs = list(dat = int_area[int_area$year >= 2000, ]), 
         SIMPLIFY = FALSE)
data.frame(
  alpha = mapply(get_deviance, dat = dev_dat, preds = preds_alpha),
  beta = mapply(get_deviance, dat = dev_dat, preds = preds_beta),
  epsilon = mapply(get_deviance, dat = dev_dat, preds = preds_epsilon),
  gamma = mapply(get_deviance, dat = dev_dat, preds = preds_gamma))
