#Looking for a method to fit groundfish harvest and effort compostion data for reporting
#would like to fit a trend to composition data
library(magrittr)
library(ggplot2)

#Read Homer data
readxl::excel_sheets("H:\\My Documents\\Southcentral halibut and groundfish\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls")
Hc <- 
  readxl::read_excel("H:\\My Documents\\Southcentral halibut and groundfish\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A5!C6:BK29",
                     col_names = c("stat_area", as.character(2003:2017)), 
                     col_types = c("text", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017)))))) %>%
  tidyr::gather(year, n, -stat_area) %>%
  dplyr::mutate(port = "Homer", 
                fleet = "Charter")
Hp <- 
  readxl::read_excel("H:\\My Documents\\Southcentral halibut and groundfish\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A5!C31:BK49",
                     col_names = c("stat_area", as.character(2003:2017)), 
                     col_types = c("text", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017)))))) %>%
  tidyr::gather(year, n, -stat_area) %>%
  dplyr::mutate(port = "Homer", 
                fleet = "Private")
dat0 <- 
  rbind(Hc, Hp) %>% 
  dplyr::mutate(fleet = factor(fleet, levels = c("Charter", "Private")))

#Group stat areas into geographic areas
sort(unique(dat0$stat_area))
plot_names <- c("Pacific Ocean", 
               "Outer Gulf Coast",
                "Pacific Ocean",
                "Pacific Ocean", #"Barren Islands",
                "Outer Gulf Coast",
                "Pacific Ocean",
                "Outer Gulf Coast",
                "Pacific Ocean",
                "Outer Gulf Coast",
                "Outer Gulf Coast",
                "Kachemak Bay",
                "Kachemak Bay",
                "Kachemak Bay",
                "Kachemak Bay",
                "Kachemak Bay",
                "Kachemak Bay",
                "Kachemak Bay",
                "Kachemak Bay",
                 "Cook Inlet",
                 "Cook Inlet",
                 "Pacific Ocean", #"Kodiak Island",
                 "Pacific Ocean", # "Barren Islands",
                 "Pacific Ocean", #"Barren Islands",
                 "Cook Inlet",
                 "Cook Inlet",
                 "Cook Inlet",
                 "Cook Inlet")
names(plot_names) <- sort(unique(dat0$stat_area))

dat <- 
  dat0 %>%
  dplyr::mutate(area = factor(plot_names[stat_area], levels = c("Kachemak Bay", "Cook Inlet", "Outer Gulf Coast", "Pacific Ocean")),
                year = as.numeric(year)) %>%
  dplyr::group_by(year, fleet, area) %>% 
  dplyr::summarise(count = sum(n)) %>%
  dplyr::ungroup()
#No observations of unguided counts in Pacific
table(dat$area, useNA = "always")

dat <- rbind(dat, data.frame(year = 2003:2017, fleet = "Private", area = "Pacific Ocean", count = 0))
#fixed
table(dat$area, useNA = "always")

dat %>% 
  dplyr::group_by(fleet, year, area) %>% 
  dplyr::summarise(count = sum(count)) %>%
  tidyr::spread(area, count) %>%
  print(n = 30)

#some varibles needed in the regression
dat$yearf <- factor(dat$year)
dat$yearc <- dat$year - mean(dat$year)

#####
#Try multinomial logistic regression
#saturated model for GOF
mod_sat <- nnet::multinom(area ~ yearf*fleet, data = dat, weights = count, maxit = 1000)
summary(mod_sat)

#Quadratic on year
dat$year2 <- dat$yearc*dat$yearc
mod_fyquad <- nnet::multinom(area ~ yearc*fleet*year2, data = dat, weights = count)
summary(mod_fyquad)

#linear on year
mod_fyli <- nnet::multinom(area ~ yearc*fleet, data = dat, weights = count)
summary(mod_fyli)

#linear wo interaction
mod_fyl <- nnet::multinom(area ~ year + fleet, data = dat, weights = count)
summary(mod_fyl)

#just fleet
mod_f <- nnet::multinom(area ~ fleet, data = dat, weights = count)
summary(mod_f)

#just year
mod_y <- nnet::multinom(area ~ yearc, data = dat, weights = count)
summary(mod_y)

#Quadratic model has the lowest AIC...
AIC(mod_sat, mod_fyquad, mod_fyli, mod_fyl, mod_f, mod_y)

#but it's not good
x2 <- deviance(mod_fyquad) - deviance(mod_sat); x2
pchisq(x2, 66, lower.tail = FALSE)

#and it's overfit in that we are trying to use nonlinearity to account for unaccounted for varibility
#visualize the fit
## store the predicted probabilities for each value of ses and write
dwrite <- data.frame(fleet = rep(c("Private", "Charter"), each = 15), 
                     yearc = rep(c(2003:2017 - mean(2003:2017)), 2),
                     year2 = rep(c(2003:2017 - mean(2003:2017))^2, 2))
pp.write <- cbind(dwrite, predict(mod_fyquad, newdata = dwrite, type = "probs", se = TRUE))

lpp <- tidyr::gather(pp.write, area, prob, -fleet, -yearc, - year2)
lpp$area <- factor(lpp$area, levels = levels(dat$area))
cum_pct <- 
  lpp %>% 
  dplyr::arrange(fleet, yearc) %>% 
  dplyr::group_by(fleet, yearc) %>% 
  dplyr::mutate(pct = cumsum(prob))
cum_pct$area <- factor(cum_pct$area, levels = rev(levels(dat$area)))

dat$areap <- factor(dat$area, levels = rev(levels(dat$area)))
dat %>%
  ggplot(aes(x = yearc, weight = count, fill = areap)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  geom_line(data = cum_pct, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, size = 1.1) +
  facet_grid(. ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(2003, 2017, 3)) +
  ggtitle(paste0("Homer")) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")
###

#visualize the fit of the linear model
#to me this looks better but note the interaction is probably not needed
## store the predicted probabilities for each value of ses and write
pp.write <- cbind(dwrite, predict(mod_fyli, newdata = dwrite, type = "probs", se = TRUE))

lpp <- tidyr::gather(pp.write, area, prob, -fleet, -yearc, - year2)
lpp$area <- factor(lpp$area, levels = levels(dat$area))
cum_pct <- 
  lpp %>% 
  dplyr::arrange(fleet, yearc) %>% 
  dplyr::group_by(fleet, yearc) %>% 
  dplyr::mutate(pct = cumsum(prob))
cum_pct$area <- factor(cum_pct$area, levels = rev(levels(dat$area)))

dat$areap <- factor(dat$area, levels = rev(levels(dat$area)))
dat %>%
  ggplot(aes(x = yearc, weight = count, fill = areap)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  geom_line(data = cum_pct, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, size = 1.1) +
  facet_grid(. ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(2003, 2017, 3)) +
  ggtitle(paste0("Homer")) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")

#Another presentaiton for the linear model
pp.write <- cbind(dwrite, predict(mod_fyli, newdata = dwrite, type = "probs", se = TRUE))

lpp$areap <- factor(lpp$area, levels = rev(levels(dat$area)))
lpp$pct <- lpp$prob
dat %>%
  dplyr::group_by(fleet, yearc) %>%
  dplyr::mutate(pct = count / sum(count)) %>%
  dplyr::group_by(fleet, yearc, areap) %>%
  dplyr::summarise(pct = sum(pct)) %>%
  ggplot(aes(x = yearc, y = pct, color = areap)) +
  geom_point() +
  geom_line(data = lpp, size = 1.1) +
  facet_grid(areap ~ fleet, scales = "free_y") +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(2003, 2017, 3)) +
  ggtitle(paste0("Homer")) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")




#####
#tried it as a gam but I think we need to account for missing varaibiity directly.
library(mgcv)
dat$y <- as.numeric(dat$area) - 1
dat$fleetn <- as.numeric(dat$fleet) - 1
l <- mapply(':', ifelse(dat$count == 0, 0, 1), dat$count)
dat_gam <-
  data.frame(
    y = dat$y[rep(1:nrow(dat), lengths(l))],
    fleet = dat$fleet[rep(1:nrow(dat), lengths(l))],
    yearc = dat$yearc[rep(1:nrow(dat), lengths(l))]
  )
dat_gam


mod_gam <- 
  mgcv::gam(list(y ~ 1 + fleet+s(yearc, k = 4), #, by = dat_gam$fleet # K = 15 is overfit
                 ~ 1 + fleet+s(yearc, k = 4),
                 ~ 1 + fleet+s(yearc, k = 4)), 
            family = mgcv::multinom(K = 3), data = dat_gam)
gam.check(mod_gam)
summary(mod_gam)

preds <- 
  predict(mod_gam, 
        newdata = data.frame(fleet = rep(c("Charter", "Private"), times = 15),
                             yearc = rep(2003:2017, each = 2) - mean(2003:2017)), 
        type = "response") %>%
  as.data.frame() %>%
  dplyr::mutate(fleet = rep(c("Charter", "Private"), times = 15),
                yearc = rep(2003:2017, each = 2) - mean(2003:2017)) %>%
  setNames(c("Kachemak Bay", "Cook Inlet", "Outer Gulf Coast", "Pacific Ocean", "fleet", "yearc")) %>%
  tidyr::gather(area0, prob, -fleet, -yearc) %>%
  dplyr::mutate(area = factor(area0, levels = levels(dat$area))) %>%
  dplyr::arrange(fleet, yearc, area) %>% 
  dplyr::group_by(fleet, yearc) %>% 
  dplyr::mutate(pct = cumsum(prob))
preds$area <- factor(preds$area, levels = rev(levels(dat$area)))

dat %>%
  ggplot(aes(x = yearc, weight = count, fill = areap)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  geom_line(data = preds, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, size = 1.1) +
  facet_grid(. ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(2003, 2017, 3)) +
  ggtitle(paste0("Homer")) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")
#####



######
#Jags wo overdispersion
#results in similar estimate as the glm above but much more reasonable parameter se

dat_jags0 <- 
  dat[, c("yearc", "fleet", "area", "count")] %>%
    tidyr::spread(area, count)

ni <- 1E4; 
nb <- ni/2 # number of iterations; number of burnins
nc <- 3; 
nt <- 10 # number of chains; thinning

parameters <- c("alpha", "beta1", "beta2")

count = 
  dat_jags0 %>% 
  dplyr::arrange(fleet, yearc) %>%
  dplyr::select(-yearc, -fleet) %>%
  as.matrix %>%
  array(dim = c(15, 2, 4))
dat_jags2 <-
  list(
    count = count,
    yearc = -7:7,
    A = 4, 
    M = apply(count, c(1,2), sum))

writeLines("
model{
  # priors:
  alpha[1] <- 0 #baseline area
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.001)}
  
  for (a in 1:A){beta[1,a] <- 0 } #charter baseline
  beta[2, 1] <- 0 ; # baseline area
  for (a in 2:A){ beta[2,a] ~ dnorm(0, 0.00001)}

  gamma[1] <- 0 #baseline area
  for (a in 2:A){gamma[a] ~ dnorm(0, 0.001)}
  
  #tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision

  for(y in 1:15){ # loop around years
    for(f in 1:2){ #loop around fleet
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + gamma[a]*yearc[y]
      }
    }
  }
}
", con="model.txt")
modfile <- 'model.txt'

parameters <- c("alpha", "beta", "gamma")
OLREjags <- jagsUI::jags(dat_jags2, 
                         #inits = inits, 
                         parameters.to.save = parameters, 
                         model.file = modfile, 
                         n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
OLREjags

x <- expand.grid(area = 1,
                 charter = 0:1,
                 private = 0:1,
                 yearc = -7:7) %>%
  dplyr::filter(charter != private) %>%
  as.matrix()
b <- 
  data.frame(
    intercept = OLREjags$mean$alpha,
    fleet = t(OLREjags$mean$beta),
    year = OLREjags$mean$gamma
  ) %>%
  as.matrix() %>%
  t()
x
b
p <- as.data.frame(exp(x%*%b)/apply(exp(x%*%b), 1, sum)) %>% setNames(c("Kachemak Bay", "Cook Inlet", "Outer Gulf Coast", "Pacific Ocean"))
p$fleet <- c("Charter", "Private")
p$yearc <- rep(-7:7, each = 2)
p
p <- tidyr::gather(p, area, prob, -fleet, -yearc) %>%
  dplyr::mutate(area = factor(area, levels = levels(dat$area))) %>%
  dplyr::arrange(fleet, yearc, area) %>% 
  dplyr::group_by(fleet, yearc) %>% 
  dplyr::mutate(pct = cumsum(prob))
p
p$area <- factor(p$area, levels = rev(levels(dat$area)))

dat %>%
  ggplot(aes(x = yearc, weight = count, fill = areap)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  geom_line(data = p, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, size = 1.1) +
  facet_grid(. ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(2003, 2017, 3)) +
  ggtitle(paste0("Homer")) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")





#jags w overdisersion on each observation
writeLines("
model{
  # priors:
  alpha[1] <- 0 #baseline area
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.001)}
  
  for (a in 1:A){beta[1,a] <- 0 } #charter baseline
  beta[2, 1] <- 0 ; # baseline area
  for (a in 2:A){ beta[2,a] ~ dnorm(0, 0.00001)}

  gamma[1] <- 0 #baseline area
  for (a in 2:A){gamma[a] ~ dnorm(0, 0.001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(y in 1:15){ # loop around years
    for(f in 1:2){ #loop around fleet
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + gamma[a]*yearc[y] + re[y,f,a]
      }
    }
  }
}
", con="model_re.txt")
modfile_re <- 'model_re.txt'

parameters_re <- c("alpha", "beta", "gamma", "re", "sd")
OLREjags_re <- jagsUI::jags(dat_jags2, 
                         #inits = inits, 
                         parameters.to.save = parameters_re, 
                         model.file = modfile_re, 
                         n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(OLREjags_re)
# jagsUI::traceplot(OLREjags_re)
# jagsUI::whiskerplot(OLREjags_re, "alpha")
# jagsUI::whiskerplot(OLREjags_re, "beta")
# jagsUI::whiskerplot(OLREjags_re, "gamma")

x <- expand.grid(area = 1,
                 charter = 0:1,
                 private = 0:1,
                 yearc = -7:7) %>%
  dplyr::filter(charter != private) %>%
  dplyr::arrange(area, private, yearc) %>%
  as.matrix()
b <- 
  data.frame(
    intercept = OLREjags_re$mean$alpha,
    fleet = t(OLREjags_re$mean$beta),
    year = OLREjags_re$mean$gamma
  ) %>%
  as.matrix() %>%
  t()
x
b
p <- as.data.frame(exp(x%*%b)/apply(exp(x%*%b), 1, sum)) %>% setNames(c("Kachemak Bay", "Cook Inlet", "Outer Gulf Coast", "Pacific Ocean"))
p$fleet <- rep(c("Charter", "Private"), each = 15)
p$yearc <- rep(-7:7, times = 2)
p
p <- tidyr::gather(p, area, prob, -fleet, -yearc) %>%
  dplyr::mutate(area = factor(area, levels = levels(dat$area))) %>%
  dplyr::arrange(fleet, yearc, area) %>% 
  dplyr::group_by(fleet, yearc) %>% 
  dplyr::mutate(pct = cumsum(prob))
p
p$area <- factor(p$area, levels = rev(levels(dat$area)))

re <- OLREjags_re$q50$re
re2 <- data.frame(c(re[,1,1],re[,2,1]), c(re[,1,2],re[,2,2]), c(re[,1,3],re[,2,3]), c(re[,1,4],re[,2,4]))
p2 <- as.data.frame(exp(x%*%b+re2)/apply(exp(x%*%b+re2), 1, sum)) %>% setNames(c("Kachemak Bay", "Cook Inlet", "Outer Gulf Coast", "Pacific Ocean"))
p2$fleet <- rep(c("Charter", "Private"), each = 15)
p2$yearc <- rep(-7:7, times = 2)
p2
p2 <- tidyr::gather(p2, area, prob, -fleet, -yearc) %>%
  dplyr::mutate(area = factor(area, levels = levels(dat$area))) %>%
  dplyr::arrange(fleet, yearc, area) %>% 
  dplyr::group_by(fleet, yearc) %>% 
  dplyr::mutate(pct = cumsum(prob))
p2
p2$area <- factor(p$area, levels = rev(levels(dat$area)))


dat %>%
  ggplot(aes(x = yearc, weight = count, fill = areap)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  geom_line(data = p, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, size = 1.1) +
  geom_line(data = p2, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, linetype = 1) +
  facet_grid(. ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(-7, 7, 3)) +
  ggtitle(paste0("Homer")) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")




#jags w overdisersion on each year
writeLines("
model{
  # priors:
  alpha[1] <- 0 #baseline area
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.001)}
  
  for (a in 1:A){beta[1,a] <- 0 } #charter baseline
  beta[2, 1] <- 0 ; # baseline area
  for (a in 2:A){ beta[2,a] ~ dnorm(0, 0.00001)}

  gamma[1] <- 0 #baseline area
  for (a in 2:A){gamma[a] ~ dnorm(0, 0.001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(y in 1:15){ # loop around years
    for(a in 1:A){
      re[y,a] ~ dnorm(0, tau)
    }
  }

  for(y in 1:15){ # loop around years
    for(f in 1:2){ #loop around fleet
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + gamma[a]*yearc[y] + re[y,a]
      }
    }
  }
}
", con="model_rey.txt")
modfile_rey <- 'model_rey.txt'

parameters_re <- c("alpha", "beta", "gamma", "re", "sd")
OLREjags_re2 <- jagsUI::jags(dat_jags2, 
                            parameters.to.save = parameters_re, 
                            model.file = modfile_rey, 
                            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(OLREjags_re2)
# jagsUI::traceplot(OLREjags_re2)
# jagsUI::whiskerplot(OLREjags_re, "alpha")
# jagsUI::whiskerplot(OLREjags_re, "beta")
# jagsUI::whiskerplot(OLREjags_re, "gamma")

x <- expand.grid(area = 1,
                 charter = 0:1,
                 private = 0:1,
                 yearc = -7:7) %>%
  dplyr::filter(charter != private) %>%
  dplyr::arrange(area, private, yearc) %>%
  as.matrix()
b <- 
  data.frame(
    intercept = OLREjags_re2$mean$alpha,
    fleet = t(OLREjags_re2$mean$beta),
    year = OLREjags_re2$mean$gamma
  ) %>%
  as.matrix() %>%
  t()
x
b
p <- as.data.frame(exp(x%*%b)/apply(exp(x%*%b), 1, sum)) %>% setNames(c("Kachemak Bay", "Cook Inlet", "Outer Gulf Coast", "Pacific Ocean"))
p$fleet <- rep(c("Charter", "Private"), each = 15)
p$yearc <- rep(-7:7, times = 2)
p
p <- tidyr::gather(p, area, prob, -fleet, -yearc) %>%
  dplyr::mutate(area = factor(area, levels = levels(dat$area))) %>%
  dplyr::arrange(fleet, yearc, area) %>% 
  dplyr::group_by(fleet, yearc) %>% 
  dplyr::mutate(pct = cumsum(prob))
p
p$areap <- factor(p$area, levels = rev(levels(dat$area)))

re <- rbind(OLREjags_re2$q50$re, OLREjags_re2$q50$re)
p2 <- as.data.frame(exp(x%*%b+re)/apply(exp(x%*%b+re), 1, sum)) %>% setNames(c("Kachemak Bay", "Cook Inlet", "Outer Gulf Coast", "Pacific Ocean"))
p2$fleet <- rep(c("Charter", "Private"), each = 15)
p2$yearc <- rep(-7:7, times = 2)
p2
p2 <- tidyr::gather(p2, area, prob, -fleet, -yearc) %>%
  dplyr::mutate(area = factor(area, levels = levels(dat$area))) %>%
  dplyr::arrange(fleet, yearc, area) %>% 
  dplyr::group_by(fleet, yearc) %>% 
  dplyr::mutate(pct = cumsum(prob))
p2
p2$areap <- factor(p$area, levels = rev(levels(dat$area)))


dat %>%
  ggplot(aes(x = yearc, weight = count, fill = areap)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  geom_line(data = p, aes(x = yearc, y = pct, color = areap), inherit.aes = FALSE, size = 1.1) +
  geom_line(data = p2, aes(x = yearc, y = pct, color = areap), inherit.aes = FALSE, linetype = 1) +
  facet_grid(. ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(-7, 7, 3), labels = seq(2003, 2017, 3)) +
  ggtitle(paste0("Homer")) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")
