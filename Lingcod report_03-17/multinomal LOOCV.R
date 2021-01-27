int_boat <- readRDS(".\\Data\\int_boat.rds")
source(".\\functions.R")
library(magrittr)
library(ggplot2)

int_area <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarize(lH = sum(lH), E = sum(E)) %>%
  dplyr::ungroup()
dat_Homer <- lapply(1993:2017, function(x) make_jagsdatpred(int_area, "Homer", "lH", x))
dat_Kodiak <- lapply(1993:2017, function(x) make_jagsdatpred(int_area, "Kodiak", "lH", x))
dat_Seward <- lapply(1993:2017, function(x) make_jagsdatpred(int_area, "Seward", "lH", x))
dat_Valdez <- lapply(1993:2017, function(x) make_jagsdatpred(int_area, "Valdez", "lH", x))
dat_Whittier <- lapply(1993:2017, function(x) make_jagsdatpred(int_area, "Whittier", "lH", x))



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
  for(f in 1:2){ #loop around fleet
    pred_count[f,1:A] ~ dmulti(q_pred[f,1:A], pred_M[f])
      for(a in 1:A){
        re_pred[f,a] ~ dnorm(0, tau)
        q_pred[f,a] <- phi_pred[f,a]/sum(phi_pred[f,])
        log(phi_pred[f,a]) <- alpha[a] + re_pred[f,a]
      }
  }
}
", con="model_alpha_pred.txt")
modfile_alpha_pred <- 'model_alpha_pred.txt'

parameters_alpha_pred <- c("alpha", "sd", "q_pred", "pred")
ni <- 5E4; nb <- ni/2; nc <- 3; nt <- 100
'post_alpha_pred_Homer' <- 
  lapply(dat_Homer, 
         function(x){x2 <- x[names(x) != "pred_area"]
                     OLREjags_re <- jagsUI::jags(x2, 
                                                 parameters.to.save = parameters_alpha_pred, 
                                                 model.file = modfile_alpha_pred, 
                                                 n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                                 parallel = TRUE)})
saveRDS(post_alpha_pred_Homer, ".\\Lingcod report_03-17\\Interview post\\post_alpha_pred_Homer.rds")
post_alpha_pred_Kodiak <- 
  lapply(dat_Kodiak, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Kodiak, ".\\Lingcod report_03-17\\Interview post\\post_alpha_pred_Kodiak.rds")

post_alpha_pred_Seward <- 
  lapply(dat_Seward, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Seward, ".\\Lingcod report_03-17\\Interview post\\post_alpha_pred_Seward.rds")

post_alpha_pred_Valdez <- 
  lapply(dat_Valdez, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Valdez, ".\\Lingcod report_03-17\\Interview post\\post_alpha_pred_Valdez.rds")

post_alpha_pred_Whittier <- 
  lapply(dat_Whittier, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Whittier, ".\\Lingcod report_03-17\\Interview post\\post_alpha_pred_Whittier.rds")



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
  for(f in 1:2){ #loop around fleet
    pred_count[f,1:A] ~ dmulti(q_pred[f,1:A], pred_M[f])
      for(a in 1:A){
        re_pred[f,a] ~ dnorm(0, tau)
        q_pred[f,a] <- phi_pred[f,a]/sum(phi_pred[f,])
        log(phi_pred[f,a]) <- alpha[a] + beta[pred_fleet[f] + 1,a] + re_pred[f,a]
      }
  }
}
", con="model_beta_pred.txt")
modfile_beta_pred <- 'model_beta_pred.txt'

parameters_beta_pred <- c("alpha", "beta", "sd", "q_pred", "pred")
post_beta_pred_Homer <- 
  lapply(dat_Homer, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Homer, ".\\Lingcod report_03-17\\Interview post\\post_beta_pred_Homer.rds")

post_beta_pred_Kodiak <- 
  lapply(dat_Kodiak, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Kodiak, ".\\Lingcod report_03-17\\Interview post\\post_beta_pred_Kodiak.rds")

post_beta_pred_Seward <- 
  lapply(dat_Seward, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Seward, ".\\Lingcod report_03-17\\Interview post\\post_beta_pred_Seward.rds")

post_beta_pred_Valdez <- 
  lapply(dat_Valdez, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Valdez, ".\\Lingcod report_03-17\\Interview post\\post_beta_pred_Valdez.rds")

post_beta_pred_Whittier <- 
  lapply(dat_Whittier, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Whittier, ".\\Lingcod report_03-17\\Interview post\\post_beta_pred_Whittier.rds")





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
  for(f in 1:2){ #loop around fleet
    pred_count[f,1:A] ~ dmulti(q_pred[f,1:A], pred_M[f])
      for(a in 1:A){
        re_pred[f,a] ~ dnorm(0, tau)
        q_pred[f,a] <- phi_pred[f,a]/sum(phi_pred[f,])
        log(phi_pred[f,a]) <- alpha[a] + beta[pred_fleet[f] + 1,a] + epsilon[a]*pred_year[f] + re_pred[f,a]
      }
  }
}
", con="model_epsilon_pred.txt")
modfile_epsilon_pred <- 'model_epsilon_pred.txt'

parameters_epsilon_pred <- c("alpha", "beta", "epsilon", "sd", "q_pred", "pred")
post_epsilon_pred_Homer <- 
  lapply(dat_Homer, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Homer, ".\\Lingcod report_03-17\\Interview post\\post_epsilon_pred_Homer.rds")

post_epsilon_pred_Kodiak <- 
  lapply(dat_Kodiak, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Kodiak, ".\\Lingcod report_03-17\\Interview post\\post_epsilon_pred_Kodiak.rds")

post_epsilon_pred_Seward <- 
  lapply(dat_Seward, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Seward, ".\\Lingcod report_03-17\\Interview post\\post_epsilon_pred_Seward.rds")

post_epsilon_pred_Valdez <- 
  lapply(dat_Valdez, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Valdez, ".\\Lingcod report_03-17\\Interview post\\post_epsilon_pred_Valdez.rds")

post_epsilon_pred_Whittier <- 
  lapply(dat_Whittier, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Whittier, ".\\Lingcod report_03-17\\Interview post\\post_epsilon_pred_Whittier.rds")



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
  for(f in 1:2){ #loop around fleet
    pred_count[f,1:A] ~ dmulti(q_pred[f,1:A], pred_M[f])
      for(a in 1:A){
        re_pred[f,a] ~ dnorm(0, tau)
        q_pred[f,a] <- phi_pred[f,a]/sum(phi_pred[f,])
        log(phi_pred[f,a]) <- alpha[a] + beta[pred_fleet[f] + 1,a] + epsilon[a]*pred_year[f] + gamma[pred_fleet[f] + 1,a]*pred_year[f] + re_pred[f,a]
      }
  }
}
", con="model_gamma_pred.txt")
modfile_gamma_pred <- 'model_gamma_pred.txt'

parameters_gamma_pred <- c("alpha", "beta", "epsilon", "gamma", "sd", "q_pred", "pred")
post_gamma_pred_Homer <- 
  lapply(dat_Homer, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Homer, ".\\Lingcod report_03-17\\Interview post\\post_gamma_pred_Homer.rds")

post_gamma_pred_Kodiak <- 
  lapply(dat_Kodiak, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Kodiak, ".\\Lingcod report_03-17\\Interview post\\post_gamma_pred_Kodiak.rds")

post_gamma_pred_Seward <- 
  lapply(dat_Seward, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Seward, ".\\Lingcod report_03-17\\Interview post\\post_gamma_pred_Seward.rds")

post_gamma_pred_Valdez <- 
  lapply(dat_Valdez, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Valdez, ".\\Lingcod report_03-17\\Interview post\\post_gamma_pred_Valdez.rds")

post_gamma_pred_Whittier <- 
  lapply(dat_Whittier, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Whittier, ".\\Lingcod report_03-17\\Interview post\\post_gamma_pred_Whittier.rds")



#Not sure how to evaluate cross-validated predictions
#Maybe the most direct route is just to calcualte the log-liklihood for the left our observations given the modeled probabilities
#Seems like the best choice
get_llpred <- function(pred, obs){
  ll <- matrix(NA, 
               nrow = dim(pred$sims.list$q_pred)[1],
               ncol = dim(pred$sims.list$q_pred)[2])
#  sum_ll <- vector(mode = "numeric", length = dim(pred$sims.list$q_pred)[1])
  for(i in 1:dim(pred$sims.list$q_pred)[1]){
    ll[i, 1] <- dmultinom(obs$lo_count[1, ], prob = pred$sims.list$q_pred[i, 1, ], log = TRUE)
    ll[i, 2] <- dmultinom(obs$lo_count[2, ], prob = pred$sims.list$q_pred[i, 2, ], log = TRUE)
  }
  apply(ll, 1, sum)
}


ll_Homer <- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Homer, obs = dat_Homer) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Homer, obs = dat_Homer) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Homer, obs = dat_Homer) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Homer, obs = dat_Homer) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Homer, FUN = median)
ggplot(data = ll_Homer, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)

ll_Kodiak <- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Kodiak, obs = dat_Kodiak) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Kodiak, obs = dat_Kodiak) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Kodiak, obs = dat_Kodiak) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Kodiak, obs = dat_Kodiak) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Kodiak, FUN = median)
ggplot(data = ll_Kodiak, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Kodiak, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Kodiak, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Kodiak, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)

ll_Seward <- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Seward, obs = dat_Seward) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Seward, obs = dat_Seward) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Seward, obs = dat_Seward) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Seward, obs = dat_Seward) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Seward, FUN = median)
ggplot(data = ll_Seward, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Seward, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Seward, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Seward, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)

ll_Valdez <- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Valdez, obs = dat_Valdez) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Valdez, obs = dat_Valdez) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Valdez, obs = dat_Valdez) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Valdez, obs = dat_Valdez) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Valdez, FUN = median)
ggplot(data = ll_Valdez, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Valdez, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Valdez, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Valdez, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)


ll_Whittier <- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Whittier[6:25], obs = dat_Whittier[6:25]) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Whittier[6:25], obs = dat_Whittier[6:25]) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Whittier[6:25], obs = dat_Whittier[6:25]) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Whittier[6:25], obs = dat_Whittier[6:25]) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Whittier, FUN = median)
ggplot(data = ll_Whittier, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Whittier, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Whittier, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Whittier, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)

