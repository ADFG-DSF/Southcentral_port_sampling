make_jagsdat <- function(dat, port, yr){
  dat$yearc <- dat$year - median(unique(dat$year))
  temp <- dat[dat$port == port & dat$year != yr, c("yearc", "fleet", "area", "E")] %>%
    dplyr::arrange(yearc, fleet, area)
  train <- temp %>%
    dplyr::group_by(yearc, fleet, area) %>%
    dplyr::summarise(E = sum(E))
  train_full <- expand.grid(list(yearc = unique(train$yearc), 
                                fleet = unique(train$fleet), 
                                area = unique(train$area))) %>%
    dplyr::left_join(train, by = c("yearc", "fleet", "area")) %>%
    dplyr::mutate(value = ifelse(is.na(E), 0, E))
  
  count = array(train_full$value,
                dim = c(length(unique(train_full$yearc)),
                        2,
                        length(unique(train_full$area))))
  
  test <- 
    dat[dat$port == port & dat$year == yr, c("yearc", "fleet", "area", "E")] %>%
    dplyr::arrange(yearc, fleet, area)
  pred_yr <- test$yearc
  pred_fleet <- ifelse(test$fleet == "Charter", 0, 1)
  pred_area <- test$area
  pred_n <- dim(test)[1]
  
  list(
    count = count,
    yearc = unique(train$yearc),
    A = length(unique(train$area)),
    Y = length(unique(train$yearc)),
    M = apply(count, c(1,2), sum),
    pred_n = pred_n,
    pred_fleet = pred_fleet,
    pred_year = pred_yr,
    pred_area = pred_area)
}
table(is.na(int_boat$E), int_boat$year)
dat_Homer <- lapply(2000:2017, function(x) make_jagsdat(int_boat[int_boat$year >= 2000, ], "Homer", x))
dat_Kodiak <- lapply(2000:2017, function(x) make_jagsdat(int_boat[int_boat$year >= 2000, ], "Kodiak", x))
dat_Seward <- lapply(2000:2017, function(x) make_jagsdat(int_boat[int_boat$year >= 2000, ], "Seward", x))
dat_Valdez <- lapply(2000:2017, function(x) make_jagsdat(int_boat[int_boat$year >= 2000, ], "Valdez", x))
dat_Whittier <- lapply(2000:2017, function(x) make_jagsdat(int_boat[int_boat$year >= 2000, ], "Whittier", x))



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
  for(p in 1:pred_n){
    pred[p,1:A] ~ dmulti(q_pred[p,1:A], 1)
    for(a in 1:A){
      re_pred[p,a] ~ dnorm(0, tau)
      q_pred[p,a] <- phi_pred[p,a]/sum(phi_pred[p,])
      log(phi_pred[p,a]) <- alpha[a] + re_pred[p,a]
    }
  }
}
", con="model_alpha_pred.txt")
modfile_alpha_pred <- 'model_alpha_pred.txt'

parameters_alpha_pred <- c("alpha", "sd", "pred")
ni <- 1E5; nb <- ni/2; nc <- 3; nt <- 20
post_alpha_pred_Homer <- 
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
  for(p in 1:pred_n){
    pred[p,1:A] ~ dmulti(q_pred[p,1:A], 1)
    for(a in 1:A){
      re_pred[p,a] ~ dnorm(0, tau)
      q_pred[p,a] <- phi_pred[p,a]/sum(phi_pred[p,])
      log(phi_pred[p,a]) <- alpha[a] + beta[pred_fleet[p] + 1,a] + re_pred[p,a]
    }
  }
}
", con="model_beta_pred.txt")
modfile_beta_pred <- 'model_beta_pred.txt'

parameters_beta_pred <- c("alpha", "beta", "sd", "pred")
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
  for(p in 1:pred_n){
    pred[p,1:A] ~ dmulti(q_pred[p,1:A], 1)
    for(a in 1:A){
      re_pred[p,a] ~ dnorm(0, tau)
      q_pred[p,a] <- phi_pred[p,a]/sum(phi_pred[p,])
      log(phi_pred[p,a]) <- alpha[a] + beta[pred_fleet[p] + 1,a] + epsilon[a]*pred_year[p] + re_pred[p,a]
    }
  }
}
", con="model_epsilon_pred.txt")
modfile_epsilon_pred <- 'model_epsilon_pred.txt'

parameters_epsilon_pred <- c("alpha", "beta", "epsilon", "sd", "pred")
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
  for(p in 1:pred_n){
    pred[p,1:A] ~ dmulti(q_pred[p,1:A], 1)
    for(a in 1:A){
      re_pred[p,a] ~ dnorm(0, tau)
      q_pred[p,a] <- phi_pred[p,a]/sum(phi_pred[p,])
      log(phi_pred[p,a]) <- alpha[a] + beta[pred_fleet[p] + 1,a] + epsilon[a]*pred_year[p] + gamma[pred_fleet[p] + 1,a]*pred_year[p] + re_pred[p,a]
    }
  }
}
", con="model_gamma_pred.txt")
modfile_gamma_pred <- 'model_gamma_pred.txt'

parameters_gamma_pred <- c("alpha", "beta", "epsilon", "gamma", "sd", "pred")
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





#get predictions for each posterior
get_pred <- function(post, dat, port){
  pred <- matrix(NA, nrow = dim(post$sims.list$pred)[2], ncol = post$mcmc.info$n.samples)
  areas <- sort(unique(dat$area[dat$port == port]))
  for(i in 1:post$mcmc.info$n.samples){
    pred[, i] <- apply(post$sims.list$pred[i,,], 1, function(x) areas[as.logical(x)])
  }
  pred
}
pred_alpha_Homer <- lapply(post_alpha_pred_Homer, get_pred, dat = int_boat, port = "Homer")
pred_alpha_Kodiak <- lapply(post_alpha_pred_Kodiak, get_pred, dat = int_boat, port = "Kodiak")
pred_alpha_Seward <- lapply(post_alpha_pred_Seward, get_pred, dat = int_boat, port = "Seward")
pred_alpha_Valdez <- lapply(post_alpha_pred_Valdez, get_pred, dat = int_boat, port = "Valdez")
pred_alpha_Whittier <- lapply(post_alpha_pred_Whittier, get_pred, dat = int_boat, port = "Whittier")
pred_beta_Homer <- lapply(post_beta_pred_Homer, get_pred, dat = int_boat, port = "Homer")
pred_beta_Kodiak <- lapply(post_beta_pred_Kodiak, get_pred, dat = int_boat, port = "Kodiak")
pred_beta_Seward <- lapply(post_beta_pred_Seward, get_pred, dat = int_boat, port = "Seward")
pred_beta_Valdez <- lapply(post_beta_pred_Valdez, get_pred, dat = int_boat, port = "Valdez")
pred_beta_Whittier <- lapply(post_beta_pred_Whittier, get_pred, dat = int_boat, port = "Whittier")
pred_epsilon_Homer <- lapply(post_epsilon_pred_Homer, get_pred, dat = int_boat, port = "Homer")
pred_epsilon_Kodiak <- lapply(post_epsilon_pred_Kodiak, get_pred, dat = int_boat, port = "Kodiak")
pred_epsilon_Seward <- lapply(post_epsilon_pred_Seward, get_pred, dat = int_boat, port = "Seward")
pred_epsilon_Valdez <- lapply(post_epsilon_pred_Valdez, get_pred, dat = int_boat, port = "Valdez")
pred_epsilon_Whittier <- lapply(post_epsilon_pred_Whittier, get_pred, dat = int_boat, port = "Whittier")
pred_gamma_Homer <- lapply(post_gamma_pred_Homer, get_pred, dat = int_boat, port = "Homer")
pred_gamma_Kodiak <- lapply(post_gamma_pred_Kodiak, get_pred, dat = int_boat, port = "Kodiak")
pred_gamma_Seward <- lapply(post_gamma_pred_Seward, get_pred, dat = int_boat, port = "Seward")
pred_gamma_Valdez <- lapply(post_gamma_pred_Valdez, get_pred, dat = int_boat, port = "Valdez")
pred_gamma_Whittier <- lapply(post_gamma_pred_Whittier, get_pred, dat = int_boat, port = "Whittier")

get_agree <- function(pred, obs){
  pred_f <- factor(pred, levels = unique(obs$pred_area))
  cf <- table(obs$pred_area, pred_f)  
  agree <- sum(diag(cf)) / sum(cf)
  agree}

hist(apply(pred_alpha_Homer[[1]], 2, function(x) get_agree(x, obs = dat_Homer[[1]])))

agree_alpha_Homer <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_alpha_Homer, y = dat_Homer)
agree_alpha_Kodiak <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_alpha_Kodiak, y = dat_Kodiak)
agree_alpha_Seward <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_alpha_Seward, y = dat_Seward)
agree_alpha_Valdez <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_alpha_Valdez, y = dat_Valdez)
agree_alpha_Whittier <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_alpha_Whittier, y = dat_Whittier)
agree_beta_Homer <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_beta_Homer, y = dat_Homer)
agree_beta_Kodiak <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_beta_Kodiak, y = dat_Kodiak)
agree_beta_Seward <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_beta_Seward, y = dat_Seward)
agree_beta_Valdez <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_beta_Valdez, y = dat_Valdez)
agree_beta_Whittier <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_beta_Whittier, y = dat_Whittier)
agree_epsilon_Homer <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_epsilon_Homer, y = dat_Homer)
agree_epsilon_Kodiak <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_epsilon_Kodiak, y = dat_Kodiak)
agree_epsilon_Seward <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_epsilon_Seward, y = dat_Seward)
agree_epsilon_Valdez <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_epsilon_Valdez, y = dat_Valdez)
agree_epsilon_Whittier <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_epsilon_Whittier, y = dat_Whittier)
agree_gamma_Homer <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_gamma_Homer, y = dat_Homer)
agree_gamma_Kodiak <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_gamma_Kodiak, y = dat_Kodiak)
agree_gamma_Seward <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_gamma_Seward, y = dat_Seward)
agree_gamma_Valdez <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_gamma_Valdez, y = dat_Valdez)
agree_gamma_Whittier <- mapply(function(x, y){apply(x, 2, get_agree, obs = y)}, x = pred_gamma_Whittier, y = dat_Whittier)

agree_Homer <- mget(ls(pattern = "^agree_.*_Homer"))
agree_Kodiak <- mget(ls(pattern = "^agree_.*_Kodiak"))
agree_Seward <- mget(ls(pattern = "^agree_.*_Seward"))
agree_Valdez <- mget(ls(pattern = "^agree_.*_Valdez"))
agree_Whittier <- mget(ls(pattern = "^agree_.*_Whittier"))

lapply(agree_Homer, function(x) apply(x, 2, mean)) %>% lapply(median)
lapply(agree_Homer, function(x) apply(x, 2, mean)) %>%
  as.data.frame() %>%
  dplyr::mutate(year = 2000:2017) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("agree")) %>%
  ggplot(aes(x = year, y = value, color = name)) +
    geom_line()

lapply(agree_Kodiak, function(x) apply(x, 2, mean)) %>% lapply(median)
lapply(agree_Kodiak, function(x) apply(x, 2, mean)) %>%
  as.data.frame() %>%
  dplyr::mutate(year = 2000:2017) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("agree")) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()

lapply(agree_Seward, function(x) apply(x, 2, mean)) %>% lapply(median)
lapply(agree_Seward, function(x) apply(x, 2, mean)) %>%
  as.data.frame() %>%
  dplyr::mutate(year = 2000:2017) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("agree")) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()

lapply(agree_Valdez, function(x) apply(x, 2, mean)) %>% lapply(median)
lapply(agree_Valdez, function(x) apply(x, 2, mean)) %>%
  as.data.frame() %>%
  dplyr::mutate(year = 2000:2017) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("agree")) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()

lapply(agree_Whittier, function(x) apply(x, 2, mean)) %>% lapply(median)
lapply(agree_Whittier, function(x) apply(x, 2, mean)) %>%
  as.data.frame() %>%
  dplyr::mutate(year = 2000:2017) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("agree")) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()





get_x <- function(dat, year, port){
  dat$yearc <- dat$year - mean(dat$year)
  temp <- dat[dat$year == year & dat$port == port, ]
  x <- matrix(NA, nrow = dim(temp)[1], ncol = 6)
  x[, 1] <- 1
  x[, 2] <- ifelse(temp$fleet == "Charter", 1, 0)
  x[, 3] <- ifelse(temp$fleet == "Private", 1, 0)
  x[, 4] <- dat$yearc[dat$year == year & dat$port == port]
  x[, 5] <- ifelse(temp$fleet == "Charter", dat$yearc[dat$year == year & dat$port == port], 0)
  x[, 6] <- ifelse(temp$fleet == "Private", dat$yearc[dat$year == year & dat$port == port], 0)
  x
}

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
  pred <- dmulti(q_pred, )
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

parameters_gamma <- c("alpha", "beta", "epsilon", "gamma", "sd")
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


plot_post <- function(post, dat, plotport){
  data <- dat[dat$port == plotport, c("year", "fleet", "area", "E")] %>%
    dplyr::arrange(area, fleet, year) %>%
    dplyr::mutate(yearc = year - median(year))
  data_full <- expand.grid(list(yearc = unique(data$yearc), 
                                fleet = unique(data$fleet), 
                                area = unique(data$area),
                                value = 0)) %>%
    dplyr::left_join(data, by = c("yearc", "fleet", "area")) %>%
    dplyr::mutate(E = ifelse(is.na(E), value, E),
                  area = factor(area, levels = unique(data$area)))
  
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

  p <- as.data.frame(exp(x%*%b)/apply(exp(x%*%b), 1, sum)) %>% setNames(unique(data$area));
  p$fleet <- rep(c("Charter", "Private"), each = length(unique(data_full$yearc)))
  p$yearc <- rep(min(data_full$yearc):max(data_full$yearc), times = 2)

  p <- 
    tidyr::pivot_longer(p, -c(fleet, yearc), names_to = "area") %>%
    dplyr::mutate(area = factor(area, levels = rev(unique(data$area)))) %>%
    dplyr::arrange(fleet, yearc, area) %>% 
    dplyr::group_by(fleet, yearc) %>% 
    dplyr::mutate(pct = cumsum(value))
  p$area <- factor(p$area, levels = unique(data$area))
  
  out <- data_full %>%
    ggplot(aes(x = yearc, weight = E, fill = area)) +
    geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
    geom_line(data = p, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, size = 1.1) +
    #  geom_line(data = p2, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, linetype = 1) +
    facet_grid(. ~ fleet) +
    scale_y_continuous(name = "Percent") +
    scale_x_continuous(name = "Year", breaks = seq(min(data$yearc), max(data$yearc), 3), labels = seq(min(data$year), max(data$year), 3)) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")
  
  return(out)
}

plot_post2 <- function(post, dat, plotport){
  data <- dat[dat$port == plotport, c("year", "fleet", "area", "E")] %>%
    dplyr::arrange(area, fleet, year) %>%
    dplyr::mutate(yearc = year - median(year))
  data_full <- expand.grid(list(yearc = unique(data$yearc), 
                                fleet = unique(data$fleet), 
                                area = unique(data$area),
                                value = 0)) %>%
    dplyr::left_join(data, by = c("yearc", "fleet", "area")) %>%
    dplyr::mutate(E = ifelse(is.na(E), value, E),
                  area = factor(area, levels = unique(data$area)))
  
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
  
  re <- function(post_re){
    re_mat <- matrix(NA, nrow = prod(dim(post_re[,,1])), ncol = dim(post_re)[3])
    for(i in 1:dim(post_re)[3]){re_mat[,i] <- c(post_re[,1,i], post_re[,2,i])}
    re_mat
  }
  
  p <- as.data.frame(exp(x%*%b)/apply(exp(x%*%b), 1, sum)) %>% setNames(unique(data$area));
  p$fleet <- rep(c("Charter", "Private"), each = length(unique(data_full$yearc)))
  p$yearc <- rep(min(data_full$yearc):max(data_full$yearc), times = 2)
  
  p <- 
    tidyr::pivot_longer(p, -c(fleet, yearc), names_to = "area") %>%
    dplyr::mutate(area = factor(area, levels = rev(unique(data$area)))) %>%
    dplyr::arrange(fleet, yearc, area) %>% 
    dplyr::group_by(fleet, yearc) %>% 
    dplyr::mutate(pct = cumsum(value))
  p$area <- factor(p$area, levels = unique(data$area))
  
  p2 <- as.data.frame(exp(x%*%b + re(post$q50$re))/apply(exp(x%*%b + re(post$q50$re)), 1, sum)) %>% setNames(unique(data$area));
  p2$fleet <- rep(c("Charter", "Private"), each = length(unique(data_full$yearc)))
  p2$yearc <- rep(min(data_full$yearc):max(data_full$yearc), times = 2)
  
  p2 <- 
    tidyr::pivot_longer(p2, -c(fleet, yearc), names_to = "area") %>%
    dplyr::mutate(area = factor(area, levels = rev(unique(data$area)))) %>%
    dplyr::arrange(fleet, yearc, area) %>% 
    dplyr::group_by(fleet, yearc) %>% 
    dplyr::mutate(pct = cumsum(value))
  p2$area <- factor(p2$area, levels = unique(data$area))
  
  out <- data_full %>%
    ggplot(aes(x = yearc, weight = E, fill = area)) +
    geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
    geom_line(data = p, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, size = 1.1) +
    geom_line(data = p2, aes(x = yearc, y = pct, color = area), inherit.aes = FALSE, linetype = 1) +
    facet_grid(. ~ fleet) +
    scale_y_continuous(name = "Percent") +
    scale_x_continuous(name = "Year", breaks = seq(min(data$yearc), max(data$yearc), 3), labels = seq(min(data$year), max(data$year), 3)) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")
  
  return(out)
}
  
p_post <- function(post, dat, plotport){
  data <- dat[dat$port == plotport, c("year", "fleet", "area", "E")] %>%
    dplyr::arrange(area, fleet, year) %>%
    dplyr::mutate(yearc = year - median(year))
  data_full <- expand.grid(list(yearc = unique(data$yearc), 
                                fleet = unique(data$fleet), 
                                area = unique(data$area),
                                value = 0)) %>%
    dplyr::left_join(data, by = c("yearc", "fleet", "area")) %>%
    dplyr::mutate(E = ifelse(is.na(E), value, E),
                  area = factor(area, levels = unique(data$area)))
  
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
  
  re <- function(post_re){
    re_mat <- matrix(NA, nrow = prod(dim(post_re)[1:2]), ncol = dim(post_re)[3])
    for(i in 1:dim(post_re)[3]){re_mat[,i] <- c(post_re[,1,i], post_re[,2,i])}
    re_mat
  }
  
  p <- as.data.frame(exp(x%*%b)/apply(exp(x%*%b), 1, sum)) %>% setNames(unique(data$area));
  # p$fleet <- rep(c("Charter", "Private"), each = length(unique(data_full$yearc)))
  # p$yearc <- rep(min(data_full$yearc):max(data_full$yearc), times = 2)
  
  p2 <- as.data.frame(exp(x%*%b + re(post$mean$re))/apply(exp(x%*%b + re(post$mean$re)), 1, sum)) %>% setNames(unique(data$area));
  # p2$fleet <- rep(c("Charter", "Private"), each = length(unique(data_full$yearc)))
  # p2$yearc <- rep(min(data_full$yearc):max(data_full$yearc), times = 2)
  
  list(p = p, p_re = p2)
}

p_4 <- p_post(post_epsilon[[4]], int_area[int_area$year >= 2000, ], ports[4])
ll <- rep(NA, dim(p_4[[1]])[1])
for(i in 1:dim(p_4[[1]])[1]) {ll[i] <- dmultinom(rbind(jags_dat[[4]]$count[,1,], jags_dat[[4]]$count[,2,])[i, ], 
                                                 prob = as.matrix(p_4[[2]][i, 1:3]), 
                                                 log = TRUE)}
-2*sum(ll)

y <- rbind(jags_dat[[4]]$count[,1,], jags_dat[[4]]$count[,2,])
p <- p_4$p_re[,1:3, ]
-2*sum(lgamma(apply(y, 1, sum) + 1) - lgamma(y + 1)[,1] - lgamma(y + 1)[,2] - lgamma(y + 1)[,3] + y[,1] * log(p[,1]) + y[,2] * log(p[,2]) + y[,3] * log(p[,3]))




get_devdata <- function(dat){rbind(dat$count[,1,], dat$count[,2,])}
dev_dat <- lapply(jags_dat, get_devdata)

get_deviance <- function(dat, preds){
  ll <- rep(NA, dim(preds[[1]])[1])
  for(i in 1:dim(preds[[1]])[1]) {ll[i] <- dmultinom(dat[i, ], 
                                                prob = as.matrix(preds[[1]][i,]), 
                                                log = TRUE)}
  -2*sum(ll)
}
preds_beta <-
  mapply(p_post, 
         post = post_beta, 
         plotport = ports, 
         MoreArgs = list(dat = int_area[int_area$year >= 2000, ]), 
         SIMPLIFY = FALSE)
mapply(get_deviance, dat = dev_dat, preds = preds_beta)
preds_epsilon <-
  mapply(p_post, 
         post = post_epsilon, 
         plotport = ports, 
         MoreArgs = list(dat = int_area[int_area$year >= 2000, ]), 
         SIMPLIFY = FALSE)
mapply(get_deviance, dat = dev_dat, preds = preds_epsilon)
preds_gamma <-
  mapply(p_post, 
         post = post_gamma, 
         plotport = ports, 
         MoreArgs = list(dat = int_area[int_area$year >= 2000, ]), 
         SIMPLIFY = FALSE)
mapply(get_deviance, dat = dev_dat, preds = preds_gamma)

preds_epsilon[[4]][1]
-2*sum(ll)
  rbind(jags_dat[[4]]$count[,1,], jags_dat[[4]]$count[,2,])[i, ]
  
  
  
get_params <- function(post){
  out <- 
    data.frame(intercept = post$mean$alpha,
               fleet = if(!is.null(post$mean$beta)){t(post$mean$beta)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2),
               year = if(!is.null(post$mean$epsilon)){post$mean$epsilon} else rep(0, length(post$mean$alpha)),
               year = if(!is.null(post$mean$gamma)){t(post$mean$gamma)} else matrix(0, nrow = length(post$mean$alpha), ncol = 2)) %>%
    as.matrix() %>%
    t()
  out
}
get_params(post_alpha[[1]])

int_boat
get_x <- function(dat, year, port){
  dat$yearc <- dat$year - mean(dat$year)
  temp <- dat[dat$year == year & dat$port == port, ]
  x <- matrix(NA, nrow = dim(temp)[1], ncol = 6)
  x[, 1] <- 1
  x[, 2] <- ifelse(temp$fleet == "Charter", 1, 0)
  x[, 3] <- ifelse(temp$fleet == "Private", 1, 0)
  x[, 4] <- dat$yearc[dat$year == year & dat$port == port]
  x[, 5] <- ifelse(temp$fleet == "Charter", dat$yearc[dat$year == year & dat$port == port], 0)
  x[, 6] <- ifelse(temp$fleet == "Private", dat$yearc[dat$year == year & dat$port == port], 0)
  x
}
get_response <- function(x, param){
  exp(x%*%param)/apply(exp(x%*%param), 1, sum)
}
get_response(get_x(int_boat, 2000, "Homer"), get_params(post_alpha[[1]]))

