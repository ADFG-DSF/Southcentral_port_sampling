int_boat <- readRDS(".\\Data\\int_boat.rds")
source(".\\functions.R")
library(magrittr)
library(ggplot2)

dat_Homer <- lapply(1993:2017, function(x) make_jagsdatpred(int_boat, "Homer", "lH", x))
dat_Kodiak <- lapply(1993:2017, function(x) make_jagsdatpred(int_boat, "Kodiak", x))
dat_Seward <- lapply(1993:2017, function(x) make_jagsdatpred(int_boat, "Seward", x))
dat_Valdez <- lapply(1993:2017, function(x) make_jagsdatpred(int_boat, "Valdez", x))
dat_Whittier <- lapply(1993:2017, function(x) make_jagsdatpred(int_boat, "Whittier", x))



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

parameters_alpha_pred <- c("alpha", "sd", "q_pred", "pred")
ni <- 1E5; nb <- ni/2; nc <- 3; nt <- 200
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
#Try a confustion matrix when the diagaonal is the agreement betweer predictions and observations
#Not sure I like this... too discrete
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

#Not sure how to evaluate cross-validated predictions
#Brier scores sum(pred_prob - obs)^2
#This score accounts for how likely the model was to predict something although results are similar to the agreement approach
get_Brier <- function(pred, obs, areas){
  obs_mat <- 
    lapply(obs$pred_area, function(x) as.numeric(x == areas)) %>% 
    do.call(rbind, .)
  devs <- vector(mode = "list", length = dim(pred$sims.list$q_pred)[1])
  mean_N <- vector(mode = "numeric", length = dim(pred$sims.list$q_pred)[1])
  mean_A <- vector(mode = "list", length = dim(pred$sims.list$q_pred)[1])
  for(i in 1:dim(pred$sims.list$q_pred)[1]){
    devs[[i]] <- (pred$sims.list$q_pred[i,,] - obs_mat)^2
    mean_A[[i]] <- apply(devs[[i]], 1, mean)
  }
  sapply(mean_A, mean)
}
areas_Homer <- sort(unique(int_boat$area[int_boat$port == "Homer"]))
Brier_alpha_Homer <- mapply(get_Brier, pred = post_alpha_pred_Homer, obs = dat_Homer, list(areas = areas_Homer))
Brier_beta_Homer <- mapply(get_Brier, pred = post_beta_pred_Homer, obs = dat_Homer, list(areas = areas_Homer))
Brier_epsilon_Homer <- mapply(get_Brier, pred = post_epsilon_pred_Homer, obs = dat_Homer, list(areas = areas_Homer))

data.frame(year = 2000:2017,
           alpha = apply(Brier_alpha_Homer, 2, mean),
           beta = apply(Brier_beta_Homer, 2, mean),
           epsilon = apply(Brier_epsilon_Homer, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
    geom_line()

data.frame(year = 2000:2017,
           alpha = apply(Brier_alpha_Homer, 2, mean),
           beta = apply(Brier_beta_Homer, 2, mean),
           epsilon = apply(Brier_epsilon_Homer, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  dplyr::group_by(name) %>% dplyr::summarise(value = mean(value))


areas_Kodiak <- sort(unique(int_boat$area[int_boat$port == "Kodiak"]))
Brier_alpha_Kodiak <- mapply(get_Brier, pred = post_alpha_pred_Kodiak, obs = dat_Kodiak, list(areas = areas_Kodiak))
Brier_beta_Kodiak <- mapply(get_Brier, pred = post_beta_pred_Kodiak, obs = dat_Kodiak, list(areas = areas_Kodiak))
Brier_epsilon_Kodiak <- mapply(get_Brier, pred = post_epsilon_pred_Kodiak, obs = dat_Kodiak, list(areas = areas_Kodiak))

data.frame(year = 2000:2017,
           alpha = apply(Brier_alpha_Kodiak, 2, mean),
           beta = apply(Brier_beta_Kodiak, 2, mean),
           epsilon = apply(Brier_epsilon_Kodiak, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()

data.frame(year = 2000:2017,
           alpha = apply(Brier_alpha_Kodiak, 2, mean),
           beta = apply(Brier_beta_Kodiak, 2, mean),
           epsilon = apply(Brier_epsilon_Kodiak, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  dplyr::group_by(name) %>% dplyr::summarise(value = mean(value))


areas_Seward <- sort(unique(int_boat$area[int_boat$port == "Seward"]))
Brier_alpha_Seward <- mapply(get_Brier, pred = post_alpha_pred_Seward, obs = dat_Seward, list(areas = areas_Seward))
Brier_beta_Seward <- mapply(get_Brier, pred = post_beta_pred_Seward, obs = dat_Seward, list(areas = areas_Seward))
Brier_epsilon_Seward <- mapply(get_Brier, pred = post_epsilon_pred_Seward, obs = dat_Seward, list(areas = areas_Seward))
Brier_gamma_Seward <- mapply(get_Brier, pred = post_gamma_pred_Seward, obs = dat_Seward, list(areas = areas_Seward))

data.frame(year = 2000:2017,
           alpha = apply(Brier_alpha_Seward, 2, mean),
           beta = apply(Brier_beta_Seward, 2, mean),
           epsilon = apply(Brier_epsilon_Seward, 2, mean),
           gamma = apply(Brier_gamma_Seward, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()

data.frame(year = 2000:2017,
           alpha = apply(Brier_alpha_Seward, 2, mean),
           beta = apply(Brier_beta_Seward, 2, mean),
           epsilon = apply(Brier_epsilon_Seward, 2, mean),
           gamma = apply(Brier_gamma_Seward, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  dplyr::group_by(name) %>% dplyr::summarise(value = mean(value))


areas_Valdez <- sort(unique(int_boat$area[int_boat$port == "Valdez"]))
Brier_alpha_Valdez <- mapply(get_Brier, pred = post_alpha_pred_Valdez, obs = dat_Valdez, list(areas = areas_Valdez))
Brier_beta_Valdez <- mapply(get_Brier, pred = post_beta_pred_Valdez, obs = dat_Valdez, list(areas = areas_Valdez))
Brier_epsilon_Valdez <- mapply(get_Brier, pred = post_epsilon_pred_Valdez, obs = dat_Valdez, list(areas = areas_Valdez))
Brier_gamma_Valdez <- mapply(get_Brier, pred = post_gamma_pred_Valdez, obs = dat_Valdez, list(areas = areas_Valdez))

data.frame(year = 2000:2017,
           alpha = apply(Brier_alpha_Valdez, 2, mean),
           beta = apply(Brier_beta_Valdez, 2, mean),
           epsilon = apply(Brier_epsilon_Valdez, 2, mean),
           gamma = apply(Brier_gamma_Valdez, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()

data.frame(year = 2000:2017,
           alpha = apply(Brier_alpha_Valdez, 2, mean),
           beta = apply(Brier_beta_Valdez, 2, mean),
           epsilon = apply(Brier_epsilon_Valdez, 2, mean),
           gamma = apply(Brier_gamma_Valdez, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  dplyr::group_by(name) %>% dplyr::summarise(value = mean(value))


areas_Whittier <- sort(unique(int_boat$area[int_boat$port == "Whittier"]))
Brier_alpha_Whittier <- mapply(get_Brier, pred = post_alpha_pred_Whittier, obs = dat_Whittier, list(areas = areas_Whittier))
Brier_beta_Whittier <- mapply(get_Brier, pred = post_beta_pred_Whittier, obs = dat_Whittier, list(areas = areas_Whittier))
Brier_epsilon_Whittier <- mapply(get_Brier, pred = post_epsilon_pred_Whittier, obs = dat_Whittier, list(areas = areas_Whittier))
Brier_gamma_Whittier <- mapply(get_Brier, pred = post_gamma_pred_Whittier, obs = dat_Whittier, list(areas = areas_Whittier))

data.frame(year = 2000:2017,
           alpha = apply(Brier_alpha_Whittier, 2, mean),
           beta = apply(Brier_beta_Whittier, 2, mean),
           epsilon = apply(Brier_epsilon_Whittier, 2, mean),
           gamma = apply(Brier_gamma_Whittier, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()

data.frame(year = 2000:2017,
           alpha = apply(Brier_alpha_Whittier, 2, mean),
           beta = apply(Brier_beta_Whittier, 2, mean),
           epsilon = apply(Brier_epsilon_Whittier, 2, mean),
           gamma = apply(Brier_gamma_Whittier, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  dplyr::group_by(name) %>% dplyr::summarise(value = mean(value))


#Not sure how to evaluate cross-validated predictions
#Maybe the most direct route is jsut to calcualte the log-liklihood for the left our observations given the modeled probabilities
#Seems like the best choice
get_llpred <- function(pred, obs, areas){
  obs_mat <- 
    lapply(obs$pred_area, function(x) as.numeric(x == areas)) %>% 
    do.call(rbind, .)
  ll <- vector(mode = "list", length = dim(pred$sims.list$q_pred)[1])
  sum_ll <- vector(mode = "numeric", length = dim(pred$sims.list$q_pred)[1])
  for(i in 1:dim(pred$sims.list$q_pred)[1]){
    ll[[i]] <- dmultinom(obs_mat, prob = pred$sims.list$q_pred[i,,], log = TRUE)
    sum_ll[[i]] <- sum(ll[[i]])
  }
  sum_ll
}

areas_Homer <- sort(unique(int_boat$area[int_boat$port == "Homer"]))
llpred_alpha_Homer <- mapply(get_llpred, pred = post_alpha_pred_Homer, obs = dat_Homer, list(areas = areas_Homer))
llpred_beta_Homer <- mapply(get_llpred, pred = post_beta_pred_Homer, obs = dat_Homer, list(areas = areas_Homer))
llpred_epsilon_Homer <- mapply(get_llpred, pred = post_epsilon_pred_Homer, obs = dat_Homer, list(areas = areas_Homer))

data.frame(year = 2000:2017,
           alpha = apply(llpred_alpha_Homer, 2, mean),
           beta = apply(llpred_beta_Homer, 2, mean),
           epsilon = apply(llpred_epsilon_Homer, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()

data.frame(year = 2000:2017,
           alpha = apply(llpred_alpha_Homer, 2, mean),
           beta = apply(llpred_beta_Homer, 2, mean),
           epsilon = apply(llpred_epsilon_Homer, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  dplyr::group_by(name) %>% dplyr::summarise(value = mean(value))


areas_Kodiak <- sort(unique(int_boat$area[int_boat$port == "Kodiak"]))
llpred_alpha_Kodiak <- mapply(get_llpred, pred = post_alpha_pred_Kodiak, obs = dat_Kodiak, list(areas = areas_Kodiak))
llpred_beta_Kodiak <- mapply(get_llpred, pred = post_beta_pred_Kodiak, obs = dat_Kodiak, list(areas = areas_Kodiak))
llpred_epsilon_Kodiak <- mapply(get_llpred, pred = post_epsilon_pred_Kodiak, obs = dat_Kodiak, list(areas = areas_Kodiak))

data.frame(year = 2000:2017,
           alpha = apply(llpred_alpha_Kodiak, 2, mean),
           beta = apply(llpred_beta_Kodiak, 2, mean),
           epsilon = apply(llpred_epsilon_Kodiak, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()

data.frame(year = 2000:2017,
           alpha = apply(llpred_alpha_Kodiak, 2, mean),
           beta = apply(llpred_beta_Kodiak, 2, mean),
           epsilon = apply(llpred_epsilon_Kodiak, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  dplyr::group_by(name) %>% dplyr::summarise(value = mean(value))


areas_Seward <- sort(unique(int_boat$area[int_boat$port == "Seward"]))
llpred_alpha_Seward <- mapply(get_llpred, pred = post_alpha_pred_Seward, obs = dat_Seward, list(areas = areas_Seward))
llpred_beta_Seward <- mapply(get_llpred, pred = post_beta_pred_Seward, obs = dat_Seward, list(areas = areas_Seward))
llpred_epsilon_Seward <- mapply(get_llpred, pred = post_epsilon_pred_Seward, obs = dat_Seward, list(areas = areas_Seward))
llpred_gamma_Seward <- mapply(get_llpred, pred = post_gamma_pred_Seward, obs = dat_Seward, list(areas = areas_Seward))

data.frame(year = 2000:2017,
           alpha = apply(llpred_alpha_Seward, 2, mean),
           beta = apply(llpred_beta_Seward, 2, mean),
           epsilon = apply(llpred_epsilon_Seward, 2, mean),
           gamma = apply(llpred_gamma_Seward, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()

data.frame(year = 2000:2017,
           alpha = apply(llpred_alpha_Seward, 2, mean),
           beta = apply(llpred_beta_Seward, 2, mean),
           epsilon = apply(llpred_epsilon_Seward, 2, mean),
           gamma = apply(llpred_gamma_Seward, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  dplyr::group_by(name) %>% dplyr::summarise(value = mean(value))


areas_Valdez <- sort(unique(int_boat$area[int_boat$port == "Valdez"]))
llpred_alpha_Valdez <- mapply(get_llpred, pred = post_alpha_pred_Valdez, obs = dat_Valdez, list(areas = areas_Valdez))
llpred_beta_Valdez <- mapply(get_llpred, pred = post_beta_pred_Valdez, obs = dat_Valdez, list(areas = areas_Valdez))
llpred_epsilon_Valdez <- mapply(get_llpred, pred = post_epsilon_pred_Valdez, obs = dat_Valdez, list(areas = areas_Valdez))
llpred_gamma_Valdez <- mapply(get_llpred, pred = post_gamma_pred_Valdez, obs = dat_Valdez, list(areas = areas_Valdez))

data.frame(year = 2000:2017,
           alpha = apply(llpred_alpha_Valdez, 2, mean),
           beta = apply(llpred_beta_Valdez, 2, mean),
           epsilon = apply(llpred_epsilon_Valdez, 2, mean),
           gamma = apply(llpred_gamma_Valdez, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()

data.frame(year = 2000:2017,
           alpha = apply(llpred_alpha_Valdez, 2, mean),
           beta = apply(llpred_beta_Valdez, 2, mean),
           epsilon = apply(llpred_epsilon_Valdez, 2, mean),
           gamma = apply(llpred_gamma_Valdez, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  dplyr::group_by(name) %>% dplyr::summarise(value = mean(value))


areas_Whittier <- sort(unique(int_boat$area[int_boat$port == "Whittier"]))
llpred_alpha_Whittier <- mapply(get_llpred, pred = post_alpha_pred_Whittier, obs = dat_Whittier, list(areas = areas_Whittier))
llpred_beta_Whittier <- mapply(get_llpred, pred = post_beta_pred_Whittier, obs = dat_Whittier, list(areas = areas_Whittier))
llpred_epsilon_Whittier <- mapply(get_llpred, pred = post_epsilon_pred_Whittier, obs = dat_Whittier, list(areas = areas_Whittier))
llpred_gamma_Whittier <- mapply(get_llpred, pred = post_gamma_pred_Whittier, obs = dat_Whittier, list(areas = areas_Whittier))

data.frame(year = 2000:2017,
           alpha = apply(llpred_alpha_Whittier, 2, mean),
           beta = apply(llpred_beta_Whittier, 2, mean),
           epsilon = apply(llpred_epsilon_Whittier, 2, mean),
           gamma = apply(llpred_gamma_Whittier, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line()

data.frame(year = 2000:2017,
           alpha = apply(llpred_alpha_Whittier, 2, mean),
           beta = apply(llpred_beta_Whittier, 2, mean),
           epsilon = apply(llpred_epsilon_Whittier, 2, mean),
           gamma = apply(llpred_gamma_Whittier, 2, mean)) %>%
  tidyr::pivot_longer(-year) %>%
  dplyr::group_by(name) %>% dplyr::summarise(value = mean(value))
