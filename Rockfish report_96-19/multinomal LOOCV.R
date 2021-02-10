int_boat <- readRDS(".\\Data\\int_boat.rds")
source(".\\functions.R")
library(magrittr)
library(ggplot2)



# Pelagic Rockfish ---------------------------------------------------------
int_area <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarize(H = sum(pH), E = sum(E)) %>%
  dplyr::ungroup()
dat_Homer <- lapply(1996:2019, function(x) make_jagsdatpred(int_area, "Homer", "H", x))
dat_Kodiak <- lapply(1996:2019, function(x) make_jagsdatpred(int_area, "Kodiak", "H", x))
dat_Seward <- lapply(1996:2019, function(x) make_jagsdatpred(int_area, "Seward", "H", x))
dat_Valdez <- lapply(1996:2019, function(x) make_jagsdatpred(int_area, "Valdez", "H", x))
dat_Whittier <- lapply(2000:2019, function(x) make_jagsdatpred(int_area, "Whittier", "H", x))



# * Alpha model -------------------------------------------------------------
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
ni <- 1E5; nb <- ni/2; nc <- 3; nt <- 200
# post_alpha_pred_Homer <- 
#   lapply(dat_Homer, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#                      OLREjags_re <- jagsUI::jags(x2, 
#                                                  parameters.to.save = parameters_alpha_pred, 
#                                                  model.file = modfile_alpha_pred, 
#                                                  n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                                  parallel = TRUE)})
# saveRDS(post_alpha_pred_Homer, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Homer.rds")
# post_alpha_pred_Kodiak <- 
#   lapply(dat_Kodiak, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_alpha_pred, 
#                                      model.file = modfile_alpha_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_alpha_pred_Kodiak, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Kodiak.rds")
# 
# post_alpha_pred_Seward <- 
#   lapply(dat_Seward, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_alpha_pred, 
#                                      model.file = modfile_alpha_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_alpha_pred_Seward, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Seward.rds")
# 
# post_alpha_pred_Valdez <- 
#   lapply(dat_Valdez, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_alpha_pred, 
#                                      model.file = modfile_alpha_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_alpha_pred_Valdez, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Valdez.rds")
# 
# post_alpha_pred_Whittier <- 
#   lapply(dat_Whittier, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_alpha_pred, 
#                                      model.file = modfile_alpha_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_alpha_pred_Whittier, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Whittier.rds")



#  * Beta model -----------------------------------------------------------
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
# post_beta_pred_Homer <- 
#   lapply(dat_Homer, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_beta_pred, 
#                                      model.file = modfile_beta_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_beta_pred_Homer, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Homer.rds")
# 
# post_beta_pred_Kodiak <- 
#   lapply(dat_Kodiak, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_beta_pred, 
#                                      model.file = modfile_beta_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_beta_pred_Kodiak, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Kodiak.rds")
# 
# post_beta_pred_Seward <- 
#   lapply(dat_Seward, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_beta_pred, 
#                                      model.file = modfile_beta_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_beta_pred_Seward, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Seward.rds")
# 
# post_beta_pred_Valdez <- 
#   lapply(dat_Valdez, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_beta_pred, 
#                                      model.file = modfile_beta_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_beta_pred_Valdez, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Valdez.rds")
# 
# post_beta_pred_Whittier <- 
#   lapply(dat_Whittier, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_beta_pred, 
#                                      model.file = modfile_beta_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_beta_pred_Whittier, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Whittier.rds")





#  * Epsilon model ----------------------------------------------------------
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
# post_epsilon_pred_Homer <- 
#   lapply(dat_Homer, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_epsilon_pred, 
#                                      model.file = modfile_epsilon_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_epsilon_pred_Homer, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Homer.rds")
# 
# post_epsilon_pred_Kodiak <- 
#   lapply(dat_Kodiak, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_epsilon_pred, 
#                                      model.file = modfile_epsilon_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_epsilon_pred_Kodiak, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Kodiak.rds")
# 
# post_epsilon_pred_Seward <- 
#   lapply(dat_Seward, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_epsilon_pred, 
#                                      model.file = modfile_epsilon_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_epsilon_pred_Seward, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Seward.rds")
# 
# post_epsilon_pred_Valdez <- 
#   lapply(dat_Valdez, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_epsilon_pred, 
#                                      model.file = modfile_epsilon_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_epsilon_pred_Valdez, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Valdez.rds")
# 
# post_epsilon_pred_Whittier <- 
#   lapply(dat_Whittier, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_epsilon_pred, 
#                                      model.file = modfile_epsilon_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_epsilon_pred_Whittier, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Whittier.rds")



#  * Gamma model ----------------------------------------------------------
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
# post_gamma_pred_Homer <- 
#   lapply(dat_Homer, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_gamma_pred, 
#                                      model.file = modfile_gamma_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_gamma_pred_Homer, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Homer.rds")
# 
# post_gamma_pred_Kodiak <- 
#   lapply(dat_Kodiak, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_gamma_pred, 
#                                      model.file = modfile_gamma_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_gamma_pred_Kodiak, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Kodiak.rds")
# 
# post_gamma_pred_Seward <- 
#   lapply(dat_Seward, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_gamma_pred, 
#                                      model.file = modfile_gamma_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_gamma_pred_Seward, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Seward.rds")
# 
# post_gamma_pred_Valdez <- 
#   lapply(dat_Valdez, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_gamma_pred, 
#                                      model.file = modfile_gamma_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_gamma_pred_Valdez, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Valdez.rds")
# 
# post_gamma_pred_Whittier <- 
#   lapply(dat_Whittier, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_gamma_pred, 
#                                      model.file = modfile_gamma_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# saveRDS(post_gamma_pred_Whittier, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Whittier.rds")

#  * Homer Charter Only  ----------------------------------------------------------
#jags w overdisersion on each observation
#area mean
writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(y in 1:Y){ # loop around years
    count[y,1,1:A] ~ dmulti(q[y,1:A], M[y,1])
    for(a in 1:A){
      re[y,a] ~ dnorm(0, tau)
      q[y,a] <- phi[y,a]/sum(phi[y,])
      log(phi[y,a]) <- alpha[a] + re[y,a]
    }
  }
  pred_count[1, 1:A] ~ dmulti(q_pred[1:A], pred_M[1])
  for(a in 1:A){
    re_pred[a] ~ dnorm(0, tau)
    q_pred[a] <- phi_pred[a]/sum(phi_pred[])
    log(phi_pred[a]) <- alpha[a] + re_pred[a]
  }
}
", con="model_alpha0_pred.txt")
modfile_alpha0_pred <- 'model_alpha0_pred.txt'
ni <- 5E5; nb <- ni/2; nc <- 3; nt <- 100
post_alpha0_pred_Homer <- 
  lapply(dat_Homer, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha0_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha0_pred_Homer, ".\\Rockfish report_96-19\\Interview post\\post_alpha0_pred_Homer.rds")


#Homer Charter only
#area slopes
writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}

  epsilon[1] <- 0 #area baseline
  for (a in 2:A){epsilon[a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(y in 1:Y){ # loop around years
    count[y,1,1:A] ~ dmulti(q[y,1:A], M[y,1])
    for(a in 1:A){
      re[y,a] ~ dnorm(0, tau)
      q[y,a] <- phi[y,a]/sum(phi[y,])
      log(phi[y,a]) <- alpha[a] + epsilon[a]*yearc[y] + re[y,a]
    }
  }
  pred_count[1,1:A] ~ dmulti(q_pred[1:A], pred_M[1])
    for(a in 1:A){
      re_pred[a] ~ dnorm(0, tau)
      q_pred[a] <- phi_pred[a]/sum(phi_pred[])
      log(phi_pred[a]) <- alpha[a] + epsilon[a]*pred_year[1] + re_pred[a]
    }
}
", con="model_epsilon0_pred.txt")
modfile_epsilon0_pred <- 'model_epsilon0_pred.txt'
post_epsilon0_pred_Homer <- 
  lapply(dat_Homer, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon0_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon0_pred_Homer, ".\\Rockfish report_96-19\\Interview post\\post_epsilon0_pred_Homer.rds")

# post_alpha_pred_Homer <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Homer.rds")
# post_alpha_pred_Kodiak <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Kodiak.rds")
# post_alpha_pred_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Seward.rds")
# post_alpha_pred_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Valdez.rds")
# post_alpha_pred_Whittier <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Whittier.rds")
# post_beta_pred_Homer <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Homer.rds")
# post_beta_pred_Kodiak <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Kodiak.rds")
# post_beta_pred_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Seward.rds")
# post_beta_pred_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Valdez.rds")
# post_beta_pred_Whittier <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Whittier.rds")
# post_epsilon_pred_Homer <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Homer.rds")
# post_epsilon_pred_Kodiak <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Kodiak.rds")
# post_epsilon_pred_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Seward.rds")
# post_epsilon_pred_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Valdez.rds")
# post_epsilon_pred_Whittier <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Whittier.rds")
# post_gamma_pred_Homer <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Homer.rds")
# post_gamma_pred_Kodiak <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Kodiak.rds")
# post_gamma_pred_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Seward.rds")
# post_gamma_pred_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Valdez.rds")
# post_gamma_pred_Whittier <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Whittier.rds")

#  * ll calcs -------------------------------------------------------------
#Not sure how to evaluate cross-validated predictions
#Maybe the most direct route is just to calcualte the log-liklihood for the left our observations given the modeled probabilities
#Seems like the best choice
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

get_llpred0 <- function(pred, obs){
  ll <- vector("numeric", length = dim(pred$sims.list$q_pred)[1])
  for(i in 1:dim(pred$sims.list$q_pred)[1]){
    ll[i] <- dmultinom(obs$lo_count[1, ], prob = pred$sims.list$q_pred[i, ], log = TRUE)
  }
  ll
}
ll_Homer0 <- 
  data.frame(alpha = Map(get_llpred0, pred = post_alpha0_pred_Homer, obs = dat_Homer) %>% Reduce('+', .),
             epsilon = Map(get_llpred0, pred = post_epsilon0_pred_Homer, obs = dat_Homer) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:epsilon)
aggregate(value ~ name, data = ll_Homer0, FUN = median)
ggplot(data = ll_Homer0, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer0, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer0, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer0, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
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
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Whittier, obs = dat_Whittier) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Whittier, obs = dat_Whittier) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Whittier, obs = dat_Whittier) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Whittier, obs = dat_Whittier) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Whittier, FUN = median)
ggplot(data = ll_Whittier, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Whittier, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Whittier, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Whittier, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)








# Non-Pelagic Rockfish ----------------------------------------------------
int_area_npy <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarize(H = sum(npyH), E = sum(E)) %>%
  dplyr::ungroup()
dat_Homer_npy<- lapply(1996:2019, function(x) make_jagsdatpred(int_area_npy, "Homer", "H", x))
dat_Kodiak_npy<- lapply(1996:2019, function(x) make_jagsdatpred(int_area_npy, "Kodiak", "H", x))
dat_Seward_npy<- lapply(1996:2019, function(x) make_jagsdatpred(int_area_npy, "Seward", "H", x))
dat_Valdez_npy<- lapply(1996:2019, function(x) make_jagsdatpred(int_area_npy, "Valdez", "H", x))
dat_Whittier_npy<- lapply(2000:2019, function(x) make_jagsdatpred(int_area_npy, "Whittier", "H", x))

ni <- 1E5; nb <- ni/2; nc <- 3; nt <- 200
#  * Alpha model ----------------------------------------------------------
post_alpha_pred_Homer_npy<- 
  lapply(dat_Homer_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Homer_npy, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Homer_npy.rds")
post_alpha_pred_Kodiak_npy<- 
  lapply(dat_Kodiak_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Kodiak_npy, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Kodiak_npy.rds")

post_alpha_pred_Seward_npy<- 
  lapply(dat_Seward_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Seward_npy, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Seward_npy.rds")

post_alpha_pred_Valdez_npy<- 
  lapply(dat_Valdez_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Valdez_npy, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Valdez_npy.rds")

post_alpha_pred_Whittier_npy<- 
  lapply(dat_Whittier_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Whittier_npy, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Whittier_npy.rds")



#  * Beta model -----------------------------------------------------------
post_beta_pred_Homer_npy<- 
  lapply(dat_Homer_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Homer_npy, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Homer_npy.rds")

post_beta_pred_Kodiak_npy<- 
  lapply(dat_Kodiak_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Kodiak_npy, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Kodiak_npy.rds")

post_beta_pred_Seward_npy<- 
  lapply(dat_Seward_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Seward_npy, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Seward_npy.rds")

post_beta_pred_Valdez_npy<- 
  lapply(dat_Valdez_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Valdez_npy, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Valdez_npy.rds")

post_beta_pred_Whittier_npy<- 
  lapply(dat_Whittier_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Whittier_npy, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Whittier_npy.rds")




# *  Epsilon model --------------------------------------------------------
post_epsilon_pred_Homer_npy<- 
  lapply(dat_Homer_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Homer_npy, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Homer_npy.rds")

post_epsilon_pred_Kodiak_npy<- 
  lapply(dat_Kodiak_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Kodiak_npy, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Kodiak_npy.rds")

post_epsilon_pred_Seward_npy<- 
  lapply(dat_Seward_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Seward_npy, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Seward_npy.rds")

post_epsilon_pred_Valdez_npy<- 
  lapply(dat_Valdez_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Valdez_npy, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Valdez_npy.rds")

post_epsilon_pred_Whittier_npy<- 
  lapply(dat_Whittier_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Whittier_npy, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Whittier_npy.rds")




#  * Gamma model ----------------------------------------------------------
post_gamma_pred_Homer_npy<- 
  lapply(dat_Homer_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Homer_npy, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Homer_npy.rds")

post_gamma_pred_Kodiak_npy<- 
  lapply(dat_Kodiak_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Kodiak_npy, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Kodiak_npy.rds")

post_gamma_pred_Seward_npy<- 
  lapply(dat_Seward_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Seward_npy, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Seward_npy.rds")

post_gamma_pred_Valdez_npy<- 
  lapply(dat_Valdez_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Valdez_npy, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Valdez_npy.rds")

post_gamma_pred_Whittier_npy<- 
  lapply(dat_Whittier_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Whittier_npy, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Whittier_npy.rds")

#  * Homer Charter Only  ----------------------------------------------------------
ni <- 5E5; nb <- ni/2; nc <- 3; nt <- 100
post_alpha0_pred_Homer_npy <- 
  lapply(dat_Homer_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha0_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha0_pred_Homer_npy, ".\\Rockfish report_96-19\\Interview post\\post_alpha0_pred_Homer_npy.rds")

post_epsilon0_pred_Homer_npy <- 
  lapply(dat_Homer_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon0_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon0_pred_Homer_npy, ".\\Rockfish report_96-19\\Interview post\\post_epsilon0_pred_Homer_npy.rds")


# post_alpha_pred_Homer_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Homer_npy.rds")
# post_alpha_pred_Kodiak_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Kodiak_npy.rds")
# post_alpha_pred_Seward_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Seward_npy.rds")
# post_alpha_pred_Valdez_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Valdez_npy.rds")
# post_alpha_pred_Whittier_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Whittier_npy.rds")
# post_beta_pred_Homer_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Homer_npy.rds")
# post_beta_pred_Kodiak_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Kodiak_npy.rds")
# post_beta_pred_Seward_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Seward_npy.rds")
# post_beta_pred_Valdez_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Valdez_npy.rds")
# post_beta_pred_Whittier_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Whittier_npy.rds")
# post_epsilon_pred_Homer_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Homer_npy.rds")
# post_epsilon_pred_Kodiak_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Kodiak_npy.rds")
# post_epsilon_pred_Seward_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Seward_npy.rds")
# post_epsilon_pred_Valdez_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Valdez_npy.rds")
# post_epsilon_pred_Whittier_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Whittier_npy.rds")
# post_gamma_pred_Homer_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Homer_npy.rds")
# post_gamma_pred_Kodiak_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Kodiak_npy.rds")
# post_gamma_pred_Seward_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Seward_npy.rds")
# post_gamma_pred_Valdez_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Valdez_npy.rds")
# post_gamma_pred_Whittier_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Whittier_npy.rds")

# *  ll calcs ---------------------------------------------------------------
#Not sure how to evaluate cross-validated predictions
#Maybe the most direct route is just to calcualte the log-liklihood for the left our observations given the modeled probabilities
#Seems like the best choice
ll_Homer_npy<- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Homer_npy, obs = dat_Homer_npy) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Homer_npy, obs = dat_Homer_npy) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Homer_npy, obs = dat_Homer_npy) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Homer_npy, obs = dat_Homer_npy) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Homer_npy, FUN = median)
ggplot(data = ll_Homer_npy, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer_npy, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer_npy, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer_npy, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)


ll_Homer0_npy <- 
  data.frame(alpha = Map(get_llpred0, pred = post_alpha0_pred_Homer_npy, obs = dat_Homer_npy) %>% Reduce('+', .),
             epsilon = Map(get_llpred0, pred = post_epsilon0_pred_Homer_npy, obs = dat_Homer_npy) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:epsilon)
aggregate(value ~ name, data = ll_Homer0_npy, FUN = median)
ggplot(data = ll_Homer0_npy, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer0_npy, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer0_npy, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer0_npy, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)


ll_Kodiak_npy<- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Kodiak_npy, obs = dat_Kodiak_npy) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Kodiak_npy, obs = dat_Kodiak_npy) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Kodiak_npy, obs = dat_Kodiak_npy) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Kodiak_npy, obs = dat_Kodiak_npy) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Kodiak_npy, FUN = median)
ggplot(data = ll_Kodiak_npy, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Kodiak_npy, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Kodiak_npy, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Kodiak_npy, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)

ll_Seward_npy<- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Seward_npy, obs = dat_Seward_npy) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Seward_npy, obs = dat_Seward_npy) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Seward_npy, obs = dat_Seward_npy) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Seward_npy, obs = dat_Seward_npy) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Seward_npy, FUN = median)
ggplot(data = ll_Seward_npy, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Seward_npy, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Seward_npy, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Seward_npy, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)

ll_Valdez_npy<- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Valdez_npy, obs = dat_Valdez_npy) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Valdez_npy, obs = dat_Valdez_npy) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Valdez_npy, obs = dat_Valdez_npy) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Valdez_npy, obs = dat_Valdez_npy) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Valdez_npy, FUN = median)
ggplot(data = ll_Valdez_npy, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Valdez_npy, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Valdez_npy, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Valdez_npy, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)


ll_Whittier_npy<- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Whittier_npy, obs = dat_Whittier_npy) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Whittier_npy, obs = dat_Whittier_npy) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Whittier_npy, obs = dat_Whittier_npy) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Whittier_npy, obs = dat_Whittier_npy) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Whittier_npy, FUN = median)
ggplot(data = ll_Whittier_npy, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Whittier_npy, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Whittier_npy, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Whittier_npy, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)




# Effort ----------------------------------------------------
int_area_E <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarize(H = sum(npyH), E = sum(E)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(year >= 2000)
dat_Homer_E<- lapply(2000:2019, function(x) make_jagsdatpred(int_area_E, "Homer", "E", x))
dat_Kodiak_E<- lapply(2000:2019, function(x) make_jagsdatpred(int_area_E, "Kodiak", "E", x))
dat_Seward_E<- lapply(2000:2019, function(x) make_jagsdatpred(int_area_E, "Seward", "E", x))
dat_Valdez_E<- lapply(2000:2019, function(x) make_jagsdatpred(int_area_E, "Valdez", "E", x))
dat_Whittier_E<- lapply(2000:2019, function(x) make_jagsdatpred(int_area_E, "Whittier", "E", x))

ni <- 1E5; nb <- ni/2; nc <- 3; nt <- 200
#  * Alpha model ----------------------------------------------------------
post_alpha_pred_Homer_E<- 
  lapply(dat_Homer_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Homer_E, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Homer_E.rds")
post_alpha_pred_Kodiak_E<- 
  lapply(dat_Kodiak_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Kodiak_E, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Kodiak_E.rds")

post_alpha_pred_Seward_E<- 
  lapply(dat_Seward_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Seward_E, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Seward_E.rds")

post_alpha_pred_Valdez_E<- 
  lapply(dat_Valdez_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Valdez_E, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Valdez_E.rds")

post_alpha_pred_Whittier_E<- 
  lapply(dat_Whittier_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_alpha_pred_Whittier_E, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Whittier_E.rds")



#  * Beta model -----------------------------------------------------------
post_beta_pred_Homer_E<- 
  lapply(dat_Homer_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Homer_E, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Homer_E.rds")

post_beta_pred_Kodiak_E<- 
  lapply(dat_Kodiak_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Kodiak_E, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Kodiak_E.rds")

post_beta_pred_Seward_E<- 
  lapply(dat_Seward_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Seward_E, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Seward_E.rds")

post_beta_pred_Valdez_E<- 
  lapply(dat_Valdez_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Valdez_E, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Valdez_E.rds")

post_beta_pred_Whittier_E<- 
  lapply(dat_Whittier_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_beta_pred_Whittier_E, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Whittier_E.rds")




# *  Epsilon model --------------------------------------------------------
post_epsilon_pred_Homer_E<- 
  lapply(dat_Homer_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Homer_E, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Homer_E.rds")

post_epsilon_pred_Kodiak_E<- 
  lapply(dat_Kodiak_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Kodiak_E, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Kodiak_E.rds")

post_epsilon_pred_Seward_E<- 
  lapply(dat_Seward_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Seward_E, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Seward_E.rds")

post_epsilon_pred_Valdez_E<- 
  lapply(dat_Valdez_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Valdez_E, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Valdez_E.rds")

post_epsilon_pred_Whittier_E<- 
  lapply(dat_Whittier_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_epsilon_pred_Whittier_E, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Whittier_E.rds")




#  * Gamma model ----------------------------------------------------------
post_gamma_pred_Homer_E<- 
  lapply(dat_Homer_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Homer_E, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Homer_E.rds")

post_gamma_pred_Kodiak_E<- 
  lapply(dat_Kodiak_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Kodiak_E, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Kodiak_E.rds")

post_gamma_pred_Seward_E<- 
  lapply(dat_Seward_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Seward_E, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Seward_E.rds")

post_gamma_pred_Valdez_E<- 
  lapply(dat_Valdez_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Valdez_E, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Valdez_E.rds")

post_gamma_pred_Whittier_E<- 
  lapply(dat_Whittier_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(post_gamma_pred_Whittier_E, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Whittier_E.rds")


# *  ll calcs ---------------------------------------------------------------
#Not sure how to evaluate cross-validated predictions
#Maybe the most direct route is just to calcualte the log-liklihood for the left our observations given the modeled probabilities
#Seems like the best choice
ll_Homer_E<- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Homer_E, obs = dat_Homer_E) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Homer_E, obs = dat_Homer_E) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Homer_E, obs = dat_Homer_E) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Homer_E, obs = dat_Homer_E) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Homer_E, FUN = median)
ggplot(data = ll_Homer_E, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer_E, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer_E, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Homer_E, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)


ll_Kodiak_E<- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Kodiak_E, obs = dat_Kodiak_E) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Kodiak_E, obs = dat_Kodiak_E) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Kodiak_E, obs = dat_Kodiak_E) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Kodiak_E, obs = dat_Kodiak_E) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Kodiak_E, FUN = median)
ggplot(data = ll_Kodiak_E, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Kodiak_E, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Kodiak_E, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Kodiak_E, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)

ll_Seward_E<- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Seward_E, obs = dat_Seward_E) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Seward_E, obs = dat_Seward_E) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Seward_E, obs = dat_Seward_E) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Seward_E, obs = dat_Seward_E) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Seward_E, FUN = median)
ggplot(data = ll_Seward_E, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Seward_E, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Seward_E, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Seward_E, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)

ll_Valdez_E<- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Valdez_E, obs = dat_Valdez_E) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Valdez_E, obs = dat_Valdez_E) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Valdez_E, obs = dat_Valdez_E) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Valdez_E, obs = dat_Valdez_E) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Valdez_E, FUN = median)
ggplot(data = ll_Valdez_E, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Valdez_E, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Valdez_E, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Valdez_E, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)


ll_Whittier_E<- 
  data.frame(alpha = Map(get_llpred, pred = post_alpha_pred_Whittier_E, obs = dat_Whittier_E) %>% Reduce('+', .),
             beta = Map(get_llpred, pred = post_beta_pred_Whittier_E, obs = dat_Whittier_E) %>% Reduce('+', .),
             epsilon = Map(get_llpred, pred = post_epsilon_pred_Whittier_E, obs = dat_Whittier_E) %>% Reduce('+', .),
             gamma = Map(get_llpred, pred = post_gamma_pred_Whittier_E, obs = dat_Whittier_E) %>% Reduce('+', .)) %>%
  tidyr::pivot_longer(cols = alpha:gamma)
aggregate(value ~ name, data = ll_Whittier_E, FUN = median)
ggplot(data = ll_Whittier_E, aes(x = value)) + 
  geom_histogram() +
  geom_vline(data = aggregate(value ~ name, data = ll_Whittier_E, FUN = median), aes(xintercept = value), col='red', size=2) +
  geom_vline(data = aggregate(value ~ name, data = ll_Whittier_E, FUN = quantile, probs = c(0.1)), aes(xintercept = value), col='green', size=1) +
  geom_vline(data = aggregate(value ~ name, data = ll_Whittier_E, FUN = quantile, probs = c(0.8)), aes(xintercept = value), col='green', size=1) +
  facet_grid(name ~ .)


# Export ll objects for Markdown ------------------------------------------
ll_Hp <- list(ll_Homer0, ll_Kodiak, ll_Seward, ll_Valdez, ll_Whittier)
saveRDS(ll_Hp, ".\\Rockfish report_96-19\\Interview post\\ll_Hp.rds")
ll_Hnpy <- list(ll_Homer0_npy, ll_Kodiak_npy, ll_Seward_npy, ll_Valdez_npy, ll_Whittier_npy)
saveRDS(ll_Hnpy, ".\\Rockfish report_96-19\\Interview post\\ll_Hnpy.rds")
ll_E <- list(ll_Homer_E, ll_Kodiak_E, ll_Seward_E, ll_Valdez_E, ll_Whittier_E)
saveRDS(ll_E, ".\\Rockfish report_96-19\\Interview post\\ll_E.rds")
