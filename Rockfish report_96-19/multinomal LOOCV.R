int_boat <- readRDS(".\\Data\\int_boat.rds")
source(".\\functions.R")
library(magrittr)
library(ggplot2)



# Pelagic Rockfish ---------------------------------------------------------
int_area <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarize(H = sum(pH), E = sum(E, na.rm = TRUE)) %>%
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
# #saveRDS(post_alpha_pred_Homer, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Homer.rds")
# post_alpha_pred_Kodiak <- 
#   lapply(dat_Kodiak, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_alpha_pred, 
#                                      model.file = modfile_alpha_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_alpha_pred_Kodiak, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Kodiak.rds")
# 
# post_alpha_pred_Seward <- 
#   lapply(dat_Seward, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_alpha_pred, 
#                                      model.file = modfile_alpha_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_alpha_pred_Seward, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Seward.rds")
# 
# post_alpha_pred_Valdez <- 
#   lapply(dat_Valdez, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_alpha_pred, 
#                                      model.file = modfile_alpha_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_alpha_pred_Valdez, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Valdez.rds")
# 
# post_alpha_pred_Whittier <- 
#   lapply(dat_Whittier, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_alpha_pred, 
#                                      model.file = modfile_alpha_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_alpha_pred_Whittier, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Whittier.rds")



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
# #saveRDS(post_beta_pred_Homer, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Homer.rds")
# 
# post_beta_pred_Kodiak <- 
#   lapply(dat_Kodiak, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_beta_pred, 
#                                      model.file = modfile_beta_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_beta_pred_Kodiak, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Kodiak.rds")
# 
# post_beta_pred_Seward <- 
#   lapply(dat_Seward, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_beta_pred, 
#                                      model.file = modfile_beta_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_beta_pred_Seward, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Seward.rds")
# 
# post_beta_pred_Valdez <- 
#   lapply(dat_Valdez, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_beta_pred, 
#                                      model.file = modfile_beta_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_beta_pred_Valdez, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Valdez.rds")
# 
# post_beta_pred_Whittier <- 
#   lapply(dat_Whittier, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_beta_pred, 
#                                      model.file = modfile_beta_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_beta_pred_Whittier, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Whittier.rds")





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
# #saveRDS(post_epsilon_pred_Homer, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Homer.rds")
# 
# post_epsilon_pred_Kodiak <- 
#   lapply(dat_Kodiak, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_epsilon_pred, 
#                                      model.file = modfile_epsilon_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_epsilon_pred_Kodiak, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Kodiak.rds")
# 
# post_epsilon_pred_Seward <- 
#   lapply(dat_Seward, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_epsilon_pred, 
#                                      model.file = modfile_epsilon_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_epsilon_pred_Seward, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Seward.rds")
# 
# post_epsilon_pred_Valdez <- 
#   lapply(dat_Valdez, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_epsilon_pred, 
#                                      model.file = modfile_epsilon_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_epsilon_pred_Valdez, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Valdez.rds")
# 
# post_epsilon_pred_Whittier <- 
#   lapply(dat_Whittier, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_epsilon_pred, 
#                                      model.file = modfile_epsilon_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_epsilon_pred_Whittier, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Whittier.rds")



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
# #saveRDS(post_gamma_pred_Homer, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Homer.rds")
# 
# post_gamma_pred_Kodiak <- 
#   lapply(dat_Kodiak, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_gamma_pred, 
#                                      model.file = modfile_gamma_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_gamma_pred_Kodiak, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Kodiak.rds")
# 
# post_gamma_pred_Seward <- 
#   lapply(dat_Seward, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_gamma_pred, 
#                                      model.file = modfile_gamma_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_gamma_pred_Seward, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Seward.rds")
# 
# post_gamma_pred_Valdez <- 
#   lapply(dat_Valdez, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_gamma_pred, 
#                                      model.file = modfile_gamma_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_gamma_pred_Valdez, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Valdez.rds")
# 
# post_gamma_pred_Whittier <- 
#   lapply(dat_Whittier, 
#          function(x){x2 <- x[names(x) != "pred_area"]
#          OLREjags_re <- jagsUI::jags(x2, 
#                                      parameters.to.save = parameters_gamma_pred, 
#                                      model.file = modfile_gamma_pred, 
#                                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
#                                      parallel = TRUE)})
# #saveRDS(post_gamma_pred_Whittier, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Whittier.rds")

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
#saveRDS(post_alpha0_pred_Homer, ".\\Rockfish report_96-19\\Interview post\\post_alpha0_pred_Homer.rds")


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
#saveRDS(post_epsilon0_pred_Homer, ".\\Rockfish report_96-19\\Interview post\\post_epsilon0_pred_Homer.rds")

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
# post_alpha0_pred_Homer <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha0_pred_Homer.rds")
# post_epsilon0_pred_Homer <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon0_pred_Homer.rds")


#  * ll calcs -------------------------------------------------------------
#Not sure how to evaluate cross-validated predictions
#Maybe the most direct route is just to calcualte the log-liklihood for the left our observations given the modeled probabilities
#Seems like the best choice
ll_Homer <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Homer, obs = dat_Homer),
       beta = mapply(get_llpred, pred = post_beta_pred_Homer, obs = dat_Homer),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Homer, obs = dat_Homer),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Homer, obs = dat_Homer))
ll_Homer
sapply(ll_Homer, sum)
ll_Homer_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Homer_diff$alpha[[i]] <- ll_Homer[[1]] - ll_Homer[[i+1]]}
for(i in 1:2){ll_Homer_diff$beta[[i]] <- ll_Homer[[2]] - ll_Homer[[i+2]]}
ll_Homer_diff$epsilon[[1]] <- ll_Homer[[3]] - ll_Homer[[4]]
lapply(ll_Homer_diff, function(x) sapply(x, sum))
lapply(ll_Homer_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))

get_llpred0 <- function(pred, obs){
  ll <- vector("numeric", length = dim(pred$sims.list$q_pred)[1])
  for(i in 1:dim(pred$sims.list$q_pred)[1]){
    ll[i] <- dmultinom(obs$lo_count[1, ], prob = pred$sims.list$q_pred[i, ])
  }
  log(mean(ll))
}
ll_Homer0 <- 
  list(alpha = mapply(get_llpred0, pred = post_alpha0_pred_Homer, obs = dat_Homer),
       epsilon = mapply(get_llpred0, pred = post_epsilon0_pred_Homer, obs = dat_Homer))
ll_Homer0
sapply(ll_Homer0, sum)
ll_Homer0_diff <- list(alpha = list())
ll_Homer0_diff$alpha[[1]] <- ll_Homer0[[2]] - ll_Homer0[[1]]
lapply(ll_Homer0_diff, function(x) sapply(x, sum))
lapply(ll_Homer0_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))


ll_Kodiak <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Kodiak, obs = dat_Kodiak),
       beta = mapply(get_llpred, pred = post_beta_pred_Kodiak, obs = dat_Kodiak),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Kodiak, obs = dat_Kodiak),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Kodiak, obs = dat_Kodiak))
ll_Kodiak
sapply(ll_Kodiak, sum)
ll_Kodiak_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Kodiak_diff$alpha[[i]] <- ll_Kodiak[[1]] - ll_Kodiak[[i+1]]}
for(i in 1:2){ll_Kodiak_diff$beta[[i]] <- ll_Kodiak[[2]] - ll_Kodiak[[i+2]]}
ll_Kodiak_diff$epsilon[[1]] <- ll_Kodiak[[3]] - ll_Kodiak[[4]]
lapply(ll_Kodiak_diff, function(x) sapply(x, sum))
lapply(ll_Kodiak_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))

ll_Seward <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Seward, obs = dat_Seward),
       beta = mapply(get_llpred, pred = post_beta_pred_Seward, obs = dat_Seward),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Seward, obs = dat_Seward),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Seward, obs = dat_Seward))
ll_Seward
sapply(ll_Seward, sum)
ll_Seward_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Seward_diff$alpha[[i]] <- ll_Seward[[1]] - ll_Seward[[i+1]]}
for(i in 1:2){ll_Seward_diff$beta[[i]] <- ll_Seward[[2]] - ll_Seward[[i+2]]}
ll_Seward_diff$epsilon[[1]] <- ll_Seward[[3]] - ll_Seward[[4]]
lapply(ll_Seward_diff, function(x) sapply(x, sum))
lapply(ll_Seward_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))

ll_Valdez <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Valdez, obs = dat_Valdez),
       beta = mapply(get_llpred, pred = post_beta_pred_Valdez, obs = dat_Valdez),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Valdez, obs = dat_Valdez),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Valdez, obs = dat_Valdez))
ll_Valdez
sapply(ll_Valdez, sum)
ll_Valdez_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Valdez_diff$alpha[[i]] <- ll_Valdez[[1]] - ll_Valdez[[i+1]]}
for(i in 1:2){ll_Valdez_diff$beta[[i]] <- ll_Valdez[[2]] - ll_Valdez[[i+2]]}
ll_Valdez_diff$epsilon[[1]] <- ll_Valdez[[3]] - ll_Valdez[[4]]
lapply(ll_Valdez_diff, function(x) sapply(x, sum))
lapply(ll_Valdez_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))


ll_Whittier <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Whittier, obs = dat_Whittier),
       beta = mapply(get_llpred, pred = post_beta_pred_Whittier, obs = dat_Whittier),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Whittier, obs = dat_Whittier),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Whittier, obs = dat_Whittier))
ll_Whittier
sapply(ll_Whittier, sum)
ll_Whittier_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Whittier_diff$alpha[[i]] <- ll_Whittier[[1]] - ll_Whittier[[i+1]]}
for(i in 1:2){ll_Whittier_diff$beta[[i]] <- ll_Whittier[[2]] - ll_Whittier[[i+2]]}
ll_Whittier_diff$epsilon[[1]] <- ll_Whittier[[3]] - ll_Whittier[[4]]
lapply(ll_Whittier_diff, function(x) sapply(x, sum))
lapply(ll_Whittier_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))








# Non-Pelagic Rockfish ----------------------------------------------------
int_area_npy <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarize(H = sum(npyH), E = sum(E, na.rm = TRUE)) %>%
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
#saveRDS(post_alpha_pred_Homer_npy, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Homer_npy.rds")

post_alpha_pred_Kodiak_npy<- 
  lapply(dat_Kodiak_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_alpha_pred_Kodiak_npy, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Kodiak_npy.rds")

post_alpha_pred_Seward_npy<- 
  lapply(dat_Seward_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_alpha_pred_Seward_npy, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Seward_npy.rds")


post_alpha_pred_Valdez_npy<- 
  lapply(dat_Valdez_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_alpha_pred_Valdez_npy, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Valdez_npy.rds")


post_alpha_pred_Whittier_npy<- 
  lapply(dat_Whittier_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_alpha_pred_Whittier_npy, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Whittier_npy.rds")


#  * Beta model -----------------------------------------------------------
post_beta_pred_Homer_npy<- 
  lapply(dat_Homer_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_beta_pred_Homer_npy, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Homer_npy.rds")

post_beta_pred_Kodiak_npy<- 
  lapply(dat_Kodiak_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_beta_pred_Kodiak_npy, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Kodiak_npy.rds")

post_beta_pred_Seward_npy<- 
  lapply(dat_Seward_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_beta_pred_Seward_npy, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Seward_npy.rds")

post_beta_pred_Valdez_npy<- 
  lapply(dat_Valdez_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_beta_pred_Valdez_npy, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Valdez_npy.rds")

post_beta_pred_Whittier_npy<- 
  lapply(dat_Whittier_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_beta_pred_Whittier_npy, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Whittier_npy.rds")



# *  Epsilon model --------------------------------------------------------
post_epsilon_pred_Homer_npy<- 
  lapply(dat_Homer_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_epsilon_pred_Homer_npy, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Homer_npy.rds")

post_epsilon_pred_Kodiak_npy<- 
  lapply(dat_Kodiak_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_epsilon_pred_Kodiak_npy, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Kodiak_npy.rds")

post_epsilon_pred_Seward_npy<- 
  lapply(dat_Seward_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_epsilon_pred_Seward_npy, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Seward_npy.rds")

post_epsilon_pred_Valdez_npy<- 
  lapply(dat_Valdez_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_epsilon_pred_Valdez_npy, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Valdez_npy.rds")

post_epsilon_pred_Whittier_npy<- 
  lapply(dat_Whittier_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_epsilon_pred_Whittier_npy, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Whittier_npy.rds")



#  * Gamma model ----------------------------------------------------------
post_gamma_pred_Homer_npy<- 
  lapply(dat_Homer_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_gamma_pred_Homer_npy, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Homer_npy.rds")

post_gamma_pred_Kodiak_npy<- 
  lapply(dat_Kodiak_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_gamma_pred_Kodiak_npy, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Kodiak_npy.rds")

post_gamma_pred_Seward_npy<- 
  lapply(dat_Seward_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_gamma_pred_Seward_npy, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Seward_npy.rds")

post_gamma_pred_Valdez_npy<- 
  lapply(dat_Valdez_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_gamma_pred_Valdez_npy, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Valdez_npy.rds")

post_gamma_pred_Whittier_npy<- 
  lapply(dat_Whittier_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_gamma_pred_Whittier_npy, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Whittier_npy.rds")


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
#saveRDS(post_alpha0_pred_Homer_npy, ".\\Rockfish report_96-19\\Interview post\\post_alpha0_pred_Homer_npy.rds")

post_epsilon0_pred_Homer_npy <- 
  lapply(dat_Homer_npy, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon0_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_epsilon0_pred_Homer_npy, ".\\Rockfish report_96-19\\Interview post\\post_epsilon0_pred_Homer_npy.rds")

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
# post_alpha0_pred_Homer_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha0_pred_Homer_npy.rds")
# post_epsilon0_pred_Homer_npy <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon0_pred_Homer_npy.rds")

# *  ll calcs ---------------------------------------------------------------
#Not sure how to evaluate cross-validated predictions
#Maybe the most direct route is just to calcualte the log-liklihood for the left our observations given the modeled probabilities
#Seems like the best choice
ll_Homer_npy <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Homer_npy, obs = dat_Homer_npy),
       beta = mapply(get_llpred, pred = post_beta_pred_Homer_npy, obs = dat_Homer_npy),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Homer_npy, obs = dat_Homer_npy),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Homer_npy, obs = dat_Homer_npy))
ll_Homer_npy
sapply(ll_Homer_npy, sum)
ll_Homer_npy_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Homer_npy_diff$alpha[[i]] <- ll_Homer_npy[[1]] - ll_Homer_npy[[i+1]]}
for(i in 1:2){ll_Homer_npy_diff$beta[[i]] <- ll_Homer_npy[[2]] - ll_Homer_npy[[i+2]]}
ll_Homer_npy_diff$epsilon[[1]] <- ll_Homer_npy[[3]] - ll_Homer_npy[[4]]
lapply(ll_Homer_npy_diff, function(x) sapply(x, sum))
lapply(ll_Homer_npy_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))


ll_Homer0_npy <- 
  list(alpha = mapply(get_llpred0, pred = post_alpha0_pred_Homer_npy, obs = dat_Homer_npy),
       epsilon = mapply(get_llpred0, pred = post_epsilon0_pred_Homer_npy, obs = dat_Homer_npy))
ll_Homer0_npy
sapply(ll_Homer0_npy, sum)
ll_Homer0_npy_diff <- list(alpha = list())
ll_Homer0_npy_diff$alpha[[1]] <- ll_Homer0_npy[[2]] - ll_Homer0_npy[[1]]
lapply(ll_Homer0_npy_diff, function(x) sapply(x, sum))
lapply(ll_Homer0_npy_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))


ll_Kodiak_npy <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Kodiak_npy, obs = dat_Kodiak_npy),
       beta = mapply(get_llpred, pred = post_beta_pred_Kodiak_npy, obs = dat_Kodiak_npy),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Kodiak_npy, obs = dat_Kodiak_npy),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Kodiak_npy, obs = dat_Kodiak_npy))
ll_Kodiak_npy
sapply(ll_Kodiak_npy, sum)
ll_Kodiak_npy_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Kodiak_npy_diff$alpha[[i]] <- ll_Kodiak_npy[[1]] - ll_Kodiak_npy[[i+1]]}
for(i in 1:2){ll_Kodiak_npy_diff$beta[[i]] <- ll_Kodiak_npy[[2]] - ll_Kodiak_npy[[i+2]]}
ll_Kodiak_npy_diff$epsilon[[1]] <- ll_Kodiak_npy[[3]] - ll_Kodiak_npy[[4]]
lapply(ll_Kodiak_npy_diff, function(x) sapply(x, sum))
lapply(ll_Kodiak_npy_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))

ll_Seward_npy <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Seward_npy, obs = dat_Seward_npy),
       beta = mapply(get_llpred, pred = post_beta_pred_Seward_npy, obs = dat_Seward_npy),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Seward_npy, obs = dat_Seward_npy),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Seward_npy, obs = dat_Seward_npy))
ll_Seward_npy
sapply(ll_Seward_npy, sum)
ll_Seward_npy_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Seward_npy_diff$alpha[[i]] <- ll_Seward_npy[[1]] - ll_Seward_npy[[i+1]]}
for(i in 1:2){ll_Seward_npy_diff$beta[[i]] <- ll_Seward_npy[[2]] - ll_Seward_npy[[i+2]]}
ll_Seward_npy_diff$epsilon[[1]] <- ll_Seward_npy[[3]] - ll_Seward_npy[[4]]
lapply(ll_Seward_npy_diff, function(x) sapply(x, sum))
lapply(ll_Seward_npy_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))

ll_Valdez_npy <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Valdez_npy, obs = dat_Valdez_npy),
       beta = mapply(get_llpred, pred = post_beta_pred_Valdez_npy, obs = dat_Valdez_npy),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Valdez_npy, obs = dat_Valdez_npy),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Valdez_npy, obs = dat_Valdez_npy))
ll_Valdez_npy
sapply(ll_Valdez_npy, sum)
ll_Valdez_npy_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Valdez_npy_diff$alpha[[i]] <- ll_Valdez_npy[[1]] - ll_Valdez_npy[[i+1]]}
for(i in 1:2){ll_Valdez_npy_diff$beta[[i]] <- ll_Valdez_npy[[2]] - ll_Valdez_npy[[i+2]]}
ll_Valdez_npy_diff$epsilon[[1]] <- ll_Valdez_npy[[3]] - ll_Valdez_npy[[4]]
lapply(ll_Valdez_npy_diff, function(x) sapply(x, sum))
lapply(ll_Valdez_npy_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))


ll_Whittier_npy <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Whittier_npy, obs = dat_Whittier_npy),
       beta = mapply(get_llpred, pred = post_beta_pred_Whittier_npy, obs = dat_Whittier_npy),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Whittier_npy, obs = dat_Whittier_npy),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Whittier_npy, obs = dat_Whittier_npy))
ll_Whittier_npy
sapply(ll_Whittier_npy, sum)
ll_Whittier_npy_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Whittier_npy_diff$alpha[[i]] <- ll_Whittier_npy[[1]] - ll_Whittier_npy[[i+1]]}
for(i in 1:2){ll_Whittier_npy_diff$beta[[i]] <- ll_Whittier_npy[[2]] - ll_Whittier_npy[[i+2]]}
ll_Whittier_npy_diff$epsilon[[1]] <- ll_Whittier_npy[[3]] - ll_Whittier_npy[[4]]
lapply(ll_Whittier_npy_diff, function(x) sapply(x, sum))
lapply(ll_Whittier_npy_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))




# Effort ----------------------------------------------------
int_area_E <- 
  int_boat[int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Lingcod"), ] %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarize(H = sum(npyH), E = sum(E, na.rm = TRUE)) %>%
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
#saveRDS(post_alpha_pred_Homer_E, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Homer_E.rds")

post_alpha_pred_Kodiak_E<- 
  lapply(dat_Kodiak_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_alpha_pred_Kodiak_E, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Kodiak_E.rds")

post_alpha_pred_Seward_E<- 
  lapply(dat_Seward_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_alpha_pred_Seward_E, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Seward_E.rds")

post_alpha_pred_Valdez_E<- 
  lapply(dat_Valdez_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_alpha_pred_Valdez_E, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Valdez_E.rds")

post_alpha_pred_Whittier_E<- 
  lapply(dat_Whittier_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_alpha_pred_Whittier_E, ".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Whittier_E.rds")



#  * Beta model -----------------------------------------------------------
post_beta_pred_Homer_E<- 
  lapply(dat_Homer_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_beta_pred_Homer_E, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Homer_E.rds")

post_beta_pred_Kodiak_E<- 
  lapply(dat_Kodiak_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_beta_pred_Kodiak_E, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Kodiak_E.rds")

post_beta_pred_Seward_E<- 
  lapply(dat_Seward_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_beta_pred_Seward_E, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Seward_E.rds")

post_beta_pred_Valdez_E<- 
  lapply(dat_Valdez_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_beta_pred_Valdez_E, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Valdez_E.rds")

post_beta_pred_Whittier_E<- 
  lapply(dat_Whittier_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_beta_pred_Whittier_E, ".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Whittier_E.rds")


# *  Epsilon model --------------------------------------------------------
post_epsilon_pred_Homer_E<- 
  lapply(dat_Homer_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_epsilon_pred_Homer_E, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Homer_E.rds")

post_epsilon_pred_Kodiak_E<- 
  lapply(dat_Kodiak_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_epsilon_pred_Kodiak_E, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Kodiak_E.rds")

post_epsilon_pred_Seward_E<- 
  lapply(dat_Seward_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_epsilon_pred_Seward_E, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Seward_E.rds")

post_epsilon_pred_Valdez_E<- 
  lapply(dat_Valdez_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_epsilon_pred_Valdez_E, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Valdez_E.rds")

post_epsilon_pred_Whittier_E<- 
  lapply(dat_Whittier_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_epsilon_pred_Whittier_E, ".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Whittier_E.rds")


#  * Gamma model ----------------------------------------------------------
post_gamma_pred_Homer_E<- 
  lapply(dat_Homer_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_gamma_pred_Homer_E, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Homer_E.rds")

post_gamma_pred_Kodiak_E<- 
  lapply(dat_Kodiak_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_gamma_pred_Kodiak_E, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Kodiak_E.rds")

post_gamma_pred_Seward_E<- 
  lapply(dat_Seward_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_gamma_pred_Seward_E, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Seward_E.rds")

post_gamma_pred_Valdez_E<- 
  lapply(dat_Valdez_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_gamma_pred_Valdez_E, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Valdez_E.rds")

post_gamma_pred_Whittier_E<- 
  lapply(dat_Whittier_E, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
#saveRDS(post_gamma_pred_Whittier_E, ".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Whittier_E.rds")


# post_alpha_pred_Homer_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Homer_E.rds")
# post_alpha_pred_Kodiak_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Kodiak_E.rds")
# post_alpha_pred_Seward_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Seward_E.rds")
# post_alpha_pred_Valdez_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Valdez_E.rds")
# post_alpha_pred_Whittier_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_alpha_pred_Whittier_E.rds")
# post_beta_pred_Homer_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Homer_E.rds")
# post_beta_pred_Kodiak_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Kodiak_E.rds")
# post_beta_pred_Seward_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Seward_E.rds")
# post_beta_pred_Valdez_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Valdez_E.rds")
# post_beta_pred_Whittier_E  <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_beta_pred_Whittier_E.rds")
# post_epsilon_pred_Homer_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Homer_E.rds")
# post_epsilon_pred_Kodiak_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Kodiak_E.rds")
# post_epsilon_pred_Seward_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Seward_E.rds")
# post_epsilon_pred_Valdez_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Valdez_E.rds")
# post_epsilon_pred_Whittier_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_epsilon_pred_Whittier_E.rds")
# post_gamma_pred_Homer_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Homer_E.rds")
# post_gamma_pred_Kodiak_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Kodiak_E.rds")
# post_gamma_pred_Seward_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Seward_E.rds")
# post_gamma_pred_Valdez_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Valdez_E.rds")
# post_gamma_pred_Whittier_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\post_gamma_pred_Whittier_E.rds")

# *  ll calcs ---------------------------------------------------------------
#Not sure how to evaluate cross-validated predictions
#Maybe the most direct route is just to calcualte the log-liklihood for the left our observations given the modeled probabilities
#Seems like the best choice
ll_Homer_E <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Homer_E, obs = dat_Homer_E),
       beta = mapply(get_llpred, pred = post_beta_pred_Homer_E, obs = dat_Homer_E),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Homer_E, obs = dat_Homer_E),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Homer_E, obs = dat_Homer_E))
ll_Homer_E
sapply(ll_Homer_E, sum)
ll_Homer_E_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Homer_E_diff$alpha[[i]] <- ll_Homer_E[[1]] - ll_Homer_E[[i+1]]}
for(i in 1:2){ll_Homer_E_diff$beta[[i]] <- ll_Homer_E[[2]] - ll_Homer_E[[i+2]]}
ll_Homer_E_diff$epsilon[[1]] <- ll_Homer_E[[3]] - ll_Homer_E[[4]]
lapply(ll_Homer_E_diff, function(x) sapply(x, sum))
lapply(ll_Homer_E_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))



ll_Kodiak_E <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Kodiak_E, obs = dat_Kodiak_E),
       beta = mapply(get_llpred, pred = post_beta_pred_Kodiak_E, obs = dat_Kodiak_E),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Kodiak_E, obs = dat_Kodiak_E),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Kodiak_E, obs = dat_Kodiak_E))
ll_Kodiak_E
sapply(ll_Kodiak_E, sum)
ll_Kodiak_E_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Kodiak_E_diff$alpha[[i]] <- ll_Kodiak_E[[1]] - ll_Kodiak_E[[i+1]]}
for(i in 1:2){ll_Kodiak_E_diff$beta[[i]] <- ll_Kodiak_E[[2]] - ll_Kodiak_E[[i+2]]}
ll_Kodiak_E_diff$epsilon[[1]] <- ll_Kodiak_E[[3]] - ll_Kodiak_E[[4]]
lapply(ll_Kodiak_E_diff, function(x) sapply(x, sum))
lapply(ll_Kodiak_E_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))

ll_Seward_E <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Seward_E, obs = dat_Seward_E),
       beta = mapply(get_llpred, pred = post_beta_pred_Seward_E, obs = dat_Seward_E),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Seward_E, obs = dat_Seward_E),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Seward_E, obs = dat_Seward_E))
ll_Seward_E
sapply(ll_Seward_E, sum)
ll_Seward_E_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Seward_E_diff$alpha[[i]] <- ll_Seward_E[[1]] - ll_Seward_E[[i+1]]}
for(i in 1:2){ll_Seward_E_diff$beta[[i]] <- ll_Seward_E[[2]] - ll_Seward_E[[i+2]]}
ll_Seward_E_diff$epsilon[[1]] <- ll_Seward_E[[3]] - ll_Seward_E[[4]]
lapply(ll_Seward_E_diff, function(x) sapply(x, sum))
lapply(ll_Seward_E_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))

ll_Valdez_E <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Valdez_E, obs = dat_Valdez_E),
       beta = mapply(get_llpred, pred = post_beta_pred_Valdez_E, obs = dat_Valdez_E),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Valdez_E, obs = dat_Valdez_E),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Valdez_E, obs = dat_Valdez_E)) 
ll_Valdez_E
sapply(ll_Valdez_E, sum)
ll_Valdez_E_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Valdez_E_diff$alpha[[i]] <- ll_Valdez_E[[1]] - ll_Valdez_E[[i+1]]}
for(i in 1:2){ll_Valdez_E_diff$beta[[i]] <- ll_Valdez_E[[2]] - ll_Valdez_E[[i+2]]}
ll_Valdez_E_diff$epsilon[[1]] <- ll_Valdez_E[[3]] - ll_Valdez_E[[4]]
lapply(ll_Valdez_E_diff, function(x) sapply(x, sum))
lapply(ll_Valdez_E_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))

ll_Whittier_E <- 
  list(alpha = mapply(get_llpred, pred = post_alpha_pred_Whittier_E, obs = dat_Whittier_E),
       beta = mapply(get_llpred, pred = post_beta_pred_Whittier_E, obs = dat_Whittier_E),
       epsilon = mapply(get_llpred, pred = post_epsilon_pred_Whittier_E, obs = dat_Whittier_E),
       gamma = mapply(get_llpred, pred = post_gamma_pred_Whittier_E, obs = dat_Whittier_E)) 
ll_Whittier_E
sapply(ll_Whittier_E, sum)
ll_Whittier_E_diff <- list(alpha = list(), beta = list(), epsilon = list())
for(i in 1:3){ll_Whittier_E_diff$alpha[[i]] <- ll_Whittier_E[[1]] - ll_Whittier_E[[i+1]]}
for(i in 1:2){ll_Whittier_E_diff$beta[[i]] <- ll_Whittier_E[[2]] - ll_Whittier_E[[i+2]]}
ll_Whittier_E_diff$epsilon[[1]] <- ll_Whittier_E[[3]] - ll_Whittier_E[[4]]
lapply(ll_Whittier_E_diff, function(x) sapply(x, sum))
lapply(ll_Whittier_E_diff, function(x) sapply(x, function(y) sqrt(prod(dim(y))*var(as.vector(y)))))
#Bootstrap CI for mod epsilon vrs mod alpha
boot_Whittier_epsilondiff <- 
  replicate(2000, sum(sample(as.vector(ll_Whittier_E_diff$alpha[[2]]), length(as.vector(ll_Whittier_E_diff$alpha[[2]])), TRUE)))
quantile(boot_Whittier_epsilondiff, c(0.025, 0.975))
quantile(boot_Whittier_epsilondiff, c(0.05, 0.95))
hist(as.vector(ll_Whittier_E_diff$alpha[[2]]))