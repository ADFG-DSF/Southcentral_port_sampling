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
  
  tau ~ dgamma(0.5, 0.5) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + epsilon[a]*yearc[y]*T[1] + gamma[f,a]*yearc[y]*T[2] + re[y,f,a]
      }
    }
  }
  dummy ~ dpois(pr)
  pr <- -log(i[summed]/(i[1] + i[2]*2 + i[3]))
  summed <- round(sum(T[]))
  for(k in 1:2){T[k]~dbern(0.5)}
  i[1] <- 1
  i[2] <- 1
  i[3] <- 1
}
", con="model_select.txt")
modfile_select <- 'model_select.txt'
for(i in 1:5) jags_dat[[i]]$dummy <- 0
parameters_select <- c("alpha", "beta", "epsilon", "gamma", "sd", "T")
# post_select <- lapply(jags_dat, 
#                      function(x){
#                        OLREjags_re <- jagsUI::jags(x, 
#                                                    parameters.to.save = parameters_select, 
#                                                    model.file = modfile_select, 
#                                                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)})
post_select <- jagsUI::jags(jags_dat[[4]], 
                            parameters.to.save = parameters_select, 
                            model.file = modfile_select, 
                            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
post_select
post_epsilon[[4]]$mean$sd

