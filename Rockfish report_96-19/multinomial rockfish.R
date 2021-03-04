#Looking for a method to fit groundfish harvest and effort compostion data for reporting
#would like to detect a trend to composition data
library(magrittr)
library(ggplot2)
int_boat <- readRDS(".\\Data\\int_boat.rds")
source(".\\functions.R")
source(".\\models.R")


# Pelagic Harvest Composition -----------------------------------------------------
int_pr <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarise(H = sum(pH, na.rm = TRUE), E = sum(E, na.rm = TRUE)) %>%
  dplyr::ungroup()
ports <- unique(int_pr$port)
areas <- lapply(ports, function(x) sort(unique(int_pr$area[int_pr$port == x])))
aggregate(E ~ year + port, int_pr, sum)
jags_datH <- lapply(ports, make_jagsdat, dat = int_pr, stat = "H")

#data for appendix
years_pr <- lapply(ports, function(x) range(int_pr$year[int_pr$port == x]))
tab_pr <- lapply(1:5, function(x) tab_data(years_pr[[x]][1]:years_pr[[x]][2], jags_datH[[x]], areas[[x]]))
names(tab_pr) <- ports
WriteXLS::WriteXLS(tab_pr, ".\\Rockfish report_96-19\\tab_Hpr.xlsx")

#jags w overdisersion on each observation
ni <- 1E5; nb <- ni/3; nc <- 3; nt <- 200
params <- list(parameters_alpha <- c("alpha", "sd", "re"),
               parameters_beta <- c("alpha", "beta", "re", "sd"),
               parameters_epsilon <- c("alpha", "beta", "epsilon", "re", "sd"),
               parameters_gamma <- c("alpha", "beta", "epsilon", "gamma", "sd", "re"))
models <- list(alpha <- modfile_alpha,
               beta <- modfile_beta,
               epsilon <- modfile_epsilon,
               gamma <- modfile_gamma)


# * Homer Harvest -----------------------------------------------------------
#None of the models are great.
#Need to omit Private data... too sparse.
postH_Homer <- mapply(jagsUI::jags,
                     parameters.to.save = params, model.file = models,
                     MoreArgs = list(data = jags_datH[[1]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                     SIMPLIFY = FALSE)
lapply(postH_Homer, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postH_Homer, ".\\Rockfish report_96-19\\Interview post\\postH_Homer.rds")
postH_Homer <- readRDS(".\\Rockfish report_96-19\\Interview post\\postH_Homer.rds")
postH_Homer[[1]]
postH_Homer[[2]]
jagsUI::traceplot(postH_Homer[[2]], c("alpha", "beta"))
postH_Homer_beta <- 
  jagsUI::jags(data = jags_datH[[1]],
  parameters.to.save = params[[2]],
  model.file = models[[2]], 
  n.chains = 4, n.thin = nt, n.iter = 1E6, n.burnin = 1E6/2,
  parallel = TRUE)
postH_Homer[[3]]
postH_Homer[[4]]
data.frame(Deviance = sapply(postH_Homer, function(x) x$mean$deviance),
           pD = sapply(postH_Homer, function(x) x$pD), 
           DIC = sapply(postH_Homer, function(x) x$DIC),
           sd = sapply(postH_Homer, function(x) x$mean$sd))
plot_post(postH_Homer[[1]], int_pr, "H", ports[1])
plot_post(postH_Homer[[2]], int_pr, "H", ports[1])
plot_post(postH_Homer[[3]], int_pr, "H", ports[1])
plot_post(postH_Homer[[4]], int_pr, "H", ports[1])
jags_datHnp[[1]]$M
#Cant get model to convergere

#Try with only Homer Charter data only
jags_datH_Homer <- jags_datH[[1]]
jags_datH_Homer$count <- jags_datH[[1]]$count[,1,]
jags_datH_Homer$M <- jags_datH[[1]]$M[,1]
ni <- 5E5; nb <- ni/3; nc <- 3; nt <- 50
postH_HomerCharter <- mapply(jagsUI::jags,
                             parameters.to.save = params[c(1, 3)], model.file = list(alpha <- modfile_alpha0,epsilon <- modfile_epsilon0),
                             MoreArgs = list(data = jags_datH_Homer, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                             SIMPLIFY = FALSE)
lapply(postH_HomerCharter, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
saveRDS(postH_HomerCharter, ".\\ROckfish report_96-19\\Interview post\\postH_HomerCharter.rds")
ni <- 1E5; nb <- ni/3; nc <- 3; nt <- 20
postH_HomerCharter[[1]]
postH_HomerCharter[[2]]
data.frame(Deviance = sapply(postH_HomerCharter, function(x) x$mean$deviance),
           pD = sapply(postH_HomerCharter, function(x) x$pD), 
           DIC = sapply(postH_HomerCharter, function(x) x$DIC),
           sd = sapply(postH_HomerCharter, function(x) x$mean$sd))
plot_post(postH_HomerCharter[[1]], int_pr[int_pr$fleet == "Charter", ], "H", ports[1], inc_pred = "mean")
plot_post(postH_HomerCharter[[2]], int_pr[int_pr$fleet == "Charter", ], "H", ports[1], inc_pred = "mean")


# * Kodiak Harvest --------------------------------------------------------
#
#
postH_Kodiak <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datH[[2]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                       SIMPLIFY = FALSE)
lapply(postH_Kodiak, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postH_Kodiak, ".\\Rockfish report_96-19\\Interview post\\postH_Kodiak.rds")
postH_Kodiak[[1]]
postH_Kodiak[[2]]
postH_Kodiak[[3]]
postH_Kodiak[[4]]
data.frame(Deviance = sapply(postH_Kodiak, function(x) x$mean$deviance),
           pD = sapply(postH_Kodiak, function(x) x$pD), 
           DIC = sapply(postH_Kodiak, function(x) x$DIC),
           sd = sapply(postH_Kodiak, function(x) x$mean$sd))
plot_post(postH_Kodiak[[1]], int_pr, "H", ports[2])
plot_post(postH_Kodiak[[2]], int_pr, "H", ports[2])
plot_post(postH_Kodiak[[3]], int_pr, "H", ports[2])
plot_post(postH_Kodiak[[4]], int_pr, "H", ports[2])
jags_datH[[2]]$M




# * Seward Harvest --------------------------------------------------------
#
#
postH_Seward <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datH[[3]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                       SIMPLIFY = FALSE)
lapply(postH_Seward, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postH_Seward, ".\\Rockfish report_96-19\\Interview post\\postH_Seward.rds")
postH_Seward[[1]]
postH_Seward[[2]]
postH_Seward[[3]]
postH_Seward[[4]]
data.frame(Deviance = sapply(postH_Seward, function(x) x$mean$deviance),
           pD = sapply(postH_Seward, function(x) x$pD), 
           DIC = sapply(postH_Seward, function(x) x$DIC),
           sd = sapply(postH_Seward, function(x) x$mean$sd))
plot_post(postH_Seward[[1]], int_pr, "H", ports[3])
plot_post(postH_Seward[[2]], int_pr, "H", ports[3])
plot_post(postH_Seward[[3]], int_pr, "H", ports[3])
plot_post(postH_Seward[[4]], int_pr, "H", ports[3])







# * Valdez Harvest --------------------------------------------------------
#
#
postH_Valdez <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datH[[4]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                       SIMPLIFY = FALSE)
lapply(postH_Valdez, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postH_Valdez, ".\\Rockfish report_96-19\\Interview post\\postH_Valdez.rds")
postH_Valdez[[1]]
postH_Valdez[[2]]
postH_Valdez[[3]]
postH_Valdez[[4]]
data.frame(Deviance = sapply(postH_Valdez, function(x) x$mean$deviance),
           pD = sapply(postH_Valdez, function(x) x$pD), 
           DIC = sapply(postH_Valdez, function(x) x$DIC),
           sd = sapply(postH_Valdez, function(x) x$mean$sd))
lapply(postH_Valdez, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)
plot_post(postH_Valdez[[1]], int_pr, "H", ports[4])
plot_post(postH_Valdez[[2]], int_pr, "H", ports[4])
plot_post(postH_Valdez[[3]], int_pr, "H", ports[4])
plot_post(postH_Valdez[[4]], int_pr, "H", ports[4])






# * Whittier Harvest ------------------------------------------------------
#
#
postH_Whittier <- mapply(jagsUI::jags,
                         parameters.to.save = params, model.file = models,
                         MoreArgs = list(data = jags_datH[[5]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                         SIMPLIFY = FALSE)
lapply(postH_Whittier, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postH_Whittier, ".\\Rockfish report_96-19\\Interview post\\postH_Whittier.rds")
postH_Whittier[[1]]
postH_Whittier[[2]]
postH_Whittier[[3]]
postH_Whittier[[4]]
data.frame(Deviance = sapply(postH_Whittier, function(x) x$mean$deviance),
           pD = sapply(postH_Whittier, function(x) x$pD), 
           DIC = sapply(postH_Whittier, function(x) x$DIC),
           sd = sapply(postH_Whittier, function(x) x$mean$sd))
lapply(postH_Whittier, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)
plot_post(postH_Whittier[[1]], int_pr, "H", ports[5])
plot_post(postH_Whittier[[2]], int_pr, "H", ports[5])
plot_post(postH_Whittier[[3]], int_pr, "H", ports[5])
plot_post(postH_Whittier[[4]], int_pr, "H", ports[5])




# Non-Pelagic Harvest Composition -----------------------------------------------------
int_npr <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarise(H = sum(npyH, na.rm = TRUE), E = sum(E, na.rm = TRUE)) %>%
  dplyr::ungroup()
jags_datHnp <- lapply(ports, make_jagsdat, dat = int_npr, stat = "H")

#data for appendix
years_npr <- lapply(ports, function(x) range(int_npr$year[int_npr$port == x]))
tab_npr <- lapply(1:5, function(x) tab_data(years_npr[[x]][1]:years_npr[[x]][2], jags_datHnp[[x]], areas[[x]]))
names(tab_npr) <- ports
WriteXLS::WriteXLS(tab_npr, ".\\Rockfish report_96-19\\tab_Hnpr.xlsx")


# * Homer Harvest -----------------------------------------------------------
#None of the models are great.
#Need to omit Private data... too sparse.
postHnp_Homer <- mapply(jagsUI::jags,
                      parameters.to.save = params, model.file = models,
                      MoreArgs = list(data = jags_datHnp[[1]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                      SIMPLIFY = FALSE)
lapply(postHnp_Homer, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postHnp_Homer, ".\\Rockfish report_96-19\\Interview post\\postHnp_Homer.rds")
postHnp_Homer  <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Homer.rds")
postHnp_Homer[[1]]
postHnp_Homer[[2]]
postHnp_Homer[[3]]
postHnp_Homer[[4]]
data.frame(Deviance = sapply(postHnp_Homer, function(x) x$mean$deviance),
           pD = sapply(postHnp_Homer, function(x) x$pD), 
           DIC = sapply(postHnp_Homer, function(x) x$DIC),
           sd = sapply(postHnp_Homer, function(x) x$mean$sd))
plot_post(postHnp_Homer[[1]], int_npr, "H", ports[1])
plot_post(postHnp_Homer[[2]], int_npr, "H", ports[1])
plot_post(postHnp_Homer[[3]], int_npr, "H", ports[1])
plot_post(postHnp_Homer[[4]], int_npr, "H", ports[1])
jags_datHnp[[1]]$M
#Insufficent Private data

#Try with only Homer Charter data only
jags_datHnp_Homer <- jags_datHnp[[1]]
jags_datHnp_Homer$count <- jags_datHnp[[1]]$count[,1,]
jags_datHnp_Homer$M <- jags_datHnp[[1]]$M[,1]
ni <- 1E5; nb <- ni/3; nc <- 3; nt <- 50
postHnp_HomerCharter <- mapply(jagsUI::jags,
                             parameters.to.save = params[c(1, 3)], model.file = list(alpha <- modfile_alpha0,epsilon <- modfile_epsilon0),
                             MoreArgs = list(data = jags_datHnp_Homer, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                             SIMPLIFY = FALSE)
lapply(postHnp_HomerCharter, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
saveRDS(postHnp_HomerCharter, ".\\ROckfish report_96-19\\Interview post\\postHnp_HomerCharter.rds")
ni <- 1E5; nb <- ni/3; nc <- 3; nt <- 20
postHnp_HomerCharter[[1]]
postHnp_HomerCharter[[2]]
data.frame(Deviance = sapply(postHnp_HomerCharter, function(x) x$mean$deviance),
           pD = sapply(postHnp_HomerCharter, function(x) x$pD), 
           DIC = sapply(postHnp_HomerCharter, function(x) x$DIC),
           sd = sapply(postHnp_HomerCharter, function(x) x$mean$sd))
plot_post(postHnp_HomerCharter[[1]], int_np[int_pr$fleet == "Charter", ], "H", ports[1], inc_pred = "mean")
plot_post(postHnp_HomerCharter[[2]], int_np[int_pr$fleet == "Charter", ], "H", ports[1], inc_pred = "mean")


# * Kodiak Harvest --------------------------------------------------------
#
#
postHnp_Kodiak <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datHnp[[2]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                       SIMPLIFY = FALSE)
lapply(postHnp_Kodiak, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postHnp_Kodiak, ".\\Rockfish report_96-19\\Interview post\\postHnp_Kodiak.rds")
postHnp_Kodiak  <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Kodiak.rds")
postHnp_Kodiak[[1]]
postHnp_Kodiak[[2]]
postHnp_Kodiak[[3]]
postHnp_Kodiak[[4]]
data.frame(Deviance = sapply(postHnp_Kodiak, function(x) x$mean$deviance),
           pD = sapply(postHnp_Kodiak, function(x) x$pD), 
           DIC = sapply(postHnp_Kodiak, function(x) x$DIC),
           sd = sapply(postHnp_Kodiak, function(x) x$mean$sd))
plot_post(postHnp_Kodiak[[1]], int_npr, "H", ports[2])
plot_post(postHnp_Kodiak[[2]], int_npr, "H", ports[2])
plot_post(postHnp_Kodiak[[3]], int_npr, "H", ports[2])
plot_post(postHnp_Kodiak[[4]], int_npr, "H", ports[2])
jags_datHnp[[2]]$M




# * Seward Harvest --------------------------------------------------------
#
#
postHnp_Seward <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datHnp[[3]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                       SIMPLIFY = FALSE)
lapply(postHnp_Seward, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postHnp_Seward, ".\\Rockfish report_96-19\\Interview post\\postHnp_Seward.rds")
postHnp_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Seward.rds")
postHnp_Seward[[1]]
postHnp_Seward[[2]]
postHnp_Seward[[3]]
postHnp_Seward[[4]]
data.frame(Deviance = sapply(postHnp_Seward, function(x) x$mean$deviance),
           pD = sapply(postHnp_Seward, function(x) x$pD), 
           DIC = sapply(postHnp_Seward, function(x) x$DIC),
           sd = sapply(postHnp_Seward, function(x) x$mean$sd))
plot_post(postHnp_Seward[[1]], int_npr, "H", ports[3])
plot_post(postHnp_Seward[[2]], int_npr, "H", ports[3])
plot_post(postHnp_Seward[[3]], int_npr, "H", ports[3])
plot_post(postHnp_Seward[[4]], int_npr, "H", ports[3])







# * Valdez Harvest --------------------------------------------------------
#
#
postHnp_Valdez <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datHnp[[4]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                       SIMPLIFY = FALSE)
lapply(postHnp_Valdez, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postHnp_Valdez, ".\\Rockfish report_96-19\\Interview post\\postHnp_Valdez.rds")
postHnp_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Valdez.rds")
postHnp_Valdez[[1]]
postHnp_Valdez[[2]]
postHnp_Valdez[[3]]
postHnp_Valdez[[4]]
data.frame(Deviance = sapply(postHnp_Valdez, function(x) x$mean$deviance),
           pD = sapply(postHnp_Valdez, function(x) x$pD), 
           DIC = sapply(postHnp_Valdez, function(x) x$DIC),
           sd = sapply(postHnp_Valdez, function(x) x$mean$sd))
lapply(postHnp_Valdez, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)
plot_post(postHnp_Valdez[[1]], int_npr, "H", ports[4])
plot_post(postHnp_Valdez[[2]], int_npr, "H", ports[4])
plot_post(postHnp_Valdez[[3]], int_npr, "H", ports[4])
plot_post(postHnp_Valdez[[4]], int_npr, "H", ports[4])






# * Whittier Harvest ------------------------------------------------------
#
#
postHnp_Whittier <- mapply(jagsUI::jags,
                         parameters.to.save = params, model.file = models,
                         MoreArgs = list(data = jags_datHnp[[5]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                         SIMPLIFY = FALSE)
lapply(postHnp_Whittier, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postHnp_Whittier, ".\\Rockfish report_96-19\\Interview post\\postHnp_Whittier.rds")
postHnp_Whittier  <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Whittier.rds")
postHnp_Whittier[[1]]
postHnp_Whittier[[2]]
postHnp_Whittier[[3]]
postHnp_Whittier[[4]]
data.frame(Deviance = sapply(postHnp_Whittier, function(x) x$mean$deviance),
           pD = sapply(postHnp_Whittier, function(x) x$pD), 
           DIC = sapply(postHnp_Whittier, function(x) x$DIC),
           sd = sapply(postHnp_Whittier, function(x) x$mean$sd))
lapply(postHnp_Whittier, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)
plot_post(postHnp_Whittier[[1]], int_npr, "H", ports[5])
plot_post(postHnp_Whittier[[2]], int_npr, "H", ports[5])
plot_post(postHnp_Whittier[[3]], int_npr, "H", ports[5])
plot_post(postHnp_Whittier[[4]], int_npr, "H", ports[5])





# Effort Description --------------------------------------------------
# * Harvest by target group ---------------------------------------
table(int_boat$year, int_boat$target)
table(int_boat$target)


#  * *  Pelagic Rockfish  -----------------------------------------------------
pH_target <- 
  aggregate(pH ~ port + target, int_boat, sum) %>%
  tidyr::pivot_wider(id_cols = port, names_from = target, values_from = pH)
pH_target[, -1]/apply(pH_target[, -1], 1, sum) 
#similar information temporally
lapply(unique(int_boat$port), function(x){dat <- int_boat[int_boat$port == x, ]
expand.grid(list(year = unique(dat$year),
                 port = unique(dat$port),
                 fleet = unique(dat$fleet), 
                 target = unique(dat$target)))}) %>%
  do.call(rbind, .) %>%
  dplyr::left_join(int_boat, by = c("year", "port", "fleet", "target")) %>%
  ggplot(aes(x = year, weight = pH, fill = target)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  facet_grid(port ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(1993, 20017, 3)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")


#  * * Non-pelagic Rockfish -----------------------------------------------
npyH_target <- 
  aggregate(npyH ~ port + target, int_boat, sum) %>%
  tidyr::pivot_wider(id_cols = port, names_from = target, values_from = npyH)
npyH_target[, -1]/apply(npyH_target[, -1], 1, sum) 
#similar information temporally
lapply(unique(int_boat$port), function(x){dat <- int_boat[int_boat$port == x, ]
expand.grid(list(year = unique(dat$year),
                 port = unique(dat$port),
                 fleet = unique(dat$fleet), 
                 target = unique(dat$target)))}) %>%
  do.call(rbind, .) %>%
  dplyr::left_join(int_boat, by = c("year", "port", "fleet", "target")) %>%
  ggplot(aes(x = year, weight = npyH, fill = target)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  facet_grid(port ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(1993, 20017, 3)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")



# * Interview stats ------------------------------------------------------
# Boat scale distribution of Rockfish harvest, total effort and CPUE by target species and fleet.
plot_int(int_boat, "pH")
plot_int(int_boat, "npyH")
#On the area scale Harvest is spread across several target groups.
int_boat %>%
  dplyr::group_by(year, fleet, target, area) %>%
  dplyr::summarize(H = sum(pH), E = sum(E)) %>%
  plot_int("H")
int_boat %>%
  dplyr::group_by(year, fleet, target, area) %>%
  dplyr::summarize(H = sum(npyH), E = sum(E)) %>%
  plot_int("H")


# * Alternative filters -----------------------------------------------------
#  * * Rockfish>target -------------------------------------------------------
int_pgtt <- int_boat[(!(int_boat$npyH + int_boat$lH + int_boat$hH >= int_boat$pH) & 
                       int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Lingcod")) &
                        !(int_boat$hH >= int_boat$pH & int_boat$target == "Halibut") & 
                        int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Halibut", "Lingcod"), ]
plot_int(int_pgtt , "pH")
table(int_pgtt$port) / table(int_boat$port)
aggregate(pH ~ port, int_pgtt, sum)[,2] / aggregate(pH ~ port, int_boat, sum)[,2]

int_npygtt <- int_boat[(!(int_boat$pH + int_boat$lH + int_boat$hH >= int_boat$npyH) & 
                        int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Lingcod")) &
                       !(int_boat$hH >= int_boat$pH & int_boat$target == "Halibut") & 
                       int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Halibut", "Lingcod"), ]
plot_int(int_npygtt, "npyH")
table(int_npygtt$port) / table(int_boat$port)
aggregate(npyH ~ port, int_npygtt, sum)[,2] / aggregate(npyH ~ port, int_boat, sum)[,2]

# * * CPUE > 1 --------------------------------------------------------------
#Martin suggests a hard number of lingcod per boat but I think we should at least scale it by angler days.
#H/E > 1
#applies to moderate ammint of trips
sapply(unique(int_boat$port), function(x){
  sum(!is.na(int_boat$E) & int_boat$pH / int_boat$E > 1 & int_boat$port == x, na.rm = TRUE) / 
    sum(!is.na(int_boat$E) & int_boat$port == x, na.rm = TRUE)
})
#But those trips make up a fair bit of the harvest
sapply(unique(int_boat$port), function(x){
  sum(int_boat$lH[!is.na(int_boat$E) & int_boat$lH / int_boat$E > 1 & int_boat$port == x], na.rm = TRUE) / 
    sum(int_boat$lH[!is.na(int_boat$E) & int_boat$port == x], na.rm = TRUE)
})

#applies to moderate amm0nt of trips
sapply(unique(int_boat$port), function(x){
  sum(!is.na(int_boat$E) & int_boat$npyH / int_boat$E > 1 & int_boat$port == x, na.rm = TRUE) / 
    sum(!is.na(int_boat$E) & int_boat$port == x, na.rm = TRUE)
})
#But those trips make up a fair bit of the harvest
sapply(unique(int_boat$port), function(x){
  sum(int_boat$lH[!is.na(int_boat$E) & int_boat$npyH / int_boat$E > 1 & int_boat$port == x], na.rm = TRUE) / 
    sum(int_boat$lH[!is.na(int_boat$E) & int_boat$port == x], na.rm = TRUE)
})

int_CPUE1 <- int_boat[!is.na(int_boat$E) & int_boat$lH / int_boat$E > 1, ]
plot_int(int_CPUE1, "lH")
table(int_CPUE1$port) / table(int_boat$port)
aggregate(lH ~ port, int_CPUE1, sum)[,2] / aggregate(lH ~ port, int_boat, sum)[,2]


# * H comp by target group -----------------------------------------------
H_comp <- 
  lapply(unique(int_boat$port), function(x){
    dat <- int_boat[int_boat$port == x, ]
    expand.grid(list(year = unique(dat$year),
                     port = unique(dat$port),
                     fleet = unique(dat$fleet), 
                     target = unique(dat$target)))}) %>%
  do.call(rbind, .) %>%
  dplyr::left_join(int_boat, by = c("year", "port", "fleet", "target")) %>%
  dplyr::select(year, fleet, port, target, hH, pH, npyH, lH) %>%
  tidyr::pivot_longer(cols = dplyr::ends_with("H"))

# * * Halibut trips -------------------------------------------------------
ggplot(H_comp[H_comp$target == "Halibut", ], aes(x = year, weight = value, fill = name)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  facet_grid(port ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(1993, 20017, 3)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")

# * * Bottomfish trips ----------------------------------------------------
ggplot(H_comp[H_comp$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Lingcod"), ], aes(x = year, weight = value, fill = name)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  facet_grid(port ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(1993, 20017, 3)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")


# Effort Composition -----------------------------------------------------
int_E <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarise(H = sum(npyH, na.rm = TRUE), E = sum(E, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(year >= 2000)
jags_datE <- lapply(ports, make_jagsdat, dat = int_E, stat = "E")

#data for appendix
years_E <- lapply(ports, function(x) range(int_E$year[int_E$port == x]))
tab_E <- lapply(1:5, function(x) tab_data(years_E[[x]][1]:years_E[[x]][2], jags_datE[[x]], areas[[x]]))
names(tab_E) <- ports
WriteXLS::WriteXLS(tab_E, ".\\Rockfish report_96-19\\tab_Ebottomfish.xlsx")

ni <- 5E5; nb <- ni/3; nc <- 3; nt <- 1000
# * Homer Harvest -----------------------------------------------------------
#None of the models are great.
#Need to omit Private data... too sparse.
postE_Homer <- mapply(jagsUI::jags,
                        parameters.to.save = params, model.file = models,
                        MoreArgs = list(data = jags_datE[[1]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE),
                        SIMPLIFY = FALSE)
lapply(postE_Homer, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postE_Homer, ".\\Rockfish report_96-19\\Interview post\\postE_Homer.rds")
#postE_Homer  <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Homer.rds")
postE_Homer[[1]]
postE_Homer[[2]]
postE_Homer[[3]]
postE_Homer[[4]]
data.frame(Deviance = sapply(postE_Homer, function(x) x$mean$deviance),
           pD = sapply(postE_Homer, function(x) x$pD), 
           DIC = sapply(postE_Homer, function(x) x$DIC),
           sd = sapply(postE_Homer, function(x) x$mean$sd))
plot_post(postE_Homer[[1]], int_E, "E", ports[1])
plot_post(postE_Homer[[2]], int_E, "E", ports[1])
plot_post(postE_Homer[[3]], int_E, "E", ports[1])
plot_post(postE_Homer[[4]], int_E, "E", ports[1])
jags_datE[[1]]$M


ni <- 1E5; nb <- ni/3; nc <- 3; nt <- 200
# * Kodiak Harvest --------------------------------------------------------
#
#
postE_Kodiak <- mapply(jagsUI::jags,
                         parameters.to.save = params, model.file = models,
                         MoreArgs = list(data = jags_datE[[2]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                         SIMPLIFY = FALSE)
lapply(postE_Kodiak, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postE_Kodiak, ".\\Rockfish report_96-19\\Interview post\\postE_Kodiak.rds")
#postE_Kodiak  <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Kodiak.rds")
postE_Kodiak[[1]]
postE_Kodiak[[2]]
postE_Kodiak[[3]]
postE_Kodiak[[4]]
data.frame(Deviance = sapply(postE_Kodiak, function(x) x$mean$deviance),
           pD = sapply(postE_Kodiak, function(x) x$pD), 
           DIC = sapply(postE_Kodiak, function(x) x$DIC),
           sd = sapply(postE_Kodiak, function(x) x$mean$sd))
plot_post(postE_Kodiak[[1]], int_E, "E", ports[2])
plot_post(postE_Kodiak[[2]], int_E, "E", ports[2])
plot_post(postE_Kodiak[[3]], int_E, "E", ports[2])
plot_post(postE_Kodiak[[4]], int_E, "E", ports[2])
jags_datE[[2]]$M




# * Seward Harvest --------------------------------------------------------
#
#
postE_Seward <- mapply(jagsUI::jags,
                         parameters.to.save = params, model.file = models,
                         MoreArgs = list(data = jags_datE[[3]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                         SIMPLIFY = FALSE)
lapply(postE_Seward, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postE_Seward, ".\\Rockfish report_96-19\\Interview post\\postE_Seward.rds")
#postE_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Seward.rds")
postE_Seward[[1]]
postE_Seward[[2]]
postE_Seward[[3]]
postE_Seward[[4]]
data.frame(Deviance = sapply(postE_Seward, function(x) x$mean$deviance),
           pD = sapply(postE_Seward, function(x) x$pD), 
           DIC = sapply(postE_Seward, function(x) x$DIC),
           sd = sapply(postE_Seward, function(x) x$mean$sd))
plot_post(postE_Seward[[1]], int_E, "E", ports[3])
plot_post(postE_Seward[[2]], int_E, "E", ports[3])
plot_post(postE_Seward[[3]], int_E, "E", ports[3])
plot_post(postE_Seward[[4]], int_E, "E", ports[3])







# * Valdez Harvest --------------------------------------------------------
#
#
postE_Valdez <- mapply(jagsUI::jags,
                         parameters.to.save = params, model.file = models,
                         MoreArgs = list(data = jags_datE[[4]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                         SIMPLIFY = FALSE)
lapply(postE_Valdez, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postE_Valdez, ".\\Rockfish report_96-19\\Interview post\\postE_Valdez.rds")
#postE_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Valdez.rds")
postE_Valdez[[1]]
postE_Valdez[[2]]
postE_Valdez[[3]]
postE_Valdez[[4]]
data.frame(Deviance = sapply(postE_Valdez, function(x) x$mean$deviance),
           pD = sapply(postE_Valdez, function(x) x$pD), 
           DIC = sapply(postE_Valdez, function(x) x$DIC),
           sd = sapply(postE_Valdez, function(x) x$mean$sd))
lapply(postE_Valdez, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)
plot_post(postE_Valdez[[1]], int_E, "E", ports[4])
plot_post(postE_Valdez[[2]], int_E, "E", ports[4])
plot_post(postE_Valdez[[3]], int_E, "E", ports[4])
plot_post(postE_Valdez[[4]], int_E, "E", ports[4])
jags_datE[[4]]$M






# * Whittier Harvest ------------------------------------------------------
#
#
postE_Whittier <- mapply(jagsUI::jags,
                           parameters.to.save = params, model.file = models,
                           MoreArgs = list(data = jags_datE[[5]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                           SIMPLIFY = FALSE)
lapply(postE_Whittier, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
#saveRDS(postE_Whittier, ".\\Rockfish report_96-19\\Interview post\\postE_Whittier.rds")
#postE_Whittier  <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Whittier.rds")
postE_Whittier[[1]]
postE_Whittier[[2]]
postE_Whittier[[3]]
postE_Whittier[[4]]
data.frame(Deviance = sapply(postE_Whittier, function(x) x$mean$deviance),
           pD = sapply(postE_Whittier, function(x) x$pD), 
           DIC = sapply(postE_Whittier, function(x) x$DIC),
           sd = sapply(postE_Whittier, function(x) x$mean$sd))
lapply(postE_Whittier, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)
plot_post(postE_Whittier[[1]], int_E, "E", ports[5])
plot_post(postE_Whittier[[2]], int_E, "E", ports[5])
plot_post(postE_Whittier[[3]], int_E, "E", ports[5])
plot_post(postE_Whittier[[4]], int_E, "E", ports[5])
