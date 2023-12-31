#Looking for a method to fit groundfish harvest and effort composition data for reporting
#would like to detect a trend to composition data
library(magrittr)
library(ggplot2)

# Read in raw data, functions and model code
int_boat <- readRDS(".\\Data\\int_boat.rds")
source(".\\functions.R")
source(".\\models.R")


# Pelagic Harvest Composition -----------------------------------------------------
#Get pelagic rockfish harvest data
int_pr <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarise(H = sum(pH, na.rm = TRUE), E = sum(E, na.rm = TRUE)) %>%
  dplyr::ungroup()
# a list of ports to lapply over later
ports <- unique(int_pr$port)
#a list of areas associated with each port
areas <- lapply(ports, function(x) sort(unique(int_pr$area[int_pr$port == x])))
aggregate(E ~ year + port, int_pr, sum)
jags_datH <- lapply(ports, make_jagsdat, dat = int_pr, stat = "H")

# #jags w overdisersion on each observation
# #set up parameter and model files which can be looped over for each combination of port and statistic
# #Four models considered: 1) constant shared mean, 2) unique constant mean for each angler type, 3) trending shared mean, 4) unique trending mean for each angler type. 
# #(posteriors contain output for each model in the order listed here)
# ni <- 1E5; nb <- ni/3; nc <- 3; nt <- 200
# params <- list(parameters_alpha <- c("alpha", "sd", "re"),
#                parameters_beta <- c("alpha", "beta", "re", "sd"),
#                parameters_epsilon <- c("alpha", "beta", "epsilon", "re", "sd"),
#                parameters_gamma <- c("alpha", "beta", "epsilon", "gamma", "sd", "re"))
# models <- list(alpha <- modfile_alpha,
#                beta <- modfile_beta,
#                epsilon <- modfile_epsilon,
#                gamma <- modfile_gamma)
# 
# 
# # * Homer Pelagic Harvest -----------------------------------------------------------
# #None of the models are great.
# #Need to omit Private data... too sparse.
# postHp_Homer <- mapply(jagsUI::jags,
#                      parameters.to.save = params, model.file = models,
#                      MoreArgs = list(data = jags_datH[[1]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                      SIMPLIFY = FALSE)
# lapply(postHp_Homer, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHp_Homer, ".\\Rockfish report_96-19\\Interview post\\postHp_Homer.rds")
postHp_Homer <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHp_Homer.rds")
#shared constant mean
postHp_Homer[[1]]
jagsUI::traceplot(postHp_Homer[[1]], c("alpha"))
plot_post(postHp_Homer[[1]], int_pr, "H", ports[1], inc_pred = "mean")
#constant mean differs by angler type
postHp_Homer[[2]]
jagsUI::traceplot(postHp_Homer[[2]], c("alpha", "beta"))
plot_post(postHp_Homer[[2]], int_pr, "H", ports[1], inc_pred = "mean")
#Can't get model to converge for private anglers


#Try with only Homer Charter data only
#modify jags dataset
# jags_datH_Homer <- jags_datH[[1]]
# jags_datH_Homer$count <- jags_datH[[1]]$count[,1,]
# jags_datH_Homer$M <- jags_datH[[1]]$M[,1]
# ni <- 5E5; nb <- ni/3; nc <- 3; nt <- 50
# postHp_HomerCharter <- mapply(jagsUI::jags,
#                              parameters.to.save = params[c(1, 3)], model.file = list(alpha <- modfile_alpha0,epsilon <- modfile_epsilon0),
#                              MoreArgs = list(data = jags_datH_Homer, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                              SIMPLIFY = FALSE)
# lapply(postHp_HomerCharter, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHp_HomerCharter, ".\\Rockfish report_96-19\\Interview post\\postHp_HomerCharter.rds")
postHp_HomerCharter <- readRDS( ".\\Rockfish report_96-19\\Interview post\\postHp_HomerCharter.rds")
#constant mean
postHp_HomerCharter[[1]]
jagsUI::traceplot(postHp_HomerCharter[[1]], c("alpha"))
#trending mean
#model selection by loo cross validation (see multinomial LOOCV.R)
#I save the preferred model in this code to present later
postHp_HomerCharter[[2]]
(plot_HomerHp <- 
  plot_post(postHp_HomerCharter[[1]], 
            int_pr, 
            "H", 
            ports[1], 
            inc_pred = "mean", 
            charteronly = TRUE,
            title = "Homer: Pelagic Rockfish"))
plot_post(postHp_HomerCharter[[2]], int_pr[int_pr$fleet == "Charter", ], "H", ports[1], inc_pred = "mean")


# * Kodiak Pelagic Harvest --------------------------------------------------------
# postHp_Kodiak <- mapply(jagsUI::jags,
#                        parameters.to.save = params, model.file = models,
#                        MoreArgs = list(data = jags_datH[[2]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                        SIMPLIFY = FALSE)
# lapply(postHp_Kodiak, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHp_Kodiak, ".\\Rockfish report_96-19\\Interview post\\postHp_Kodiak.rds")
postHp_Kodiak <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHp_Kodiak.rds")
postHp_Kodiak[[1]]
# Unique constant mean model preferred
postHp_Kodiak[[2]]
jagsUI::traceplot(postHp_Kodiak[[2]], c("alpha", "beta"))
postHp_Kodiak[[3]]
postHp_Kodiak[[4]]
plot_post(postHp_Kodiak[[1]], int_pr, "H", ports[2])
# Unique constant mean model preferred
(plot_KodiakHp <- plot_post(postHp_Kodiak[[2]], int_pr, "H", ports[2], title= "Kodiak: Pelagic Rockfish"))
plot_post(postHp_Kodiak[[3]], int_pr, "H", ports[2])
plot_post(postHp_Kodiak[[4]], int_pr, "H", ports[2])
jags_datH[[2]]$M




# * Seward Pelagic Harvest --------------------------------------------------------
# postHp_Seward <- mapply(jagsUI::jags,
#                        parameters.to.save = params, model.file = models,
#                        MoreArgs = list(data = jags_datH[[3]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                        SIMPLIFY = FALSE)
# lapply(postHp_Seward, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHp_Seward, ".\\Rockfish report_96-19\\Interview post\\postHp_Seward.rds")
postHp_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHp_Seward.rds")
postHp_Seward[[1]]
postHp_Seward[[2]]
# shared trending mean model preferred
postHp_Seward[[3]]
jagsUI::traceplot(postHp_Seward[[3]], c("alpha", "beta", "epsilon"))
postHp_Seward[[4]]
plot_post(postHp_Seward[[1]], int_pr, "H", ports[3])
plot_post(postHp_Seward[[2]], int_pr, "H", ports[3])
# shared trending mean model preferred
(plot_SewardHp <- plot_post(postHp_Seward[[3]], int_pr, "H", ports[3], title = "Seward: Pelagic Rockfish"))
plot_post(postHp_Seward[[4]], int_pr, "H", ports[3])







# * Valdez Pelagic Harvest --------------------------------------------------------
# postHp_Valdez <- mapply(jagsUI::jags,
#                        parameters.to.save = params, model.file = models,
#                        MoreArgs = list(data = jags_datH[[4]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                        SIMPLIFY = FALSE)
# lapply(postHp_Valdez, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHp_Valdez, ".\\Rockfish report_96-19\\Interview post\\postHp_Valdez.rds")
postHp_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHp_Valdez.rds")
postHp_Valdez[[1]]
postHp_Valdez[[2]]
# shared trending mean model preferred
postHp_Valdez[[3]]
jagsUI::traceplot(postHp_Valdez[[3]], c("alpha", "beta", "epsilon"))
postHp_Valdez[[4]]
plot_post(postHp_Valdez[[1]], int_pr, "H", ports[4])
plot_post(postHp_Valdez[[2]], int_pr, "H", ports[4])
# shared trending mean model preferred
(plot_ValdezHp <- plot_post(postHp_Valdez[[3]], int_pr, "H", ports[4], title = "Valdez: Pelagic Rockfish"))
plot_post(postHp_Valdez[[4]], int_pr, "H", ports[4])






# * Whittier Pelagic Harvest ------------------------------------------------------
# postHp_Whittier <- mapply(jagsUI::jags,
#                          parameters.to.save = params, model.file = models,
#                          MoreArgs = list(data = jags_datH[[5]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                          SIMPLIFY = FALSE)
# lapply(postHp_Whittier, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHp_Whittier, ".\\Rockfish report_96-19\\Interview post\\postHp_Whittier.rds")
postHp_Whittier <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHp_Whittier.rds")
postHp_Whittier[[1]]
# Unique constant mean model preferred
postHp_Whittier[[2]]
jagsUI::traceplot(postHp_Whittier[[2]], c("alpha", "beta"))
postHp_Whittier[[3]]
postHp_Whittier[[4]]
#Large variance estimated for random effect.
#We are assuming our model section criteria is robust to this variability.
#but need to make sure biologists understand the models are used to distinguish patterns rather than make predictions.
lapply(postHp_Whittier, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)
plot_post(postHp_Whittier[[1]], int_pr, "H", ports[5])
# Unique constant mean model preferred
(plot_WhittierHp <- plot_post(postHp_Whittier[[2]], int_pr, "H", ports[5], title = "Whittier: Pelagic Rockfish"))
plot_post(postHp_Whittier[[3]], int_pr, "H", ports[5])
plot_post(postHp_Whittier[[4]], int_pr, "H", ports[5])





# Non-Pelagic Harvest Composition -----------------------------------------------------
#follows format above
int_npr <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarise(H = sum(npyH, na.rm = TRUE), E = sum(E, na.rm = TRUE)) %>%
  dplyr::ungroup()
jags_datHnp <- lapply(ports, make_jagsdat, dat = int_npr, stat = "H")


# # * Homer Non-Pelagic Harvest -----------------------------------------------------------
# #None of the models are great.
# #Need to omit Private data... too sparse.
# postHnp_Homer <- mapply(jagsUI::jags,
#                       parameters.to.save = params, model.file = models,
#                       MoreArgs = list(data = jags_datHnp[[1]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                       SIMPLIFY = FALSE)
# lapply(postHnp_Homer, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHnp_Homer, ".\\Rockfish report_96-19\\Interview post\\postHnp_Homer.rds")
postHnp_Homer  <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Homer.rds")
postHnp_Homer[[1]]
plot_post(postHnp_Homer[[1]], int_npr, "H", ports[1])
jags_datHnp[[1]]$M
#Insufficient Private data

#Try with only Homer Charter data only
# jags_datHnp_Homer <- jags_datHnp[[1]]
# jags_datHnp_Homer$count <- jags_datHnp[[1]]$count[,1,]
# jags_datHnp_Homer$M <- jags_datHnp[[1]]$M[,1]
# ni <- 1E5; nb <- ni/3; nc <- 3; nt <- 50
# postHnp_HomerCharter <- mapply(jagsUI::jags,
#                              parameters.to.save = params[c(1, 3)], model.file = list(alpha <- modfile_alpha0,epsilon <- modfile_epsilon0),
#                              MoreArgs = list(data = jags_datHnp_Homer, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                              SIMPLIFY = FALSE)
# lapply(postHnp_HomerCharter, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHnp_HomerCharter, ".\\Rockfish report_96-19\\Interview post\\postHnp_HomerCharter.rds")
postHnp_HomerCharter <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_HomerCharter.rds")
postHnp_HomerCharter[[1]]
jagsUI::traceplot(postHnp_HomerCharter[[1]], c("alpha"))
postHnp_HomerCharter[[2]]
#constant mean model preferred
(plot_HomerHnp <-
  plot_post(postHnp_HomerCharter[[1]], 
            int_npr, 
            "H", 
            ports[1], 
            charteronly = TRUE,
            inc_pred = "mean", title = "Homer: Non-Pelagic Rockfish"))
plot_post(postHnp_HomerCharter[[2]], int_npr[int_npr$fleet == "Charter", ], "H", ports[1], inc_pred = "mean")


# # * Kodiak Non-Pelagic Harvest --------------------------------------------------------
# postHnp_Kodiak <- mapply(jagsUI::jags,
#                        parameters.to.save = params, model.file = models,
#                        MoreArgs = list(data = jags_datHnp[[2]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                        SIMPLIFY = FALSE)
# lapply(postHnp_Kodiak, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHnp_Kodiak, ".\\Rockfish report_96-19\\Interview post\\postHnp_Kodiak.rds")
postHnp_Kodiak  <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Kodiak.rds")
postHnp_Kodiak[[1]]
jagsUI::traceplot(postHnp_Kodiak[[1]], c("alpha"))
postHnp_Kodiak[[2]]
postHnp_Kodiak[[3]]
postHnp_Kodiak[[4]]
plot_post(postHnp_Kodiak[[1]], int_npr, "H", ports[2])
plot_post(postHnp_Kodiak[[2]], int_npr, "H", ports[2], title = "Kodiak: Non-Pelagic Rockfish")
plot_post(postHnp_Kodiak[[3]], int_npr, "H", ports[2])
plot_post(postHnp_Kodiak[[4]], int_npr, "H", ports[2])
jags_datHnp[[2]]$M
#Private data is too sparse for non-pelagic rockfish in Kodiak


#Try with only Kodiak Charter data only
# jags_datHnp_Kodiak <- jags_datHnp[[2]]
# jags_datHnp_Kodiak$count <- jags_datHnp[[2]]$count[,1,]
# jags_datHnp_Kodiak$M <- jags_datHnp[[2]]$M[,1]
# ni <- 1E5; nb <- ni/3; nc <- 3; nt <- 50
# postHnp_KodiakCharter <- mapply(jagsUI::jags,
#                              parameters.to.save = params[c(1, 3)], model.file = list(alpha <- modfile_alpha0,epsilon <- modfile_epsilon0),
#                              MoreArgs = list(data = jags_datHnp_Kodiak, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                              SIMPLIFY = FALSE)
# lapply(postHnp_KodiakCharter, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHnp_KodiakCharter, ".\\ROckfish report_96-19\\Interview post\\postHnp_KodiakCharter.rds")
postHnp_KodiakCharter <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_KodiakCharter.rds")
postHnp_KodiakCharter[[1]]
postHnp_KodiakCharter[[2]]
#Trending mean model prefered
(plot_KodiakHnp <-
    plot_post(postHnp_KodiakCharter[[2]],
              int_npr,
              "H",
              ports[2],
              charteronly = TRUE,
              inc_pred = "mean", title = "Kodiak: Non-Pelagic Rockfish"))
plot_post(postHnp_KodiakCharter[[2]], int_npr, "H", ports[2], charteronly = TRUE, inc_pred = "mean")




# * Seward Non-Pelagic Harvest --------------------------------------------------------
# postHnp_Seward <- mapply(jagsUI::jags,
#                        parameters.to.save = params, model.file = models,
#                        MoreArgs = list(data = jags_datHnp[[3]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                        SIMPLIFY = FALSE)
# lapply(postHnp_Seward, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHnp_Seward, ".\\Rockfish report_96-19\\Interview post\\postHnp_Seward.rds")
postHnp_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Seward.rds")
postHnp_Seward[[1]]
postHnp_Seward[[2]]
postHnp_Seward[[3]]
jagsUI::traceplot(postHnp_Seward[[3]], c("alpha", "beta", "epsilon"))
postHnp_Seward[[4]]
plot_post(postHnp_Seward[[1]], int_npr, "H", ports[3])
plot_post(postHnp_Seward[[2]], int_npr, "H", ports[3])
#shared trending mean model preferred
(plot_SewardHnp <- plot_post(postHnp_Seward[[3]], int_npr, "H", ports[3], title = "Seward:Non-Pelagic Rockfish"))
plot_post(postHnp_Seward[[4]], int_npr, "H", ports[3])







# * Valdez Non-Pelagic Harvest --------------------------------------------------------
# postHnp_Valdez <- mapply(jagsUI::jags,
#                        parameters.to.save = params, model.file = models,
#                        MoreArgs = list(data = jags_datHnp[[4]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                        SIMPLIFY = FALSE)
# lapply(postHnp_Valdez, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHnp_Valdez, ".\\Rockfish report_96-19\\Interview post\\postHnp_Valdez.rds")
postHnp_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Valdez.rds")
postHnp_Valdez[[1]]
postHnp_Valdez[[2]]
postHnp_Valdez[[3]]
postHnp_Valdez[[4]]
jagsUI::traceplot(postHnp_Valdez[[4]], c("alpha", "beta", "epsilon", "gamma"))
plot_post(postHnp_Valdez[[1]], int_npr, "H", ports[4])
plot_post(postHnp_Valdez[[2]], int_npr, "H", ports[4])
plot_post(postHnp_Valdez[[3]], int_npr, "H", ports[4], title = "Valdez: Non-Pelagic Rockfish")
#Unique trending mean model preferred
(plot_ValdezHnp <- plot_post(postHnp_Valdez[[4]], int_npr, "H", ports[4], title = "Valdez: Non-Pelagic Rockfish"))





# * Whittier Non-Pelagic Harvest ------------------------------------------------------
# postHnp_Whittier <- mapply(jagsUI::jags,
#                          parameters.to.save = params, model.file = models,
#                          MoreArgs = list(data = jags_datHnp[[5]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                          SIMPLIFY = FALSE)
# lapply(postHnp_Whittier, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postHnp_Whittier, ".\\Rockfish report_96-19\\Interview post\\postHnp_Whittier.rds")
postHnp_Whittier  <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Whittier.rds")
postHnp_Whittier[[1]]
postHnp_Whittier[[2]]
jagsUI::traceplot(postHnp_Whittier[[2]], c("alpha", "beta"))
postHnp_Whittier[[3]]
postHnp_Whittier[[4]]
plot_post(postHnp_Whittier[[1]], int_npr, "H", ports[5])
#unique constant mean model preferred
(plot_WhittierHnp <- plot_post(postHnp_Whittier[[2]], int_npr, "H", ports[5], title = "Whittier: Non-Pelagic Rockfish"))
plot_post(postHnp_Whittier[[3]], int_npr, "H", ports[5])
plot_post(postHnp_Whittier[[4]], int_npr, "H", ports[5])
jags_datHnp[[5]]$M





# Effort Description --------------------------------------------------
#Not sure how to best describe effort since it is reported by trip and most trips include salmon and halibut effort that may not correlate with rockfish effort.
#In the past they have excluded halibut trips but Martin wanted to see if there was a better option.
#Many more halibut and salmon+bottom fish trips than anything else.
# * Harvest by target group ---------------------------------------
table(int_boat$year, int_boat$target)
table(int_boat$target)


#  * *  Pelagic Rockfish  -----------------------------------------------------
#pelagic harvest distributed between bottomfish, salmon and bottomfish and halibut trips. port dependent (rows in table below)
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
#non-pelagic harvest distributed between bottomfish, salmon and bottomfish and halibut trips. port dependent (rows in table below)
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
# very skewed data
plot_int(int_boat, "pH")
plot_int(int_boat, "npyH")
#On the area scale Harvest is concentrated in bottomfish, salmon and bottomfish and halibut trips.
int_boat %>%
  dplyr::group_by(year, fleet, target, area) %>%
  dplyr::summarize(H = sum(pH), E = sum(E)) %>%
  plot_int("H")
int_boat %>%
  dplyr::group_by(year, fleet, target, area) %>%
  dplyr::summarize(H = sum(npyH), E = sum(E)) %>%
  plot_int("H")


# * Alternative filters -----------------------------------------------------
# Filter to only include trips where rockfish were the majority of the harvest
#  * * Rockfish>target -------------------------------------------------------
#pelagic rockfish
int_pgtt <- int_boat[(!(int_boat$npyH + int_boat$lH + int_boat$hH >= int_boat$pH) & 
                       int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Lingcod")) &
                        !(int_boat$hH >= int_boat$pH & int_boat$target == "Halibut") & 
                        int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Halibut", "Lingcod"), ]
plot_int(int_pgtt , "pH")
#small percentage of interviews retained
table(int_pgtt$port) / table(int_boat$port)
#better percentage of harvest retained
aggregate(pH ~ port, int_pgtt, sum)[,2] / aggregate(pH ~ port, int_boat, sum)[,2]

#non-pelagic rockfish
int_npygtt <- int_boat[(!(int_boat$pH + int_boat$lH + int_boat$hH >= int_boat$npyH) & 
                        int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Lingcod")) &
                       !(int_boat$hH >= int_boat$pH & int_boat$target == "Halibut") & 
                       int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Halibut", "Lingcod"), ]
plot_int(int_npygtt, "npyH")
#small percentage of interviews and harvest retained
table(int_npygtt$port) / table(int_boat$port)
aggregate(npyH ~ port, int_npygtt, sum)[,2] / aggregate(npyH ~ port, int_boat, sum)[,2]



# * * CPUE > 1 --------------------------------------------------------------
#Martin suggested a hard number of rockfish per boat but I think we should at least scale it by angler days... i.e. H/E > 1
#pelagic rockfish
#applies to moderate amount of trips
sapply(unique(int_boat$port), function(x){
  sum(!is.na(int_boat$E) & int_boat$pH / int_boat$E > 1 & int_boat$port == x, na.rm = TRUE) / 
    sum(!is.na(int_boat$E) & int_boat$port == x, na.rm = TRUE)
})
#But those trips make up a fair bit of the harvest
sapply(unique(int_boat$port), function(x){
  sum(int_boat$lH[!is.na(int_boat$E) & int_boat$lH / int_boat$E > 1 & int_boat$port == x], na.rm = TRUE) / 
    sum(int_boat$lH[!is.na(int_boat$E) & int_boat$port == x], na.rm = TRUE)
})

#non-pelagic rockfish
#applies to moderate amount of trips
sapply(unique(int_boat$port), function(x){
  sum(!is.na(int_boat$E) & int_boat$npyH / int_boat$E > 1 & int_boat$port == x, na.rm = TRUE) / 
    sum(!is.na(int_boat$E) & int_boat$port == x, na.rm = TRUE)
})
#But those trips make up a fair bit of the harvest
sapply(unique(int_boat$port), function(x){
  sum(int_boat$lH[!is.na(int_boat$E) & int_boat$npyH / int_boat$E > 1 & int_boat$port == x], na.rm = TRUE) / 
    sum(int_boat$lH[!is.na(int_boat$E) & int_boat$port == x], na.rm = TRUE)
})



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
#Mostly halibut for most areas/user groups
ggplot(H_comp[H_comp$target == "Halibut", ], aes(x = year, weight = value, fill = name)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  facet_grid(port ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(1993, 20017, 3)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")

# * * Bottomfish trips ----------------------------------------------------
#large percentages of halibut for most areas/user groups but less than 50% for most areas/user groups
ggplot(H_comp[H_comp$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Lingcod"), ], aes(x = year, weight = value, fill = name)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  facet_grid(port ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(1993, 20017, 3)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")




# Effort Composition -----------------------------------------------------
#Elected to eliminate Halibut focused trips from the analysis. This is consistent with past practices and the other options we identified eliminated so may interviews.
#As such this section really describes bottomfish effort in general and could be replicated unaltered in a future lingcod report.
int_E <-
  int_boat[int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Lingcod"), ] %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarise(H = sum(npyH, na.rm = TRUE), E = sum(E, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(year >= 2000)
jags_datE <- lapply(ports, make_jagsdat, dat = int_E, stat = "E")

# # * Homer Effort --------------------------------------------------------
# ni <- 5E6; nb <- ni/3; nc <- 3; nt <- 1000
# postE_Homer <- mapply(jagsUI::jags,
#                         parameters.to.save = params, model.file = models,
#                         MoreArgs = list(data = jags_datE[[1]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE, parallel = TRUE),
#                         SIMPLIFY = FALSE)
# lapply(postE_Homer, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postE_Homer, ".\\Rockfish report_96-19\\Interview post\\postE_Homer.rds")
postE_Homer  <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Homer.rds")
postE_Homer[[1]]
postE_Homer[[2]]
#shared trend model preferred
#note ns differences for unique mean
postE_Homer[[3]]
jagsUI::traceplot(postE_Homer[[3]], c("alpha", "beta", "epsilon")) #Beta converges poorly.
postE_Homer[[4]]
plot_post(postE_Homer[[1]], int_E, "E", ports[1])
plot_post(postE_Homer[[2]], int_E, "E", ports[1])
#shared trend model preferred
(plot_HomerE <- plot_post(postE_Homer[[3]], int_E, "E", ports[1], title = "Homer: Effort"))
plot_post(postE_Homer[[4]], int_E, "E", ports[1])
jags_datE[[1]]$M

# ni <- 1E5; nb <- ni/3; nc <- 3; nt <- 200
# # * Kodiak Effort --------------------------------------------------------
# postE_Kodiak <- mapply(jagsUI::jags,
#                          parameters.to.save = params, model.file = models,
#                          MoreArgs = list(data = jags_datE[[2]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                          SIMPLIFY = FALSE)
# lapply(postE_Kodiak, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postE_Kodiak, ".\\Rockfish report_96-19\\Interview post\\postE_Kodiak.rds")
postE_Kodiak  <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Kodiak.rds")
postE_Kodiak[[1]]
#unique mean preferred
postE_Kodiak[[2]]
jagsUI::traceplot(postE_Kodiak[[2]], c("alpha", "beta"))
postE_Kodiak[[3]]
postE_Kodiak[[4]]
plot_post(postE_Kodiak[[1]], int_E, "E", ports[2])
#unique mean preferred
(plot_KodiakE <- plot_post(postE_Kodiak[[2]], int_E, "E", ports[2], title = "Kodiak: Effort"))
plot_post(postE_Kodiak[[3]], int_E, "E", ports[2])
plot_post(postE_Kodiak[[4]], int_E, "E", ports[2])




# * Seward Effort --------------------------------------------------------
# postE_Seward <- mapply(jagsUI::jags,
#                          parameters.to.save = params, model.file = models,
#                          MoreArgs = list(data = jags_datE[[3]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                          SIMPLIFY = FALSE)
# lapply(postE_Seward, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postE_Seward, ".\\Rockfish report_96-19\\Interview post\\postE_Seward.rds")
postE_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Seward.rds")
postE_Seward[[1]]
#unique mean preferred
postE_Seward[[2]]
jagsUI::traceplot(postE_Seward[[2]], c("alpha", "beta"))
postE_Seward[[3]]
postE_Seward[[4]]
plot_post(postE_Seward[[1]], int_E, "E", ports[3])
#unique mean preferred
(plot_SewardE <- plot_post(postE_Seward[[2]], int_E, "E", ports[3], title = "Seward: Effort"))
plot_post(postE_Seward[[3]], int_E, "E", ports[3])
plot_post(postE_Seward[[4]], int_E, "E", ports[3])







# * Valdez Effort --------------------------------------------------------
# postE_Valdez <- mapply(jagsUI::jags,
#                          parameters.to.save = params, model.file = models,
#                          MoreArgs = list(data = jags_datE[[4]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                          SIMPLIFY = FALSE)
# lapply(postE_Valdez, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postE_Valdez, ".\\Rockfish report_96-19\\Interview post\\postE_Valdez.rds")
postE_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Valdez.rds")
postE_Valdez[[1]]
postE_Valdez[[2]]
#shared trend model preferred
postE_Valdez[[3]]
jagsUI::traceplot(postE_Valdez[[3]], c("alpha", "beta", "epsilon"))
postE_Valdez[[4]]
plot_post(postE_Valdez[[1]], int_E, "E", ports[4])
plot_post(postE_Valdez[[2]], int_E, "E", ports[4])
#shared trend model preferred
(plot_ValdezE <- plot_post(postE_Valdez[[3]], int_E, "E", ports[4], title = "Valdez: Effort"))
plot_post(postE_Valdez[[4]], int_E, "E", ports[4])
jags_datE[[4]]$M


# * Whittier Effort ------------------------------------------------------
# postE_Whittier <- mapply(jagsUI::jags,
#                            parameters.to.save = params, model.file = models,
#                            MoreArgs = list(data = jags_datE[[5]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
#                            SIMPLIFY = FALSE)
# lapply(postE_Whittier, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
# saveRDS(postE_Whittier, ".\\Rockfish report_96-19\\Interview post\\postE_Whittier.rds")
postE_Whittier  <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Whittier.rds")
postE_Whittier[[1]]
#unique mean preferred
postE_Whittier[[2]]
jagsUI::traceplot(postE_Whittier[[2]], c("alpha", "beta"))
postE_Whittier[[3]]
postE_Whittier[[4]]
plot_post(postE_Whittier[[1]], int_E, "E", ports[5])
#unique mean preferred
(plot_WhittierE <- plot_post(postE_Whittier[[2]], int_E, "E", ports[5], title = "Whittier: Effort"))
plot_post(postE_Whittier[[3]], int_E, "E", ports[5])
plot_post(postE_Whittier[[4]], int_E, "E", ports[5])



# Save results ------------------------------------------------------------
#code to load all the posteriors
# postE_Homer <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Homer.rds")
# postE_Kodiak <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Kodiak.rds")
# postE_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Seward.rds")
# postE_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Valdez.rds")
# postE_Whittier <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Whittier.rds")
# postHnp_HomerCharter <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_HomerCharter.rds")
# postHnp_KodiakCharter <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_KodiakCharter.rds")
# postHnp_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Seward.rds")
# postHnp_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Valdez.rds")
# postHnp_Whittier <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Whittier.rds")
# postHp_HomerCharter <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHp_HomerCharter.rds")
# postHp_Kodiak <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHp_Kodiak.rds")
# postHp_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHp_Seward.rds")
# postHp_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHp_Valdez.rds")
# postHp_Whittier <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHp_Whittier.rds")

#save comp plots
comp_plots <- 
  list(plot_HomerE, plot_HomerHp, plot_HomerHnp,
       plot_KodiakE, plot_KodiakHp, plot_KodiakHnp,
       plot_SewardE, plot_SewardHp, plot_SewardHnp, 
       plot_ValdezE, plot_ValdezHp, plot_ValdezHnp,
       plot_WhittierE, plot_WhittierHp, plot_WhittierHnp)
saveRDS(comp_plots, ".\\Rockfish report_96-19\\comp_plots.rds")

#save model posterior estimates
mod_p <- 
  list(
    HomerE = p_post(postE_Homer[[3]], yrs = NULL, areas = areas[[1]]),
    HomerHp = p_post(postHp_HomerCharter[[1]], fleet = "charter", yrs = 0, areas = areas[[1]]),
    HomerHnp = p_post(postHnp_HomerCharter[[1]], fleet = "charter", yrs = 0, areas = areas[[1]]),
    KodiakE = p_post(postE_Kodiak[[2]], yrs = 0, areas = areas[[2]]),
    KodiakHp = p_post(postHp_Kodiak[[2]], yrs = 0, areas = areas[[2]]),
    KodiakHnp = p_post(postHnp_KodiakCharter[[2]], fleet = "charter", yrs = NULL, areas = areas[[2]]),
    SewardE = p_post(postE_Seward[[2]], yrs = 0, areas = areas[[3]]),
    SewardHp = p_post(postHp_Seward[[3]], yrs = NULL, areas = areas[[3]]),
    SewardHnp = p_post(postHnp_Seward[[3]], yrs = NULL, areas = areas[[3]]),
    ValdezE = p_post(postE_Valdez[[3]], yrs = NULL, areas = areas[[4]]),
    ValdezHp = p_post(postHp_Valdez[[3]], yrs = NULL, areas = areas[[4]]),
    ValdezHnp = p_post(postHnp_Valdez[[4]], yrs = NULL, areas = areas[[4]]),
    WhittierE = p_post(postE_Whittier[[2]], yrs = 0, areas = areas[[5]]),
    WhittierHp = p_post(postHp_Whittier[[2]], yrs = 0, areas = areas[[5]]),
    WhittierHnp = p_post(postHnp_Whittier[[2]], yrs = 0, areas = areas[[5]])
)
saveRDS(mod_p, ".\\Rockfish report_96-19\\mod_p.rds")

post_list <- list(postHp_Homer=postHp_HomerCharter, postHnp_Homer=postHnp_HomerCharter, postE_Homer=postE_Homer, 
                  postHp_Kodiak=postHp_Kodiak, postHnp_Kodiak=postHnp_KodiakCharter, postE_Kodiak=postE_Kodiak, 
                  postHp_Seward=postHp_Seward, postHnp_Seward=postHnp_Seward, postE_Seward=postE_Seward, 
                  postHp_Valdez=postHp_Valdez, postHnp_Valdez=postHnp_Valdez, postE_Valdez=postE_Valdez, 
                  postHp_Whittier=postHp_Whittier, postHnp_Whittier=postHnp_Whittier, postE_Whittier=postE_Whittier)
tab_sd <- 
  lapply(seq_along(post_list), get_sd, posts = post_list, obj_names = names(post_list)) %>% 
  do.call(rbind, .)
saveRDS(tab_sd, ".\\Rockfish report_96-19\\tab_sd.rds")
