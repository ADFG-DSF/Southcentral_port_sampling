#Looking for a method to fit groundfish harvest and effort compostion data for reporting
#would like to detect a trend to composition data
library(magrittr)
library(ggplot2)
int_boat <- readRDS(".\\Data\\int_boat.rds")
source(".\\functions.R")
source(".\\models.R")


# Harvest Composition -----------------------------------------------------
int_ling <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>% 
  dplyr::summarise(H = sum(lH), E = sum(E)) %>%
  dplyr::ungroup()
ports <- unique(int_ling$port)
aggregate(E ~ year + port, int_ling, sum)
jags_datH <- lapply(ports, make_jagsdat, dat = int_ling, stat = "H")

#jags w overdisersion on each observation
ni <- 1E4; nb <- ni/3; nc <- 3; nt <- 20
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
#Need to omit Parivate data... too sparse.
post_Homer <- mapply(jagsUI::jags,
                     parameters.to.save = params, model.file = models,
                     MoreArgs = list(data = jags_datH[[1]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                     SIMPLIFY = FALSE)
lapply(post_Homer, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
post_Homer[[1]]
post_Homer[[2]]
post_Homer[[3]]
post_Homer[[4]]
data.frame(Deviance = sapply(post_Homer, function(x) x$mean$deviance),
           pD = sapply(post_Homer, function(x) x$pD), 
           DIC = sapply(post_Homer, function(x) x$DIC),
           sd = sapply(post_Homer, function(x) x$mean$sd))
plot_post(post_Homer[[1]], int_ling, "H", ports[1])
plot_post(post_Homer[[2]], int_ling, "H", ports[1])
plot_post(post_Homer[[3]], int_ling, "H", ports[1])
plot_post(post_Homer[[4]], int_ling, "H", ports[1])

#Homer Charter data only
jags_datH[[1]]$F = 1
post_HomerCharter <- mapply(jagsUI::jags,
                            parameters.to.save = params[c(1, 3)], model.file = list(alpha <- modfile_alpha,epsilon <- modfile_epsilon0),
                            MoreArgs = list(data = jags_datH[[1]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                            SIMPLIFY = FALSE)
lapply(post_HomerCharter, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
post_HomerCharter[[1]]
post_HomerCharter[[2]]
data.frame(Deviance = sapply(post_HomerCharter, function(x) x$mean$deviance),
           pD = sapply(post_HomerCharter, function(x) x$pD), 
           DIC = sapply(post_HomerCharter, function(x) x$DIC),
           sd = sapply(post_HomerCharter, function(x) x$mean$sd))
plot_post(post_HomerCharter[[1]], int_ling, "H", ports[1])
plot_post(post_HomerCharter[[2]], int_ling, "H", ports[1])




# * Kodiak Harvest --------------------------------------------------------
#
#
postH_Kodiak <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datH[[2]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                       SIMPLIFY = FALSE)
lapply(postH_Kodiak, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
postH_Kodiak[[1]]
postH_Kodiak[[2]]
postH_Kodiak[[3]]
postH_Kodiak[[4]]
data.frame(Deviance = sapply(postH_Kodiak, function(x) x$mean$deviance),
           pD = sapply(postH_Kodiak, function(x) x$pD), 
           DIC = sapply(postH_Kodiak, function(x) x$DIC),
           sd = sapply(postH_Kodiak, function(x) x$mean$sd))
plot_post(postH_Kodiak[[1]], int_ling, "H", ports[2])
plot_post(postH_Kodiak[[2]], int_ling, "H", ports[2])
plot_post(postH_Kodiak[[3]], int_ling, "H", ports[2])
plot_post(postH_Kodiak[[4]], int_ling, "H", ports[2])




# * Seward Harvest --------------------------------------------------------
#
#
postH_Seward <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datH[[3]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                       SIMPLIFY = FALSE)
lapply(postH_Seward, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
postH_Seward[[1]]
postH_Seward[[2]]
postH_Seward[[3]]
postH_Seward[[4]]
data.frame(Deviance = sapply(postH_Seward, function(x) x$mean$deviance),
           pD = sapply(postH_Seward, function(x) x$pD), 
           DIC = sapply(postH_Seward, function(x) x$DIC),
           sd = sapply(postH_Seward, function(x) x$mean$sd))
plot_post(postH_Seward[[1]], int_ling, "H", ports[3])
plot_post(postH_Seward[[2]], int_ling, "H", ports[3])
plot_post(postH_Seward[[3]], int_ling, "H", ports[3])
plot_post(postH_Seward[[4]], int_ling, "H", ports[3])









# * Valdez Harvest --------------------------------------------------------
#
#
postH_Valdez <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datH[[4]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                       SIMPLIFY = FALSE)
lapply(postH_Valdez, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
postH_Valdez[[1]]
postH_Valdez[[2]]
postH_Valdez[[3]]
postH_Valdez[[4]]
data.frame(Deviance = sapply(postH_Valdez, function(x) x$mean$deviance),
           pD = sapply(postH_Valdez, function(x) x$pD), 
           DIC = sapply(postH_Valdez, function(x) x$DIC),
           sd = sapply(postH_Valdez, function(x) x$mean$sd))
plot_post(postH_Valdez[[1]], int_ling, "H", ports[4])
plot_post(postH_Valdez[[2]], int_ling, "H", ports[4])
plot_post(postH_Valdez[[3]], int_ling, "H", ports[4])
plot_post(postH_Valdez[[4]], int_ling, "H", ports[4])






# * Whittier Harvest ------------------------------------------------------
#
#
postH_Whittier <- mapply(jagsUI::jags,
                         parameters.to.save = params, model.file = models,
                         MoreArgs = list(data = jags_datH[[5]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                         SIMPLIFY = FALSE)
lapply(postH_Whittier, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
postH_Whittier[[1]]
postH_Whittier[[2]]
postH_Whittier[[3]]
postH_Whittier[[4]]
data.frame(Deviance = sapply(postH_Whittier, function(x) x$mean$deviance),
           pD = sapply(postH_Whittier, function(x) x$pD), 
           DIC = sapply(postH_Whittier, function(x) x$DIC),
           sd = sapply(postH_Whittier, function(x) x$mean$sd))
plot_post(postH_Whittier[[1]], int_ling, "H", ports[5])
plot_post(postH_Whittier[[2]], int_ling, "H", ports[5])
plot_post(postH_Whittier[[3]], int_ling, "H", ports[5])
plot_post(postH_Whittier[[4]], int_ling, "H", ports[5])




# Lingcod Effort --------------------------------------------------

# * Harvest by target group ---------------------------------------
#Bycatch dominates
lH_target <- 
  aggregate(lH ~ port + target, int_boat, sum) %>%
  tidyr::pivot_wider(id_cols = port, names_from = target, values_from = lH)
lH_target[, -1]/apply(lH_target[, -1], 1, sum)
#similar information temporally
lapply(unique(int_boat$port), function(x){dat <- int_boat[int_boat$port == x, ]
expand.grid(list(year = unique(dat$year),
                 port = unique(dat$port),
                 fleet = unique(dat$fleet), 
                 target = unique(dat$target)))}) %>%
  do.call(rbind, .) %>%
  dplyr::left_join(int_boat, by = c("year", "port", "fleet", "target")) %>%
  ggplot(aes(x = year, weight = lH, fill = target)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  facet_grid(port ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(1993, 20017, 3)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")

# #Pelagic Rockfish harvest by target group
# pH_target <- 
#   aggregate(pH ~ port + target, int_boat, sum) %>%
#   tidyr::pivot_wider(id_cols = port, names_from = target, values_from = pH)
# pH_target[, -1]/apply(pH_target[, -1], 1, sum)
# 
# #Non-pelagic Rockfish harvest by target group
# table(int_boat$year, is.na(int_boat$npH))
# table(int_boat$year, is.na(int_boat$yH))
# int_boat$npyH <- ifelse(is.na(int_boat$npH), NA, ifelse(is.na(int_boat$yH), int_boat$npH, int_boat$npH + int_boat$yH))
# table(int_boat$year, is.na(int_boat$npyH))
# npyH_target <- 
#   aggregate(npyH ~ port + target, int_boat, sum) %>%
#   tidyr::pivot_wider(id_cols = port, names_from = target, values_from = npyH)
# npyH_target[, -1]/apply(npyH_target[, -1], 1, sum)



# * Interview stats ------------------------------------------------------
# Boat scale distribution of Lingcod harvest, total effort and CPUE by target species and fleet.
# Note there are negligable trips harvesting lingcode but those trip do well
plot_int(int_boat, "lH")
#On the area scale Harvest is dominated by trips for other species.
int_boat %>%
  dplyr::group_by(year, fleet, target, area) %>%
  dplyr::summarize(lH = sum(lH), E = sum(E)) %>%
  plot_int("lH")


# * Alternative filters -----------------------------------------------------
#  * * Lingcod>target -------------------------------------------------------
#More lingcod than halibut on a halibut trip
#Applies to few trips...
sum(is.na(int_boat$hH))
sum(is.na(int_boat$lH))
sapply(unique(int_boat$port), function(x){
  sum(int_boat$hH < int_boat$lH & int_boat$target == "Halibut" & int_boat$port == x, na.rm = TRUE) / 
    sum(int_boat$target == "Halibut" & int_boat$port == x, na.rm = TRUE)
  })
#int_boat[int_boat$hH < int_boat$lH & int_boat$target == "Halibut", ] %>% print(n = 500)
#But those trips make up a fair bit of the harvest
sapply(unique(int_boat$port), function(x){
  sum(int_boat$lH[int_boat$hH < int_boat$lH & int_boat$target == "Halibut" & int_boat$port == x], na.rm = TRUE) / 
    sum(int_boat$lH[int_boat$target == "Halibut" & int_boat$port == x], na.rm = TRUE)
  })
int_lgth <- int_boat[!(int_boat$hH >= int_boat$lH & int_boat$target == "Halibut"), ]
plot_int(int_lgth, "lH")

#More lingcod than bottomfish on a bottomfish trip
#Applies to few trips...
int_boat$bH <- rowSums(int_boat[, c("pH", "npH", "yH", "hH")], na.rm = TRUE)
sum(is.na(int_boat$bH))
sapply(unique(int_boat$port), function(x){
  sum(int_boat$bH < int_boat$lH & int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon") & int_boat$port == x, na.rm = TRUE) / 
    sum(int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon") & int_boat$port == x, na.rm = TRUE)
  })
#int_boat[int_boat$bH <= int_boat$lH & int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon"), ]  %>% print(n = 500)
#But those trips make up a fair bit of the harvest
sapply(unique(int_boat$port), function(x){
  sum(int_boat$lH[int_boat$bH <= int_boat$lH & int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon") & int_boat$port == x], na.rm = TRUE) /
    sum(int_boat$lH[int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon") & int_boat$port == x], na.rm = TRUE)
  })
int_lgtb <- int_boat[!(int_boat$bH >= int_boat$lH & int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon")), ]
plot_int(int_lgtb, "lH")
#Use both criteria
int_lgtt <- int_boat[!(int_boat$bH >= int_boat$lH & int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon")) &
                        !(int_boat$hH >= int_boat$lH & int_boat$target == "Halibut"), ]
plot_int(int_lgtt , "lH")


# * * CPUE > 1 --------------------------------------------------------------
#Martin suggests a hard number of lingcod per boat but I think we should at least scale it by angler days.
#lH/E > 1
sum(is.na(int_boat$lH))
sum(is.na(int_boat$E))
#Applies to few trips...
sapply(unique(int_boat$port), function(x){
  sum(!is.na(int_boat$E) & int_boat$lH / int_boat$E > 1 & int_boat$port == x, na.rm = TRUE) / 
    sum(!is.na(int_boat$E) & int_boat$port == x, na.rm = TRUE)
})
#int_boat[!is.na(int_boat$E) & int_boat$lH / int_boat$E > 1, ] %>% print(n = 500)
#But those trips make up a fair bit of the harvest
sapply(unique(int_boat$port), function(x){
  sum(int_boat$lH[!is.na(int_boat$E) & int_boat$lH / int_boat$E > 1 & int_boat$port == x], na.rm = TRUE) / 
    sum(int_boat$lH[!is.na(int_boat$E) & int_boat$port == x], na.rm = TRUE)
})
int_CPUE1 <- int_boat[!is.na(int_boat$E) & int_boat$lH / int_boat$E > 1, ]
plot_int(int_CPUE1, "lH")
int_CPUE4 <- int_boat[!is.na(int_boat$E) & int_boat$lH / int_boat$E > 0.4, ]
plot_int(int_CPUE4, "lH")
#Note CPUE > .4 rough equivilent to Martin's > 2 lingcod.
quantile(int_boat$E, na.rm = TRUE)


# * * >2 harvested ----------------------------------------------------------
#Applies to moderate number of trips...
sapply(unique(int_boat$port), function(x){
  sum((int_boat$target == "Lingcod" | int_boat$lH >= 2) & int_boat$port == x, na.rm = TRUE) / 
    sum(int_boat$port == x, na.rm = TRUE)
})
#int_boat[int_boat$target == "Lingcod" | int_boat$lH >= 2, ] %>% print(n = 500)
#and those trips make up almost all of the harvest
sapply(unique(int_boat$port), function(x){
  sum(int_boat$lH[(int_boat$target == "Lingcod" | int_boat$lH >= 2) & int_boat$port == x], na.rm = TRUE) / 
    sum(int_boat$lH[int_boat$port == x], na.rm = TRUE)
})
int_2l <- int_boat[int_boat$target == "Lingcod" | int_boat$lH >= 2, ]
plot_int(int_2l, "lH")


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

H_comp <- 
  dplyr::select(int_boat, year, fleet, port, target, hH, pH, npyH, lH) %>%
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
ggplot(H_comp[H_comp$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon"), ], aes(x = year, weight = value, fill = name)) +
  geom_area(stat = "count", position = "fill", color = "white", alpha = 0.25) +
  facet_grid(port ~ fleet) +
  scale_y_continuous(name = "Percent") +
  scale_x_continuous(name = "Year", breaks = seq(1993, 20017, 3)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")


int_filter <- int_boat[int_boat$target %in% c("Lingcod", "Rockfish", "Bottomfish", "Bottomfish & Salmon") &
                         int_boat$year >= 2000, ]
int_Halibut <- int_boat[int_boat$target == "Halibut" &
                          int_boat$year >= 2000, ]
dat_plot <- list(int_filter, int_Halibut, int_lgtt[int_lgtt$year >= 2000, ], int_CPUE1, int_CPUE4, int_2l[int_2l$year >= 2000, ])
plot_Homer <- lapply(dat_plot, plot_post, post = NA, stat = "E", plotport = "Homer", inc_pred = "none")
gridExtra::marrangeGrob(plot_Homer, nrow = 3, ncol = 2)
plot_Kodiak <- lapply(dat_plot, plot_post, post = NA, stat = "E", plotport = "Kodiak", inc_pred = "none")
gridExtra::marrangeGrob(plot_Kodiak, nrow = 3, ncol = 2)
plot_Seward <- lapply(dat_plot, plot_post, post = NA, stat = "E", plotport = "Seward", inc_pred = "none")
gridExtra::marrangeGrob(plot_Seward, nrow = 3, ncol = 2)
plot_Valdez <- lapply(dat_plot, plot_post, post = NA, stat = "E", plotport = "Valdez", inc_pred = "none")
gridExtra::marrangeGrob(plot_Valdez, nrow = 3, ncol = 2)
plot_Whittier <- lapply(dat_plot, plot_post, post = NA, stat = "E", plotport = "Whittier", inc_pred = "none")
gridExtra::marrangeGrob(plot_Whittier, nrow = 3, ncol = 2)
