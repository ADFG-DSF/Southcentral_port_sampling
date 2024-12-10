library(tidyverse)
library(FSA) #contains the length-age models used in the code.
library(FSAmisc)
library(car)
library(boot)
library(nlme)
library(ggforce)

# Black Rockfish ---------------------------------------------------
# * data prep ---------------------------------------------------
# data import
rflgage <- 
  readRDS(".\\Rockfish report_96-19\\composition\\rfcomp.rds") %>% 
  filter(!is.na(age) & !is.na(length) & sex != "") %>%
  arrange(spp, year)

#Plot length of all spp.
lgplot_names <- 
  table(rflgage$species) %>% 
  as.data.frame() %>% 
  filter(Freq > 100, !(Var1 %in% c("DuskyDrk"))) %>%
  mutate(spp2 = factor(Var1, labels = c("Black", "China", "Copper", "Dark", "Dusky", "Quillback", "Shortraker", "Sivergrey", "Tiger", "Yelloweye", "Yellowtail"))) %>%
  rename(species = Var1)

rflgage %>%
  filter(species %in% lgplot_names$species) %>%
  left_join(lgplot_names, by = "species") %>%
  ggplot(aes(x = spp2, y = length)) +
    geom_boxplot() +
    geom_text(data = lgplot_names, aes(x = spp2, Inf, label = Freq), vjust = 1)

#Prep data for analysis
rf_black <- rflgage[rflgage$spp == "Black", ]

rf_black$loglength <- log(rf_black$length)
rf_black$sex <- as.factor(rf_black$sex)
rf_black$port <- as.factor(rf_black$port)
rf_black$year2 <- cut(rf_black$year, breaks = c(1995, 2005, 2014, 2019))
rf_black$portsex <- as.factor(paste0(rf_black$port, rf_black$sex))
rf_black$sexyear2 <- as.factor(paste0(rf_black$sex, rf_black$year2))
rf_black$portyear2 <- as.factor(paste0(rf_black$port, rf_black$year2))
rf_black$portsexyear2 <- as.factor(paste0(rf_black$port, rf_black$sex, unique(rf_black$year2)))


# Going to fit Von Bertalanffy curves to length-age data and use AIC model selection to test for 
# test for differences between sexes, posts, time periods, and all combinations of each.
# ala Olge http://derekogle.com/fishR/examples/oldFishRVignettes/VonBertalanffy.pdf
# The nls function is finicky so there is quite a bit of troubleshooting which I have left as a roadmap.

#Set up
# Starting values
(svb <- vbStarts(length ~ age, data = rf_black))

# von Bertalanffy function
vb <- vbFuns("Typical")
# von Bertalanffy function for which allows groups
vb_group <- function(age, group, Linf, K, t0){
  Linf[group] * (1 - exp(-K[group] * (age - t0[group])))}

# * common model ---------------
# plot data
plot(rf_black$age, rf_black$length)
# fit
# Normal errors
fitb_common_normal <- nls(length ~ vb(age, Linf, K, t0), data = rf_black, start = svb)
residPlot(fitb_common_normal)
data.frame(resid = residuals(fitb_common_normal), fitted = fitted.values(fitb_common_normal)) %>%
  ggplot(aes(x = fitted, y =resid)) +
  geom_hex(binwidth = c(1, 1)) +
  scale_fill_viridis_c(name = "Sample size", option = "E", direction = -1, trans = scales::sqrt_trans())
#lognormal errors
fitb_common_lognormal <- nls(loglength ~ log(vb(age, Linf, K, t0)), data = rf_black, start = svb)
residPlot(fitb_common_lognormal)
data.frame(resid = residuals(fitb_common_lognormal), fitted = fitted.values(fitb_common_lognormal)) %>%
  ggplot(aes(x = fitted, y =resid)) +
  geom_hex(binwidth = c(.05, .05)) +
  scale_fill_viridis_c(name = "Sample size", option = "E", direction = -1, trans = scales::sqrt_trans())
#slight preference for the lognormal error model
fitb_common <- fitb_common_lognormal
summary(fitb_common)

# * sex model ------------
# plot
ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(.~sex)
# fit
fitb_sex <- nls(loglength ~ log(vb_group(age, sex, Linf, K, t0)), 
                 data = rf_black, 
                 start = lapply(svb, rep, 2))
summary(fitb_sex)


# * port model ---------
# plot
ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(.~port)
# fit
# Note this method fails.
fitb_port <- nls(loglength ~ log(vb_group(age, port, Linf, K, t0)), 
                  data = rf_black, 
                  start = lapply(svb, rep, 5),
                  algorithm = "plinear")
# problem w valdez
nls(loglength ~ log(vb(age, Linf, K, t0)), 
    data = rf_black[rf_black$port == "Cook Inlet",], 
    start = svb)
nls(loglength ~ log(vb(age, Linf, K, t0)), 
    data = rf_black[rf_black$port == "Kodiak",], 
    start = svb)
nls(loglength ~ log(vb(age, Linf, K, t0)), 
    data = rf_black[rf_black$port == "Seward",], 
    start = svb)
nls(loglength ~ log(vb(age, Linf, K, t0)), 
    data = rf_black[rf_black$port == "Valdez",], 
    start = svb)
nls(loglength ~ log(vb(age, Linf, K, t0)), 
    data = rf_black[rf_black$port == "Whittier",], 
    start = svb)
# try better starting values
# individual starting values for each port instead of just replication of the common starting values
temp_port <- sapply(sort(unique(rf_black$port)), 
                    function(x) vbStarts(length ~ age, data = rf_black[rf_black$port == x, ]))
svb_port <- list(Linf = unlist(temp_port[1,]), 
                  K = unlist(temp_port[2,]),  
                  t0 = unlist(temp_port[3,]))
# works for Valdez alone
nls(loglength ~ log(vb(age, Linf, K, t0)), 
    data = rf_black[rf_black$port == "Valdez",], 
    start = lapply(svb_port, function(x) x[4]))
# Still does not work when written as the full model
# Not sure why!
fitb_port <- nls(loglength ~ log(vb_group(age, port, Linf, K, t0)), 
                  data = rf_black, 
                  start = svb_port)

# nls List allows us to run each model individually
# Seems to need data sorted by grouped variable
rf_black_port <- 
  rf_black %>%
  arrange(port, age) %>%
  select(loglength, age, port)
fitb_port  <- nlsList(loglength ~ log(vb(age, Linf, K, t0)) | port, 
                       rf_black_port, 
                       lapply(svb_port, function(x) x[4]))

summary(fitb_port)
# Problem: nlsList runs each regression individually so while parameter estimates match 
# you cant use AIC to compare models.
# Demonstrate w sex groups since it runs both ways.
rf_black_sex <- 
  rf_black %>%
  arrange(sex, age) %>%
  select(loglength, age, sex)
fitb_sexList <- nlsList(loglength ~ log(vb(age, Linf, K, t0)) | sex, 
                         rf_black_sex, 
                         svb)
# same parameter estimates
fitb_sexList
fitb_sex
# Can sum AIC from each group since the models are perfect subsets
# function to do that
AICList <-function(x){
  res <- lapply(x, function(x) resid(x)) %>% unlist(use.names = FALSE)
  L <- length(x)
  N <- length(res)
  ll <- -N/2 * (log(2 * pi) + 1 - log(N) + log(sum(res^2)))
  2*(L*3 + 1) - 2*ll
}
AICList(fitb_sexList)
#matches the AIC for the first method.
AIC(fitb_sex)


# * sex and port model --------
# plot
ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex~port)
fitb_portsex <- nls(loglength ~ log(vb_group(age, portsex, Linf, K, t0)), 
                     data = rf_black,
                     start = lapply(svb, rep, 10))
summary(fitb_portsex)


# * year model ---------
# plot
ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(. ~ year2)
# fit - fails to run
fitb_year <- nls(loglength ~ log(vb_group(age, year2, Linf, K, t0)), 
                  data = rf_black, 
                  start = lapply(svb, rep, 3))
# Run individually
# better starting values
temp_year <- sapply(sort(unique(rf_black$year2)), 
                    function(x) vbStarts(length ~ age, data = rf_black[rf_black$year2 == x, ]))
svb_year <- list(Linf = unlist(temp_year[1,]), 
                  K = unlist(temp_year[2,]),  
                  t0 = unlist(temp_year[3,]))
# sort dataset
rf_black_year2 <-
  rf_black %>%
  arrange(year2, age) %>%
  select(loglength, age, year2)
# fit (not sure why the starting values for the second year group work. Trial and error)
fitb_year  <- nlsList(loglength ~ log(vb(age, Linf, K, t0)) | year2, 
                       rf_black_year2, 
                       lapply(svb_year, function(x) x[2]))
fitb_year

# * sex and year model ----------
# plot
ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex ~ year2)
# fit
fitb_sexyear <- 
  nls(loglength ~ log(vb_group(age, sexyear2, Linf, K, t0)), 
      data = rf_black, 
      start = lapply(svb, rep, 6))
summary(fitb_sexyear)

# * port and year model ------
# plot
ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(port ~ year2)
# fit - fails to run
fitb_portyear <- nls(loglength ~ log(vb_group(age, portyear2, Linf, K, t0)), 
                      data = rf_black, 
                      start = lapply(svb, rep, 15),
                      control = list(minFactor = 1/10000))
# Run individually
# better starting values
temp_portyear <- sapply(sort(unique(rf_black$portyear2)), 
                        function(x) vbStarts(length ~ age, data = rf_black[rf_black$portyear2 == x, ]))
svb_portyear <- list(Linf = unlist(temp_portyear[1,]), 
                      K = unlist(temp_portyear[2,]),  
                      t0 = unlist(temp_portyear[3,]))
# sort dataset
rf_black_portyear2 <-
  rf_black %>%
  arrange(portyear2, age) %>%
  select(loglength, age, portyear2)
# fit (not sure why the starting values for the third group work. Trial and error)
#Run individually
fitb_portyear  <- 
  nlsList(loglength ~ log(vb(age, Linf, K, t0)) | portyear2, 
          rf_black_portyear2, 
          lapply(svb_portyear, function(x) x[3]))
summary(fitb_portyear)


# * port, sex and year model ----------
# plot
plot_portsexyear2 <-
  ggplot(rf_black, aes(x = age, y = length)) +
  geom_point()
plot_portsexyear2 + facet_grid_paginate(sex ~ port + year2, nrow = 2, ncol = 3, page = 1)
plot_portsexyear2 + facet_grid_paginate(sex ~ port + year2, nrow = 2, ncol = 3, page = 2)
plot_portsexyear2 + facet_grid_paginate(sex ~ port + year2, nrow = 2, ncol = 3, page = 3)
plot_portsexyear2 + facet_grid_paginate(sex ~ port + year2, nrow = 2, ncol = 3, page = 4)
plot_portsexyear2 + facet_grid_paginate(sex ~ port + year2, nrow = 2, ncol = 3, page = 5)
# fit
fitb_portsexyear <- 
  nls(loglength ~ log(vb_group(age, portsexyear2, Linf, K, t0)), 
      data = rf_black, 
      start = lapply(svb, rep, 30))
summary(fitb_portsexyear)

# * AIC-port & sex best----------
# port and sex model fits best
AIC(fitb_common, fitb_sex, fitb_portsex, fitb_sexyear, fitb_portsexyear)
AICList(fitb_port)
AICList(fitb_year)
AICList(fitb_portyear)

# * plot best model ---------
tempb <- 
  coef(fitb_portsex) %>%
  as.data.frame() %>%
  mutate(var = c(rep("Linf", 10), rep("K", 10), rep("t0", 10)),
         port = rep(rep(c("Cook Inlet", "Kodiak", "Seward", "Valdez", "Whittier"), each = 2), times = 3),
         sex = rep(rep(c("Female", "Male"), times = 5), times = 3)) %>%
  pivot_wider(names_from = "var", values_from = ".")
plotb <- 
  do.call("rbind", replicate(max(rf_black$age), tempb, simplify = FALSE)) %>%
  mutate(age = rep(1:max(rf_black$age), each = 10),
         length = vb(age, Linf, K, t0))
rf_black %>%
  mutate(sex = factor(sex, c("M", "F"), c("Male", "Female"))) %>%
  ggplot(aes(x = age, y = length, group = age)) +
  geom_hex(aes(x = age, y = length), binwidth = c(2, 2), inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "Sample size", breaks = c( 10, 100, 300), option = "E", direction = -1, trans = scales::sqrt_trans()) +
  guides(fill = guide_colorbar(label.theme = element_text(angle = 45))) +
  #geom_point(shape = '.', col = 'white') +
  geom_line(data = plotb, mapping = aes(x = age, y = length), linewidth = 1, inherit.aes = FALSE) +
  facet_grid(sex ~ port) +
  scale_y_continuous(name = "Length (cm)", limits = c(20, 70)) +
  scale_x_continuous(name = "Age") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")

# * Bootstrap best model ------------
# control parameters
ports <- unique(sort(rf_black$port))
nports <- length(ports)
sexes <- unique(sort(rf_black$sex))
nsexes <- 2
predict2 <- function(x) predict(x, data.frame(age=ages))
# observed age range by port and sex
age_rangeblack <- 
  rf_black %>%
  group_by(port, sex) %>%
  summarise(min_age = min(age), max_age = max(age), max_lg = max(length))

# Bootstrap to get parameter estimates CIs and length-at-age predictions and CIs
# Also to extrapolate model to all age ranges observed
boot_params_black0 <- boot_ci_black0 <- preds_all_black <- preds_obs_black <- NULL
for (i in 1:nports) {
  for (y in 1:nsexes) {
    ## Loop notification (for peace of mind)
    cat("port ", ports[i], " sex ", sexes[y],"Loop\n")
    ## Isolate data for each port and sex
    tmp_data <- filter(rf_black, port==ports[i], sex==sexes[y])
    ## Fit von B to that port and sex
    tmp_fit <- nls(length ~ vb(age, Linf,K,t0), data=tmp_data, start = svb)
    ## Extract and store parameter estimates and CIs
    boot_params_black0 <- rbind(boot_params_black0, coef(tmp_fit)) #coefficents
    boot_ci <- Boot(tmp_fit, R = 250) 
    tmp_ci <-  confint(boot_ci, type = "perc") 
    boot_ci_black0 <- rbind(boot_ci_black0, c(tmp_ci["Linf",], tmp_ci["K",], tmp_ci["t0",])) #CI
    ## Predict mean lengths-at-age with CIs
    ##   preds_all_black -> across all ages
    ##   preds_obs_black -> across observed ages only
    ages <- seq(-1, max(age_rangeblack$max_age), 1)
    boot_pred <- Boot(tmp_fit, f=predict2, R = 250)
    tmp_pred <- data.frame(port=ports[i], sex = sexes[y] ,age=ages,
                           predict(tmp_fit, data.frame(age=ages)),
                           confint(boot_pred, type = "perc"))
    # lower = confint(boot_pred)[,1] - sqrt(mean(resid(fit1)^2)),
    # upper = confint(boot_pred)[,2] + sqrt(mean(resid(fit1)^2)))
    
    preds_all_black <- rbind(preds_all_black, tmp_pred)
    tmp_pred_filter <- 
      filter(tmp_pred,
             age >= age_rangeblack$min_age[age_rangeblack$port == ports[i] & age_rangeblack$sex == sexes[y]],
             age <= age_rangeblack$max_age[age_rangeblack$port == ports[i] & age_rangeblack$sex == sexes[y]])
    preds_obs_black <- rbind(preds_obs_black, tmp_pred_filter)
  }
}
rownames(boot_params_black0) <- 
  rownames(boot_ci_black0) <- 
  paste0(rep(ports, each = 2), "_",rep(sexes, times = 5))
colnames(boot_ci_black0) <- 
  paste(rep(c("Linf","K","t0"),each=2),
  rep(c("LCI","UCI"),times=2),sep=".")
colnames(preds_all_black) <- 
  colnames(preds_obs_black) <- 
  c("port","sex", "age","fit","LCI","UCI") 

# plot of model fits (extrapolation shown by dotted line)
ggplot(data=rf_black, aes(x = age, y = length, group = age)) +
  geom_hex(aes(x = age, y = length), binwidth = c(2, 2), inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "Sample size", option = "E", direction = -1, trans = scales::sqrt_trans()) +
  geom_ribbon(data=preds_obs_black, aes(x=age, ymin=LCI, ymax=UCI), alpha=0.3, inherit.aes = FALSE) +
  geom_line(data=preds_all_black, aes(y=fit, x=age), linewidth = 1, linetype = 2, inherit.aes = FALSE) +
  geom_line(data=preds_obs_black, aes(y=fit, x=age), linewidth = 1, inherit.aes = FALSE) +
  scale_y_continuous(name="Total Length (mm)", limits = c(0, max(age_rangeblack$max_lg))) +
  scale_x_continuous(name="Age (years)", limits = c(0, max(age_rangeblack$max_age))) +
  facet_grid(sex ~ port) +
  theme_bw(base_size = 16)

# plot parameter CI's
boot_params_black <- data.frame(port = rep(ports, each = 2), sex = rep(sexes, times = 5), boot_params_black0)
boot_params_black_long <-
  boot_params_black %>%
  pivot_longer(cols = -c(port, sex), names_to = "var", values_to = "val") %>%
  filter(var %in% c("Linf", "K", "t0"))
boot_ci_black <- data.frame(port = rep(ports, each = 2), sex = rep(sexes, times = 5), boot_ci_black0)
boot_ci_black_long <-
  boot_ci_black %>%
  pivot_longer(cols = -c(port, sex), names_to = "temp", values_to = "val") %>%
  mutate(var = gsub("(.*)\\..*", ("\\1"), temp),
         bound = gsub(".*\\.(.*)", ("\\1"), temp)) %>%
  select(-temp) %>%
  pivot_wider(id_cols = c("port", "sex", "var"), names_from = "bound", values_from = "val") %>%
  filter(var %in% c("Linf", "K", "t0")) %>%
  as.data.frame()
# plot of Linf estimates and CI's by port and sex.
ggplot() +
  geom_point(data = boot_params_black_long[boot_params_black_long$var == "Linf", ], 
             aes(x = port, y = val, color = sex), 
             position = position_dodge(width = 0.5), 
             size = 3) +
  geom_errorbar(data = boot_ci_black_long[boot_ci_black_long$var == "Linf", ], 
                aes(x = port, color = sex, ymin = LCI, ymax = UCI), 
                size = 1, 
                width=0.3, 
                position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Black", "grey40")) +
  labs(color = "Sex", x = "Port", y = expression(L[infinity])) +
  theme_bw(base_size = 16)

#  * parameter table ------------------------------------------------------
#  ** best model ----------------------------------------------------------
param_best_black <-
  boot_params_black_long %>%
  left_join(boot_ci_black_long, by = c("port", "sex", "var")) %>%
  pivot_longer(c(val, LCI, UCI), names_to = "stat", values_to = "value") %>%
  pivot_wider(id_cols = c("port", "sex"), names_from = c("var", "stat"), values_from = "value")

flextable(param_best_black) %>%
  set_header_labels(values = list(port = "Port", 
                                  sex = "Sex", 
                                  Linf_val = "Mean", 
                                  Linf_LCI = "Lower CI Bound (95%)", 
                                  Linf_UCI = "Upper CI Bound (95%)", 
                                  K_val = "Mean", 
                                  K_LCI = "Lower CI Bound (95%)", 
                                  K_UCI = "Upper CI Bound (95%)", 
                                  t0_val = "Mean", 
                                  t0_LCI = "Lower CI Bound (95%)", 
                                  t0_UCI = "Upper CI Bound (95%)")) %>%
  add_header_row(values = c("", "Linf", "K", "t0"), colwidths = c(2, 3, 3, 3)) %>%
  colformat_double(j = c(3:5, 9:11), digits = 1) %>%
  colformat_double(j = 6:8, digits = 3) %>%
  align(align = "center", part = "header") %>%
  font(fontname = "Times New Roman") %>%
  fontsize(size = 10) %>%
  autofit() |>
  vline(j = c(2, 5, 8))

#  ** common model --------------------------------------------------------
# Adam St. Savior felt this was of interest.
# bootstrap for CIs
common_CI <- confint(Boot(fitb_common, R = 250), type = "perc")
params_common_black <- 
  data.frame(param = c("Linf", "K", "t0"),
             estimate = coef(fitb_common),
             LCI = common_CI[, 1],
             UCI = common_CI[, 2])


# Yelloweye Rockfish ---------------------------------------------------
#  * data prep ------------------------------------------------------------
rf_yelloweye <- rflgage[rflgage$spp == "Yelloweye", ]

table(rflgage$port[rflgage$spp == "Yelloweye"], rflgage$sex[rflgage$spp == "Yelloweye"])

table(rf_yelloweye$age)

rf_yelloweye$loglength <- log(rf_yelloweye$length)
rf_yelloweye$sex <- as.factor(rf_yelloweye$sex)
rf_yelloweye$port <- as.factor(rf_yelloweye$port)
rf_yelloweye$year2 <- cut(rf_yelloweye$year, breaks = c(1995, 2005, 2014, 2019))
rf_yelloweye$portsex <- as.factor(paste0(rf_yelloweye$port, rf_yelloweye$sex))
rf_yelloweye$sexyear2 <- as.factor(paste0(rf_yelloweye$sex, rf_yelloweye$year2))
rf_yelloweye$portyear2 <- as.factor(paste0(rf_yelloweye$port, rf_yelloweye$year2))
rf_yelloweye$portsexyear2 <- as.factor(paste0(rf_yelloweye$port, rf_yelloweye$sex, unique(rf_yelloweye$year2)))

# starting values
(svy <- vbStarts(length ~ age, data = rf_yelloweye))

#  * common model ----------------------------------------------------
plot(rf_yelloweye$age, rf_yelloweye$length)

# Normal errors
fity_common_normal <- nls(length ~ vb(age, Linf, K, t0), 
                          data = rf_yelloweye, 
                          start = svy)
residPlot(fity_common_normal)
data.frame(resid = residuals(fity_common_normal), fitted = fitted.values(fity_common_normal)) %>%
  ggplot(aes(x = fitted, y =resid)) +
  geom_hex(binwidth = c(1, 1)) +
  scale_fill_viridis_c(name = "Sample size", option = "E", direction = -1, trans = scales::sqrt_trans())
#Assumptions look good

fity_common <- fity_common_normal
summary(fity_common)

# * sex model ---------------------
ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(.~sex)
fity_sex <- nls(length ~ vb_group(age, sex, Linf, K, t0), 
                data = rf_yelloweye, 
                start = lapply(svy, rep, 2))
summary(fity_sex)

# * port model -------------------------
ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(.~port)
fity_port <- nls(length ~ vb_group(age, port, Linf, K, t0), 
                  data = rf_yelloweye, 
                  start = lapply(svy, rep, 5))
summary(fity_port)

# * sex and port model ------------------
ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex~port)
fity_portsex <- nls(length ~ vb_group(age, portsex, Linf, K, t0), 
                    data = rf_yelloweye,
                    start = lapply(svy, rep, 10))
summary(fity_portsex)

# * year model -------------
ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(. ~ year2)
fity_year <- nls(length ~ vb_group(age, year2, Linf, K, t0), 
                  data = rf_yelloweye, 
                  start = lapply(svy, rep, 3))
summary(fity_year)

# * sex and year model ---------------
ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex ~ year2)
fity_sexyear <- nls(length ~ vb_group(age, sexyear2, Linf, K, t0), 
                    data = rf_yelloweye, 
                    start = lapply(svy, rep, 6))
summary(fity_sexyear)

# * port and year model --------------
ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(port ~ year2)
fity_portyear <- nls(length ~ vb_group(age, portyear2, Linf, K, t0), 
                    data = rf_yelloweye, 
                    start = lapply(svy, rep, 15),
                    control = list(minFactor = 1/10000))
summary(fity_portyear)


#  * port, sex, year model ------------------------------------------------
plot_portsexyear2 <-
  ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point()
plot_portsexyear2 + facet_grid_paginate(sex ~ port + year2, nrow = 2, ncol = 3, page = 1)
plot_portsexyear2 + facet_grid_paginate(sex ~ port + year2, nrow = 2, ncol = 3, page = 2)
plot_portsexyear2 + facet_grid_paginate(sex ~ port + year2, nrow = 2, ncol = 3, page = 3)
plot_portsexyear2 + facet_grid_paginate(sex ~ port + year2, nrow = 2, ncol = 3, page = 4)
plot_portsexyear2 + facet_grid_paginate(sex ~ port + year2, nrow = 2, ncol = 3, page = 5)
# fit
fity_portsexyear <- 
  nls(length ~ vb_group(age, portsexyear2, Linf, K, t0), 
      data = rf_yelloweye, 
      start = lapply(svy, rep, 30))
summary(fity_portsexyear)



#  * AIC-port & sex best------------------------------------------------------------------
# port and sex model is best
AIC(fity_common, 
    fity_sex, 
    fity_port, 
    fity_year, 
    fity_portsex, 
    fity_sexyear, 
    fity_portyear, 
    fity_portsexyear)

# * plot best model --------------
tempy <- 
  data.frame(fit = coefficients(fity_portsex),
             param = rep(c("Linf", "K", "t0"), each = 10), 
             port = rep(rep(c("Cook Inlet", "Kodiak", "Seward", "Valdez", "Whittier"), each = 2), times = 3),
             sex = rep(c("Female", "Male"), times = 15)) %>%
  pivot_wider(names_from = param, values_from = fit)
ploty <- 
  do.call("rbind", replicate(max(rf_yelloweye$age), tempy, simplify = FALSE)) %>%
  mutate(age = rep(1:max(rf_yelloweye$age), each = 10),
         length = vb(age, Linf, K, t0))
rf_yelloweye %>%
  mutate(sex = factor(sex, c("M", "F"), c("Male", "Female"))) %>%
  ggplot(aes(x = age, y = length, group = age)) +
  geom_hex(aes(x = age, y = length), binwidth = c(2, 2), inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "Sample size", breaks = c(3, 15, 45), option = "E", direction = -1, trans = scales::sqrt_trans()) +
  guides(fill = guide_colorbar(label.theme = element_text(angle = 45))) +
  geom_line(data = ploty, mapping = aes(x = age, y = length), size = 1, inherit.aes = FALSE) +
  facet_grid(sex ~ port) +
  scale_y_continuous(name = "Length (cm)", limits = c(20, 90)) +
  scale_x_continuous(name = "Age") +
  theme_bw(base_size = 16)


#  * bootstrap best model -------------------------------------------------
# observed age range by port and sex
age_rangeyellow <- 
  rf_yelloweye %>%
  group_by(port, sex) %>%
  summarise(min = min(age), max = max(age))

# Bootstrap to get parameter estimates CIs and length-at-age predictions and CIs
# Also to extrapolate model to all age ranges observed
boot_params_yellow0 <- boot_ci_yellow0 <- preds_all_yellow <- preds_obs_yellow <- NULL
for (i in 1:nports) {
  for (y in 1:nsexes) {
    ## Loop notification (for peace of mind)
    cat("port ", ports[i], " sex ", sexes[y],"Loop\n")
    ## Isolate year's data
    tmp_data <- filter(rf_yelloweye, port==ports[i], sex==sexes[y])
    ## Fit von B to that year
    tmp_fit <- nls(length ~ vb(age, Linf,K,t0), data=tmp_data, start = svy)
    ## Extract and store parameter estimates and CIs
    boot_params_yellow0 <- rbind(boot_params_yellow0, coef(tmp_fit))
    boot_ci <- Boot(tmp_fit, R = 250)
    tmp_ci <-  confint(boot_ci, type = "perc")
    boot_ci_yellow0 <- rbind(boot_ci_yellow0, c(tmp_ci["Linf",], tmp_ci["K",], tmp_ci["t0",]))
    ## Predict mean lengths-at-age with CIs
    ##   preds_all_yellow -> across all ages
    ##   preds_obs_yellow -> across observed ages only
    ages <- seq(-1,max(age_rangeyellow$max),1)
    boot_pred <- Boot(tmp_fit, f=predict2, R = 250)
    tmp_preds <- data.frame(port=ports[i], sex = sexes[y] ,age=ages,
                       predict(tmp_fit, data.frame(age=ages)),
                       confint(boot_pred, type = "perc"))
                       # lower = confint(boot_pred)[,1] - sqrt(mean(resid(tmp_fit)^2)),
                       # upper = confint(boot_pred)[,2] + sqrt(mean(resid(tmp_fit)^2)))
    
    preds_all_yellow <- rbind(preds_all_yellow, tmp_preds)
    tmp_pred_filter <- 
      filter(tmp_preds,
             age >= age_rangeyellow$min[age_rangeyellow$port == ports[i] & age_rangeyellow$sex == sexes[y]],
             age <= age_rangeyellow$max[age_rangeyellow$port == ports[i] & age_rangeyellow$sex == sexes[y]])
    preds_obs_yellow <- rbind(preds_obs_yellow, tmp_pred_filter)
  }
}

rownames(boot_params_yellow0) <- 
  rownames(boot_ci_yellow0) <- 
  paste0(rep(ports, each = 2), "_",rep(sexes, times = 5))
colnames(boot_ci_yellow0) <- 
  paste(rep(c("Linf","K","t0"),each=2),
  rep(c("LCI","UCI"),times=2),sep=".")
colnames(preds_all_yellow) <- 
  colnames(preds_obs_yellow) <- 
  c("port","sex", "age","fit","LCI","UCI") 

ggplot(data = rf_yelloweye, aes(x = age, y = length, group = age)) +
  geom_hex(aes(x = age, y = length), binwidth = c(2, 2), inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "Sample size", option = "E", direction = -1, trans = scales::sqrt_trans()) +
  geom_ribbon(data=preds_obs_yellow, aes(x=age, ymin = LCI, ymax = UCI),alpha=0.3, inherit.aes = FALSE) +
  geom_line(data=preds_all_yellow, aes(y=fit,x=age), size=1, linetype=2, inherit.aes = FALSE) +
  geom_line(data=preds_obs_yellow, aes(y=fit, x=age), size=1, inherit.aes = FALSE) +
  scale_y_continuous(name = "Total Length (mm)") +
  scale_x_discrete(name = "Age (years)") +
  facet_grid(sex ~ port) +
  theme_bw(base_size = 16)

boot_params_yellow <- data.frame(port = rep(ports, each = 2), sex = rep(sexes, times = 5), boot_params_yellow0)
boot_params_yellow_long <-
  boot_params_yellow %>%
  pivot_longer(cols = -c(port, sex), names_to = "var", values_to = "val") %>%
  filter(var %in% c("Linf", "K", "t0"))
boot_ci_yellow <- data.frame(port = rep(ports, each = 2), sex = rep(sexes, times = 5), boot_ci_yellow0)
boot_ci_yellow_long <-
  boot_ci_yellow %>%
  pivot_longer(cols = -c(port, sex), names_to = "temp", values_to = "val") %>%
  mutate(var = gsub("(.*)\\..*", ("\\1"), temp),
         bound = gsub(".*\\.(.*)", ("\\1"), temp)) %>%
  select(-temp) %>%
  pivot_wider(id_cols = c("port", "sex", "var"), names_from = "bound", values_from = "val") %>%
  filter(var %in% c("Linf", "K", "t0")) %>%
  as.data.frame()
boot_params_yellow_long %>% 
  filter(var == "Linf") %>%
  ggplot() +
  geom_point(aes(x=port,y=val, color = sex), position=position_dodge(width=0.5), size = 3) +
  geom_errorbar(data = boot_ci_yellow_long[boot_ci_yellow_long$var == "Linf",], 
                aes(x=port,color = sex, ymin = LCI, ymax = UCI), 
                size = 1, 
                width=0.3, 
                position=position_dodge(width=0.5)) +
  scale_color_manual(values = c("Black", "grey40")) +
  labs(color = "Sex", x = "Port", y = expression(L[infinity])) +
  theme_bw(base_size = 16)
boot_params_yellow_long %>% 
  filter(var == "K") %>%
  ggplot() +
  geom_point(aes(x=port, y=val, color = sex), position = position_dodge(width=0.5), size = 3) +
  geom_errorbar(data = boot_ci_yellow_long[boot_ci_yellow_long$var == "K",], 
                aes(x=port,color = sex, 
                    ymin=LCI,ymax=UCI), 
                size = 1, width=0.3, 
                position=position_dodge(width=0.5)) +
  scale_color_manual(values = c("Black", "grey40")) +
  labs(color = "Sex", x = "Port", y = expression(K)) +
  theme_bw(base_size = 16)


#  * parameter table ------------------------------------------------------
#  ** best model ----------------------------------------------------------
param_best_yellow <-
  boot_params_yellow_long %>%
  left_join(boot_ci_yellow_long, by = c("port", "sex", "var")) %>%
  pivot_longer(c(val, LCI, UCI), names_to = "stat", values_to = "value") %>%
  pivot_wider(id_cols = c("port", "sex"), names_from = c("var", "stat"), values_from = "value")
  
flextable(param_best_yellow) %>%
  set_header_labels(values = list(port = "Port", 
                                  sex = "Sex", 
                                  Linf_val = "Mean", 
                                  Linf_LCI = "Lower CI Bound (95%)", 
                                  Linf_UCI = "Upper CI Bound (95%)", 
                                  K_val = "Mean", 
                                  K_LCI = "Lower CI Bound (95%)", 
                                  K_UCI = "Upper CI Bound (95%)", 
                                  t0_val = "Mean", 
                                  t0_LCI = "Lower CI Bound (95%)", 
                                  t0_UCI = "Upper CI Bound (95%)")) %>%
  add_header_row(values = c("", "Linf", "K", "t0"), colwidths = c(2, 3, 3, 3)) %>%
  colformat_double(j = c(3:5, 9:11), digits = 1) %>%
  colformat_double(j = 6:8, digits = 3) %>%
  align(align = "center", part = "header") %>%
  font(fontname = "Times New Roman") %>%
  fontsize(size = 10) %>%
  autofit() |>
  vline(j = c(2, 5, 8))

#  ** common model --------------------------------------------------------
# Adam St. Savior felt this was of interest.
# bootstrap for CIs
common_CI <- confint(Boot(fity_common, R = 250), type = "perc")
params_common_yellow <- 
  data.frame(param = c("Linf", "K", "t0"),
             estimate = coef(fity_common),
             LCI = common_CI[, 1],
             UCI = common_CI[, 2])


#  Outputs ----------------------------------------------------------------


#output a list for markdown
list_lgage <- list(plotb, 
                   boot_params_black_long, 
                   boot_ci_black_long, 
                   ploty, 
                   boot_params_yellow_long, 
                   boot_ci_yellow_long)
saveRDS(list_lgage, ".\\Rockfish report_96-19\\lengthage\\list_lgagedata.rds")

saveRDS(list(best_black = param_best_black, 
             common_black = params_common_black,
             best_yellow = param_best_yellow, 
             common_yellow = params_common_yellow),
             ".\\Rockfish report_96-19\\lengthage\\list_lgageparams.rds")

