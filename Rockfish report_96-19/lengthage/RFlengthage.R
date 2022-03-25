library(tidyverse)
library(FSA)
library(car)
library(boot)

# Black Rockfish ---------------------------------------------------
# * data prep ---------------------------------------------------
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


#  * Francis parameterization ----------------------------------------------------
#  Francis worked for Black but switched to typical since I could not get nls to find global minimums w Francis for yelloweye
#  THis code is dated
table(rf_black$age)
ages_black <- c(7, 28)
mean(rf_black$length[rf_black$age == ages_black[1]])
mean(rf_black$length[rf_black$age == ages_black[2]])

plot(rf_black$age, rf_black$length)
#Fit common model
(svbt_common <- vbStarts(length ~ age, data = rf_black, type="Francis", tFrancis = ages))
vbFrancis <- vbFuns("Francis")
fitbt_common <- nls(length ~ vbFrancis(age, L1, L2, L3, t1 = ages_black), data = rf_black, start = svbf_common)
summary(fitbt_common)

#Francis model that accepts categorical variables
vbFrancis_group <- function(age, group, L1, L2, L3, t){
  L1[group] + (L3[group] - L1[group]) * 
    ((1 - (L3[group] - L2[group])/(L2[group] - L1[group])^(2 * ((age - t[1])/(t[2] - t[1]))))/(1 - (L3[group] - L2[group])/(L2[group] - L1[group])^2))
}

ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(.~sex)
# fit sex model
(svbf_sex <- lapply(svbf_common, rep, 2))
fitbf_sex <- nls(length ~ vbFrancis_group(age, sex2, L1, L2, L3, ages_black), data = rf_black, start = svbf_sex)
summary(fitbf_sex)

ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(.~port)
#fit port model
(svbf_port <- lapply(svbf_common, rep, 5))
fitbf_port <- nls(length ~ vbFrancis_group(age, port2, L1, L2, L3, ages_black), data = rf_black, start = svbf_port)
summary(fitbf_port)

ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex~port)
#Fit sex and port model
(svbf_portsex <- lapply(svbf_common, rep, 10))
fitbf_portsex <- nls(length ~ vbFrancis_group(age, portsex2, L1, L2, L3, ages_black), data = rf_black, start = svbf_portsex)
summary(fitbf_portsex)

ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(. ~ year2)
#fit year model
(svbf_year <- lapply(svbf_common, rep, 3))
fitbf_year <- nls(length ~ vbFrancis_group(age, year2, L1, L2, L3, ages_black), data = rf_black, start = svbf_year)
summary(fitbf_year)

ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex ~ year2)
#fit sex and year model
(svbf_sexyear <- lapply(svbf_common, rep, dim(grid_sexyear)[1]))
fitbf_sexyear <- nls(length ~ vbFrancis_group(age, sexyear2, L1, L2, L3, ages_black), data = rf_black, start = svbf_sexyear)
summary(fitbf_sexyear)

ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(port ~ year2)
#fit port and year model
(svbf_portyear <- lapply(svbf_common, rep, dim(grid_portyear)[1]))
fitbf_portyear <- nls(length ~ vbFrancis_group(age, portyear2, L1, L2, L3, ages_black), data = rf_black, start = svbf_portyear)
summary(fitbf_portyear)

# fit port sex and year model
(svbf_portsexyear <- lapply(svbf_common, rep, dim(grid_portsexyear)[1]))
fitbf_portsexyear <- nls(length ~ vbFrancis_group(age, portsexyear2, L1, L2, L3, ages_black), data = rf_black, start = svbf_portsexyear)
summary(fitbf_portsexyear)
#Heteroscedacity
residPlot(fitbf_portsexyear)

# port and sex model is best
AIC(fitbf_common, fitbf_sex, fitbf_port, fitbf_year, fitbf_portsex, fitbf_sexyear, fitbf_portyear, fitbf_portsexyear)







#Heteroscedacity
residPlot(fitb_portsex)

#Not sure why but cant get nls to solve w lognormal errors
#tried typical paramaterization w similar problem
#tried in jags. took too long
fitb_portsexlog <- nls(loglength ~ log(vbFrancis_group(age, portsex2, L1, L2, L3, ages_black)), data = rf_black, start = svb_portsex)

fitb_log <- 
  mapply(function(x, y){
    nls(loglength ~ log(vbFrancis(age, L1, L2, L3, t1 = ages_black)), 
                        data = rf_black[rf_black$port == x & rf_black$sex == y,], 
        start = svb_common)},
         rep(unique(rf_black$port), each = 2),
         rep(c("M", "F"), times = 5),
         SIMPLIFY = FALSE)
lapply(fitb_log, summary)
# better
lapply(fitb_log, residPlot)

tempb_Francis <- 
  fitb_log %>%
  sapply(coef) %>%
  t() %>%
  as.data.frame() %>%
  mutate(port = rep(c("Cook Inlet", "Kodiak", "Seward", "Valdez", "Whittier"), each = 2),
         sex = rep(c("Male", "Female"), times = 5))
plotb_Francis <- 
  do.call("rbind", replicate(max(rf_black$age), tempb_Francis, simplify = FALSE)) %>%
  mutate(age = rep(1:max(rf_black$age), each = 10),
         length = vbFrancis(age, L1, L2, L3, ages_black))
rf_black %>%
  mutate(sex = factor(sex, c("M", "F"), c("Male", "Female"))) %>%
  ggplot(aes(x = age, y = length, group = age)) +
  geom_hex(aes(x = age, y = length), binwidth = c(2, 2), inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "Sample size", option = "E", direction = -1, trans = scales::sqrt_trans()) +
  #geom_point(shape = '.', col = 'white') +
  geom_line(data = plotb_Francis, mapping = aes(x = age, y = length), size = 1, inherit.aes = FALSE) +
  facet_grid(sex ~ port) +
  scale_y_continuous(name = "Length (cm)", limits = c(20, 70)) +
  scale_x_continuous(name = "Age") +
  theme_bw(base_size = 18)




#  * Typical parameterization ----------------------------------------------------
plot(rf_black$age, rf_black$length)
#Starting values
(svbt <- vbStarts(length ~ age, data = rf_black))

#Try modeling fixed parameters ala Olge
#http://derekogle.com/fishR/examples/oldFishRVignettes/VonBertalanffy.pdf
vb_group <- function(age, group, Linf, K, t0){
  Linf[group] * (1 - exp(-K[group] * (age - t0[group])))}

#port sex and year model
fitbt_portsexyear0 <- nls(length ~ vb_group(age, portsexyear2, Linf, K, t0), 
                         data = rf_black, 
                         start = lapply(svbt, rep, length(unique(rf_black$portsexyear2))),
                         control = list(minFactor = 1/10000))
summary(fitbt_portsexyear0)
#Heteroscedacity
residPlot(fitbt_portsexyear0)
data.frame(resid = residuals(fitbt_portsexyear0), fitted = fitted.values(fitbt_portsexyear0)) %>%
  ggplot(aes(x = fitted, y =resid)) +
  geom_hex(binwidth = c(1, 1)) +
  scale_fill_viridis_c(name = "Sample size", option = "E", direction = -1, trans = scales::sqrt_trans())

#port sex and year model w multiplicative errors
fitbt_portsexyear <- nls(loglength ~ log(vb_group(age, portsexyear2, Linf, K, t0)), 
                         data = rf_black, 
                         start = lapply(svbt, rep, length(unique(rf_black$portsexyear2))),
                         control = list(minFactor = 1/10000))
summary(fitbt_portsexyear)
#Better. Use multiplicative errors
residPlot(fitbt_portsexyear)
data.frame(resid = residuals(fitbt_portsexyear), fitted = fitted.values(fitbt_portsexyear)) %>%
  ggplot(aes(x = fitted, y =resid)) +
  geom_hex(binwidth = c(.02, .02)) +
  scale_fill_viridis_c(name = "Sample size", option = "E", direction = -1, trans = scales::sqrt_trans())

#COmmon model
vb <- vbFuns("Typical")
fitbt_common <- nls(loglength ~ log(vb(age, Linf, K, t0)), data = rf_black, start = svbt)
summary(fitbt_common)

#sex model
ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(.~sex)
fitbt_sex <- nls(loglength ~ log(vb_group(age, sex, Linf, K, t0)), data = rf_black, start = lapply(svbt, rep, 2))
summary(fitbt_sex)

#fit port model
ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(.~port)
#Not this method fails here and its hard to trouble shoot or fix.
fitbt_port <- nls(loglength ~ log(vb_group(age, port, Linf, K, t0)), data = rf_black, start = lapply(svbt, rep, 5))
fitbt_port
#try better starting values
temp_port <- sapply(sort(unique(rf_black$port)), function(x) vbStarts(length ~ age, data = rf_black[rf_black$port == x, ]))
svbt_port <- list(Linf = unlist(temp_port[1,]), K = unlist(temp_port[2,]),  t0 = unlist(temp_port[3,]))
#Still does not work
fitbt_port <- nls(loglength ~ log(vb_group(age, port, Linf, K, t0)), data = rf_black, start = svbt_port)
#problem w valdez
nls(loglength ~ log(vb(age, Linf, K, t0)), data = rf_black[rf_black$port == "Cook Inlet",], start = svbt)
nls(loglength ~ log(vb(age, Linf, K, t0)), data = rf_black[rf_black$port == "Kodiak",], start = svbt)
nls(loglength ~ log(vb(age, Linf, K, t0)), data = rf_black[rf_black$port == "Seward",], start = svbt)
nls(loglength ~ log(vb(age, Linf, K, t0)), data = rf_black[rf_black$port == "Valdez",], start = svbt)
nls(loglength ~ log(vb(age, Linf, K, t0)), data = rf_black[rf_black$port == "Whittier",], start = svbt)
#Good starting values Valdez
nls(loglength ~ log(vb(age, Linf, K, t0)), data = rf_black[rf_black$port == "Valdez",], start = list(Linf = 56, K = .25, t0 = 1.4))
svbt_port$Linf[4] <- 56
svbt_port$K[4] <- 0.25
svbt_port$t0[4] <- 1.4
#Still Does not work!!!!
fitbt_port <- nls(loglength ~ log(vb_group(age, port, Linf, K, t0)), data = rf_black, start = svbt_port)
#nls List from nlme at least ids the problem
library(nlme)
fitbt_port  <- nlsList(loglength ~ log(vb(age, Linf, K, t0)) | port, rf_black, svbt)
fitbt_port
#And will run with good starts
fitbt_port  <- nlsList(loglength ~ log(vb(age, Linf, K, t0)) | port, rf_black, list(Linf = 56, K = .25, t0 = 1.4))
summary(fitbt_port)
#Problem: nlsList runs each regression individually so while parameter estimates match you cant use AIC to compare models.
#Demonstrate w sex groups since it runs both ways.
fitbt_sexList <- nlsList(loglength ~ log(vb(age, Linf, K, t0)) | sex, rf_black, svbt)
fitbt_sexList
fitbt_sex
#SO I wrote a custom function to get the AIC from the nlsList output
AICList <-function(x){
  res <- lapply(x, function(x) resid(x)) %>% unlist(use.names = FALSE)
  L <- length(x)
  N <- length(res)
  ll <- -N/2 * (log(2 * pi) + 1 - log(N) + log(sum(res^2)))
  2*(L*3 + 1) - 2*ll
}
AICList(fitbt_sexList)
#matches the AIC for the first method.
AIC(fitbt_sex)
#This is messy but it works. Next time try ADmodelbuilder

#Fit sex and port model
ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex~port)
fitbt_portsex <- nls(loglength ~ log(vb_group(age, portsex, Linf, K, t0)), 
                     data = rf_black,
                     start = lapply(svbt, rep, 10))
summary(fitbt_portsex)

#fit year model
ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(. ~ year2)
fitbt_year <- nls(loglength ~ log(vb_group(age, year2, Linf, K, t0)), data = rf_black, start = lapply(svbt, rep, 3))
#try better starting values
temp_year <- sapply(sort(unique(rf_black$year2)), function(x) vbStarts(length ~ age, data = rf_black[rf_black$year2 == x, ]))
svbt_year <- list(Linf = unlist(temp_year[1,]), K = unlist(temp_year[2,]),  t0 = unlist(temp_year[3,]))
fitbt_year <- nls(loglength ~ log(vb_group(age, year2, Linf, K, t0)), data = rf_black, start = svbt_year)
#Run individually
fitbt_year  <- nlsList(loglength ~ log(vb(age, Linf, K, t0)) | year2, rf_black, svbt)
fitbt_year
fitbt_year  <- nlsList(loglength ~ log(vb(age, Linf, K, t0)) | year2, rf_black, list(Linf = 56, K = .15, t0 = 0))
summary(fitbt_year)

#fit sex and year model
ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex ~ year2)
fitbt_sexyear <- nls(loglength ~ log(vb_group(age, sexyear2, Linf, K, t0)), data = rf_black, start = lapply(svbt, rep, 6))
summary(fitbt_sexyear)

#fit port and year model
ggplot(rf_black, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(port ~ year2)
fitbt_portyear <- nls(loglength ~ log(vb_group(age, portyear2, Linf, K, t0)), 
                      data = rf_black, 
                      start = lapply(svbt, rep, 15),
                      control = list(minFactor = 1/10000))
temp_portyear <- sapply(sort(unique(rf_black$portyear2)), function(x) vbStarts(length ~ age, data = rf_black[rf_black$portyear2 == x, ]))
svbt_portyear <- list(Linf = unlist(temp_portyear[1,]), K = unlist(temp_portyear[2,]),  t0 = unlist(temp_portyear[3,]))
fitbt_portyear <- nls(loglength ~ log(vb_group(age, portyear2, Linf, K, t0)), data = rf_black, start = svbt_year)
#Run individually
fitbt_portyear  <- nlsList(loglength ~ log(vb(age, Linf, K, t0)) | portyear2, rf_black, svbt)
fitbt_portyear
#Early years again a problem try t0 = 0 which worked for the year only model
fitbt_portyear  <- nlsList(loglength ~ log(vb(age, Linf, K, t0)) | portyear2, rf_black, list(Linf = 56, K = .15, t0 = 0))
summary(fitbt_portyear)

# port and sex model is best
AIC(fitbt_common, fitbt_sex, fitbt_portsex, fitbt_sexyear, fitbt_portsexyear)
AICList(fitbt_port)
AICList(fitbt_year)
AICList(fitbt_portyear)

tempb <- 
  coef(fitbt_portsex) %>%
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
  geom_line(data = plotb, mapping = aes(x = age, y = length), size = 1, inherit.aes = FALSE) +
  facet_grid(sex ~ port) +
  scale_y_continuous(name = "Length (cm)", limits = c(20, 70)) +
  scale_x_continuous(name = "Age") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")

#Get parameter CIs
ports <- unique(sort(rf_black$port))
nports <- length(ports)
sexes <- unique(sort(rf_black$sex))
nsexes <- 2
predict2 <- function(x) predict(x,data.frame(age=ages))
age_rangeblack <- 
  rf_black %>%
  group_by(port, sex) %>%
  summarise(min_age = min(age), max_age = max(age), max_lg = max(length))

cfsb <- cisb <- preds1b <- preds2b <- NULL
for (i in 1:nports) {
  for (y in 1:nsexes) {
    ## Loop notification (for peace of mind)
    cat(ports[i], " ", sexes[y],"Loop\n")
    ## Isolate year's data
    tmp1 <- filter(rf_black, port==ports[i], sex==sexes[y])
    ## Fit von B to that year
    fit1 <- nls(length ~ vb(age, Linf,K,t0), data=tmp1, start = svbt)
    ## Extract and store parameter estimates and CIs
    cfsb <- rbind(cfsb,coef(fit1))
    boot1 <- Boot(fit1, R = 250)
    tmp2 <-  confint(boot1, type = "perc")
    cisb <- rbind(cisb, c(tmp2["Linf",],tmp2["K",],tmp2["t0",]))
    ## Predict mean lengths-at-age with CIs
    ##   preds1 -> across all ages
    ##   preds2 -> across observed ages only
    ages <- seq(-1, max(age_rangeblack$max_age), 1)
    boot2 <- Boot(fit1, f=predict2, R = 250)
    tmp2 <- data.frame(port=ports[i], sex = sexes[y] ,age=ages,
                       predict(fit1, data.frame(age=ages)),
                       confint(boot2, type = "perc"))
    # lower = confint(boot2)[,1] - sqrt(mean(resid(fit1)^2)),
    # upper = confint(boot2)[,2] + sqrt(mean(resid(fit1)^2)))
    
    preds1b <- rbind(preds1b,tmp2)
    tmp2 <- filter(tmp2,
                   age >= age_rangeblack$min_age[age_rangeblack$port == ports[i] & age_rangeblack$sex == sexes[y]],
                   age <= age_rangeblack$max_age[age_rangeblack$port == ports[i] & age_rangeblack$sex == sexes[y]])
    preds2b <- rbind(preds2b,tmp2)
  }
}

rownames(cfsb) <- rownames(cisb) <- paste0(rep(ports, each = 2), "_",rep(sexes, times = 5))
colnames(cisb) <- paste(rep(c("Linf","K","t0"),each=2),
                       rep(c("LCI","UCI"),times=2),sep=".")
colnames(preds1b) <- colnames(preds2b) <- c("port","sex", "age","fit","LCI","UCI") 

ggplot(data=rf_black, aes(x = age, y = length, group = age)) +
  geom_hex(aes(x = age, y = length), binwidth = c(2, 2), inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "Sample size", option = "E", direction = -1, trans = scales::sqrt_trans()) +
  geom_ribbon(data=preds2b, aes(x=age,ymin=LCI,ymax=UCI),alpha=0.3, inherit.aes = FALSE) +
  geom_line(data=preds1b,aes(y=fit,x=age),size=1,linetype=2, inherit.aes = FALSE) +
  geom_line(data=preds2b,aes(y=fit,x=age),size=1, inherit.aes = FALSE) +
  scale_y_continuous(name="Total Length (mm)", limits = c(0, max(age_rangeblack$max_lg))) +
  scale_x_discrete(name="Age (years)") +
  facet_grid(sex~port) +
  theme_bw(base_size = 16)

cfsb <- data.frame(port = rep(ports, each = 2), sex = rep(sexes, times = 5), cfsb)
cfsb_long <-
  cfsb %>%
  pivot_longer(cols = -c(port, sex), names_to = "var", values_to = "val") %>%
  filter(var == "Linf")
cisb <- data.frame(port = rep(ports, each = 2), sex = rep(sexes, times = 5), cisb)
cisb_long <-
  cisb %>%
  pivot_longer(cols = -c(port, sex), names_to = "temp", values_to = "val") %>%
  mutate(var = gsub("(.*)\\..*", ("\\1"), temp),
         bound = gsub(".*\\.(.*)", ("\\1"), temp)) %>%
  select(-temp) %>%
  pivot_wider(id_cols = c("port", "sex", "var"), names_from = "bound", values_from = "val") %>%
  filter(var == "Linf") %>%
  as.data.frame()
ggplot() +
  geom_point(data=cfsb_long,aes(x=port,y=val, color = sex), position=position_dodge(width=0.5), size = 3) +
  geom_errorbar(data=cisb_long,aes(x=port,color = sex, ymin=LCI,ymax=UCI), size = 1, width=0.3, position=position_dodge(width=0.5)) +
  scale_color_manual(values = c("Black", "grey40")) +
  labs(color = "Sex", x = "Port", y = expression(L[infinity])) +
  theme_bw(base_size = 16)




# Yelloweye length-age ---------------------------------------------------
ggplot(rflgage[rflgage$spp == "Yelloweye", ],
       aes(x = age, y = length)) +
  geom_point() +
  facet_grid(port ~ sex)
table(rflgage$port[rflgage$spp == "Yelloweye"], rflgage$sex[rflgage$spp == "Yelloweye"])

#Prep data for analysis
rf_yelloweye <- rflgage[rflgage$spp == "Yelloweye", ]
rf_yelloweye$loglength <- log(rf_yelloweye$length)
rf_yelloweye$sex <- as.factor(rf_yelloweye$sex)
rf_yelloweye$port <- as.factor(rf_yelloweye$port)
rf_yelloweye$year2 <- cut(rf_yelloweye$year, breaks = c(1995, 2005, 2014, 2019))
rf_yelloweye$portsex <- as.factor(paste0(rf_yelloweye$port, rf_yelloweye$sex))
rf_yelloweye$sexyear2 <- as.factor(paste0(rf_yelloweye$sex, rf_yelloweye$year2))
rf_yelloweye$portyear2 <- as.factor(paste0(rf_yelloweye$port, rf_yelloweye$year2))
rf_yelloweye$portsexyear2 <- as.factor(paste0(rf_yelloweye$port, rf_yelloweye$sex, unique(rf_yelloweye$year2)))


#  * Francis parameterization ----------------------------------------------------
# Does not find global minumum when run w groups. RUnning w groups is desirable for AIC model selection
# Code not updated
table(rf_yelloweye$age)
ages_ye <- c(10, 60)
mean(rf_yelloweye$length[rf_yelloweye$age == ages_ye[1]])
mean(rf_yelloweye$length[rf_yelloweye$age == ages_ye[2]])


plot(rf_yelloweye$age, rf_yelloweye$length)
#Fit common model
(sv_common <- vbStarts(length ~ age, data = rf_yelloweye, type="Francis", tFrancis = ages_ye))
(sv_common <- list(L1 = 25, L2 = 60, L3 = 80))
vbFrancis <- vbFuns("Francis")
fit_common <- nls(length ~ vbFrancis(age, L1, L2, L3, t1 = ages_ye), data = rf_yelloweye, start = sv_common)
summary(fit_common)

ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(.~sex)
# fit sex model
(sv_sex <- lapply(sv_common, rep, 2))
fit_sex <- nls(length ~ vbFrancis_group(age, sex2, L1, L2, L3, ages_ye), data = rf_yelloweye, start = sv_sex)
summary(fit_sex)

ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(.~port)
#fit port model
(sv_port <- lapply(sv_common, rep, 5))
fit_port <- nls(length ~ vbFrancis_group(age, port2, L1, L2, L3, ages_ye), data = rf_yelloweye, start = sv_port)
summary(fit_port)

ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex~port)
#Fit sex and port model
(sv_portsex <- lapply(sv_common, rep, 10))
fit_portsex <- nls(length ~ vbFrancis_group(age, portsex2, L1, L2, L3, ages_ye), 
                   data = rf_yelloweye,
                   start = sv_portsex)
summary(fit_portsex)

ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex~port)
#Fit sex and port model
(sv_portsex <- lapply(sv_common, rep, 10))
fit_portsex <- nls(length ~ vbFrancis_group(age, portsex2, L1, L2, L3, ages_ye), 
                   data = rf_yelloweye,
                   start = sv_portsex)
summary(fit_portsex)

ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(. ~ year2)
#fit year model
(sv_year <- lapply(sv_common, rep, 3))
fit_year <- nls(length ~ vbFrancis_group(age, year2, L1, L2, L3, ages_ye), data = rf_yelloweye, start = sv_year)
summary(fit_year)

ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex ~ year2)
#fit sex and year model
(sv_sexyear <- lapply(sv_common, rep, dim(grid_sexyear)[1]))
fit_sexyear <- nls(length ~ vbFrancis_group(age, sexyear2, L1, L2, L3, ages_ye), data = rf_yelloweye, start = sv_sexyear)
summary(fit_sexyear)

ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(port ~ year2)
#fit port and year model
(sv_portyear <- lapply(sv_common, rep, dim(grid_portyear)[1]))
fit_portyear <- nls(length ~ vbFrancis_group(age, portyear2, L1, L2, L3, ages_ye), 
                    data = rf_yelloweye, 
                    start = sv_portyear,
                    control = list(minFactor = 1/10000))
summary(fit_portyear)

# fit port sex and year model
(sv_portsexyear <- lapply(sv_common, rep, dim(grid_portsexyear)[1]))
fit_portsexyear <- nls(length ~ vbFrancis_group(age, portsexyear2, L1, L2, L3, ages_ye), 
                       data = rf_yelloweye, 
                       start = sv_portsexyear,
                       control = list(minFactor = 1/10000))
summary(fit_portsexyear)
#Assumptions look Ok to me.
residPlot(fit_portsexyear)
data.frame(resid = residuals(fit_portsexyear), fitted = fitted.values(fit_portsexyear)) %>%
  ggplot(aes(x = fitted, y =resid)) +
  geom_hex(binwidth = c(2, 2)) +
  scale_fill_viridis_c(name = "Sample size", option = "E", direction = -1, trans = scales::sqrt_trans())
  

# port and sex model is best
AIC(fit_common, fit_sex, fit_port, fit_year, fit_portsex, fit_sexyear, fit_portyear)

#  * Typical parameterization ----------------------------------------------------
table(rf_yelloweye$age)

plot(rf_yelloweye$age, rf_yelloweye$length)
(svyt <- vbStarts(length ~ age, data = rf_yelloweye))
# fit port sex and year model
fityt_portsexyear <- nls(length ~ vb_group(age, portsexyear2, Linf, K, t0), 
                         data = rf_yelloweye, 
                         start = lapply(svyt, rep, length(unique(rf_yelloweye$portsexyear2))),
                         control = list(minFactor = 1/10000))
summary(fityt_portsexyear)
#Assumptions look Ok to me.
residPlot(fityt_portsexyear)
data.frame(resid = residuals(fityt_portsexyear), fitted = fitted.values(fityt_portsexyear)) %>%
  ggplot(aes(x = fitted, y =resid)) +
  geom_hex(binwidth = c(2, 2)) +
  scale_fill_viridis_c(name = "Sample size", option = "E", direction = -1, trans = scales::sqrt_trans())

#Fit common model
vb <- vbFuns("Typical")
fityt_common <- nls(length ~ vb(age, Linf, K, t0), data = rf_yelloweye, start = svyt)
summary(fityt_common)

# fit sex model
ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(.~sex)
fityt_sex <- nls(length ~ vb_group(age, sex, Linf, K, t0), data = rf_yelloweye, start = lapply(svyt, rep, 2))
summary(fityt_sex)

#fit port model
ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(.~port)
fityt_port <- nls(length ~ vb_group(age, port, Linf, K, t0), data = rf_yelloweye, start = lapply(svyt, rep, 5))
summary(fityt_port)

#Fit sex and port model
ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex~port)
fityt_portsex <- nls(length ~ vb_group(age, portsex, Linf, K, t0), 
                   data = rf_yelloweye,
                   start = lapply(svyt, rep, 10))
summary(fityt_portsex)

#fit year model
ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(. ~ year2)
fityt_year <- nls(length ~ vb_group(age, year2, Linf, K, t0), data = rf_yelloweye, start = lapply(svyt, rep, 3))
summary(fityt_year)

#fit sex and year model
ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(sex ~ year2)
fityt_sexyear <- nls(length ~ vb_group(age, sexyear2, Linf, K, t0), data = rf_yelloweye, start = lapply(svyt, rep, 6))
summary(fityt_sexyear)

#fit port and year model
ggplot(rf_yelloweye, aes(x = age, y = length)) +
  geom_point() +
  facet_grid(port ~ year2)
fityt_portyear <- nls(length ~ vb_group(age, portyear2, Linf, K, t0), 
                    data = rf_yelloweye, 
                    start = lapply(svyt, rep, 15),
                    control = list(minFactor = 1/10000))
summary(fityt_portyear)

# port and sex model is best
AIC(fityt_common, fityt_sex, fityt_port, fityt_year, fityt_portsex, fityt_sexyear, fityt_portyear, fityt_portsexyear)

tempy <- 
  data.frame(fit = coefficients(fityt_portsex),
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
  guides(fill = guide_colorbar(label.theme = element_text(angle = 45)))
  geom_line(data = ploty, mapping = aes(x = age, y = length), size = 1, inherit.aes = FALSE) +
  facet_grid(sex ~ port) +
  scale_y_continuous(name = "Length (cm)", limits = c(20, 90)) +
  scale_x_continuous(name = "Age") +
  theme_bw(base_size = 16)

#Get parameter CIs
age_rangeyellow <- 
  rf_yelloweye %>%
  group_by(port, sex) %>%
  summarise(min = min(age), max = max(age))

cfsy <- cisy <- preds1y <- preds2y <- NULL
for (i in 1:nports) {
  for (y in 1:nsexes) {
    ## Loop notification (for peace of mind)
    cat(ports[i], " ", sexes[y],"Loop\n")
    ## Isolate year's data
    tmp1 <- filter(rf_yelloweye, port==ports[i], sex==sexes[y])
    ## Fit von B to that year
    fit1 <- nls(length ~ vb(age, Linf,K,t0), data=tmp1, start = svyt)
    ## Extract and store parameter estimates and CIs
    cfsy <- rbind(cfsy,coef(fit1))
    boot1 <- Boot(fit1, R = 250)
    tmp2 <-  confint(boot1, type = "perc")
    cisy <- rbind(cisy,c(tmp2["Linf",],tmp2["K",],tmp2["t0",]))
    ## Predict mean lengths-at-age with CIs
    ##   preds1 -> across all ages
    ##   preds2 -> across observed ages only
    ages <- seq(-1,max(age_rangeyellow$max),1)
    boot2 <- Boot(fit1,f=predict2, R = 250)
    tmp2 <- data.frame(port=ports[i], sex = sexes[y] ,age=ages,
                       predict(fit1,data.frame(age=ages)),
                       confint(boot2, type = "perc"))
                       # lower = confint(boot2)[,1] - sqrt(mean(resid(fit1)^2)),
                       # upper = confint(boot2)[,2] + sqrt(mean(resid(fit1)^2)))
    
    preds1y <- rbind(preds1y,tmp2)
    tmp2 <- filter(tmp2,
                   age >= age_rangeyellow$min[age_rangeyellow$port == ports[i] & age_rangeyellow$sex == sexes[y]],
                   age <= age_rangeyellow$max[age_rangeyellow$port == ports[i] & age_rangeyellow$sex == sexes[y]])
    preds2y <- rbind(preds2y,tmp2)
  }
}

rownames(cfsy) <- rownames(cisy) <- paste0(rep(ports, each = 2), "_",rep(sexes, times = 5))
colnames(cisy) <- paste(rep(c("Linf","K","t0"),each=2),
                       rep(c("LCI","UCI"),times=2),sep=".")
colnames(preds1y) <- colnames(preds2y) <- c("port","sex", "age","fit","LCI","UCI") 

ggplot(data=rf_yelloweye, aes(x = age, y = length, group = age)) +
  geom_hex(aes(x = age, y = length), binwidth = c(2, 2), inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "Sample size", option = "E", direction = -1, trans = scales::sqrt_trans()) +
  geom_ribbon(data=preds2y, aes(x=age,ymin=LCI,ymax=UCI),alpha=0.3, inherit.aes = FALSE) +
  geom_line(data=preds1y,aes(y=fit,x=age),size=1,linetype=2, inherit.aes = FALSE) +
  geom_line(data=preds2y,aes(y=fit,x=age),size=1, inherit.aes = FALSE) +
  scale_y_continuous(name="Total Length (mm)") +
  scale_x_discrete(name="Age (years)") +
  facet_grid(sex~port) +
  theme_bw(base_size = 16)

cfsy <- data.frame(port = rep(ports, each = 2), sex = rep(sexes, times = 5), cfsy)
cfsy_long <-
  cfsy %>%
  pivot_longer(cols = -c(port, sex), names_to = "var", values_to = "val") %>%
  filter(var %in% c("Linf", "K"))
cisy <- data.frame(port = rep(ports, each = 2), sex = rep(sexes, times = 5), cisy)
cisy_long <-
  cisy %>%
  pivot_longer(cols = -c(port, sex), names_to = "temp", values_to = "val") %>%
  mutate(var = gsub("(.*)\\..*", ("\\1"), temp),
         bound = gsub(".*\\.(.*)", ("\\1"), temp)) %>%
  select(-temp) %>%
  pivot_wider(id_cols = c("port", "sex", "var"), names_from = "bound", values_from = "val") %>%
  filter(var %in% c("Linf", "K")) %>%
  as.data.frame()
cfsy_long %>% 
  filter(var == "Linf") %>%
  ggplot() +
  geom_point(aes(x=port,y=val, color = sex), position=position_dodge(width=0.5), size = 3) +
  geom_errorbar(data = cisy_long[cisy_long$var == "Linf",], aes(x=port,color = sex, ymin=LCI,ymax=UCI), size = 1, width=0.3, position=position_dodge(width=0.5)) +
  scale_color_manual(values = c("Black", "grey40")) +
  labs(color = "Sex", x = "Port", y = expression(L[infinity])) +
  theme_bw(base_size = 16)
cfsy_long %>% 
  filter(var == "K") %>%
  ggplot() +
  geom_point(aes(x=port,y=val, color = sex), position=position_dodge(width=0.5), size = 3) +
  geom_errorbar(data = cisy_long[cisy_long$var == "K",], aes(x=port,color = sex, ymin=LCI,ymax=UCI), size = 1, width=0.3, position=position_dodge(width=0.5)) +
  scale_color_manual(values = c("Black", "grey40")) +
  labs(color = "Sex", x = "Port", y = expression(K)) +
  theme_bw(base_size = 16)

#output a list for markdown
list_lgage <- list(plotb, cfsb_long, cisb_long, ploty, cfsy_long, cisy_long)
saveRDS(list_lgage, ".\\Rockfish report_96-19\\lengthage\\list_lgage.rds")

