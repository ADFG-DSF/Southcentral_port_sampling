library(tidyverse)


# Black Rockfish ---------------------------------------------------
# * data prep ---------------------------------------------------
rfwgtlen <- 
  readRDS(".\\Rockfish report_96-19\\composition\\rfcomp.rds") %>% 
  filter(!is.na(weight) & !is.na(length) & sex != "") %>%
  filter(species %in% c("Black", "Copper", "Dark", "Dusky", "Quill", "Yelleye")) %>%
  mutate(spp2 = factor(species, 
                       levels = c("Black", "Copper", "Dark", "Dusky", "Quill", "Yelleye"),
                       labels = c("Black", "Copper", "Dark", "Dusky", "Quillback", "Yelloweye")),
         logW = log(weight),
         logL = log(length)) %>%
  arrange(spp, year)


mod1 <- sapply(c("Black", "Copper", "Dark", "Dusky", "Quillback", "Yelloweye"), 
               function(x) lm(logW ~ logL, rfwgtlen[rfwgtlen$spp2 == x, ]), simplify = FALSE, USE.NAMES = TRUE)

#residuals look good but have heavy tails
par(mfrow = c(2,2))
lapply(mod1, plot)
par(mfrow = c(1,1))
lapply(mod1, summary)

#try student t errors
library(brms)
#method results in same point estimates for the normal model
mod2g <- 
  brm(logW ~ 1 + logL,
      data = rfwgtlen[rfwgtlen$spp == "Black",], 
      family = gaussian,
      prior = c(prior(normal(0, 100), class = Intercept),
                prior(normal(0, 100), class = b),
                prior(cauchy(0, 1),  class = sigma)),
      seed = 1)
mod2g

#Note that correcting for the tails does not result in meaningfully different estimates
mod2 <-
  sapply(c("Black", "Copper", "Dark", "Dusky", "Quillback", "Yelloweye"),
         function(x){
          brm(logW ~ 1 + logL,
              data = rfwgtlen[rfwgtlen$spp2 == x,], 
              family = student,
              prior = c(prior(normal(0, 100), class = Intercept),
                        prior(normal(0, 100), class = b),
                        prior(gamma(2, 0.1), class = nu),
                        prior(cauchy(0, 1),  class = sigma)),
              seed = 1)},
         simplify = FALSE, USE.NAMES = TRUE)

point_fit <- lapply(mod2, function(x) fitted(x)[, 1])
point_resid <- lapply(mod2, function(x) residuals(x)[, 1])
par(mfrow = c(3,2))
mapply(plot, x = point_fit, y = point_resid)
lapply(point_resid, function(x){
  z <- length(x)
  qqplot(x, rt(z, 4))}) #Should get df from mod2
par(mfrow = c(1,1))

#plot
WLine <-
  mapply(function(x, y){
           length <- min(rfwgtlen$length[rfwgtlen$spp2 == x]):max(rfwgtlen$length[rfwgtlen$spp2 == x])
           data.frame(length = length,
                      weight = exp(y[1,1] + y[2,1]*log(length)),
                      spp2 = x)},
         c("Black", "Copper", "Dark", "Dusky", "Quillback", "Yelloweye"),
         lapply(mod2, fixef),
         SIMPLIFY = FALSE) %>%
  do.call(rbind, .)


fig_weight <-
  rfwgtlen %>%
    ggplot(aes(length, weight)) +
    geom_hex(binwidth= c(1, .25)) +
    scale_fill_viridis_c(name = "Sample size", breaks = c(5, 150, 500), option = "E", direction = -1, trans = scales::sqrt_trans()) +
    guides(fill = guide_colorbar(label.theme = element_text(angle = 45))) +
    geom_line(data = WLine) +
    facet_wrap(.~spp2, scales = "free")  +
    scale_x_continuous(name = "Length (cm)") +
    scale_y_continuous(name = "Weight (kg)") +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")
fig_weight
saveRDS(fig_weight, ".\\Rockfish report_96-19\\fig_weight.rds")

 
par_t0 <- lapply(mod2, 
                 function(x) summary(x)$spec_pars %>%
                   rownames_to_column("var") %>%
                   select(var, mean = Estimate, sd = Est.Error))
names_t <- names(par_t0)
par_t <- 
  mapply(function(x, y) mutate(x, spp = y), x = par_t0, y = names_t, SIMPLIFY = FALSE) %>%
  bind_rows() %>%
  mutate(var = ifelse(var =="nu", "df", "sigma")) %>%
  pivot_wider(spp, names_from = var, values_from = c(mean, sd))

par_f0 <- lapply(mod2, 
                 function(x) summary(x)$fixed %>%
                   rownames_to_column("var") %>%
                   select(var, mean = Estimate, sd = Est.Error))
names_f <- names(par_f0)
par_f <- 
  mapply(function(x, y) mutate(as.data.frame(x), spp = y), x = par_f0, y = names_f, SIMPLIFY = FALSE) %>%
  bind_rows() %>%
  mutate(var = ifelse(var =="Intercept", "a", "b")) %>%
  pivot_wider(spp, names_from = var, values_from = c(mean, sd))

mod_n0 <- lapply(mod2, function(x) length(x$data$logW))
names_n <- names(mod_n0)
mod_n <- 
  mapply(function(x, y) mutate(setNames(as.data.frame(x), "N"), spp = y), x = mod_n0, y = names_n, SIMPLIFY = FALSE) %>%
  bind_rows()

mod_weight <- 
 left_join(mod_n, par_f, "spp") %>% 
  left_join(par_t, "spp") %>%
  select(spp, N, ends_with("_a"), ends_with("b"), ends_with("df"), ends_with("sigma"))
saveRDS(mod_weight, ".\\Rockfish report_96-19\\mod_weight.rds")
