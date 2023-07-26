library(tidyverse)
library(ggplot2)


# Data prep ---------------------------------------------------------------

# * Composition data --------------------------------------------------------
# Original data maker and many comments in 
# "H:\My Documents\Southcentral halibut and groundfish\Rockfish report_96-19\spp comp\RFspcomp9616_port.sas"
#  some comments transferred below
rfcomp00 <- haven::read_sas(".\\Rockfish report_96-19\\composition\\rfcompdata.sas7bdat")
rfcomp00 <- setNames(rfcomp00, tolower(names(rfcomp00)))


# * * Clean data --------------------------------------------------------------
# Exclude rockfish marked as "rare" (Seward only) for species comp -- these are uncommon species that were sampled in
#   addition to the usual protocol to boost sample size for age and growth studies.
table(rfcomp00$port, rfcomp00$rare)
#Format species and estimates for a species comp table and figure

# Find out why some spp = 154 are labeled Dusky
# Not covered in sas code although they correct to DuskyDrk (line 186)
table(rfcomp00$sp, rfcomp00$species)
table(rfcomp00[rfcomp00$species == "Dusky", ]$year,
      rfcomp00[rfcomp00$species == "Dusky", ]$port,
      rfcomp00[rfcomp00$species == "Dusky", ]$sp)

#Sparse sampling in Whittier in 1998
#Insufficient Cordova samples
table(rfcomp00$port, rfcomp00$year)

#6 species field is blank
table(rfcomp00$species, useNA = "always")

# define names for species and assemblage comp
names_spp <- 
  c(c("Black", "Dark", "Dusky", "DuskyDrk", "Yelleye", "Copper", "Quill"),
    sort(unique(rfcomp00$species))[!(sort(unique(rfcomp00$species)) %in% 
                                      c("Black", "Dark", "Dusky", "DuskyDrk", "Yelleye", "Copper", "Quill"))])
names_assem <-
  c(c("Black", "Dark", "Dusky", "DuskyDrk", "Yelltail", "Widow"),
    sort(unique(rfcomp00$species))[!(sort(unique(rfcomp00$species)) %in%
                                      c("Black", "Dark", "Dusky", "DuskyDrk", "Yelltail", "Widow"))])

rfcomp0 <- 
  setNames(rfcomp00, tolower(names(rfcomp00))) %>%
  filter(rare != "R",
         port != "Cordova",
         !(port == "Whittier" & year == 1998),
         species != "") %>%
  mutate(species = ifelse(sp == 154, "DuskyDrk", species),
         spp = factor(species, 
                      levels = names_spp,
                      labels = c("Black", "Dusky/Dark", "Dusky/Dark", "Dusky/Dark", "Yelloweye", "Copper", "Quillback", rep("Other", 19))),
         assem = factor(species, 
                        levels = names_assem,
                        labels = c(rep("Pelagic", 6), rep("non-Pelagic", 20))),
         port = factor(port, 
                       levels = c("CI", "Kodiak", "Seward", "Valdez", "Whittier"),
                       labels = c("Cook Inlet", "Kodiak", "Seward", "Valdez", "Whittier"))) %>%
  select(port, user, year, species, sp, spp, assem, sex, length, weight, age, boatname, month, day)
table(rfcomp0$sp, rfcomp0$species) #renamed spp = 154 Dusky
#Note Rares (check file sizes) and small N ports deleted 
table(rfcomp0$port, rfcomp0$year)
table(rfcomp0$species, rfcomp0$assem, useNA = "always")
#rfcomp00(75838)-Cordova(79)-Whittier98(48)-Rares(16)-sppblank(6)=rfcomp0(75689)



# * * *  Military and Unknown Users ----------------------------------------------
table(rfcomp0$user, rfcomp0$year, rfcomp0$port)
# Military affects Seward 1996-2000
# Spp comps differ by user group
# Private and miltary more similar but real issue is how military anglers identified themselves in the SWHS.
dat_Sew <- 
  rfcomp0[rfcomp0$port == "Seward" & rfcomp0$user != "Unknown" & rfcomp0$year %in% 1996:2000,
         which(names(rfcomp0) %in% c("spp", "user", "year"))]
table_Sew <- with(dat_Sew,table(user, spp, year))
table_Sew
for(i in 1:5) print(table_Sew[, c(1, 3, 6),i] / rowSums(table_Sew[, c(1, 3, 6),i]))
#Chi-sq all
for(y in 1:5){print(chisq.test(table_Sew[,c(1, 3, 6),y]))}
#Chi-sq charter & mil
for(y in 1:5){print(chisq.test(table_Sew[,c(1, 3, 6),y][c(1,3),]))}
#Chi-sq private & mil
for(y in 1:5){print(chisq.test(table_Sew[,c(1, 3, 6),y][c(2,3),]))}

#We have estimates of military harvest and have compared pooled (all users) and weighted (charter/private by SWHS, mil by logbook)
#estimates of stock composition. small differences.
#Previous researchers focused on this comparison.
est_Sewmil <- 
  readxl::read_excel(".\\Rockfish report_96-19\\composition\\SpeciesCompBefore2001.xlsx",
                     range = "Seward!AC12:AF53",
                     col_names = c("spp", "n", "pool", "weight")) %>%
  select(-n) %>%
  filter(!is.na(spp), spp != "Other") %>%
  mutate(year = as.numeric(rep(1996:2000, each = 2))) %>%
  pivot_longer(pool:weight, names_to = "method", values_to = "p_pys") %>%
  mutate(p_pys = as.numeric(p_pys),
         spp = ifelse(spp == "Yelleye", "Yelloweye", spp))
ggplot(est_Sewmil, aes(x = year, y = p_pys, color = method)) +
  geom_point(position=position_dodge(width=0.5)) +
  facet_grid(. ~ spp)

#unknown users generally a small fraction of total
table(rfcomp0$user, rfcomp0$year, rfcomp0$port)
#No comp diff in Kodiak 1997
tab_Kod97 <- rfcomp0[rfcomp0$port == "Kodiak" & rfcomp0$year %in% 1997, which(names(rfcomp0) %in% c("spp", "user", "year"))] %>%
  with(table(user, spp))
chisq.test(tab_Kod97[, c(1, 3)])

#Comp diff in Valdez 1996
tab_Val96 <- 
  rfcomp0[rfcomp0$port == "Valdez" & rfcomp0$year %in% 1996, which(names(rfcomp0) %in% c("spp", "user", "year"))] %>%
  with(table(user, spp))
chisq.test(tab_Val96[, c(1, 3)])

#Martin and I can't see why we would not just consider military as charter since that's what was done after 2000.
#I will revisit this after we have estimates w that assumption.
#Regarding other the best thing seems to be to drop
table(rfcomp0$user, rfcomp0$year, rfcomp0$port)
table(rfcomp0$user)
rfcomp <- 
  rfcomp0 %>%
  mutate(user = ifelse(user == "SewMilC", "Charter", user)) %>%
  filter(user != "Unknown")
#rfcomp0(75689) - 338 = rfcomp(75351)
table(rfcomp$user, rfcomp$year, rfcomp$port)
#saveRDS(rfcomp, ".\\Rockfish report_96-19\\composition\\rfcomp.rds")


# * SWHS data ---------------------------------------------------------------
SWHS0 <-
  readxl::read_excel(".\\Rockfish report_96-19\\composition\\RFHarvByUser9619.xlsx", 
                     range = "SWHS96-19!A1:H121")
SWHS0 <- setNames(SWHS0, tolower(names(SWHS0)))
SWHS <- 
  SWHS0 %>%
  pivot_longer(ends_with("harv")) %>%
  mutate(user = ifelse(grepl("charv", name), "Charter", 
                       ifelse(grepl("pharv", name), "Private", "Both")),
         stat = ifelse(grepl("se", name), "seH_puy", "H_puy"),
         port = factor(port, 
                       levels = c("CI", "Kodiak", "Seward", "Valdez", "Whittier"),
                       labels = c("Cook Inlet", "Kodiak", "Seward", "Valdez", "Whittier"))) %>%
  select(-name) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(vH_puy = seH_puy^2) %>%
  select(-seH_puy)
SWHS <- setNames(SWHS, ifelse(grepl("H", names(SWHS)), names(SWHS), tolower(names(SWHS))))
#p = port, u = user group, y = year.
SWHS_puy <- SWHS[SWHS$user %in% c("Charter", "Private"), ]
SWHS_py <- SWHS[SWHS$user %in% c("Both"), ] %>%
  select(-user)
SWHS_py <- setNames(SWHS_py, gsub("puy", "py", names(SWHS_py)))


# Species comp ---------------------------------------------------
#Note sample sizes OK in most year/port/user combinations
table(rfcomp$year, rfcomp$port, rfcomp$user)

#number available for composition estimates, used in report.
n_comp <- 
  rfcomp %>%
  group_by(port, year, user) %>%
  summarise(n = sum(!is.na(spp)))
#quantiles
quantile(n_comp$n[n_comp$user == "Charter"], c(0, 0.05, .5, 1))
quantile(n_comp$n[n_comp$user == "Private"], c(0, 0.05, .5, 1))
#only 2 port/year combination less than 30 for private anglers
table(n_comp$n[n_comp$user == "Private"])
sum(n_comp$n[n_comp$user == "Private"] < 30)

#Stratification needed. Most compositions (yeat and port combinations) differ by user group.
#use the Benjamini–Hochberg procedure to account for false positives associated with multiple tests
test_comp <- 
  n_comp %>% 
  pivot_wider(names_from = user, values_from = n) %>% 
  rowwise()
test_comp$pval <- NA
for(i in 1:dim(test_comp)[1]){
  test_comp$pval[i] <- 
    fisher.test(
      rfcomp$user[rfcomp$port == test_comp[[i, 1]] & rfcomp$year == test_comp[[i, 2]]], 
      rfcomp$spp[rfcomp$port == test_comp[[i, 1]] & rfcomp$year == test_comp[[i, 2]]], simulate.p.value = TRUE)$p.value
}
test_comp$p05 <- ifelse(test_comp$pval < 0.05, TRUE, FALSE)
test_comp$p.adjust <- p.adjust(test_comp$pval, method = "BH")
test_comp$p05a <- ifelse(test_comp$p.adjust < 0.05, TRUE, FALSE)
#Note 100 out of 117 sig with or wo correction
table(test_comp$p05)
table(test_comp$p05a)
#Benjamini–Hochberg plot
plot(1:length(test_comp$pval),sort(test_comp$pval))
abline(0,1:length(test_comp$pval)/length(test_comp$pval)*.05)


# raw proportions
prop <- 
  rfcomp %>%
  group_by(port, user, year, spp) %>% 
  summarise(cnt = n()) %>% 
  pivot_wider(names_from = user, values_from = cnt) %>% #next 3 lines fill missing spp
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  pivot_longer(Charter:Private, names_to = "user", values_to = "cnt") %>%
  group_by(port, user, year) %>%
  mutate (p_puys = cnt / sum(cnt),
          vp_puys = p_puys * (1 - p_puys) / sum(cnt)) %>%
  select(-cnt) 
#low sample size est by user are not terrible outliers
prop[prop$port == "Cook Inlet" & prop$year == 1996, ]
prop[prop$port == "Cook Inlet" & prop$year == 2006, ]
plot(prop$p_puys[prop$port == "Cook Inlet" & prop$user == "Charter" & prop$spp == "Yelloweye"], 
     prop$p_puys[prop$port == "Cook Inlet" & prop$user == "Private" & prop$spp == "Yelloweye"])
abline(a = 0, b = 1)
plot(prop$p_puys[prop$port == "Cook Inlet" & prop$user == "Charter" & prop$spp == "Dusky/Dark"], 
     prop$p_puys[prop$port == "Cook Inlet" & prop$user == "Private" & prop$spp == "Dusky/Dark"])
abline(a = 0, b = 1)


#Bootstrap by boat
#Note: boats always named after 2008.
table(rfcomp$year, rfcomp$boatname == "")

#id unique trips for each each, port, user and year combination
unique_boats <- 
  rfcomp %>% 
  filter(year >= 2009) %>%
  select(port, user, year, boatname, month, day) %>%
  arrange(port, user, year, boatname, month, day) %>%
  group_by(port, user, year, boatname, month, day) %>% 
  distinct() %>%
  ungroup(boatname, month, day) %>%
  mutate(id = row_number())
rfcomp_id <- left_join(rfcomp, unique_boats, by = c("port", "user", "year", "boatname", "month", "day"))

#check sample sizes (# of boats available to bootstrap)
unique_boats %>% 
  group_by(port, user, year) %>% 
  summarise(cnt = n()) %>% 
  print(n = 100)
unique_boats %>% filter(port == "Kodiak", user == "Private", year == 2010)

#see link for slick way to do clustered bootstrap
#https://www.r-bloggers.com/2018/08/bootstrapping-clustered-data/
#Use nest to create list columns (each row of the list column is a list of data representing each boat)
#bootstrap rows of list column
rfcomp_listcol <- 
  left_join(rfcomp[rfcomp$year >= 2009, ], unique_boats,
            c("port", "user", "year", "boatname", "month", "day")) %>%
  arrange(port, user, year, id) %>%
  dplyr::mutate(strata = paste0(port, "-", user, "-", year)) %>%
  group_by(strata, id) %>%
  nest()

#identify strata to bootstrap within
strata <- unique(rfcomp_listcol$strata)

#bootstrap
boot_out <- list()
for(i in 1:length(strata)){
  boot_out[[i]] <- 
    rsample::bootstraps(rfcomp_listcol[rfcomp_listcol$strata == strata[i], c(1, 3)], times = 100)
}

#Note: some bootstrap samples will not have all species.
#This method will fill w NA if a species shows up in any sample.
# #Test: make sure all spp are represented.
# a <- data.frame(sp = c("Dark", "Black", "Yelloweye"), n = 1:3)
# b <- data.frame(sp = c("Dark", "Copper", "Yelloweye"), n = 1:3)
# c <- data.frame(sp = c("Dark", "Black", "China"), n = 1:3)
# d <- data.frame(sp = c("Dark", "Copper", "China"), n = 1:3)
# e <- list(a, b, c, d)
# reduce(e, function(...) full_join(..., by = "sp"))

#Convert list columns back to dataframe within each strata
#object still a list where each element of the list is a df for one strata (port, user, year combination).
boot_p <- 
  lapply(boot_out, 
         function(x){
          lapply(x$splits, function(x){
            as_tibble(x) %>%
              unnest(data) %>%
              group_by(spp) %>% 
              summarise(cnt = n()) %>% 
              mutate(p = cnt / sum(cnt)) %>%
              select(spp, p)
          }) %>%
          Reduce(function(...) full_join(..., by='spp'), .) %>%
          pivot_longer(-spp, values_to = "p") %>%
          mutate(p = ifelse(is.na(p), 0, p)) %>%
          select(-name)}
  )

#Check bootstrap distributions 
# Map(function(x, y){
#   ggplot(x, aes(x = p)) +
#     geom_histogram() +
#     facet_grid(spp ~ .) +
#     ggtitle(y)
#     },
#   boot_p[25:48],
#   strata[25:48]
# )

#Summary stats from bootstrap
boot_stats0 <- 
  lapply(boot_p, 
         function(x){x %>%
             group_by(spp) %>%
             summarise(pboot_puys = mean(p),
                       vpboot_puys = var(p))}
  )

#add strata ids
boot_stats <-
  Map(function(x, y){
    x$port = gsub("(.*)-(.*)-(.*)", '\\1', y);
    x$user = gsub("(.*)-(.*)-(.*)", '\\2', y);
    x$year = as.numeric(gsub("(.*)-(.*)-(.*)", '\\3', y));
    return(x)}, 
    x = boot_stats0, y = strata) %>%
  do.call(rbind, .)


#add bootstrap ps to raw proportions
#Merge w SWHS
est <-
  left_join(prop, SWHS_puy, by = c("port", "user", "year")) %>%
    left_join(boot_stats, by = c("port", "user", "year", "spp")) %>%
    mutate(pboot_puys = ifelse(is.na(pboot_puys), 0, pboot_puys),
           vpboot_puys = ifelse(is.na(vpboot_puys) & year >=2009, 0, vpboot_puys),
           H_puys = p_puys * H_puy,
           vH_puys = p_puys^2*vH_puy + vp_puys*H_puy^2 - vp_puys*vH_puy,
           vHboot_puys = p_puys^2*vH_puy + vpboot_puys*H_puy^2 - vpboot_puys*vH_puy) %>%
    pivot_wider(names_from = user, values_from = c(p_puys, vp_puys, vH_puys,
                                                   pboot_puys, vpboot_puys, vHboot_puys,
                                                   H_puy, vH_puy, H_puys)) %>%
    left_join(SWHS_py, by = c("port", "year")) %>%
    mutate(H_pys = H_puys_Charter + H_puys_Private,
           vH_pys = vH_puys_Charter + vH_puys_Private,
           vHboot_pys = vHboot_puys_Charter + vHboot_puys_Private,
           p_pys = H_pys / H_py,
           vp_pys = (1 / H_py^2) * 
             (vH_puy_Charter * (p_puys_Charter * H_puy_Private - H_puys_Private)^2 / H_py^2 +
                vH_puy_Private * (p_puys_Private * H_puy_Charter - H_puys_Charter)^2 / H_py^2 +
                vp_puys_Charter * H_puy_Charter^2 + 
                vp_puys_Private * H_puy_Private^2),
           vpboot_pys = (1 / H_py^2) * 
             (vH_puy_Charter * (pboot_puys_Charter * H_puy_Private - H_puys_Private)^2 / H_py^2 +
                vH_puy_Private * (pboot_puys_Private * H_puy_Charter - H_puys_Charter)^2 / H_py^2 +
                vpboot_puys_Charter * H_puy_Charter^2 + 
                vpboot_puys_Private * H_puy_Private^2),
           sep_pys = sqrt(vp_pys),
           sepboot_pys = sqrt(vpboot_pys)) %>%
    select(port, year, spp, p_pys, vp_pys, vpboot_pys, sep_pys, sepboot_pys, starts_with("H_puys"), starts_with("vH_puys"))
#saveRDS(est, ".\\Rockfish report_96-19\\composition\\est_spp.rds")
est <- readRDS(".\\Rockfish report_96-19\\composition\\est_spp.rds")


#Plot bootstrap and SRS SE's
est %>% 
  ggplot(aes(x= sep_pys, y = sepboot_pys)) +
    geom_point() +
    geom_abline(slope = 1) +
    labs(x = "SRS SE", y = "bootstrap SE") +
    theme_bw(base_size = 18) +
    ggtitle("Standard errors for species composition proportions")
lm(est$sepboot_pys ~ est$sep_pys)


# * Revisit Seward military -------------------------------------------------
comp_Sew <-
  est %>% 
  filter(port == "Seward", year <= 2000) %>%
  mutate(pMil = "0",
         p_pys_l = p_pys - 2 * 2*sep_pys, # extra 2 to account for average underestimate of SRS variance
         p_pys_u = p_pys + 2 * 2*sep_pys)
bind_Sew <- comp_Sew[, c(3, 2, 4, 14, 15)] %>%
  mutate(method = "comm") %>%
  filter(spp %in% c("Black", "Yelloweye")) %>%
  select(spp, year, method, p_pys, p_pys_l, p_pys_u)
#notice the CI's of the composite estimates bound either the pooled or weighted estimates calculated before. No need to separate military anglers
bind_rows(est_Sewmil, bind_Sew) %>%
  ggplot(aes(x = year, y = p_pys, color = method)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = p_pys_l, ymax = p_pys_u), position=position_dodge(width=0.5)) +
  facet_grid(. ~ spp)
#% and absolute diff - small differences
est_Sewmil %>%
  select(spp, year, method, p_pys_alt= p_pys) %>%
  left_join(bind_Sew, by = c("spp", "year")) %>%
  mutate(abs_diff = p_pys_alt - p_pys,
         pctdiff = p_pys_alt/p_pys) %>%
  arrange(spp, method.x)

#  * Format for report ----------------------------------------------------
est_format <- 
  est %>%
    mutate(sep_pys = sqrt(vp_pys),
           sepboot_pys = sqrt(vpboot_pys),
           sepreport_pys = ifelse(is.na(vpboot_pys), sep_pys * 2, sepboot_pys),
           est = paste0(format(round(p_pys, 3), digits = 3), "(", format(round(sepreport_pys, 3), digits = 3), ")"))

est_format %>%
    ggplot(aes(x = year, y = p_pys, color = spp, fill = spp)) +
      geom_bar(stat = "identity", position = "fill") +
      facet_grid(port ~ .) +
      scale_fill_manual(values = c("Black" = "#333333", "Dusky/Dark" = "#666666", "Yelloweye" = "#FFFF66",
                                   "Copper" = "#FF9900", "Quillback" = "#993300", "Other" = "#CCCCCC"),
                        name = "Species") +
      scale_color_manual(values = c("Black" = "#333333", "Dusky/Dark" = "#666666", "Yelloweye" = "#FFFF66",
                                    "Copper" = "#FF9900", "Quillback" = "#993300", "Other" = "#CCCCCC"),
                        name = "Species")  +
      labs(x = "Year", y = "Percent of rockfish catch") +
      theme_bw(base_size = 18) +
      ggtitle("GOA Rockfish Species Composition")

est_format %>%
  pivot_wider(id_cols = c("port", "year"), names_from = "spp", values_from = "est") %>%
  print(n = 150)


# Assemblage comp ------------------------------------------------------------
#Stratification likely needed.  >50% compositions (year and port combinations) differ by user group.
#use the Benjamini–Hochberg procedure to account for false positives associated with multiple tests
test_assem <- 
  n_comp %>% 
  pivot_wider(names_from = user, values_from = n) %>% 
  rowwise()
test_assem$pval <- NA

for(i in 1:dim(test_comp)[1]){
  test_assem$pval[i] <- 
    fisher.test(
      rfcomp$user[rfcomp$port == test_comp[[i, 1]] & rfcomp$year == test_comp[[i, 2]]], 
      rfcomp$assem[rfcomp$port == test_comp[[i, 1]] & rfcomp$year == test_comp[[i, 2]]], simulate.p.value = TRUE)$p.value
}
test_assem$p05 <- ifelse(test_assem$pval < 0.05, TRUE, FALSE)
test_assem$p.adjust <- p.adjust(test_assem$pval, method = "BH")
test_assem$p05a <- ifelse(test_assem$p.adjust < 0.05, TRUE, FALSE)
#Note 64 out of 117 with pval < .05
table(test_assem$p05)
table(test_assem$p05a)
plot(1:length(test_assem$pval),sort(test_assem$pval))
abline(0,1:length(test_assem$pval)/length(test_assem$pval)*.05)


#Summary stats from bootstrap
boot_p_assem <- 
  lapply(boot_out, 
         function(x){
           lapply(x$splits, function(x){
             as_tibble(x) %>%
               unnest(data) %>%
               mutate() %>%
               group_by(assem) %>% 
               summarise(cnt = n()) %>% 
               mutate(p = cnt / sum(cnt)) %>%
               select(assem, p)
           }) %>%
             Reduce(function(...) full_join(..., by='assem'), .) %>%
             pivot_longer(-assem, values_to = "p") %>%
             mutate(p = ifelse(is.na(p), 0, p)) %>%
             select(-name)}
  )

boot_stats0_assem <- 
  lapply(boot_p_assem, 
         function(x){
           x %>%
           group_by(assem) %>%
           summarise(pboot_puys = mean(p),
                     vpboot_puys = var(p))
  })

#add strata ids
boot_stats_assem <-
  Map(function(x, y){
    x$port = gsub("(.*)-(.*)-(.*)", '\\1', y);
    x$user = gsub("(.*)-(.*)-(.*)", '\\2', y);
    x$year = as.numeric(gsub("(.*)-(.*)-(.*)", '\\3', y));
    return(x)}, 
    x = boot_stats0_assem, y = strata) %>%
  do.call(rbind, .)


#add bootstrap ps to raw proportions
prop_assem <- 
  rfcomp %>%
  group_by(port, user, year, assem) %>% 
  summarise(cnt = n()) %>% 
  pivot_wider(names_from = user, values_from = cnt) %>% #next 3 lines fill missing spp
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  pivot_longer(Charter:Private, names_to = "user", values_to = "cnt") %>%
  group_by(port, user, year) %>%
  mutate (p_puys = cnt / sum(cnt),
          vp_puys = p_puys * (1 - p_puys) / sum(cnt)) %>%
  select(-cnt) 

#Merge w SWHS
assem <-
  left_join(prop_assem, SWHS_puy, by = c("port", "user", "year")) %>%
  left_join(boot_stats_assem, by = c("port", "user", "year", "assem")) %>%
  mutate(pboot_puys = ifelse(is.na(pboot_puys), 0, pboot_puys),
         vpboot_puys = ifelse(is.na(vpboot_puys) & year >=2009, 0, vpboot_puys),
         H_puys = p_puys * H_puy,
         vH_puys = p_puys^2*vH_puy + vp_puys*H_puy^2 - vp_puys*vH_puy,
         vHboot_puys = p_puys^2*vH_puy + vpboot_puys*H_puy^2 - vpboot_puys*vH_puy) %>%
  pivot_wider(names_from = user, values_from = c(p_puys, vp_puys, vH_puys,
                                                 pboot_puys, vpboot_puys, vHboot_puys,
                                                 H_puy, vH_puy, H_puys)) %>%
  left_join(SWHS_py, by = c("port", "year")) %>%
  mutate(H_pys = H_puys_Charter + H_puys_Private,
         vH_pys = vH_puys_Charter + vH_puys_Private,
         vHboot_pys = vHboot_puys_Charter + vHboot_puys_Private,
         p_pys = H_pys / H_py,
         vp_pys = (1 / H_py^2) * 
           (vH_puy_Charter * (p_puys_Charter * H_puy_Private - H_puys_Private)^2 / H_py^2 +
              vH_puy_Private * (p_puys_Private * H_puy_Charter - H_puys_Charter)^2 / H_py^2 +
              vp_puys_Charter * H_puy_Charter^2 + 
              vp_puys_Private * H_puy_Private^2),
         vpboot_pys = (1 / H_py^2) * 
           (vH_puy_Charter * (pboot_puys_Charter * H_puy_Private - H_puys_Private)^2 / H_py^2 +
              vH_puy_Private * (pboot_puys_Private * H_puy_Charter - H_puys_Charter)^2 / H_py^2 +
              vpboot_puys_Charter * H_puy_Charter^2 + 
              vpboot_puys_Private * H_puy_Private^2),
         sep_pys = sqrt(vp_pys),
         sepboot_pys = sqrt(vpboot_pys)) %>%
  select(port, year, assem, p_pys, vp_pys, vpboot_pys, sep_pys, sepboot_pys)
#saveRDS(assem, ".\\Rockfish report_96-19\\composition\\est_assem.rds")
assem <- readRDS(".\\Rockfish report_96-19\\composition\\est_assem.rds")

#Plot bootstrap and SRS SE's
assem %>% 
  ggplot(aes(x= sep_pys, y = sepboot_pys)) +
  geom_point() +
  geom_abline(slope = 1) +
  labs(x = "SRS SE", y = "bootstrap SE") +
  theme_bw(base_size = 18) +
  ggtitle("Standard errors for assemblage composition proportions")
lm(assem$sepboot_pys ~ assem$sep_pys)

#  * Format for report ----------------------------------------------------
assem_format <- 
  assem %>%
  mutate(sep_pys = sqrt(vp_pys),
         sepboot_pys = sqrt(vpboot_pys),
         sepreport_pys = ifelse(is.na(vpboot_pys), sep_pys * 2, sepboot_pys),
         est = paste0(format(round(p_pys, 3), digits = 3), "(", format(round(sepreport_pys, 3), digits = 3), ")"))

assem_format %>%
  ggplot(aes(x = year, y = p_pys, color = assem, fill = assem)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(port ~ .) +
  scale_fill_manual(values = c("Pelagic" = "#333333", "non-Pelagic" = "#666666"),
                    name = "Assemblage") +
  scale_color_manual(values = c("Pelagic" = "#333333", "non-Pelagic" = "#666666"),
                     name = "Assemblage")  +
  labs(x = "Year", y = "Percent of rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Rockfish Assemblage Composition")

assem_format %>%
  pivot_wider(id_cols = c("port", "year"), names_from = "assem", values_from = "est") %>%
  print(n = 150)




# Sex comp ---------------------------------------------------
# raw proportions
rfsex <- rfcomp[!is.na(rfcomp$sex) & rfcomp$sex != "", ]
#Note sample sizes are not great
#Low sample sizes are problematic w stratification because you could base weight some samples on very poor estimates in one user group.
table(rfsex$spp, rfsex$year, rfsex$port, rfsex$user)

#number available for composition estimates, used in report.
n_sex <- 
  rfsex %>%
  group_by(port, year, user, spp) %>%
  summarise(n = sum(sex %in% c("M", "F")))
aggregate(n ~ spp + user, n_sex[!is.na(n_sex$n), ], quantile)


#Focus on Black and Yelloweye rockfish for tests
test_sex0 <- 
  n_sex %>% 
  filter(spp %in% c("Black", "Yelloweye")) %>%
  pivot_wider(names_from = user, values_from = n) %>% 
  rowwise() 
#number of 117 combinations of port and year for which we have at least one sample 
aggregate(test_sex0, list(sex = test_sex0$spp), length)

#Not no evidence we need to stratify by used group
#0 out of 216 species, port, year pairs are significant when accounting for multiple tests
test_sex <- 
  test_sex0 %>%
  mutate(min = min(Charter, Private)) %>% 
  filter(min > 0) %>% #can't compare sex comps without samples from both users
  select(port, year, spp, min)
test_sex$pval <- NA
for(i in 1:dim(test_sex)[1]){
  test_sex$pval[i] <- 
    fisher.test(
      rfsex$user[rfsex$port == test_sex[[i, 1]] & rfsex$year == test_sex[[i, 2]] & rfsex$spp == test_sex[[i, 3]]], 
      rfsex$sex[rfsex$port == test_sex[[i, 1]] & rfsex$year == test_sex[[i, 2]] & rfsex$spp == test_sex[[i, 3]]])$p.value
}
#No multiple test adjustment
test_sex$p05 <- ifelse(test_sex$pval < 0.05, TRUE, FALSE)
test_sex$min_cut <- cut(test_sex$min, c(0, 15, 40, 1000))
#Note 17 out of 216 with pval < .0
table(test_sex$p05, test_sex$spp)
test_sex[test_sex$p05 == TRUE, ] %>% print(n = 100)
#significant tests occur at both large and small sample sizes
table(test_sex$p05, test_sex$min_cut)

#Benjamini–Hochberg multiple test adjustment
test_sex$p.adjust <- p.adjust(test_sex$pval, method = "BH")
test_sex$p05a <- ifelse(test_sex$p.adjust < 0.05, TRUE, FALSE)
#Note 0 out of 216 with pval < .05 when accounting for multiple tests. 
table(test_sex$p05a, test_sex$spp)
min(test_sex$p.adjust)
#Benjamini–Hochberg plot
plot(1:length(test_sex$pval),sort(test_sex$pval))
abline(0,1:length(test_sex$pval)/length(test_sex$pval)*.05)


#Pooled estimates for later comparison 
prop_sexpool <- 
  rfsex %>%
  group_by(port, year, spp, sex) %>% 
  summarise(cnt = n()) %>% 
  pivot_wider(names_from = sex, values_from = cnt) %>% #next 3 lines fill missing spp
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  pivot_longer(F:M, names_to = "sex", values_to = "cnt") %>%
  group_by(port, year, spp) %>%
  mutate (ppool_pysx = cnt / sum(cnt),
          vppool_pysx = ppool_pysx * (1 - ppool_pysx) / sum(cnt)) %>%
  select(-cnt) 

#Carry out the stratified and bootstrapped estimates anyway.
#Point estimates by user
prop_sex <- 
  rfsex %>%
  group_by(port, user, year, spp, sex) %>% 
  summarise(cnt = n()) %>% 
  pivot_wider(names_from = user, values_from = cnt) %>% #next 3 lines fill missing users
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  pivot_longer(Charter:Private, names_to = "user", values_to = "cnt") %>%
  group_by(port, user, year, spp) %>%
  mutate (p_puysx = cnt / sum(cnt),
          vp_puysx = p_puysx * (1 - p_puysx) / sum(cnt)) %>%
  select(-cnt) 

#Bootstrapped estimates
#Convert list columns back to dataframe within each strata
#object still a list where each element of the list is a df for one strata (port, user, year combination).
boot_p_sex <- 
  lapply(boot_out, 
         function(x){
           lapply(x$splits, function(x){
             as_tibble(x) %>%
               unnest(data) %>%
               group_by(spp, sex) %>%
               filter(sex != "" & spp %in% c("Black", "Yelloweye", "Quillback", "Dusky/Dark")) %>%
               summarise(cnt = n()) %>% 
               mutate(p = cnt / sum(cnt)) %>%
               select(spp, sex, p)
           }) %>%
             Reduce(function(...) full_join(..., by=c('spp', 'sex')), .) %>%
             pivot_longer(starts_with("p"), values_to = "p") %>%
             filter(!is.na(p)) %>%
             select(-name)}
  )

#Check bootstrap distributions 
# Map(function(x, y){
#   ggplot(x, aes(x = p)) +
#     geom_histogram() +
#     facet_grid(spp ~ sex) +
#     ggtitle(y)
#     },
#   boot_p_sex[-27],
#   strata[-27]
# )

#Summary stats from bootstrap
boot_stats0_sex <- 
  lapply(boot_p_sex[-27], 
         function(x){x %>%
             group_by(spp, sex) %>%
             summarise(pboot_puysx = mean(p),
                       vpboot_puysx = var(p))}
  )

#add strata ids
boot_stats_sex <-
  Map(function(x, y){
    x$port = as.factor(gsub("(.*)-(.*)-(.*)", '\\1', y));
    x$user = gsub("(.*)-(.*)-(.*)", '\\2', y);
    x$year = as.numeric(gsub("(.*)-(.*)-(.*)", '\\3', y));
    return(x)}, 
    x = boot_stats0_sex, y = strata[-27]) %>%
  do.call(rbind, .)


#Plot bootstrap and SRS SE's
#note: little evidence bootstrapping is required. Sex comp less subject to bias w cluster sampling 
comp_se_sex <- 
  prop_sex %>%
  left_join(boot_stats_sex, by = c("port", "year", "user", "spp", "sex")) %>%
  left_join(n_sex, by = c("port", "year", "user", "spp")) %>%
  mutate(sep_puysx = sqrt(vp_puysx),
         sepboot_puysx = sqrt(vpboot_puysx),
         n_cut = cut(n, breaks = c(0, 15, 100, 1000)))
ggplot(comp_se_sex, aes(x= sep_puysx, y = sepboot_puysx, color = n_cut)) + #[comp_se_sex$n > 5, ]
  geom_point() +
  stat_smooth(data = comp_se_sex[comp_se_sex$n > 15, ], method = "lm") +
  geom_abline(slope = 1) +
  labs(x = "SRS SE", y = "bootstrap SE") +
  theme_bw(base_size = 18) +
  ggtitle("Standard errors for sex composition proportions")
summary(lm(comp_se_sex$sepboot_puysx[comp_se_sex$n > 15] ~ comp_se_sex$sep_puysx[comp_se_sex$n > 15]))



#add bootstrap ps to raw proportions and pooled estimates
#Merge w SWHS
SWHS_puys <- 
  left_join(est %>%
              select(port, year, spp, starts_with("H_puys")) %>%
              pivot_longer(contains("H_puys"), names_to = "user", names_prefix = ".*_.*_", values_to = "H_puys"),
            est %>%
              select(port, year, spp, starts_with("vH_puys")) %>%
              pivot_longer(contains("H_puys"), names_to = "user", names_prefix = ".*_.*_", values_to = "vH_puys"),
            by = c("port", "year", "user", "spp"))
SWHS_pys <- 
  SWHS_puys %>%
    group_by(port, year, spp) %>%
    summarise(H_pys = sum(H_puys))
est_sex <-
  left_join(prop_sex, SWHS_puys, by = c("port", "user", "year", "spp")) %>%
  left_join(boot_stats_sex, by = c("port", "user", "year", "spp", "sex")) %>%
  mutate(pboot_puysx = ifelse(is.na(pboot_puysx), 0, pboot_puysx),
         vpboot_puysx = ifelse(is.na(vpboot_puysx) & year >=2009, 0, vpboot_puysx),
         H_puysx = p_puysx * H_puys,
         Hboot_puysx = pboot_puysx * H_puys,
         vH_puysx = p_puysx^2*vH_puys + vp_puysx*H_puys^2 - vp_puysx*vH_puys,
         vHboot_puysx = p_puysx^2*vH_puys + vpboot_puysx*H_puys^2 - vpboot_puysx*vH_puys) %>%
  pivot_wider(names_from = user, values_from = c(p_puysx, vp_puysx, H_puysx, vH_puysx,
                                                 pboot_puysx, vpboot_puysx, Hboot_puysx, vHboot_puysx,
                                                 H_puys, vH_puys)) %>%
  left_join(SWHS_pys, by = c("port", "year", "spp")) %>%
  mutate(H_pysx = H_puysx_Charter + H_puysx_Private,
         Hboot_pysx = Hboot_puysx_Charter + Hboot_puysx_Private,
         # vH_pysx = vH_puysx_Charter + vH_puysx_Private,
         # vHboot_pysx = vHboot_puysx_Charter + vHboot_puysx_Private,
         p_pysx = H_pysx / H_pys,
         pboot_pysx = Hboot_pysx / H_pys,
         vp_pysx = (1 / H_pys^2) * 
           (vH_puys_Charter * (p_puysx_Charter * H_puys_Private - H_puysx_Private)^2 / H_pys^2 +
              vH_puys_Private * (p_puysx_Private * H_puys_Charter - H_puysx_Charter)^2 / H_pys^2 +
              vp_puysx_Charter * H_puys_Charter^2 + 
              vp_puysx_Private * H_puys_Private^2),
         vpboot_pysx = (1 / H_pys^2) * 
           (vH_puys_Charter * (pboot_puysx_Charter * H_puys_Private - H_puysx_Private)^2 / H_pys^2 +
              vH_puys_Private * (pboot_puysx_Private * H_puys_Charter - H_puysx_Charter)^2 / H_pys^2 +
              vpboot_puysx_Charter * H_puys_Charter^2 + 
              vpboot_puysx_Private * H_puys_Private^2),
         sep_pysx = sqrt(vp_pysx),
         sepboot_pysx = sqrt(vpboot_pysx)) %>%
  left_join(prop_sexpool, by = c("port", "year", "spp", "sex"))%>%
  mutate(seppool_pysx = sqrt(vppool_pysx)) %>%
  left_join(aggregate(n ~ port+year+spp, data = n_sex, FUN = sum), by = c("port", "year", "spp")) %>%
  select(port, year, spp, sex, n, p_pysx, pboot_pysx, ppool_pysx, vp_pysx, vpboot_pysx, vppool_pysx, sep_pysx, sepboot_pysx, seppool_pysx)
#saveRDS(est_sex, ".\\Rockfish report_96-19\\composition\\est_sex.rds")
est_sex <- readRDS(".\\Rockfish report_96-19\\composition\\est_sex.rds")

#very little bias indicated by bootstrap
n_sex %>%
  group_by(port, year, spp) %>%
  summarise(n = sum(n)) %>%
  mutate(n_cut = cut(n, breaks = c(0, 50, 100, 1000))) %>%
  left_join(est_sex, by = c("port", "year", "spp")) %>%
  ggplot(aes(x = p_pysx, y = pboot_pysx, color = n_cut)) +
    geom_point() 

#Plot bootstrap and SRS SE's
est_sex %>% 
  ggplot(aes(x= sep_pysx, y = sepboot_pysx)) +
  geom_point() +
  geom_abline(slope = 1) +
  labs(x = "SRS SE", y = "bootstrap SE") +
  theme_bw(base_size = 18) +
  ggtitle("Standard errors for sex composition proportions")
lm(est_sex$sepboot_pysx ~ est_sex$sep_pysx)


#  * Format for report ----------------------------------------------------
#I don't see much bias in the se or the point estimate of p.
#but the sex comps are not different for the almost all cases, use pooled estimates wo bootstrapping.
sex_format <- 
  est_sex %>%
  mutate(p_pysx_report = ppool_pysx,
         sep_pysx_report = sqrt(vppool_pysx),
         est = ifelse(is.na(p_pysx_report),
                      NA,
                      paste0(format(round(p_pysx_report, 3), digits = 3), "(", format(round(seppool_pysx, 3), digits = 3), ")")),
         lcilogitp_pysx = log(p_pysx_report / (1-p_pysx_report)) - 1.96 * sqrt(1 / p_pysx_report^2 / (1-p_pysx_report)^2 * sep_pysx_report^2),
         ucilogitp_pysx = log(p_pysx_report / (1-p_pysx_report)) + 1.96 * sqrt(1 / p_pysx_report^2 / (1-p_pysx_report)^2 * sep_pysx_report^2),
         lcip_pysx = exp(lcilogitp_pysx)/(1 + exp(lcilogitp_pysx)),
         ucip_pysx = exp(ucilogitp_pysx)/(1 + exp(ucilogitp_pysx)))


sex_format %>%
  filter(spp == "Black", sex == "F") %>%
  ggplot(aes(x = year, y = p_pysx_report, color = sex, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lcip_pysx, ymax = ucip_pysx), color = "black") +
  facet_grid(port ~ .) +
  scale_fill_manual(values = c("F" = "light gray"), guide = FALSE) +
  scale_color_manual(values = c("F" = "light gray"), guide = FALSE)  +
  labs(x = "Year", y = "Percent of black rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Black Rockfish Sex Composition")

sex_format %>%
  filter(spp == "Yelloweye", sex == "F") %>%
  ggplot(aes(x = year, y = p_pysx_report, color = sex, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lcip_pysx, ymax = ucip_pysx), color = "black") +
  facet_grid(port ~ .) +
  scale_fill_manual(values = c("F" = "light gray"), guide = FALSE) +
  scale_color_manual(values = c("F" = "light gray"), guide = FALSE)  +
  labs(x = "Year", y = "Percent of yelloweye rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Yelloweye Rockfish Sex Composition")

sex_format %>%
  filter(spp == "Dusky/Dark", sex == "F") %>%
  ggplot(aes(x = year, y = p_pysx_report, color = sex, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lcip_pysx, ymax = ucip_pysx), color = "black") +
  facet_grid(port ~ .) +
  scale_fill_manual(values = c("F" = "light gray"), guide = FALSE) +
  scale_color_manual(values = c("F" = "light gray"), guide = FALSE)  +
  labs(x = "Year", y = "Percent of dusky/dark rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Dusky/Dark Rockfish Sex Composition")

sex_format %>%
  filter(spp == "Copper", sex == "F") %>%
  ggplot(aes(x = year, y = p_pysx_report, color = sex, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lcip_pysx, ymax = ucip_pysx), color = "black") +
  facet_grid(port ~ .) +
  scale_fill_manual(values = c("F" = "light gray"), guide = FALSE) +
  scale_color_manual(values = c("F" = "light gray"), guide = FALSE)  +
  labs(x = "Year", y = "Percent of copper rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Copper Rockfish Sex Composition")

sex_format %>%
  filter(spp == "Quillback", sex == "F") %>%
  ggplot(aes(x = year, y = p_pysx_report, color = sex, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lcip_pysx, ymax = ucip_pysx), color = "black") +
  facet_grid(port ~ .) +
  scale_fill_manual(values = c("F" = "light gray"), guide = FALSE) +
  scale_color_manual(values = c("F" = "light gray"), guide = FALSE)  +
  labs(x = "Year", y = "Percent of quillback rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Quillback Rockfish Sex Composition")

sex_format %>%
  filter(sex == "F" & spp == "Yelloweye") %>%
  pivot_wider(id_cols = "year", names_from = "port", values_from = "est")




# Age comp ---------------------------------------------------
# raw proportions
rfage <- rfcomp[!is.na(rfcomp$age) & rfcomp$age != "", ]
#Note sample sizes are not great
#Low sample sizes are problematic w stratification because you could base weight some samples on very poor estimates in one user group.
table(rfage$spp, rfage$year, rfage$user, rfage$port)

#number available for composition estimates, used in report.
n_age <- 
  rfage %>%
  #filter(spp %in% c("Black", "Yelloweye")) %>%
  group_by(port, year, user, spp) %>%
  summarise(n = sum(!is.na(age)))
aggregate(n ~ spp + user, n_age[!is.na(n_age$n), ], quantile)

#Evidence that stratification needed for at least some of the age comps
n_age %>% 
  filter(spp %in% c("Black", "Yelloweye")) %>%
  pivot_wider(names_from = user, values_from = n) %>% 
  rowwise() %>%
  mutate(min = min(Charter, Private)) %>% 
  filter(is.na(min))

test_age0 <- 
  n_age %>% 
  filter(spp %in% c("Black", "Yelloweye")) %>%
  pivot_wider(names_from = user, values_from = n) %>% 
  rowwise() 
aggregate(test_age0, list(sex = test_age0$spp), length)

test_age <- 
  test_age0 %>%
  mutate(min = min(Charter, Private)) %>% 
  filter(min > 0) #can't compare age comps without samples from both users
test_age$pval <- NA
for(i in 1:dim(test_age)[1]){
  test_age$pval[i] <- 
    fisher.test(
      rfage$user[rfage$port == test_age[[i, 1]] & rfage$year == test_age[[i, 2]] & rfage$spp == test_age[[i, 3]]], 
      rfage$age[rfage$port == test_age[[i, 1]] & rfage$year == test_age[[i, 2]] & rfage$spp == test_age[[i, 3]]], 
      simulate.p.value = TRUE,
      B = 5000)$p.value
}
#no multiple comparison adjustment
test_age$p05 <- ifelse(test_age$pval < 0.05, TRUE, FALSE)
test_age$min_cut <- cut(test_age$min, c(0, 15, 50, 1000))
#56 of 219 are significant
table(test_age$p05)
table(test_age$p05, test_age$spp)
#many significant test for combinations with significant sample sizes
table(test_age$p05, test_age$min_cut)

#Corrected for multiple comparisons
test_age$p.adjust <- p.adjust(test_age$pval, method = "BH")
test_age$p05a <- ifelse(test_age$p.adjust < 0.05, TRUE, FALSE)
#still 47 of 219 are significant when accounting for multiple comparisons
table(test_age$p05a)
table(test_age$p05a, test_age$spp)
#many significant test for combinations with significant sample sizes
table(test_age$p05a, test_age$min_cut)
test_age[test_age$p05a == TRUE, ] %>% arrange(min)
#BH plot
plot(1:length(test_age$pval),sort(test_age$pval))
abline(0,1:length(test_age$pval)/length(test_age$pval)*.05)


#Pooled estimates for later comparison 
prop_agepool <- 
  rfage %>%
  group_by(port, year, spp, age) %>% 
  summarise(cnt = n()) %>% 
  pivot_wider(names_from = age, values_from = cnt) %>% #next 3 lines fill missing spp
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  pivot_longer(-c(port, year, spp), names_to = "age", values_to = "cnt") %>%
  group_by(port, year, spp) %>%
  mutate (age = as.numeric(age), 
          ppool_pysa = cnt / sum(cnt),
          vppool_pysa = ppool_pysa * (1 - ppool_pysa) / sum(cnt)) %>%
  select(-cnt) 


#Stratified and bootstrapped estimates.
#Point estimates by user
prop_age <- 
  rfage %>%
  group_by(port, user, year, spp, age) %>% 
  summarise(cnt = n()) %>% 
  pivot_wider(names_from = user, values_from = cnt) %>% #next 3 lines fill missing users
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  pivot_longer(Charter:Private, names_to = "user", values_to = "cnt") %>%
  group_by(port, user, year, spp) %>%
  mutate (p_puysa = cnt / sum(cnt),
          vp_puysa = p_puysa * (1 - p_puysa) / sum(cnt)) %>%
  select(-cnt) 

#Bootstrapped estimates
#Convert list columns back to dataframe within each strata
#object still a list where each element of the list is a df for one strata (port, user, year combination).
boot_p_age <- 
  lapply(boot_out, 
         function(x){
           lapply(x$splits, function(x){
             as_tibble(x) %>%
               unnest(data) %>%
               group_by(spp, age) %>%
               filter(age != "" & spp %in% c("Black", "Yelloweye")) %>%
               summarise(cnt = n()) %>% 
               mutate(p = cnt / sum(cnt)) %>%
               select(spp, age, p)
           }) %>%
             Reduce(function(...) full_join(..., by=c('spp', 'age')), .) %>%
             pivot_longer(starts_with("p"), values_to = "p") %>%
             filter(!is.na(p)) %>%
             select(-name)}
  )

#Check bootstrap distributions 
# Map(function(x, y){
#   ggplot(x, aes(x = p)) +
#     geom_histogram() +
#     facet_grid(spp ~ age) +
#     ggtitle(y)
#     },
#   boot_p_age,
#   strata
# )

#Summary stats from bootstrap
boot_stats0_age <- 
  lapply(boot_p_age, 
         function(x){x %>%
             group_by(spp, age) %>%
             summarise(pboot_puysa = mean(p),
                       vpboot_puysa = var(p))}
  )

#add strata ids
boot_stats_age <-
  Map(function(x, y){
    x$port = as.factor(gsub("(.*)-(.*)-(.*)", '\\1', y));
    x$user = gsub("(.*)-(.*)-(.*)", '\\2', y);
    x$year = as.numeric(gsub("(.*)-(.*)-(.*)", '\\3', y));
    return(x)}, 
    x = boot_stats0_age, y = strata) %>%
  do.call(rbind, .)


#Plot bootstrap and SRS SE's
#note: little evidence bootsrapping is required. Age comp less subject to bias w cluser sampling 
comp_se_age <- 
  prop_age %>%
  left_join(boot_stats_age, by = c("port", "year", "user", "spp", "age")) %>%
  left_join(n_age[n_age$spp %in% c("Black", "Yelloweye"), ], by = c("port", "year", "user", "spp")) %>%
  mutate(sep_puysa = sqrt(vp_puysa),
         sepboot_puysa = sqrt(vpboot_puysa),
         n_cut = cut(n, breaks = c(0, 15, 1000)))
ggplot(comp_se_age, aes(x= sep_puysa, y = sepboot_puysa, color = n_cut)) +
  geom_point() +
  stat_smooth(data = comp_se_age, method = "lm") +
  geom_abline(slope = 1) +
  labs(x = "SRS SE", y = "bootstrap SE") +
  theme_bw(base_size = 18) +
  ggtitle("Standard errors for age composition proportions")
summary(lm(comp_se_age$sepboot_puysa[comp_se_age$n > 15] ~ comp_se_age$sep_puysa[comp_se_age$n > 15]))



#add bootstrap ps to raw proportions and pooled estiamtes
#Merge w SWHS
est_age <-
  left_join(prop_age[prop_age$spp %in% c("Black", "Yelloweye"), ], SWHS_puys, by = c("port", "user", "year", "spp")) %>%
  left_join(boot_stats_age, by = c("port", "user", "year", "spp", "age")) %>%
  mutate(pboot_puysa = ifelse(is.na(pboot_puysa), 0, pboot_puysa),
         vpboot_puysa = ifelse(is.na(vpboot_puysa) & year >=2009, 0, vpboot_puysa),
         H_puysa = p_puysa * H_puys,
         Hboot_puysa = pboot_puysa * H_puys,
         vH_puysa = p_puysa^2*vH_puys + vp_puysa*H_puys^2 - vp_puysa*vH_puys,
         vHboot_puysa = p_puysa^2*vH_puys + vpboot_puysa*H_puys^2 - vpboot_puysa*vH_puys) %>%
  pivot_wider(names_from = user, values_from = c(p_puysa, vp_puysa, H_puysa, vH_puysa,
                                                 pboot_puysa, vpboot_puysa, Hboot_puysa, vHboot_puysa,
                                                 H_puys, vH_puys)) %>%
  left_join(SWHS_pys, by = c("port", "year", "spp")) %>%
  mutate(H_pysa = H_puysa_Charter + H_puysa_Private,
         Hboot_pysa = Hboot_puysa_Charter + Hboot_puysa_Private,
         # vH_pysa = vH_puysa_Charter + vH_puysa_Private,
         # vHboot_pysa = vHboot_puysa_Charter + vHboot_puysa_Private,
         p_pysa = H_pysa / H_pys,
         pboot_pysa = Hboot_pysa / H_pys,
         vp_pysa = (1 / H_pys^2) * 
           (vH_puys_Charter * (p_puysa_Charter * H_puys_Private - H_puysa_Private)^2 / H_pys^2 +
              vH_puys_Private * (p_puysa_Private * H_puys_Charter - H_puysa_Charter)^2 / H_pys^2 +
              vp_puysa_Charter * H_puys_Charter^2 + 
              vp_puysa_Private * H_puys_Private^2),
         vpboot_pysa = (1 / H_pys^2) * 
           (vH_puys_Charter * (pboot_puysa_Charter * H_puys_Private - H_puysa_Private)^2 / H_pys^2 +
              vH_puys_Private * (pboot_puysa_Private * H_puys_Charter - H_puysa_Charter)^2 / H_pys^2 +
              vpboot_puysa_Charter * H_puys_Charter^2 + 
              vpboot_puysa_Private * H_puys_Private^2),
         sep_pysa = sqrt(vp_pysa),
         sepboot_pysa = sqrt(vpboot_pysa)) %>%
  left_join(prop_agepool, by = c("port", "year", "spp", "age")) %>%
  mutate(seppool_pysa = sqrt(vppool_pysa)) %>%
  left_join(test_age, by = c("port", "year", "spp")) %>%
  left_join(aggregate(n ~ port+year+spp, data = n_age[n_age$spp %in% c("Black", "Yelloweye"), ], FUN = sum), by = c("port", "year", "spp")) %>%
  select(port, year, spp, age, n, p_pysa, pboot_pysa, ppool_pysa, vp_pysa, vpboot_pysa, vppool_pysa, sep_pysa, sepboot_pysa, seppool_pysa, p05a, min)
#saveRDS(est_age, ".\\Rockfish report_96-19\\spp comp\\est_age.rds")
est_age <- readRDS(".\\Rockfish report_96-19\\composition\\est_age.rds")


#bias indicated by bootstrap might be sample size related
n_age[n_age$spp %in% c("Black", "Yelloweye"), ] %>%
  group_by(port, year, spp) %>%
  summarise(n = sum(n)) %>%
  mutate(n_cut = cut(n, breaks = c(0, 50, 100, 1000))) %>%
  left_join(est_age, by = c("port", "year", "spp")) %>%
  ggplot(aes(x = p_pysa, y = pboot_pysa, color = n_cut)) +
  geom_point() 

#Plot bootstrap and SRS SE's
est_age %>% 
  ggplot(aes(x= sep_pysa, y = sepboot_pysa)) +
  geom_point() +
  geom_abline(slope = 1) +
  labs(x = "SRS SE", y = "bootstrap SE") +
  theme_bw(base_size = 18) +
  ggtitle("Standard errors for species composition proportions")
lm(est_age$sepboot_pysa ~ est_age$sep_pysa)

#Bias often associated with significant differences between guided and private samples but not always
est_age %>%
  group_by(port, year, spp) %>%
  filter(p_pysa == max(p_pysa)) %>%
  ggplot(aes(p_pysa, ppool_pysa, color = p05a)) +
  geom_point()
#note ~ 1/4 > +/- 15%
est_age %>%
  mutate(diff = ppool_pysa/p_pysa) %>%
  filter(p05a == TRUE) %>%
  group_by(port, year, spp) %>%
  filter(p_pysa == max(p_pysa)) %>%
  arrange(-diff) %>%
  print(n = 90)

#Bias associated with small samples sizes per strata larger than bias from ignoring strata.
plotly::plot_ly(x = est_age$p_pysa, y = est_age$ppool_pysa, z = est_age$min, 
                type = "scatter3d", 
                mode = "markers", 
                color = est_age$p05a,
                text = ~paste(est_age$port, "-", est_age$year, "-", est_age$spp))

#Take a look at some of the most egregious cases.
#Whittier-2016-Yelloweye
#Largest bias below 1:1 line
#Only 4 private samples
est_age[est_age$port == "Whittier" & est_age$year == 2016 & est_age$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Whittier" & prop_age$year == 2016 & prop_age$spp == "Yelloweye", ] %>% arrange(user, age) %>% print(n = 100)
n_age[n_age$port == "Whittier" & n_age$year == 2016 & n_age$spp == "Yelloweye", ]

#Whittier-2015-Black
#One of the largest bias with sig age comps below 1:1 line
#27 private samples vrs 359 Charter samples
#I'm OK with stratifying here
est_age[est_age$port == "Whittier" & est_age$year == 2015 & est_age$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Whittier" & prop_age$year == 2015 & prop_age$spp == "Black", ] %>% arrange(user, age) %>% print(n = 100)
n_age[n_age$port == "Whittier" & n_age$year == 2015 & n_age$spp == "Black", ]
SWHS_puys[SWHS_puys$port == "Whittier" & SWHS_puys$year == 2015 & SWHS_puys$spp == "Black", ]

#Valdez-2011-Yelloweye
#Largest bias below the 1:1 line w a significant test
#>50 samples in both user groups but much more private harvest
#stratified preferable here
est_age[est_age$port == "Valdez" & est_age$year == 2011 & est_age$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Valdez" & prop_age$year == 2011 & prop_age$spp == "Yelloweye", ] %>% arrange(user, age) %>% print(n = 100)
n_age[n_age$port == "Valdez" & n_age$year == 2011 & n_age$spp == "Yelloweye", ]
SWHS_puys[SWHS_puys$port == "Valdez" & SWHS_puys$year == 2011 & SWHS_puys$spp == "Yelloweye", ]

#Kodiak-1997-Black
#Large bias above 1:1 line a significant test
#~35 samples in both user groups but much more private harvest
#I'm Ok with this stratified but questionable
est_age[est_age$port == "Kodiak" & est_age$year == 1997 & est_age$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Kodiak" & prop_age$year == 1997 & prop_age$spp == "Black", ] %>% arrange(user, age) %>% print(n = 46)
n_age[n_age$port == "Kodiak" & n_age$year == 1997 & n_age$spp == "Black", ]
SWHS_puys[SWHS_puys$port == "Kodiak" & SWHS_puys$year == 1997 & SWHS_puys$spp == "Black", ]

#Cook Inlet-2000-Black
#Large bias above 1:1 line a significant test
#samples skewed towards Charter harvest
#I'm Ok with this stratified but questionable
est_age[est_age$port == "Cook Inlet" & est_age$year == 2000 & est_age$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Cook Inlet" & prop_age$year == 2000 & prop_age$spp == "Black", ] %>% arrange(user, age) %>% print(n = 46)
n_age[n_age$port == "Cook Inlet" & n_age$year == 2000 & n_age$spp == "Black", ]
SWHS_puys[SWHS_puys$port == "Cook Inlet" & SWHS_puys$year == 2000 & SWHS_puys$spp == "Black", ]

#Cook Inlet-1998-Black
#Large bias above 1:1 line a significant test
#samples skewed towards Charter harvest
#Similar to the prior 2 samples small but skewed charter while harvest is skewed private
est_age[est_age$port == "Cook Inlet" & est_age$year == 1998 & est_age$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Cook Inlet" & prop_age$year == 1998 & prop_age$spp == "Black", ] %>% arrange(user, age) %>% print(n = 46)
n_age[n_age$port == "Cook Inlet" & n_age$year == 1998 & n_age$spp == "Black", ]
SWHS_puys[SWHS_puys$port == "Cook Inlet" & SWHS_puys$year == 1998 & SWHS_puys$spp == "Black", ]

#### Is there a sample size where we have a fairly good sample w so many categories
#suggest 50 is decent
#average ages per port, and year
rfage %>% 
  group_by(port, year, spp) %>% 
  filter(spp %in% c("Black", "Yelloweye")) %>% 
  summarise(n = unique(age)) %>%  #Unique ages per port, year, spp
  summarise(n = length(n)) %>% # # of Unique ages per port, year, spp 
  group_by(spp) %>%
  summarise(n = mean(n)) #Mean of unique ages by spp

#Thompson 87 eq 1
#Can archive around +-0.1 at alpha .1 with ~ 23 samples for the number of categories we are seeing 
m <- 20:40
alpha <- .1
d <- 0.11
z <- qnorm(alpha/2/m)
n <- z^2/m*(1-1/m)/d^2
data.frame(m, n)

#the largest age class averages 13% of yelloweye and 24 % for black
est_age %>% 
  group_by(port, year, spp) %>% 
  summarise(max = max(p_pysa, na.rm = TRUE)) %>% 
  group_by(spp) %>% summarise(median = median(max))


#80+ percent chance of seeing most samples in largest age class when you sample 50 fish
#60+ percent chance of seeing most samples in largest age class when you sample 30 fish 
#and there are 30 ages
#and the largest age class is 15 % of the sample
sapply(seq(20, 50, 10), function(y) mean(rmultinom(10000, y, c(.15, rep(.85/29, 29))) %>% apply(2, function(x)x[1] > max(x[2:30]))))
sapply(seq(20, 50, 10), function(y) mean(rmultinom(10000, y, c(.15, .15, rep(.7/28, 28))) %>% apply(2, function(x) min(x[1], x[2]) > max(x[3:30]))))

#small samples by port and spp
temp <- 
  n_age %>% 
  pivot_wider(id_cols = c("port", "year", "spp"), values_from = "n", names_from = "user") %>% 
  mutate(small = Charter + Private < 30)
table(temp$port, temp$spp, temp$small)
#None of the n<30 samples are significant
test_age %>% 
  filter(Charter + Private < 30) %>%
  print( n = 100)
# 9 of the significant samples have n < 30 for on user group
test_age %>% 
  filter((Charter < 30 | Private <30) & p05a == TRUE) %>%
  print( n = 100)
#Cook Inlet-2008-Black --- OK
est_age[est_age$port == "Cook Inlet" & est_age$year == 2008 & est_age$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Cook Inlet" & prop_age$year == 2008 & prop_age$spp == "Black", ] %>% arrange(user, age) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Cook Inlet" & SWHS_puys$year == 2008 & SWHS_puys$spp == "Black", ]
###### Cook Inlet-2011-Yelloweye --- Sample size ridiculous but very small harvest to match
est_age[est_age$port == "Cook Inlet" & est_age$year == 2011 & est_age$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Cook Inlet" & prop_age$year == 2011 & prop_age$spp == "Yelloweye", ] %>% arrange(user, age) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Cook Inlet" & SWHS_puys$year == 2011 & SWHS_puys$spp == "Yelloweye", ]
#Seward-1998-Yelloweye
est_age[est_age$port == "Seward" & est_age$year == 1998 & est_age$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Seward" & prop_age$year == 1998 & prop_age$spp == "Yelloweye", ] %>% arrange(user, age) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Seward" & SWHS_puys$year == 1998 & SWHS_puys$spp == "Yelloweye", ]
#Valdez-2007-Black
est_age[est_age$port == "Valdez" & est_age$year == 2007 & est_age$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Valdez" & prop_age$year == 2007 & prop_age$spp == "Black", ] %>% arrange(user, age) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Valdez" & SWHS_puys$year == 2007 & SWHS_puys$spp == "Black", ]
#Valdez-2007-Yelloweye
est_age[est_age$port == "Valdez" & est_age$year == 2007 & est_age$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Valdez" & prop_age$year == 2007 & prop_age$spp == "Yelloweye", ] %>% arrange(user, age) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Valdez" & SWHS_puys$year == 2007 & SWHS_puys$spp == "Yelloweye", ]
#Valdez-2008-Yelloweye
est_age[est_age$port == "Valdez" & est_age$year == 2008 & est_age$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Valdez" & prop_age$year == 2008 & prop_age$spp == "Yelloweye", ] %>% arrange(user, age) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Valdez" & SWHS_puys$year == 2008 & SWHS_puys$spp == "Yelloweye", ]
#Whittier-2015-Black
est_age[est_age$port == "Whittier" & est_age$year == 2015 & est_age$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysa:ppool_pysa, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = age, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_age[prop_age$port == "Whittier" & prop_age$year == 2015 & prop_age$spp == "Black", ] %>% arrange(user, age) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Whittier" & SWHS_puys$year == 2015 & SWHS_puys$spp == "Black", ]


#  * Format for report ----------------------------------------------------
#Tricky, I dont think we can do a blanket method
#Use pooled everywhere (included cases with no samples from one user group and thus no test) but when bonferroni is sig
#This seems like a good compromise BC all of the very small sample size sig differences are eliminated (min = 20)
table(test_age$min, test_age$p05a)
age_format <- 
  est_age %>%
  rowwise() %>%
  filter(spp %in% c("Black", "Yelloweye")) %>%
  mutate(p_pysa_report = ifelse(p05a == FALSE | is.na(p05a) | min < 20, ppool_pysa, p_pysa),
         sep_pysa_report = ifelse(p05a == FALSE | is.na(p05a) | min < 20, seppool_pysa, sep_pysa),
         est = ifelse(is.na(p_pysa_report),
                      NA,
                      paste0(format(round(p_pysa_report, 3), digits = 3), "(", format(round(sep_pysa_report, 3), digits = 3), ")")),
         lcilogitp_pysa = log(p_pysa_report / (1-p_pysa_report)) - 1.96 * sqrt(1 / p_pysa_report^2 / (1-p_pysa_report)^2 * sep_pysa_report^2),
         ucilogitp_pysa = log(p_pysa_report / (1-p_pysa_report)) + 1.96 * sqrt(1 / p_pysa_report^2 / (1-p_pysa_report)^2 * sep_pysa_report^2),
         lcip_pysa = exp(lcilogitp_pysa)/(1 + exp(lcilogitp_pysa)),
         ucip_pysa = exp(ucilogitp_pysa)/(1 + exp(ucilogitp_pysa)),
         est_ci = ifelse(is.na(p_pysa_report),
                      NA,
                      paste0(format(round(p_pysa_report, 3), digits = 3), 
                             "(", 
                             format(round(lcip_pysa, 3), digits = 3), 
                             "-",
                             format(round(ucip_pysa, 3), digits = 3),
                             ")")))


scale_sec_black <- max(age_format$n[age_format$spp == "Black"]) / max(age_format$age[age_format$spp == "Black"])
age_format[age_format$spp == "Black", ] %>%
  ggplot(aes(x = year, y = age), fill == "grey") +
  geom_point(aes(size = p_pysa_report), shape = 1) +
  geom_line(aes(y = n/scale_sec_black), color = "gray") +
  facet_wrap(~ port, 2, 3) +
  scale_y_continuous(name = "Age", breaks = seq(0, 60, 10), 
                     sec.axis = sec_axis(~. * scale_sec_black, name = "Sample Size")) +
  scale_x_continuous(name = "Year", breaks = seq(1996, 2019, 5)) +
  scale_size_area(name = "Percent", breaks = c(0, .04, .08, .12, .16, .24, .36, .48, .60)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")
scale_sec_yelloweye <- max(age_format$n[age_format$spp == "Yelloweye"]) / max(age_format$age[age_format$spp == "Yelloweye"])
age_format[age_format$spp == "Yelloweye" & age_format$port != "Kodiak", ] %>%
  ggplot(aes(x = year, y = age), fill == "grey") +
  geom_point(aes(size = p_pysa_report), shape = 1) +
  geom_line(aes(y = n/scale_sec_yelloweye), color = "gray") +
  facet_wrap(~ port, 2, 2) +
  scale_y_continuous(name = "Age", breaks = seq(0, 110, 10), 
                     sec.axis = sec_axis(~. * scale_sec_yelloweye, name = "Sample Size")) +
  scale_x_continuous(name = "Year", breaks = seq(1996, 2019, 5)) +
  scale_size_area(name = "Percent", breaks = c(0, .04, .08, .12, .16, .24, .36, .48, .60)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")




age_format %>%
  filter(spp == "Black") %>%
  pivot_wider(id_cols = c("year", "age"), names_from = "port", values_from = "est_ci") %>%
  arrange(year, age) %>% print(n = 90)
age_format %>%
  filter(spp == "Yelloweye") %>%
  arrange(year, age, port) %>%
  pivot_wider(id_cols = c("year", "age"), names_from = "port", values_from = "est_ci")





# length comp ---------------------------------------------------
# raw proportions
aggregate(length ~ spp, data = rfcomp, FUN = range, na.rm = TRUE)
rflgbin <- 
  rfcomp[!is.na(rfcomp$length) & rfcomp$length != 0, ] %>%
  mutate(lgbin = cut(length, seq(20, 90, 5)))
#Note sample sizes are not great
#Low sample sizes are problematic w stratification because you could weight some samples on very poor estimates in one user group.
table(rflgbin$spp, rflgbin$year, rflgbin$user, rflgbin$port)

#numbers used in report
n_lgbin <- 
  rflgbin %>%
  #filter(spp %in% c("Black", "Yelloweye")) %>%
  group_by(port, year, user, spp) %>%
  summarise(n = sum(!is.na(lgbin)))
aggregate(n ~ spp + user, n_lgbin[!is.na(n_lgbin$n), ], quantile)

#Evidence that stratification needed for at least some of the lgbin comps
test_lgbin0 <- 
  n_lgbin %>% 
  filter(spp %in% c("Black", "Yelloweye")) %>%
  pivot_wider(names_from = user, values_from = n) %>% 
  rowwise()
aggregate(test_lgbin0, list(test_lgbin0$spp), length)

test_lgbin <-
  test_lgbin0 %>%
  mutate(min = min(Charter, Private)) %>% 
  filter(min > 0) #can't comare lgbin comps without samples from both users
test_lgbin$pval <- NA
for(i in 1:dim(test_lgbin)[1]){
  test_lgbin$pval[i] <- 
    fisher.test(
      rflgbin$user[rflgbin$port == test_lgbin[[i, 1]] & rflgbin$year == test_lgbin[[i, 2]] & rflgbin$spp == test_lgbin[[i, 3]]], 
      rflgbin$lgbin[rflgbin$port == test_lgbin[[i, 1]] & rflgbin$year == test_lgbin[[i, 2]] & rflgbin$spp == test_lgbin[[i, 3]]], 
      simulate.p.value = TRUE,
      B = 5000)$p.value
}
#adjusted
test_lgbin$p05 <- ifelse(test_lgbin$pval < 0.05, TRUE, FALSE)
test_lgbin$min_cut <- cut(test_lgbin$min, c(0, 20, 40, 1000))
#Note 107 out of 216 with pval < .05
table(test_lgbin$p05)

#adjusted
test_lgbin$p.adjust <- p.adjust(test_lgbin$pval, method = "BH")
test_lgbin$p05a <- ifelse(test_lgbin$p.adjust < 0.05, TRUE, FALSE)
#Note 83 out of 216 with pval < .05
table(test_lgbin$p05a)
table(test_lgbin$p05a, test_lgbin$min_cut)
test_lgbin[test_lgbin$p05a == TRUE, ] %>% arrange(min)
#BH plot
plot(1:length(test_lgbin$pval),sort(test_lgbin$pval))
abline(0,1:length(test_lgbin$pval)/length(test_lgbin$pval)*.05)

#Pooled estimates for later comparison 
prop_lgbinpool <- 
  rflgbin %>%
  group_by(port, year, spp, lgbin) %>% 
  summarise(cnt = n()) %>% 
  pivot_wider(names_from = lgbin, values_from = cnt) %>% #next 3 lines fill missing spp
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  pivot_longer(-c(port, year, spp), names_to = "lgbin", values_to = "cnt") %>%
  group_by(port, year, spp) %>%
  mutate (lgbin = as.factor(lgbin), 
          ppool_pysl = cnt / sum(cnt),
          vppool_pysl = ppool_pysl * (1 - ppool_pysl) / sum(cnt)) %>%
  select(-cnt) 


#Stratified and bootstrapped estimates.
#Point estimates by user
prop_lgbin <- 
  rflgbin %>%
  group_by(port, user, year, spp, lgbin) %>% 
  summarise(cnt = n()) %>% 
  pivot_wider(names_from = user, values_from = cnt) %>% #next 3 lines fill missing users
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  pivot_longer(Charter:Private, names_to = "user", values_to = "cnt") %>%
  group_by(port, user, year, spp) %>%
  mutate (p_puysl = cnt / sum(cnt),
          vp_puysl = p_puysl * (1 - p_puysl) / sum(cnt)) %>%
  select(-cnt) 

#Bootstrapped estimates
#Convert list columns back to dataframe within each strata
#object still a list where each element of the list is a df for one strata (port, user, year combination).
boot_p_lgbin <- 
  lapply(boot_out, 
         function(x){
           lapply(x$splits, function(x){
             as_tibble(x) %>%
               unnest(data) %>%
               mutate(lgbin = cut(length, seq(20, 90, 5))) %>%
               group_by(spp, lgbin) %>%
               filter(lgbin != "" & spp %in% c("Black", "Yelloweye")) %>%
               summarise(cnt = n()) %>% 
               mutate(p = cnt / sum(cnt)) %>%
               select(spp, lgbin, p)
           }) %>%
             Reduce(function(...) full_join(..., by=c('spp', 'lgbin')), .) %>%
             pivot_longer(starts_with("p"), values_to = "p") %>%
             filter(!is.na(p)) %>%
             select(-name)}
  )

#Check bootstrap distributions 
library(ggplot2)
# Map(function(x, y){
#   ggplot(x, aes(x = p)) +
#     geom_histogram() +
#     facet_grid(spp ~ lgbin) +
#     ggtitle(y)
#     },
#   boot_p_lgbin,
#   strata
# )

#Summary stats from bootstrap
boot_stats0_lgbin <- 
  lapply(boot_p_lgbin, 
         function(x){x %>%
             group_by(spp, lgbin) %>%
             summarise(pboot_puysl = mean(p),
                       vpboot_puysl = var(p))}
  )

#add strata ids
boot_stats_lgbin <-
  Map(function(x, y){
    x$port = as.factor(gsub("(.*)-(.*)-(.*)", '\\1', y));
    x$user = gsub("(.*)-(.*)-(.*)", '\\2', y);
    x$year = as.numeric(gsub("(.*)-(.*)-(.*)", '\\3', y));
    return(x)}, 
    x = boot_stats0_lgbin, y = strata) %>%
  do.call(rbind, .)


#Plot bootstrap and SRS SE's
#note: little evidence bootsrapping is required. lgbin comp less subject to bias w cluser sampling 
comp_se_lgbin <- 
  prop_lgbin %>%
  left_join(boot_stats_lgbin, by = c("port", "year", "user", "spp", "lgbin")) %>%
  left_join(n_lgbin[n_lgbin$spp %in% c("Black", "Yelloweye"), ], by = c("port", "year", "user", "spp")) %>%
  mutate(sep_puysl = sqrt(vp_puysl),
         sepboot_puysl = sqrt(vpboot_puysl),
         n_cut = cut(n, breaks = c(0, 15, 1000)))
ggplot(comp_se_lgbin, aes(x= sep_puysl, y = sepboot_puysl, color = n_cut)) +
  geom_point() +
  stat_smooth(data = comp_se_lgbin, method = "lm") +
  geom_abline(slope = 1) +
  labs(x = "SRS SE", y = "bootstrap SE") +
  theme_bw(base_size = 18) +
  ggtitle("Standard errors for lgbin composition proportions")
summary(lm(comp_se_lgbin$sepboot_puysl ~ comp_se_lgbin$sep_puysl))



#add bootstrap ps to raw proportions and pooled estiamtes
#Merge w SWHS
est_lgbin <-
  left_join(prop_lgbin[prop_lgbin$spp %in% c("Black", "Yelloweye"), ], SWHS_puys, by = c("port", "user", "year", "spp")) %>%
  left_join(boot_stats_lgbin, by = c("port", "user", "year", "spp", "lgbin")) %>%
  mutate(pboot_puysl = ifelse(is.na(pboot_puysl), 0, pboot_puysl),
         vpboot_puysl = ifelse(is.na(vpboot_puysl) & year >=2009, 0, vpboot_puysl),
         H_puysl = p_puysl * H_puys,
         Hboot_puysl = pboot_puysl * H_puys,
         vH_puysl = p_puysl^2*vH_puys + vp_puysl*H_puys^2 - vp_puysl*vH_puys,
         vHboot_puysl = p_puysl^2*vH_puys + vpboot_puysl*H_puys^2 - vpboot_puysl*vH_puys) %>%
  pivot_wider(names_from = user, values_from = c(p_puysl, vp_puysl, H_puysl, vH_puysl,
                                                 pboot_puysl, vpboot_puysl, Hboot_puysl, vHboot_puysl,
                                                 H_puys, vH_puys)) %>%
  left_join(SWHS_pys, by = c("port", "year", "spp")) %>%
  mutate(H_pysl = H_puysl_Charter + H_puysl_Private,
         Hboot_pysl = Hboot_puysl_Charter + Hboot_puysl_Private,
         # vH_pysl = vH_puysl_Charter + vH_puysl_Private,
         # vHboot_pysl = vHboot_puysl_Charter + vHboot_puysl_Private,
         p_pysl = H_pysl / H_pys,
         pboot_pysl = Hboot_pysl / H_pys,
         vp_pysl = (1 / H_pys^2) * 
           (vH_puys_Charter * (p_puysl_Charter * H_puys_Private - H_puysl_Private)^2 / H_pys^2 +
              vH_puys_Private * (p_puysl_Private * H_puys_Charter - H_puysl_Charter)^2 / H_pys^2 +
              vp_puysl_Charter * H_puys_Charter^2 + 
              vp_puysl_Private * H_puys_Private^2),
         vpboot_pysl = (1 / H_pys^2) * 
           (vH_puys_Charter * (pboot_puysl_Charter * H_puys_Private - H_puysl_Private)^2 / H_pys^2 +
              vH_puys_Private * (pboot_puysl_Private * H_puys_Charter - H_puysl_Charter)^2 / H_pys^2 +
              vpboot_puysl_Charter * H_puys_Charter^2 + 
              vpboot_puysl_Private * H_puys_Private^2),
         sep_pysl = sqrt(vp_pysl),
         sepboot_pysl = sqrt(vpboot_pysl)) %>%
  left_join(prop_lgbinpool, by = c("port", "year", "spp", "lgbin")) %>%
  mutate(seppool_pysl = sqrt(vppool_pysl)) %>%
  left_join(test_lgbin, by = c("port", "year", "spp")) %>%
  left_join(aggregate(n ~ port+year+spp, data = n_lgbin[n_lgbin$spp %in% c("Black", "Yelloweye"), ], FUN = sum), by = c("port", "year", "spp")) %>%
  select(port, year, spp, lgbin, n, p_pysl, pboot_pysl, ppool_pysl, vp_pysl, vpboot_pysl, vppool_pysl, sep_pysl, sepboot_pysl, seppool_pysl, p05a, min)
#saveRDS(est_lgbin, ".\\Rockfish report_96-19\\spp comp\\est_lgbin.rds")
est_lgbin <- readRDS(".\\Rockfish report_96-19\\composition\\est_lgbin.rds")


#bias indicated by bootstrap might be sample size related
n_lgbin[n_lgbin$spp %in% c("Black", "Yelloweye"), ] %>%
  group_by(port, year, spp) %>%
  summarise(n = sum(n)) %>%
  mutate(n_cut = cut(n, breaks = c(0, 50, 100, 1000))) %>%
  left_join(est_lgbin, by = c("port", "year", "spp")) %>%
  ggplot(aes(x = p_pysl, y = pboot_pysl, color = n_cut)) +
  geom_point() 

#Plot bootstrap and SRS SE's
est_lgbin %>% 
  ggplot(aes(x= sep_pysl, y = sepboot_pysl)) +
  geom_point() +
  geom_abline(slope = 1) +
  labs(x = "SRS SE", y = "bootstrap SE") +
  theme_bw(base_size = 18) +
  ggtitle("Standard errors for length composition proportions")
lm(est_lgbin$sepboot_pysl ~ est_lgbin$sep_pysl)

#Sig estimates vrs. potential bias 
est_lgbin %>%
  group_by(port, year, spp) %>%
  filter(p_pysl == max(p_pysl)) %>%
  ggplot(aes(p_pysl, ppool_pysl, color = p05a)) +
  geom_point()
#note ~ 1/9 > +/- 15%
est_lgbin %>%
  mutate(diff = ppool_pysl/p_pysl) %>%
  filter(p05a == TRUE) %>%
  group_by(port, year, spp) %>%
  filter(p_pysl == max(p_pysl)) %>%
  arrange(-diff) %>%
  print(n = 90)

#Bias associated with small samples sizes per strata larger than bias from ignoring strata.
plotly::plot_ly(x = est_lgbin$p_pysl, y = est_lgbin$ppool_pysl, z = est_lgbin$min, 
                type = "scatter3d", 
                mode = "markers", 
                color = est_lgbin$p05a,
                text = ~paste(est_lgbin$port, "-", est_lgbin$year, "-", est_lgbin$spp))

#### Is there a sample size where we have a fairly good sample w so many categories
#averlgbin lgbins per port, and year
rflgbin %>% 
  group_by(port, year, spp) %>% 
  filter(spp %in% c("Black", "Yelloweye")) %>% 
  summarise(n = unique(lgbin)) %>%  #Unique lgbins per port, year spp
  summarise(n = length(n)) %>% # # of Unique lgbins per port, year spp 
  group_by(spp) %>%
  summarise(n = mean(n)) #Mean of Unique lgbins by spp

#Thompson 87 eq 1
#Can achieve around +-0.1 at alpha .1 with ~ 70 samples for the number of categories we are seeing 
m <- 6:9
alpha <- .05
d <- 0.15
z <- qnorm(alpha/2/m)
n <- z^2/m*(1-1/m)/d^2
data.frame(m, n)

#the largest lgbin class average 26% of yelloweye and 44 % for black
est_lgbin %>% 
  group_by(port, year, spp) %>% 
  summarise(max = max(p_pysl, na.rm = TRUE)) %>% 
  group_by(spp) %>% summarise(median = median(max))


#90+ percent chance of seeing most samples in largest lgbin class when you sample 50 fish
#and there are 10 lgbins
#and the largest lgbin class is 25 % of the sample
sapply(seq(5, 30, 5), function(y) mean(rmultinom(10000, y, c(.25, rep(.25/9, 9))) %>% apply(2, function(x)x[1] > max(x[2:10]))))

#small samples by port and spp
temp <- 
  n_lgbin %>% 
  pivot_wider(id_cols = c("port", "year", "spp"), values_from = "n", names_from = "user") %>% 
  mutate(small = Charter + Private < 30)
table(temp$port, temp$spp, temp$small)
#None of the n<30 samples are significant
test_lgbin %>% 
  filter(Charter + Private < 30) %>%
  print( n = 100)
# 16 of the significant samples have n < 30 for one user group
test_lgbin %>% 
  filter((Charter < 30 | Private <30) & p05a == TRUE) %>%
  print( n = 100)
#Cook Inlet-1997-Black --- sample and SWHS oppose. Probably better to stratify.
est_lgbin[est_lgbin$port == "Cook Inlet" & est_lgbin$year == 1997 & est_lgbin$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = .2)
prop_lgbin[prop_lgbin$port == "Cook Inlet" & prop_lgbin$year == 1997 & prop_lgbin$spp == "Black", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Cook Inlet" & SWHS_puys$year == 1997 & SWHS_puys$spp == "Black", ]
#Cook Inlet-1998-Black --- sample and SWHS oppose. Probably better to stratify.
est_lgbin[est_lgbin$port == "Cook Inlet" & est_lgbin$year == 1998 & est_lgbin$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = .2)
prop_lgbin[prop_lgbin$port == "Cook Inlet" & prop_lgbin$year == 1998 & prop_lgbin$spp == "Black", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Cook Inlet" & SWHS_puys$year == 1998 & SWHS_puys$spp == "Black", ]
#Cook Inlet-2000-Black --- sample and SWHS oppose. Probably better to stratify.
est_lgbin[est_lgbin$port == "Cook Inlet" & est_lgbin$year == 2000 & est_lgbin$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = .2)
prop_lgbin[prop_lgbin$port == "Cook Inlet" & prop_lgbin$year == 2000 & prop_lgbin$spp == "Black", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Cook Inlet" & SWHS_puys$year == 2000 & SWHS_puys$spp == "Black", ]
#Cook Inlet-2002-Black --- charter oversampled. Probably better to stratify.
est_lgbin[est_lgbin$port == "Cook Inlet" & est_lgbin$year == 2002 & est_lgbin$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = .2)
prop_lgbin[prop_lgbin$port == "Cook Inlet" & prop_lgbin$year == 2002 & prop_lgbin$spp == "Black", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Cook Inlet" & SWHS_puys$year == 2002 & SWHS_puys$spp == "Black", ]
#Cook Inlet-2008-Black --- OK either way
est_lgbin[est_lgbin$port == "Cook Inlet" & est_lgbin$year == 2008 & est_lgbin$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = .2)
prop_lgbin[prop_lgbin$port == "Cook Inlet" & prop_lgbin$year == 2008 & prop_lgbin$spp == "Black", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Cook Inlet" & SWHS_puys$year == 2008 & SWHS_puys$spp == "Black", ]
# Cook Inlet-2011-Yelloweye --- Sample size ridiculous but very small harvest to match. Ok either way
est_lgbin[est_lgbin$port == "Cook Inlet" & est_lgbin$year == 2011 & est_lgbin$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_lgbin[prop_lgbin$port == "Cook Inlet" & prop_lgbin$year == 2011 & prop_lgbin$spp == "Yelloweye", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Cook Inlet" & SWHS_puys$year == 2011 & SWHS_puys$spp == "Yelloweye", ]
# Cook Inlet-2018-Yelloweye --- Ok either way
est_lgbin[est_lgbin$port == "Cook Inlet" & est_lgbin$year == 2018 & est_lgbin$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_lgbin[prop_lgbin$port == "Cook Inlet" & prop_lgbin$year == 2018 & prop_lgbin$spp == "Yelloweye", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Cook Inlet" & SWHS_puys$year == 2018 & SWHS_puys$spp == "Yelloweye", ]

# Kodiak-2006-Yelloweye --- Sample size ridiculous but very small harvest to match. Ok either way
est_lgbin[est_lgbin$port == "Kodiak" & est_lgbin$year == 2006 & est_lgbin$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.2)
prop_lgbin[prop_lgbin$port == "Kodiak" & prop_lgbin$year == 2006 & prop_lgbin$spp == "Yelloweye", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Kodiak" & SWHS_puys$year == 2006 & SWHS_puys$spp == "Yelloweye", ]

#Seward-1999-Yelloweye #better to stratify
est_lgbin[est_lgbin$port == "Seward" & est_lgbin$year == 1999 & est_lgbin$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge")
prop_lgbin[prop_lgbin$port == "Seward" & prop_lgbin$year == 1999 & prop_lgbin$spp == "Yelloweye", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Seward" & SWHS_puys$year == 1999 & SWHS_puys$spp == "Yelloweye", ]

#Valdez-1997-Black #Sample size ridiculous but very small harvest to match. Ok either way 
est_lgbin[est_lgbin$port == "Valdez" & est_lgbin$year == 1997 & est_lgbin$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.2)
prop_lgbin[prop_lgbin$port == "Valdez" & prop_lgbin$year == 1997 & prop_lgbin$spp == "Black", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Valdez" & SWHS_puys$year == 1997 & SWHS_puys$spp == "Black", ]
#Valdez-2004-Black better to stratify
est_lgbin[est_lgbin$port == "Valdez" & est_lgbin$year == 2004 & est_lgbin$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.2)
prop_lgbin[prop_lgbin$port == "Valdez" & prop_lgbin$year == 2004 & prop_lgbin$spp == "Black", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Valdez" & SWHS_puys$year == 2004 & SWHS_puys$spp == "Black", ]
#Valdez-2004-Yelloweye better to stratify
est_lgbin[est_lgbin$port == "Valdez" & est_lgbin$year == 2004 & est_lgbin$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.2)
prop_lgbin[prop_lgbin$port == "Valdez" & prop_lgbin$year == 2004 & prop_lgbin$spp == "Yelloweye", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Valdez" & SWHS_puys$year == 2004 & SWHS_puys$spp == "Yelloweye", ]
#Valdez-2005-Yelloweye better to stratify
est_lgbin[est_lgbin$port == "Valdez" & est_lgbin$year == 2005 & est_lgbin$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.2)
prop_lgbin[prop_lgbin$port == "Valdez" & prop_lgbin$year == 2005 & prop_lgbin$spp == "Yelloweye", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Valdez" & SWHS_puys$year == 2005 & SWHS_puys$spp == "Yelloweye", ]
#Valdez-2006-Yelloweye better to stratify
est_lgbin[est_lgbin$port == "Valdez" & est_lgbin$year == 2006 & est_lgbin$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.2)
prop_lgbin[prop_lgbin$port == "Valdez" & prop_lgbin$year == 2006 & prop_lgbin$spp == "Yelloweye", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Valdez" & SWHS_puys$year == 2006 & SWHS_puys$spp == "Yelloweye", ]
#Valdez-2007-Black # Small private sample but also different estimates. best to drop
est_lgbin[est_lgbin$port == "Valdez" & est_lgbin$year == 2007 & est_lgbin$spp == "Black", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.2)
prop_lgbin[prop_lgbin$port == "Valdez" & prop_lgbin$year == 2007 & prop_lgbin$spp == "Black", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Valdez" & SWHS_puys$year == 2007 & SWHS_puys$spp == "Black", ]
#Valdez-2007-Yelloweye Small private sample but also different estimates. best to drop
est_lgbin[est_lgbin$port == "Valdez" & est_lgbin$year == 2007 & est_lgbin$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.2)
prop_lgbin[prop_lgbin$port == "Valdez" & prop_lgbin$year == 2007 & prop_lgbin$spp == "Yelloweye", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Valdez" & SWHS_puys$year == 2007 & SWHS_puys$spp == "Yelloweye", ]
#Valdez-2008-Yelloweye best to stratify
est_lgbin[est_lgbin$port == "Valdez" & est_lgbin$year == 2008 & est_lgbin$spp == "Yelloweye", ] %>% 
  pivot_longer(cols = p_pysl:ppool_pysl, names_to = "est", values_to = "p") %>% 
  ggplot(aes(x = lgbin, y = p, fill = est)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.2)
prop_lgbin[prop_lgbin$port == "Valdez" & prop_lgbin$year == 2008 & prop_lgbin$spp == "Yelloweye", ] %>% arrange(user, lgbin) %>% print(n = 46)
SWHS_puys[SWHS_puys$port == "Valdez" & SWHS_puys$year == 2008 & SWHS_puys$spp == "Yelloweye", ]


#  * Format for report ----------------------------------------------------
#Tricky, I don't think we can do a blanket method
#Use pooled everywhere (included cases with no samples form one user group and thus no test) but when test is sig
#This seems like a good compromise BC all of the very small sample size sig differences are eliminated (min = 20)
table(test_lgbin$min, test_lgbin$p05a)
lgbin_format <- 
  est_lgbin %>%
  rowwise() %>%
  filter(spp %in% c("Black", "Yelloweye")) %>%
  mutate(p_pysl_report = ifelse(p05a == FALSE | is.na(p05a) | min < 20, ppool_pysl, p_pysl),
         sep_pysl_report = ifelse(p05a == FALSE | is.na(p05a) | min < 20, seppool_pysl, sep_pysl),
         est = ifelse(is.na(p_pysl_report),
                      NA,
                      paste0(format(round(p_pysl_report, 3), digits = 3), "(", format(round(sep_pysl_report, 3), digits = 3), ")")),
         lcilogitp_pysl = log(p_pysl_report / (1-p_pysl_report)) - 1.96 * sqrt(1 / p_pysl_report^2 / (1-p_pysl_report)^2 * sep_pysl_report^2),
         ucilogitp_pysl = log(p_pysl_report / (1-p_pysl_report)) + 1.96 * sqrt(1 / p_pysl_report^2 / (1-p_pysl_report)^2 * sep_pysl_report^2),
         lcip_pysl = exp(lcilogitp_pysl)/(1 + exp(lcilogitp_pysl)),
         ucip_pysl = exp(ucilogitp_pysl)/(1 + exp(ucilogitp_pysl)),
         est_ci = ifelse(is.na(p_pysl_report),
                         NA,
                         paste0(format(round(p_pysl_report, 3), digits = 3), 
                                "(", 
                                format(round(lcip_pysl, 3), digits = 3), 
                                "-",
                                format(round(ucip_pysl, 3), digits = 3),
                                ")")))

lgbin_format[lgbin_format$spp == "Black", ] %>%
  ggplot(aes(x = year, y = p_pysl_report, color = lgbin, fill = lgbin)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(port ~ .) +
  # scale_fill_manual(values = c("Black" = "#333333", "Dusky/Dark" = "#666666", "Yelloweye" = "#FFFF66",
  #                              "Copper" = "#FF9900", "Quillback" = "#993300", "Other" = "#CCCCCC"),
  #                   name = "Species") +
  # scale_color_manual(values = c("Black" = "#333333", "Dusky/Dark" = "#666666", "Yelloweye" = "#FFFF66",
  #                               "Copper" = "#FF9900", "Quillback" = "#993300", "Other" = "#CCCCCC"),
  #                    name = "Species")  +
  labs(x = "Year", y = "Percent of rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Black Rockfish Length Composition")

lgbin_format[lgbin_format$spp == "Black", ] %>%
  ggplot(aes(x = year, y = p_pysl_report, color = lgbin, fill = lgbin)) +
  geom_line() +
  facet_grid(port ~ .) +
  # scale_fill_manual(values = c("Black" = "#333333", "Dusky/Dark" = "#666666", "Yelloweye" = "#FFFF66",
  #                              "Copper" = "#FF9900", "Quillback" = "#993300", "Other" = "#CCCCCC"),
  #                   name = "Species") +
  # scale_color_manual(values = c("Black" = "#333333", "Dusky/Dark" = "#666666", "Yelloweye" = "#FFFF66",
  #                               "Copper" = "#FF9900", "Quillback" = "#993300", "Other" = "#CCCCCC"),
  #                    name = "Species")  +
  labs(x = "Year", y = "Percent of rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Black Rockfish Length Composition")


lgbin_format[lgbin_format$spp == "Yelloweye", ] %>%
  ggplot(aes(x = year, y = p_pysl_report, color = lgbin, fill = lgbin)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(port ~ .) +
  # scale_fill_manual(values = c("Black" = "#333333", "Dusky/Dark" = "#666666", "Yelloweye" = "#FFFF66",
  #                              "Copper" = "#FF9900", "Quillback" = "#993300", "Other" = "#CCCCCC"),
  #                   name = "Species") +
  # scale_color_manual(values = c("Black" = "#333333", "Dusky/Dark" = "#666666", "Yelloweye" = "#FFFF66",
  #                               "Copper" = "#FF9900", "Quillback" = "#993300", "Other" = "#CCCCCC"),
  #                    name = "Species")  +
  labs(x = "Year", y = "Percent of rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Yelloweye Rockfish Length Composition")


lgbin_format[lgbin_format$spp == "Yelloweye", ] %>%
  ggplot(aes(x = year, y = p_pysl_report, color = lgbin, fill = lgbin)) +
  geom_line() +
  facet_grid(port ~ .) +
  # scale_fill_manual(values = c("Black" = "#333333", "Dusky/Dark" = "#666666", "Yelloweye" = "#FFFF66",
  #                              "Copper" = "#FF9900", "Quillback" = "#993300", "Other" = "#CCCCCC"),
  #                   name = "Species") +
  # scale_color_manual(values = c("Black" = "#333333", "Dusky/Dark" = "#666666", "Yelloweye" = "#FFFF66",
  #                               "Copper" = "#FF9900", "Quillback" = "#993300", "Other" = "#CCCCCC"),
  #                    name = "Species")  +
  labs(x = "Year", y = "Percent of rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Yelloweye Rockfish Length Composition")


lgbin_format %>%
  filter(spp == "Black") %>%
  pivot_wider(id_cols = c("year", "lgbin"), names_from = "port", values_from = "est_ci") %>%
  arrange(year, lgbin) %>% print(n = 90)
lgbin_format %>%
  filter(spp == "Yelloweye") %>%
  arrange(year, lgbin, port) %>%
  pivot_wider(id_cols = c("year", "lgbin"), names_from = "port", values_from = "est_ci")

