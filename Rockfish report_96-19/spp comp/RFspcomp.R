library(tidyverse)


# Data prep ---------------------------------------------------------------

# * Composition data --------------------------------------------------------

# Original data maker and many comments in 
# "H:\My Documents\Southcentral halibut and groundfish\Rockfish report_96-19\spp comp\RFspcomp9616_port.sas"
#  some comments transfered below
rfcomp00 <- haven::read_sas(".\\Rockfish report_96-19\\spp comp\\rfcompdata.sas7bdat")
rfcomp00 <- setNames(rfcomp00, tolower(names(rfcomp00)))


# * * Clean data --------------------------------------------------------------
# Exclude rockfish marked as "rare" (Seward only) for species comp -- these are uncommon species that were sampled in
#   addition to the usual protocol to boost sample size for age and growth studies. These should be excluded from species 
#   comp but included for all other metrics (age/length/sex comp)
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
                        labels = c(rep("Pelagic", 6), rep("non-Pelagic", 20)))) %>%
  select(port, user, year, species, sp, spp, assem, boatname, month, day)
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
#Previous researchers focused on this comparision.
est_Sewmil <- 
  readxl::read_excel("O:\\DSF\\GOAB\\RockfishReport_96-16\\SpeciesComp\\SpeciesCompBefore2001.xlsx",
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
#I will revist this after we have estimates w that asumption.
#Regarding other the best thing seems to be to drop
table(rfcomp0$user, rfcomp0$year, rfcomp0$port)
rfcomp <- 
  rfcomp0 %>%
  mutate(user = ifelse(user == "SewMilC", "Charter", user)) %>%
  filter(user != "Unknown")
#rfcomp0(75689) - 338 = rfcomp(75351)
table(rfcomp$user, rfcomp$year, rfcomp$port)

# * SWHS data ---------------------------------------------------------------
SWHS <-
  readxl::read_excel(".\\Rockfish report_96-19\\spp comp\\RFHarvByUser9619.xlsx", 
                     range = "SWHS96-19!A1:H121") %>%
  pivot_longer(ends_with("harv")) %>%
  mutate(user = ifelse(grepl("Charv", name), "Charter", 
                       ifelse(grepl("Pharv", name), "Private", "Both")),
         stat = ifelse(grepl("SE", name), "seH_puy", "H_puy")) %>%
  select(-name) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(vH_puy = seH_puy^2) %>%
  select(-seH_puy)
SWHS <- setNames(SWHS, ifelse(grepl("H", names(SWHS)), names(SWHS), tolower(names(SWHS))))
SWHS_puy <- SWHS[SWHS$user %in% c("Charter", "Private"), ]
SWHS_py <- SWHS[SWHS$user %in% c("Both"), ] %>%
  select(-user)
SWHS_py <- setNames(SWHS_py, gsub("puy", "py", names(SWHS_py)))


# Species comp ---------------------------------------------------
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
library(ggplot2)
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
           sepboot_pys = sqrt(vpboot_pys),
           port = factor(port, 
                         levels = c("CI", "Kodiak", "Seward", "Valdez", "Whittier"),
                         labels = c("Cook Inlet", "Kodiak", "Seward", "Valdez", "Whittier"))) %>%
    select(port, year, spp, p_pys, vp_pys, vpboot_pys, sep_pys, sepboot_pys)

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
bind_Sew <- comp_Sew[, c(3, 2, 4, 10, 11)] %>%
  mutate(method = "comm") %>%
  filter(spp %in% c("Black", "Yelloweye")) %>%
  select(spp, year, method, p_pys, p_pys_l, p_pys_u)
bind_rows(est_Sewmil, bind_Sew) %>%
  ggplot(aes(x = year, y = p_pys, color = method)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = p_pys_l, ymax = p_pys_u), position=position_dodge(width=0.5)) +
  facet_grid(. ~ spp)
#% and absolute diff
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
         sepboot_pys = sqrt(vpboot_pys),
         port = factor(port, 
                       levels = c("CI", "Kodiak", "Seward", "Valdez", "Whittier"),
                       labels = c("Cook Inlet", "Kodiak", "Seward", "Valdez", "Whittier"))) %>%
  select(port, year, assem, p_pys, vp_pys, vpboot_pys, sep_pys, sepboot_pys)

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
