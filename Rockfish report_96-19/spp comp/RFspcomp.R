library(tidyverse)

# Read and clean rf data
# Original data maker and many comments in 
# "H:\My Documents\Southcentral halibut and groundfish\Rockfish report_96-19\spp comp\RFspcomp9616_port.sas"
#  some comments transfered below
rfcomp00 <- haven::read_sas(".\\Rockfish report_96-19\\spp comp\\rfcompdata.sas7bdat")
rfcomp0 <- setNames(rfcomp00, tolower(names(rfcomp00)))

# Exclude rockfish marked as "rare" (Seward only) for species comp -- these are uncommon species that were sampled in
#   addition to the usual protocol to boost sample size for age and growth studies. These should be excluded from species 
#   comp but included for all other metrics (age/length/sex comp
table(rfcomp0$port, rfcomp0$rare)
# Find out why some spp = 154 are labeled Dusky
# Not covered in sas code although they make the same correction (line 186)
table(rfcomp0$sp, rfcomp0$species)
table(rfcomp0[rfcomp0$species == "Dusky", ]$year,
      rfcomp0[rfcomp0$species == "Dusky", ]$port,
      rfcomp0[rfcomp0$species == "Dusky", ]$sp)
#Sparse sampling in Whittier in 1998
#Insufficient Cordova samples
table(rfcomp0$port, rfcomp0$year)
#6 species field is blank
table(rfcomp0$species, useNA = "always")

rfcomp <- 
  setNames(rfcomp0, tolower(names(rfcomp0))) %>%
  filter(rare != "R",
         port != "Cordova",
         !(port == "Whittier" & year == 1998),
         species != "") %>%
  mutate(species = ifelse(sp == 154, "DuskyDrk", species)) %>%
  select(port, user, year, species, sp, boatname, month, day)
table(rfcomp$sp, rfcomp$species) #renamed spp = 154 Dusky
#Note Rares (check file sizes) and small N ports deleted 
table(rfcomp$port, rfcomp$year)
table(rfcomp$species, useNA = "always")
#rfcomp0(75838)-Cordova(79)-Whittier98(48)-Rares(16)-sppblank(6)=rfcomp(75689)



# Read SWHS data
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

#raw proportions
prop <- 
  rfcomp %>%
  filter(user %in% c("Charter", "Private")) %>% #Note:189 rows dropped
  group_by(port, user, year, species) %>% 
  summarise(cnt = n()) %>% 
  pivot_wider(names_from = user, values_from = cnt) %>% #next 3 lines fill missing spp
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  pivot_longer(Charter:Private, names_to = "user", values_to = "cnt") %>%
  group_by(port, user, year) %>%
  mutate (p_puys = cnt / sum(cnt),
          vp_puys = p_puys * (1 - p_puys) / sum(cnt)) %>%
  select(-cnt) 



########## Bootstrap raw proportions ##########
#Note: boats always named after 2008.
#Should be able to bootstrap by boat after 2008.
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
            as.tibble(x) %>%
              unnest(data) %>%
              group_by(species) %>% 
              summarise(cnt = n()) %>% 
              mutate(p = cnt / sum(cnt)) %>%
              select(species, p)
          }) %>%
          Reduce(function(...) full_join(..., by='species'), .) %>%
          pivot_longer(-species, values_to = "p") %>%
          mutate(p = ifelse(is.na(p), 0, p)) %>%
          select(-name)}
  )

#Check bootstrap distributions 
library(ggplot2)
# Map(function(x, y){
#   ggplot(x, aes(x = p)) +
#     geom_histogram() + 
#     facet_grid(species ~ .) +
#     ggtitle(y)
#     },
#   boot_p,
#   strata
# )

#Summary stats from bootstrap
boot_stats0 <- 
  lapply(boot_p, 
         function(x){x %>%
             group_by(species) %>%
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


########## add bootstrap ps to raw proportions ##########
#Merge w SWHS
est <-
  left_join(prop, SWHS_puy, by = c("port", "user", "year")) %>%
    left_join(boot_stats, by = c("port", "user", "year", "species")) %>%
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
    select(port, year, species, p_pys, vp_pys, vpboot_pys, sep_pys, sepboot_pys)

#Plot bootstrap and SRS SE's
est %>% 
  ggplot(aes(x= sep_pys, y = sepboot_pys)) +
    geom_point() +
    geom_abline(slope = 1) +
    labs(x = "SRS SE", y = "bootstrap SE") +
    theme_bw(base_size = 18) +
    ggtitle("Standard errors for species composition proportions")

#Format species and estiamtes for a species comp table and figure
names <- 
  c(c("Black", "Dark", "Dusky", "DuskyDrk", "Yelleye", "Copper", "Quill"),
    sort(unique(est$species))[!(sort(unique(est$species)) %in% 
                                  c("Black", "Dark", "Dusky", "DuskyDrk", "Yelloweye", "Copper", "Quillback"))])
est_format <- 
  est %>%
    mutate(spp = factor(species, 
                        levels = names,
                        labels = c("Black", "Dusky/Dark", "Dusky/Dark", "Dusky/Dark", "Yelloweye", "Copper",
                        "Quillback", rep("Other", 20)))) %>%
    group_by(port, year, spp) %>%
    summarize(p_pys = sum(p_pys), vp_pys = sum(vp_pys), vpboot_pys = sum(vpboot_pys)) %>%
    mutate(sep_pys = sqrt(vp_pys),
           sepboot_pys = sqrt(vpboot_pys),
           est = paste0(format(round(p_pys, 3), digits = 3), "(", format(round(sepboot_pys, 3), digits = 3), ")"))

est_format %>%
    ggplot(aes(x = year, y = p_pys, color = spp, fill = spp)) +
      geom_area() +
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


########## Assemby comp ##########
assem <- 
  c(c("Black", "Dark", "Dusky", "DuskyDrk", "Yellowtail", "Widow"),
    sort(unique(est$species))[!(sort(unique(est$species)) %in% 
                                  c("Black", "Dark", "Dusky", "DuskyDrk", "Yellowtail", "Widow"))])

assem_format <-
  est %>%
  mutate(assem = factor(species, 
                      levels = assem,
                      labels = c(rep("Pelagic", 6), rep("non-Pelagic", 20)))) %>%
  group_by(port, year, assem) %>%
  summarize(p_pys = sum(p_pys), vp_pys = sum(vp_pys), vpboot_pys = sum(vpboot_pys)) %>%
  mutate(sep_pys = sqrt(vp_pys),
         sepboot_pys = sqrt(vpboot_pys),
         est = paste0(format(round(p_pys, 3), digits = 3), "(", format(round(sepboot_pys, 3), digits = 3), ")"))

assem_format %>%
  ggplot(aes(x = year, y = p_pys, color = assem, fill = assem)) +
  geom_area() +
  facet_grid(port ~ .) +
  scale_fill_manual(values = c("Pelagic" = "black", "non-Pelagic" = "grey"),
                    name = "Assemblage") +
  scale_color_manual(values = c("Pelagic" = "black", "non-Pelagic" = "grey"),
                     name = "Assemblage")  +
  labs(x = "Year", y = "Percent of rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Rockfish Assemblage Composition")

assem_format %>%
  pivot_wider(id_cols = c("port", "year"), names_from = "assem", values_from = "est") %>%
  print(n = 150)

########## Seward Military and Unkniwn users ##########
table(rfcomp$user, rfcomp$year, rfcomp$port)
# Problem affects Seward 1996-2000
# Spp comps differ by user group
test_Sew <- list()
dat_Sew <- 
  rfcomp[rfcomp$port == "Seward" & rfcomp$user != "Unknown" & rfcomp$year %in% 1996:2000,
         which(names(rfcomp) %in% c("species", "user", "year"))] %>%
  mutate(spp = factor(species, 
                      levels = names,
                      labels = c("Black", "Dusky/Dark", "Dusky/Dark", "Dusky/Dark", "Yelloweye", rep("Other", 22))))
table_Sew <- with(dat_Sew,table(user, spp, year))
for(y in 1:5){
  test_Sew[[y]] <- chisq.test(table_Sew[,,y])
}
table_Sew
for(i in 1:5) print(table_Sew[,,i] / rowSums(table_Sew[,,i]))
test_Sew

# But note Spp comp estimated are minimally changed even if military harvest is 40% of total harvest.
prop_Sew <- 
  rfcomp %>%
  filter(user != "Unknown", port == "Seward", year <= 2000) %>%
  group_by(port, user, year, species) %>% 
  summarise(cnt = n()) %>% 
  pivot_wider(names_from = user, values_from = cnt) %>% #next 3 lines fill missing spp
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  pivot_longer(Charter:SewMilC, names_to = "user", values_to = "cnt") %>%
  group_by(port, user, year) %>%
  mutate (p_puys = cnt / sum(cnt)) %>%
  select(-cnt) 

Hmil <- function(p){
  SWHS %>%
    filter(port == "Seward", year <= 2000) %>%
    pivot_wider(id_cols = c("port", "year"), names_from = user, values_from = H_puy) %>%
    mutate(SewMilC = p * Both,
           Charter = Charter - SewMilC * Charter / Both,
           Private = Private - SewMilC * Private / Both) %>%
    select(-Both) %>%
    pivot_longer(Charter:SewMilC, names_to = "user", values_to = "H_puy")
}

est_Sew.2 <-
  left_join(prop_Sew, Hmil(.2), by = c("port", "user", "year")) %>%
  filter(user != "Unknown", !is.na(H_puy)) %>%
  mutate(H_puys = p_puys * H_puy) %>%
  pivot_wider(names_from = user, values_from = c(p_puys, H_puy, H_puys)) %>%
  left_join(SWHS_py, by = c("port", "year")) %>%
  mutate(H_pys = H_puys_Charter + H_puys_Private + H_puys_SewMilC,
         p_pys = H_pys / H_py,
         pMil = "0.2") %>%
  select(port, year, species, p_pys, pMil)

est_Sew.4 <-
  left_join(prop_Sew, Hmil(.4), by = c("port", "user", "year")) %>%
  filter(user != "Unknown", !is.na(H_puy)) %>%
  mutate(H_puys = p_puys * H_puy) %>%
  pivot_wider(names_from = user, values_from = c(p_puys, H_puy, H_puys)) %>%
  left_join(SWHS_py, by = c("port", "year")) %>%
  mutate(H_pys = H_puys_Charter + H_puys_Private + H_puys_SewMilC,
         p_pys = H_pys / H_py,
         pMil = "0.4") %>%
  select(port, year, species, p_pys, pMil)

comp_Sew <-
  est %>% 
  filter(port == "Seward", year <= 2000) %>%
  mutate(pMil = "0",
         p_pys_l = p_pys - 2*sep_pys,
         p_pys_u = p_pys + 2*sep_pys) %>%
  select(port, year, species, p_pys, p_pys_l, p_pys_u, pMil) %>%
  rbind(est_Sew.2) %>%
  rbind(est_Sew.4) %>%
  filter(species %in% c("Black", "Yelleye")) 

comp_Sew %>%
  ggplot(aes(x = year, y = p_pys, color = pMil)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = p_pys_l, ymax = p_pys_u), position=position_dodge(width=0.5)) +
  facet_grid(. ~ species)
comp_Sew[comp_Sew$year == 1997,]

#Also note 
bind_Sew <- comp_Sew[comp_Sew$pMil == "0", c(3, 2, 4, 5, 6)] %>%
  mutate(method = "drop") %>%
  select(species, year, method, p_pys, p_pys_l, p_pys_u)
readxl::read_excel("O:\\DSF\\GOAB\\RockfishReport_96-16\\SpeciesComp\\SpeciesCompBefore2001.xlsx",
                   range = "Seward!AC12:AF53",
                   col_names = c("species", "n", "pool", "weight")) %>%
  select(-n) %>%
  filter(!is.na(species), species != "Other") %>%
  mutate(year = rep(1996:2000, each = 2)) %>%
  pivot_longer(pool:weight, names_to = "method", values_to = "p_pys") %>%
  mutate(p_pys = as.numeric(p_pys), p_pys_l = NA, p_pys_u = NA) %>%
  rbind(bind_Sew) %>%
    ggplot(aes(x = year, y = p_pys, color = method)) +
      geom_point(position=position_dodge(width=0.5)) +
      geom_errorbar(aes(ymin = p_pys_l, ymax = p_pys_u), position=position_dodge(width=0.5)) +
      facet_grid(. ~ species)
  

# Kodiak 1997
# Valdez 1996

rfcomp %>%
  filter(port == "Seward" & year <= 2000 & user != "Unknown") %>%
  mutate(species = factor(species, 
                      levels = names,
                      labels = c("Black", "Dusky/Dark", "Dusky/Dark", "Dusky/Dark", "Yelloweye", "Copper",
                                 "Quillback", rep("Other", 20)))) %>%
  group_by(port, user, year, species) %>%
  summarise(cnt = n()) %>% 
  pivot_wider(names_from = user, values_from = cnt) %>% #next 3 lines fill missing spp
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  pivot_longer(Charter:SewMilC, names_to = "user", values_to = "cnt") %>%
  group_by(port, user, year) %>%
  mutate (p_puys = cnt / sum(cnt)) %>%
  select(-cnt) %>%
  pivot_wider(names_from = species, values_from = p_puys) %>%
  print(n = 100)





test <- list()
dat <- 
  rfcomp[rfcomp$port == "Seward" & rfcomp$user %in% c("Private", "SewMilC") & rfcomp$year %in% 1996:2000,
         which(names(rfcomp) %in% c("species", "user", "year"))] %>%
  mutate(spp = factor(species, 
                      levels = names,
                      labels = c("Black", "Dusky/Dark", "Dusky/Dark", "Dusky/Dark", "Yelloweye", rep("Other", 22))))
table <- with(dat,table(user, spp, year))
for(y in 1:5){
  test[[y]] <- chisq.test(table[,,y])
}
test


rfcomp[rfcomp$port == "Kodiak" & rfcomp$year %in% 1997,
       which(names(rfcomp) %in% c("species", "user", "year"))] %>%
  mutate(spp = factor(species, 
                      levels = names,
                      labels = c("Black", "Dusky/Dark", "Dusky/Dark", "Dusky/Dark", 
                                 rep("Other", 23)))) %>%
  with(table(user, spp)) %>%
  chisq.test()

rfcomp[rfcomp$port == "Valdez" & rfcomp$year %in% 1996,
       which(names(rfcomp) %in% c("species", "user", "year"))] %>%
  mutate(spp = factor(species, 
                      levels = names,
                      labels = c("Black", "Dusky/Dark", "Dusky/Dark", "Dusky/Dark", "Yelloweye", "Copper", "Quillback",
                                 rep("Other", 20)))) %>%
  with(table(user, spp)) %>%
  chisq.test()



  


