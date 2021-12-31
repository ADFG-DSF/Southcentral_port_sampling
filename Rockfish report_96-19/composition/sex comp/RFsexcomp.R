library(tidyverse)


# Data prep ---------------------------------------------------------------

# * Composition data --------------------------------------------------------

# Original data maker was to section of "O:\DSF\GOAB\RockfishReport_96-16\SexComp\RFsexcomp9618.sas"
sexcomp00 <- haven::read_sas(".\\Rockfish report_96-19\\sex comp\\sexcompdata.sas7bdat")
sexcomp00 <- setNames(sexcomp00, tolower(names(sexcomp00)))


# * * Clean data --------------------------------------------------------------
# Exclude rockfish marked as "rare" (Seward only) for species comp -- these are uncommon species that were sampled in
#   addition to the usual protocol to boost sample size for age and growth studies. These should be excluded from species 
#   comp but included for all other metrics (age/length/sex comp)
table(sexcomp00$port, sexcomp00$rare)
#Format species and estimates for a species comp table and figure

#Sparse sampling in Whittier in 1998
#Insufficient Cordova samples
table(sexcomp00$port, sexcomp00$year)

#6 species field is blank
table(sexcomp00$species, useNA = "always")

table(sexcomp00$port, sexcomp00$user, useNA = "always")


# define names for species and assemblage comp
names_spp <- 
  c(c("Black", "Dark", "Dusky", "DuskyDrk", "Yelloweye", "Copper", "Quill"),
    sort(unique(sexcomp00$species))[!(sort(unique(sexcomp00$species)) %in% 
                                      c("Black", "Dark", "Dusky", "DuskyDrk", "Yelloweye", "Copper", "Quill"))])

sexcomp <-
  sexcomp00 %>%
  filter(rare != "R",
         port != "Cordova",
         !(port == "Whittier" & year == 1998),
         user != "Unknown",
         user != "") %>%
  mutate(spp = factor(species, 
                      levels = names_spp,
                      labels = c("Black", "Dusky/Dark", "Dusky/Dark", "Dusky/Dark", "Yelloweye", "Copper", "Quillback", rep("Other", 21))),
         user = ifelse(user == "SewMilC", "Charter", user)) %>%
  select(port, user, year, species, sp, spp, sex, length, age, boatname, month, day)

#Note Rares (check file sizes) and small N ports deleted 
table(sexcomp$port, sexcomp$year)
#rfcomp00(75838)-Cordova(79)-Whittier98(48)-Rares(16)-sppblank(6)=rfcomp0(75689)



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
           sepboot_pys = sqrt(vpboot_pys)) %>%
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


# Sex comp ---------------------------------------------------
# raw proportions
rfsex <- sexcomp[!is.na(sexcomp$sex) & sexcomp$sex != "", ] #not nesseasry but save for now, easier to run following code
table(rfsex$spp, rfsex$year, rfsex$port)
prop_sex <- 
  rfsex %>%
  group_by(port, user, year, spp, sex) %>% 
  summarise(cnt = n()) %>% 
  pivot_wider(names_from = user, values_from = cnt) %>% #next 3 lines fill missing spp
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .)) %>%
  pivot_longer(Charter:Private, names_to = "user", values_to = "cnt") %>%
  group_by(port, user, year, spp) %>%
  mutate (p_puys = cnt / sum(cnt),
          vp_puys = p_puys * (1 - p_puys) / sum(cnt)) %>%
  select(-cnt) 



#Convert list columns back to dataframe within each strata
#object still a list where each element of the list is a df for one strata (port, user, year combination).
boot_p_sex <- 
  lapply(boot_out, 
         function(x){
           lapply(x$splits, function(x){
             as_tibble(x) %>%
               unnest(data) %>%
               group_by(spp, sex) %>%
               filter(sex != "" & spp %in% c("Black", "Dusky/Dark", "Yelloweye", "Quillback")) %>%
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
library(ggplot2)
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
             summarise(pboot_puys = mean(p),
                       vpboot_puys = var(p))}
  )

#add strata ids
boot_stats_sex <-
  Map(function(x, y){
    x$port = gsub("(.*)-(.*)-(.*)", '\\1', y);
    x$user = gsub("(.*)-(.*)-(.*)", '\\2', y);
    x$year = as.numeric(gsub("(.*)-(.*)-(.*)", '\\3', y));
    return(x)}, 
    x = boot_stats0_sex, y = strata[-27]) %>%
  do.call(rbind, .)


#Plot bootstrap and SRS SE's
n_sex <- 
  rfsex %>%
  group_by(port, year, user, spp) %>%
  summarise(n = sum(sex %in% c("M", "F")))
comp_se_sex <- 
  prop_sex %>%
  left_join(boot_stats_sex, by = c("port", "year", "user", "spp", "sex")) %>%
  left_join(n_sex, by = c("port", "year", "user", "spp")) %>%
  mutate(sep_puys = sqrt(vp_puys),
         sepboot_puys = sqrt(vpboot_puys),
         n_cut = cut(n, breaks = c(0, 15, 100, 1000)))
ggplot(comp_se_sex, aes(x= sep_puys, y = sepboot_puys, color = n_cut)) + #[comp_se_sex$n > 5, ]
  geom_point() +
  stat_smooth(data = comp_se_sex[comp_se_sex$n > 15, ], method = "lm") +
  geom_abline(slope = 1) +
  labs(x = "SRS SE", y = "bootstrap SE") +
  theme_bw(base_size = 18) +
  ggtitle("Standard errors for sex composition proportions")
summary(lm(comp_se_sex$sepboot_puys[comp_se_sex$n > 15] ~ comp_se_sex$sep_puys[comp_se_sex$n > 15]))



#add bootstrap ps to raw proportions
#Merge w SWHS
est_sex <-
  left_join(prop_sex, SWHS_puy, by = c("port", "user", "year")) %>%
  left_join(boot_stats_sex, by = c("port", "user", "year", "spp", "sex")) %>%
  mutate(pboot_puys = ifelse(is.na(pboot_puys), 0, pboot_puys),
         vpboot_puys = ifelse(is.na(vpboot_puys) & year >=2009, 0, vpboot_puys),
         H_puys = p_puys * H_puy,
         Hboot_puys = pboot_puys * H_puy,
         vH_puys = p_puys^2*vH_puy + vp_puys*H_puy^2 - vp_puys*vH_puy,
         vHboot_puys = p_puys^2*vH_puy + vpboot_puys*H_puy^2 - vpboot_puys*vH_puy) %>%
  pivot_wider(names_from = user, values_from = c(p_puys, vp_puys, vH_puys,
                                                 pboot_puys, vpboot_puys, vHboot_puys,
                                                 H_puy, vH_puy, H_puys, Hboot_puys)) %>%
  left_join(SWHS_py, by = c("port", "year")) %>%
  mutate(H_pys = H_puys_Charter + H_puys_Private,
         Hboot_pys = Hboot_puys_Charter + Hboot_puys_Private,
         vH_pys = vH_puys_Charter + vH_puys_Private,
         vHboot_pys = vHboot_puys_Charter + vHboot_puys_Private,
         p_pys = H_pys / H_py,
         pboot_pys = Hboot_pys / H_py,
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
  select(port, year, spp, sex, p_pys, pboot_pys, vp_pys, vpboot_pys, sep_pys, sepboot_pys)

n_sex %>%
  group_by(port, year, spp) %>%
  summarise(n = sum(n)) %>%
  mutate(port = ifelse(port == "CI", "Cook Inlet", port),
         n_cut = cut(n, breaks = c(0, 50, 100, 1000))) %>%
  left_join(est_sex, by = c("port", "year", "spp")) %>%
  ggplot(aes(x = p_pys, y = pboot_pys, color = n_cut)) +
    geom_point() 
  

#  * Format for report ----------------------------------------------------
#I dont see much bias in the se or the point estiamte of p. Use SRS values.
sex_format <- 
  est_sex %>%
  mutate(sep_pys = sqrt(vp_pys),
         sepboot_pys = sqrt(vpboot_pys),
         est = ifelse(is.na(p_pys),
                      NA,
                      paste0(format(round(p_pys, 3), digits = 3), "(", format(round(sep_pys, 3), digits = 3), ")")),
         lcilogitp_pys = log(p_pys / (1-p_pys)) - 1.96 * sqrt(1 / p_pys^2 / (1-p_pys)^2 * sep_pys^2),
         ucilogitp_pys = log(p_pys / (1-p_pys)) + 1.96 * sqrt(1 / p_pys^2 / (1-p_pys)^2 * sep_pys^2),
         lcip_pys = exp(lcilogitp_pys)/(1 + exp(lcilogitp_pys)),
         ucip_pys = exp(ucilogitp_pys)/(1 + exp(ucilogitp_pys)))


sex_format %>%
  filter(spp == "Black", sex == "F") %>%
  ggplot(aes(x = year, y = p_pys, color = sex, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lcip_pys, ymax = ucip_pys), color = "black") +
  facet_grid(port ~ .) +
  scale_fill_manual(values = c("F" = "light gray"), guide = FALSE) +
  scale_color_manual(values = c("F" = "light gray"), guide = FALSE)  +
  labs(x = "Year", y = "Percent of black rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Black Rockfish Sex Composition")
n_sex %>% 
  filter(spp == "Black") %>%
  pivot_wider(names_from = port, values_from = n) %>%
  print(n = 100)

sex_format %>%
  filter(spp == "Yelloweye", sex == "F") %>%
  ggplot(aes(x = year, y = p_pys, color = sex, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lcip_pys, ymax = ucip_pys), color = "black") +
  facet_grid(port ~ .) +
  scale_fill_manual(values = c("F" = "light gray"), guide = FALSE) +
  scale_color_manual(values = c("F" = "light gray"), guide = FALSE)  +
  labs(x = "Year", y = "Percent of yelloweye rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Yelloweye Rockfish Sex Composition")
n_sex %>% 
  filter(spp == "Yelloweye") %>%
  pivot_wider(names_from = port, values_from = n) %>%
  arrange(year, user) %>%
  mutate_if(is.numeric, function(x)ifelse(x == 0, NA, x)) %>%
  print(n = 100)

sex_format %>%
  filter(spp == "Dusky/Dark", sex == "F") %>%
  ggplot(aes(x = year, y = p_pys, color = sex, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lcip_pys, ymax = ucip_pys), color = "black") +
  facet_grid(port ~ .) +
  scale_fill_manual(values = c("F" = "light gray"), guide = FALSE) +
  scale_color_manual(values = c("F" = "light gray"), guide = FALSE)  +
  labs(x = "Year", y = "Percent of dusky/dark rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Dusky/Dark Rockfish Sex Composition")
n_sex %>% 
  filter(spp == "Dusky/Dark") %>%
  pivot_wider(names_from = port, values_from = n) %>%
  arrange(year, user) %>%
  mutate_if(is.numeric, function(x)ifelse(x == 0, NA, x)) %>%
  print(n = 100)

sex_format %>%
  filter(spp == "Copper", sex == "F") %>%
  ggplot(aes(x = year, y = p_pys, color = sex, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lcip_pys, ymax = ucip_pys), color = "black") +
  facet_grid(port ~ .) +
  scale_fill_manual(values = c("F" = "light gray"), guide = FALSE) +
  scale_color_manual(values = c("F" = "light gray"), guide = FALSE)  +
  labs(x = "Year", y = "Percent of copper rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Copper Rockfish Sex Composition")

sex_format %>%
  filter(spp == "Quillback", sex == "F") %>%
  ggplot(aes(x = year, y = p_pys, color = sex, fill = sex)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lcip_pys, ymax = ucip_pys), color = "black") +
  facet_grid(port ~ .) +
  scale_fill_manual(values = c("F" = "light gray"), guide = FALSE) +
  scale_color_manual(values = c("F" = "light gray"), guide = FALSE)  +
  labs(x = "Year", y = "Percent of quillback rockfish catch") +
  theme_bw(base_size = 18) +
  ggtitle("GOA Quillback Rockfish Sex Composition")


sex_format %>%
  filter(sex == "F" & spp == "Yelloweye") %>%
  pivot_wider(id_cols = "year", names_from = "port", values_from = "est")

uport = unique(sexcomp$port)
uyear = unique(rfcomp$year)
for(i in 1: length(uport)){
  for(y in 1:length(uyear)){
    chisq.test(sexcomp$user[sexcomp$year == uport[i] & sexcomp$port == uyear[y]],
               sexcomp$sex[sexcomp$year == uport[i] & sexcomp$port == uyear[y]])
  }
}
