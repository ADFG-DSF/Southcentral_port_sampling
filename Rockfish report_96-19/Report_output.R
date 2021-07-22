source(".\\functions.R")
library(magrittr)
library(ggplot2)

#input loocv log likelihoods
ll_Hp <- readRDS(".\\Rockfish report_96-19\\Interview post\\ll_Hp.rds")
ll_Hnpy <- readRDS(".\\Rockfish report_96-19\\Interview post\\ll_Hnpy.rds")
ll_E <- readRDS(".\\Rockfish report_96-19\\Interview post\\ll_E.rds")

#tables with model selection criteria. One for each composition
tab_select <-
  Map(function(x, y){tab_ll(x) %>% dplyr::mutate(port = y, composition = "Harvest: Pelagic")}, 
      x = ll_Hp,
      y = factor(c("Homer", "Kodiak", "Seward", "Valdez", "Whittier"), ordered = TRUE)) %>%
  do.call(rbind, .)
tab_select_npy <-
  Map(function(x, y){tab_ll(x) %>% dplyr::mutate(port = y, composition = "Harvest: Non-Pelagic")}, 
      x = ll_Hnpy,
      y = factor(c("Homer", "Kodiak", "Seward", "Valdez", "Whittier"), ordered = TRUE)) %>%
  do.call(rbind, .)
tab_select_E <-
  Map(function(x, y){tab_ll(x) %>% dplyr::mutate(port = y, composition = "Effort")}, 
      x = ll_E,
      y = factor(c("Homer", "Kodiak", "Seward", "Valdez", "Whittier"), ordered = TRUE)) %>%
  do.call(rbind, .)

#input posteriors
postHp_Homer <- readRDS(".\\Rockfish report_96-19\\Interview post\\postH_HomerCharter.rds")
postHp_Kodiak <- readRDS(".\\Rockfish report_96-19\\Interview post\\postH_Kodiak.rds")
postHp_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\postH_Seward.rds")
postHp_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\postH_Valdez.rds")
postHp_Whittier <- readRDS(".\\Rockfish report_96-19\\Interview post\\postH_Whittier.rds")
postHnp_Homer <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_HomerCharter.rds")
postHnp_Kodiak <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Kodiak.rds")
postHnp_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Seward.rds")
postHnp_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Valdez.rds")
postHnp_Whittier <- readRDS(".\\Rockfish report_96-19\\Interview post\\postHnp_Whittier.rds")
postE_Homer <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Homer.rds")
postE_Kodiak <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Kodiak.rds")
postE_Seward <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Seward.rds")
postE_Valdez <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Valdez.rds")
postE_Whittier <- readRDS(".\\Rockfish report_96-19\\Interview post\\postE_Whittier.rds")

#table with random effect standard deviations
post_list <- list(postHp_Homer=postHp_Homer, postHnp_Homer=postHnp_Homer, postE_Homer=postE_Homer, 
                  postHp_Kodiak=postHp_Kodiak, postHnp_Kodiak=postHnp_Kodiak, postE_Kodiak=postE_Kodiak, 
                  postHp_Seward=postHp_Seward, postHnp_Seward=postHnp_Seward, postE_Seward=postE_Seward, 
                  postHp_Valdez=postHp_Valdez, postHnp_Valdez=postHnp_Valdez, postE_Valdez=postE_Valdez, 
                  postHp_Whittier=postHp_Whittier, postHnp_Whittier=postHnp_Whittier, postE_Whittier=postE_Whittier)


tab_sd <- 
  lapply(seq_along(post_list), get_sd, posts = post_list, obj_names = names(post_list)) %>% 
  do.call(rbind, .)

#report appendix for composition data with model selection and RE sigma
out_select <- 
  list(tab_select, tab_select_npy, tab_select_E) %>% 
  do.call(rbind, .) %>% 
  dplyr::arrange(port, desc(composition)) %>%
  dplyr::select(port, composition, model, ll, diff, p_diff) %>%
  dplyr::left_join(tab_sd, by = c("port", "composition", "model"))
WriteXLS::WriteXLS(out_select, ".\\Rockfish report_96-19\\select_mods.xlsx")

