library(magrittr)

###Figure 5
readxl::excel_sheets(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls")
H <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Figure 5!M3:S29",
                     col_names = c("year", "Kodiak", "Homer", "Seward", "Whittier", "Valdez", "Total"), 
                     na = c("-")) %>%
  tidyr::gather(port, harvest, -year)
R <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Figure 5!M36:S61",
                     col_names = c("year", "Kodiak", "Homer", "Seward", "Whittier", "Valdez", "Total"), 
                     na = c("--")) %>%
  tidyr::gather(port, released, -year)
SWHS <- 
  dplyr::left_join(H, R, by = c("year", "port")) %>%
  dplyr::filter(port != "Total" & year >= 2003) %>%
  dplyr::mutate(pct = released / (harvest + released),
                port = factor(port,
                              levels = c("Kodiak", "Homer", "Seward", "Whittier", "Valdez")),
                regs = ifelse(port == "Kodiak", "No size restriction", "Minimum size resriction (35in)"))

library(ggplot2)
ggplot(SWHS, aes(x = year, y = pct, color = port)) +
  geom_line(aes(linetype = regs)) +
  geom_point() +
  scale_y_continuous(name = "Percent of Catch Released") +
  scale_x_continuous(name = "Year", breaks = 2003:2017) +
  scale_color_discrete(name = "Port") +
  scale_linetype_discrete(name = "Regulations") +
  theme_gray(base_size = 16)

###Figure 6
readxl::excel_sheets(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls")
Kodiak <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A1!A5:BJ31",
                     col_names = c("age", as.character(2003:2017)), 
                     col_types = c("numeric", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017))))),
                     na = c("-")) %>%
  tidyr::gather(year, pct, -age) %>%
  dplyr::mutate(port = "Kodiak")
Homer <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A1!A37:BJ63",
                     col_names = c("age", as.character(2003:2017)), 
                     col_types = c("numeric", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017))))),
                     na = c("-")) %>%
  tidyr::gather(year, pct, -age) %>%
  dplyr::mutate(port = "Homer")
Seward <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A1!A70:BJ97",
                     col_names = c("age", as.character(2003:2017)), 
                     col_types = c("numeric", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017))))),
                     na = c("-")) %>%
  tidyr::gather(year, pct, -age) %>%
  dplyr::mutate(port = "Seward")
Whittier <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A1!A103:BJ127",
                     col_names = c("age", as.character(2003:2017)), 
                     col_types = c("numeric", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017))))),
                     na = c("-")) %>%
  tidyr::gather(year, pct, -age) %>%
  dplyr::mutate(port = "Whittier")
Valdez <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A1!A133:BJ157",
                     col_names = c("age", as.character(2003:2017)), 
                     col_types = c("numeric", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017))))),
                     na = c("-")) %>%
  tidyr::gather(year, pct, -age) %>%
  dplyr::mutate(port = "Valdez")

rbind(Kodiak, Homer, Seward, Valdez, Whittier) %>%
  dplyr::mutate(port = factor(port,
                              levels = c("Kodiak", "Homer", "Seward", "Whittier", "Valdez"))) %>%
  ggplot(aes(x = year, y = age)) +
    geom_point(aes(size = pct), shape = 1) +
    facet_wrap(~ port, 1, 5) +
    scale_y_continuous(name = "Age", breaks = seq(0, 30, 3)) +
    scale_x_discrete(name = "Year", breaks = seq(2003, 2017, 3)) +
    scale_size_area(name = "Percent", breaks = c(0, .04, .08, .12, .16, .24, .36, .48, .60)) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")



###Figure X: effort by stat area
readxl::excel_sheets(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls")
Hc <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A5!C6:BL29",
                     col_names = c("stat_area", as.character(2003:2017)), 
                     col_types = c("text", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017)))))) %>%
  tidyr::gather(year, pct, -stat_area) %>%
  dplyr::mutate(port = "Homer", 
                fleet = "Charter")

Hp <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A5!C31:BL49",
                     col_names = c("stat_area", as.character(2003:2017)), 
                     col_types = c("text", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017)))))) %>%
  tidyr::gather(year, pct, -stat_area) %>%
  dplyr::mutate(port = "Homer", 
                fleet = "Private")

Kc <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A5!C51:BL66",
                     col_names = c("stat_area", as.character(2003:2017)), 
                     col_types = c("text", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017)))))) %>%
  tidyr::gather(year, pct, -stat_area) %>%
  dplyr::mutate(port = "Kodiak", 
                fleet = "Charter")

Kp <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A5!C68:BL79",
                     col_names = c("stat_area", as.character(2003:2017)), 
                     col_types = c("text", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017)))))) %>%
  tidyr::gather(year, pct, -stat_area) %>%
  dplyr::mutate(port = "Kodiak", 
                fleet = "Private")

Sc <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A5!C81:BL114",
                     col_names = c("stat_area", as.character(2003:2017)), 
                     col_types = c("text", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017)))))) %>%
  tidyr::gather(year, pct, -stat_area) %>%
  dplyr::mutate(port = "Seward", 
                fleet = "Charter")

Sp <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A5!C116:BL144",
                     col_names = c("stat_area", as.character(2003:2017)), 
                     col_types = c("text", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017)))))) %>%
  tidyr::gather(year, pct, -stat_area) %>%
  dplyr::mutate(port = "Seward", 
                fleet = "Private")

Vc <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A5!C146:BL162",
                     col_names = c("stat_area", as.character(2003:2017)), 
                     col_types = c("text", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017)))))) %>%
  tidyr::gather(year, pct, -stat_area) %>%
  dplyr::mutate(port = "Valdez", 
                fleet = "Charter")

Vp <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A5!C164:BL186",
                     col_names = c("stat_area", as.character(2003:2017)), 
                     col_types = c("text", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017)))))) %>%
  tidyr::gather(year, pct, -stat_area) %>%
  dplyr::mutate(port = "Valdez", 
                fleet = "Private")

Wc <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A5!C188:BL212",
                     col_names = c("stat_area", as.character(2003:2017)), 
                     col_types = c("text", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017)))))) %>%
  tidyr::gather(year, pct, -stat_area) %>%
  dplyr::mutate(port = "Whittier", 
                fleet = "Charter",
                pct = ifelse(year == 2008, NA, pct)) #Make zeros in 2008 NA

Wp <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Appendix A5!C214:BL248",
                     col_names = c("stat_area", as.character(2003:2017)), 
                     col_types = c("text", "skip", c(rbind(rep("numeric", length(2003:2017)),  matrix("skip", 3, length(2003:2017)))))) %>%
  tidyr::gather(year, pct, -stat_area) %>%
  dplyr::mutate(port = "Whittier", 
                fleet = "Private")

dat <- 
  rbind(Kc, Kp, Hc, Hp, Sc, Sp, Vc, Vp, Wc, Wp) %>% 
  dplyr::mutate(port = factor(port, levels = c("Kodiak", "Homer", "Seward", "Whittier", "Valdez")),
                fleet = factor(fleet, levels = c("Charter", "Private")))
lapply(c("Kodiak", "Homer", "Seward", "Whittier", "Valdez"),
       function(x){
         temp <- dat[dat$port == x, ]
         
         areas <- unique(temp[temp$pct > 0.10, "stat_area"])[[1]]
         temp$stat_area <- ifelse(temp$stat_area %in% areas, temp$stat_area, "Other area")

        temp %>%
          dplyr::group_by(port, fleet, year, stat_area) %>%
          dplyr::summarise(pct = sum(pct)) %>%
          ggplot(aes(x = as.numeric(year), y = pct, fill = stat_area)) +
            geom_area(color = "white") +
            facet_grid(. ~ fleet) +
            scale_y_continuous(name = "Percent") +
            scale_x_continuous(name = "Year", breaks = seq(2003, 2017, 3)) +
            ggtitle(paste0(x)) +
            theme_bw(base_size = 16) +
            theme(legend.position = "bottom")}
)
# Note 2008 Whittier Charter interpolated
  

###Figure 8: Sex comp
readxl::excel_sheets(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls")
#move calcs to R
sexcomp <- 
  readxl::read_excel(".\\Lingcod report_03-17\\Copy of LingcodTables_03-17_Final.xls", 
                     range = "Figure 8!H5:Z83",
                     col_names = c("port", "year", "n_c", "x_c", "swhs_c", "vswhs_c", "n_p", "x_p", "swhs_p", "vswhs_p"), 
                     col_types = c("text", "text", 
                                   "numeric", "numeric", rep("skip", 3), "numeric", "skip", "numeric",
                                   "skip",
                                   "numeric", "numeric", rep("skip", 3), "numeric", "skip", "numeric")) %>%
  dplyr::filter(!is.na(port)) %>%
  dplyr::mutate(p_c = x_c/n_c,
                vp_c = p_c * (1 - p_c) / n_c,
                p_p = x_p / n_p,
                vp_p = p_p * (1 - p_p) / n_p,
                Hf = p_c * swhs_c + p_p * swhs_p,
                H = (swhs_c + swhs_p),
                propf = Hf/H,
                v_propf = 
                  (1/H)^2 * (
                  (vswhs_c * (p_c * swhs_p - p_p * swhs_p)^2) / H^2 + 
                  (vswhs_p * (p_p * swhs_c - p_c * swhs_c)^2) / H^2 + 
                  vp_c * swhs_c^2 + 
                  vp_p * swhs_p^2),
                logit = log(propf / (1 - propf)),
                logit_lb = logit - 1.96 * sqrt(v_propf / propf^2 / (1 - propf)^2),
                logit_ub = logit + 1.96 * sqrt(v_propf / propf^2 / (1 - propf)^2),
                propf_lb = exp(logit_lb) / (1 + exp(logit_lb)),
                propf_ub = exp(logit_ub) / (1 + exp(logit_ub)))


ggplot(sexcomp, aes(x = as.numeric(year), y = propf, fill = port, color = port)) +
  geom_point(position = "dodge", size = 2) + #geom_col(position = "dodge", alpha = 0.25, width = 1) +
  geom_errorbar(aes(ymin = propf_lb, ymax = propf_ub), position = "dodge", size = 1) +
  facet_grid(. ~ port) +
  scale_x_continuous(name = "Year", breaks = seq(2003, 2017, 4)) +
  scale_y_continuous(name = "Percent Female")  +
  scale_color_discrete(name = "Port")  +
  scale_fill_discrete(name = "Port")  +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")

