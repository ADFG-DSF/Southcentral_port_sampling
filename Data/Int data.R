library(magrittr)

#stat_areas
stat_areas <- 
  read.csv(".\\Data\\stat_areas.csv", na.strings = "", stringsAsFactors = FALSE) %>%
  setNames(c("stat_area", "CFunit", "SFUnit", "area"))

#Interview data by boat
int_boat <- 
  readxl::read_excel(".\\Data\\dat_int.xls", 
                     range = "dat_int!A2:K38301",
                     col_names = c("year", "port", "hH", "pH", "npH", "H", "fleet", "target", "stat_area", "E", "yH"), 
                     col_types = c("numeric", "text", rep("numeric", 4), rep("text", 2), rep("numeric", 3))) %>%
  dplyr::left_join(stat_areas, by = "stat_area") %>%
  dplyr::mutate(year = as.numeric(year))

lapply(int_boat, table, useNA = "ifany")

#Some Cordova entries
#Not enought to be useable. Delete
table(int_boat$port)
int_boat[int_boat$port == "Cordova", ] %>% print(n = 200)
int_boat <- int_boat[!int_boat$port == "Cordova", ] #38319 before, 38192 after

#Rows with H = NA
table(int_boat$year, is.na(int_boat$H))
table(int_boat$year, is.na(int_boat$hH) & is.na(int_boat$npH) & is.na(int_boat$pH) & is.na(int_boat$H) & is.na(int_boat$yH)) #OK
#Note in early years there is no data in these rows. Delete
int_boat[int_boat$year == 1992 & is.na(int_boat$H), ] %>% print(n = 1000) 
int_boat[int_boat$year == 1993 & is.na(int_boat$H), ] %>% print(n = 1000)
int_boat[int_boat$year == 1994 & is.na(int_boat$H), ] %>% print(n = 1000)
int_boat <- int_boat[!(is.na(int_boat$H) & int_boat$year %in% 1992:1994), ] #38192 before, 37972 after
#Note in later years crews appeared to use NA in place of zero. Change to zero
int_boat[int_boat$year == 2008 & is.na(int_boat$H), ] %>% print(n = 1000)
int_boat[int_boat$year == 2009 & is.na(int_boat$H), ] %>% print(n = 1000)
int_boat[int_boat$year == 2011 & is.na(int_boat$H), ] %>% print(n = 1000)
int_boat$H <- ifelse(is.na(int_boat$H) & int_boat$year %in% c(2008, 2009, 2011), 0, int_boat$H)
table(int_boat$year, is.na(int_boat$H)) #OK
table(int_boat$year, is.na(int_boat$hH) & is.na(int_boat$npH) & is.na(int_boat$pH) & is.na(int_boat$H) & is.na(int_boat$yH)) #OK

#Note Military fleet in Seward
table(int_boat$fleet)
aggregate(E ~ port + fleet, int_boat, sum)
int_boat[int_boat$fleet == "Military", ] %>% print(n = 100)
#Assume Military is Guided (check this with Martin)
int_boat$fleet <- ifelse(int_boat$fleet == "Military", "Charter", int_boat$fleet)
table(int_boat$fleet) #OK

#A few oddball Target fisheries, most of which have some lingcod harvest
table(int_boat$target, useNA = "ifany")
int_boat[int_boat$target %in% "O", ] %>% print(n = 100)
int_boat[int_boat$target %in% "SSK", ] %>% print(n = 100)
int_boat[is.na(int_boat$target), ] %>% print(n = 100)
int_boat$target <- ifelse(int_boat$target %in% c("O", "SSK") | is.na(int_boat$target), "O", int_boat$target)
int_boat$target <- 
  factor(int_boat$target, 
         levels = c("R", "B", "B+S", "H", "L", "O", "S"),
         labels = c("Rockfish", "Bottomfish", "Bottomfish + Salmon", "Halibut", "Lingcod", "Other", "Salmon"))
table(int_boat$target, useNA = "ifany")

# No effort data prior to 2000
lapply(unique(int_boat$port), function(x){aggregate(E ~ year, dat = int_boat[int_boat$port == x, ], sum)})

# note missing areas have bad stat areas or was a halibut trip with no lingcod harvest
table(int_boat$area, int_boat$fleet, useNA = "always")
int_boat[is.na(int_boat$area), ]
int_boat <- int_boat[!is.na(int_boat$area), ] #37972 before, 37960 after

#Stat area errors? (temporary fixes, check with Martin)
table(int_boat$area, int_boat$port)
int_boat[int_boat$port == "Homer" & int_boat$area == "North Gulf", ] #OK
int_boat[int_boat$port == "Seward" & int_boat$area == "Eastern PWS Inside", ]
int_boat$port <- ifelse(int_boat$port == "Seward" & int_boat$area == "Eastern PWS Inside", "Valdez", int_boat$port)
int_boat[int_boat$port == "Valdez" & int_boat$area == "Resurrection Bay", ]
int_boat$port <- ifelse(int_boat$port == "Valdez" & int_boat$area == "Resurrection Bay", "Seward", int_boat$port)
table(int_boat$area, int_boat$port)

saveRDS(int_boat, ".//Data//int_boat.rds")
