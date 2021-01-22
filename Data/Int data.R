library(magrittr)

#stat_areas
stat_areas <- 
  read.csv(".\\Data\\stat_areas.csv", na.strings = "", stringsAsFactors = FALSE) %>%
  setNames(c("stat_area", "CFunit", "SFUnit", "area"))

#Interview data by boat
int_boat <- 
  readxl::read_excel(".\\Data\\dat_int.xls", 
                     range = "dat_int!A2:K38301",
                     col_names = c("year", "port", "hH", "pH", "npH", "lH", "fleet", "target", "stat_area", "E", "yH"), 
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
H_NA <- is.na(int_boat$hH) & is.na(int_boat$npH) & is.na(int_boat$pH) & is.na(int_boat$lH) & is.na(int_boat$yH)
table(int_boat$year, H_NA) #OK
#Note in early years there is no data in these rows. Delete
int_boat[int_boat$year == 1992 & H_NA, ] %>% print(n = 1000) 
int_boat[int_boat$year == 1993 & H_NA, ] %>% print(n = 1000)
int_boat[int_boat$year == 1994 & H_NA, ] %>% print(n = 1000)
#Note in later years crews appeared to use NA in place of zero. Change to zero
int_boat[int_boat$year == 2009 & H_NA, ] %>% print(n = 1000) #mostly salmon trips
int_boat$hH[int_boat$year == 2009 & H_NA & int_boat$target == "H"] <- 0 #Change Halibut trips to H = 0
sum(H_NA)
H_NA <- is.na(int_boat$hH) & is.na(int_boat$npH) & is.na(int_boat$pH) & is.na(int_boat$lH) & is.na(int_boat$yH)
sum(H_NA)
int_boat <- int_boat[!(H_NA & int_boat$year %in% c(1992, 1993, 1994, 2009)), ] #38192 before, 37970 after

#Now there is no record without some harvest of interest.
sum(is.na(int_boat$hH) & is.na(int_boat$npH) & is.na(int_boat$pH) & is.na(int_boat$lH) & is.na(int_boat$yH))
#Interpreting NA as zero once the species was added to the survey
table(int_boat$year, is.na(int_boat$hH)) 
int_boat[is.na(int_boat$hH), ] %>% print(n = 1000)
int_boat$hH <- ifelse(is.na(int_boat$hH), 0, int_boat$hH)
table(int_boat$year, is.na(int_boat$hH)) #OK

table(int_boat$year, is.na(int_boat$pH)) #OK
int_boat[is.na(int_boat$pH) & !(int_boat$year %in% 1992:1994), ] %>% print(n = 1000)
int_boat$pH <- ifelse(is.na(int_boat$pH) & int_boat$year > 1994, 0, int_boat$pH)
table(int_boat$year, is.na(int_boat$pH)) #OK

table(int_boat$year, is.na(int_boat$npH))
int_boat[is.na(int_boat$pH) & !(int_boat$year %in% 1992:1994), ] %>% print(n = 1000)
int_boat$npH <- ifelse(is.na(int_boat$npH) & int_boat$year > 1994, 0, int_boat$npH)
table(int_boat$year, is.na(int_boat$pH)) #OK

table(int_boat$year, is.na(int_boat$lH)) #OK
int_boat[is.na(int_boat$pH) & !(int_boat$year %in% 1992:1994), ] %>% print(n = 1000)
int_boat$lH <- ifelse(is.na(int_boat$lH), 0, int_boat$lH)
table(int_boat$year, is.na(int_boat$lH)) #OK

table(int_boat$year, is.na(int_boat$yH)) #OK

# E = 0
int_boat[int_boat$E %in% 0, ] #Delete
int_boat <- int_boat[!(int_boat$E %in% 0), ] #37970 before, 37953 after

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
         labels = c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Halibut", "Lingcod", "Other", "Salmon"))
table(int_boat$target, useNA = "ifany")

# No effort data prior to 2000
lapply(unique(int_boat$port), function(x){aggregate(E ~ year, dat = int_boat[int_boat$port == x, ], sum)})

# note missing areas have bad stat areas or was a halibut trip with no lingcod harvest
table(int_boat$area, int_boat$fleet, useNA = "always")
int_boat[is.na(int_boat$area), ]
int_boat <- int_boat[!is.na(int_boat$area), ] #37953 before, 37941 after

#Stat area errors? (temporary fixes, check with Martin)
table(int_boat$area, int_boat$port)
int_boat[int_boat$port == "Homer" & int_boat$area == "North Gulf", ] #OK
int_boat[int_boat$port == "Seward" & int_boat$area == "Eastern PWS Inside", ]
int_boat$port <- ifelse(int_boat$port == "Seward" & int_boat$area == "Eastern PWS Inside", "Valdez", int_boat$port)
int_boat[int_boat$port == "Valdez" & int_boat$area == "Resurrection Bay", ]
int_boat$port <- ifelse(int_boat$port == "Valdez" & int_boat$area == "Resurrection Bay", "Seward", int_boat$port)
table(int_boat$area, int_boat$port)

saveRDS(int_boat, ".//Data//int_boat.rds")

