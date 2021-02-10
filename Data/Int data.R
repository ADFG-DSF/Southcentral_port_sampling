library(magrittr)

#stat_areas
stat_areas <- 
  read.csv(".\\Data\\stat_areas.csv", na.strings = "", stringsAsFactors = FALSE) %>%
  setNames(c("stat_area", "CFunit", "SFUnit", "area"))

#Interview data by boat
int_boat <- 
  readxl::read_excel(".\\Data\\dat_int.xls", 
                     range = "dat_int!A2:L60551",
                     col_names = c("year", "month", "port", "hH", "pH", "npH", "lH", "fleet", "target", "stat_area", "E", "yH"), 
                     col_types = c(rep("numeric", 2), "text", rep("numeric", 4), rep("text", 2), rep("numeric", 3))) %>%
  dplyr::left_join(stat_areas, by = "stat_area") %>%
  dplyr::mutate(year = as.numeric(year))

lapply(int_boat, table, useNA = "ifany")

#Don't use data prior to 1996
int_boat <- int_boat[int_boat$year >= 1996, ] #60,576 before, 54,804 after

#Some Cordova entries
#Not enought to be useable. Delete
table(int_boat$port)
int_boat[int_boat$port == "Cordova", ] %>% print(n = 200)
int_boat <- int_boat[!int_boat$port == "Cordova", ] #54,804 before, 54,665 after

#Rows with H = NA
H_NA <- is.na(int_boat$hH) & is.na(int_boat$npH) & is.na(int_boat$pH) & is.na(int_boat$lH) & is.na(int_boat$yH)
table(int_boat$year, H_NA) #OK

#Martin tells me every question gets asked and we can assume H = NA is a zero.
table(is.na(int_boat$hH), int_boat$year)
int_boat$hH <- ifelse(is.na(int_boat$hH), 0, int_boat$hH)
table(is.na(int_boat$hH), int_boat$year)

table(is.na(int_boat$pH), int_boat$year)
int_boat$pH <- ifelse(is.na(int_boat$pH), 0, int_boat$pH)
table(is.na(int_boat$pH), int_boat$year)

table(is.na(int_boat$npH), int_boat$year)
int_boat$npH <- ifelse(is.na(int_boat$npH), 0, int_boat$npH)
table(is.na(int_boat$npH), int_boat$year)

table(is.na(int_boat$lH), int_boat$year)
int_boat$lH <- ifelse(is.na(int_boat$lH), 0, int_boat$lH)
table(is.na(int_boat$lH), int_boat$year)

table(is.na(int_boat$yH), int_boat$year) #Yelloweye were not included till 2011

int_boat$npyH <- ifelse(int_boat$year <= 2010, int_boat$npH, int_boat$npH + int_boat$yH)
table(is.na(int_boat$npyH), int_boat$year) #OK

sum(is.na(int_boat$hH) & is.na(int_boat$npH) & is.na(int_boat$pH) & is.na(int_boat$lH) & is.na(int_boat$yH))


# Effort asked after 2000
table(is.na(int_boat$E), int_boat$year)
int_boat[is.na(int_boat$E) & int_boat$year >= 2000, ] # I dont think we can do anything about these but keep for harvest

#Some E = 0
int_boat[int_boat$E %in% 0, ] %>% print(n = 100) #Some of these have harvest... Change to E = NA
int_boat$E <- ifelse(int_boat$E == 0, NA, int_boat$E)
int_boat[int_boat$E %in% 0, ] %>% print(n = 100) #OK


#Note Military fleet in Seward
table(int_boat$fleet)
aggregate(E ~ port + fleet, int_boat, sum)
int_boat[int_boat$fleet == "Military", ] %>% print(n = 100)
#Treat Military as Charter
int_boat$fleet <- ifelse(int_boat$fleet == "Military", "Charter", int_boat$fleet)
table(int_boat$fleet) #OK

#A few oddball Target fisheries, most of which have some lingcod harvest
table(int_boat$target, useNA = "ifany")
int_boat[int_boat$target %in% "O", ] %>% print(n = 100)
int_boat[int_boat$target %in% "SSK", ] %>% print(n = 100)
int_boat[is.na(int_boat$target), ] %>% print(n = 100)
int_boat$target <- ifelse(int_boat$target %in% c("O", "SSK") | is.na(int_boat$target), "O", int_boat$target)
table(int_boat$target, useNA = "ifany")
int_boat$target <- 
  factor(int_boat$target, 
         levels = c("R", "B", "B+S", "H", "L", "O", "S"),
         labels = c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Halibut", "Lingcod", "Other", "Salmon"))
table(int_boat$target, useNA = "ifany")

# note missing areas have bad stat areas or was a halibut trip with no lingcod or rockfish harvest
table(int_boat$area, int_boat$fleet, useNA = "always")
int_boat[is.na(int_boat$area), ]
int_boat <- int_boat[!is.na(int_boat$area), ] #54665 before, 54664 after

#Stat area errors? (temporary fixes, check with Martin)
table(int_boat$area, int_boat$port)
int_boat[int_boat$port == "Homer" & int_boat$area == "North Gulf", ] #OK
int_boat[int_boat$port == "Seward" & int_boat$area == "Eastern PWS Inside", ] #Delete
int_boat <- int_boat[!(int_boat$port == "Seward" & int_boat$area == "Eastern PWS Inside"), ] #54664 before, 54663 after
int_boat[int_boat$port == "Valdez" & int_boat$area == "Resurrection Bay", ] #Delete
int_boat <- int_boat[!(int_boat$port == "Valdez" & int_boat$area == "Resurrection Bay"), ] #54663 before, 54662 after
table(int_boat$area, int_boat$port)

saveRDS(int_boat, ".//Data//int_boat.rds")

