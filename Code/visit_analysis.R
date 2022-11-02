# libraries
library(here)
library(tidyverse)
library(lubridate)
library(cowplot)
library(sf)
library(rgeos)
library(GGally)

# load data
date_palm_vil <- readRDS(here("Data", "date_palm_with_villages.RDS"))
fruit_data_vil <- readRDS(here("Data", "fruit_tree_with_villages.RDS"))
fruit_survey <- readRDS(here("Data", "community_resident_fruit_survey.RDS"))
roosts <- readRDS(here("Data", "master_coord.RDS"))

names(fruit_survey)[names(fruit_survey) == 'correct_villid'] <- "Village_ID"

# Read in Bangladesh shapefile for country border
bd0 <- read_sf(dsn = "./Data/bgd_adm_bbs_20201113_SHP",
               layer = "bgd_admbnda_adm0_bbs_20201113")
bd0_sp <- methods::as(object = bd0, Class = "Spatial")
bd0_fort <- fortify(gSimplify(bd0_sp, tol = 0.001))

# Read in Bangladesh shapefile for divisions
bd1 <- read_sf(dsn = "./Data/bgd_adm_bbs_20201113_SHP",
               layer = "bgd_admbnda_adm1_bbs_20201113")
bd1_sp <- methods::as(object = bd1, Class = "Spatial")
bd1_fort <- fortify(gSimplify(bd1_sp, tol = 0.001))

# Read in Bangladesh shapefile for districts
bd2 <- read_sf(dsn = "./Data/bgd_adm_bbs_20201113_SHP",
               layer = "bgd_admbnda_adm2_bbs_20201113")
bd2_sp <- methods::as(object = bd2, Class = "Spatial")
bd2_fort <- fortify(gSimplify(bd2_sp, tol = 0.001))

# roosts1 <- roosts
# 
# coordinates(roosts1) <- ~ Roost_long + Roost_lat
# proj4string(roosts1) <- proj4string(bd2_sp)
# 
# faridpur <- bd2_sp[bd2_sp$ADM2_EN == "Faridpur",]
# roosts_in_faridpur <- over(roosts1, faridpur, returnList = FALSE)
# roosts2 <- cbind(roosts, roosts_in_faridpur) %>%
#   filter(ADM2_EN == "Faridpur")
# 
# ggplot() +
#   geom_polygon(data = bd2_fort, aes(x = long, y = lat, group = group)) +
#   geom_point(data = roosts2, aes(x = Roost_long, y = Roost_lat), shape = ".")
# write.csv(roosts2, "./Data/Faridpur_roosts.csv")

# Create combined dataset of all villages included in study with number of fruit trees and date palms visited

# fix tree type in date palm data
unique(date_palm_vil$Tree_type)
date_palm_vil <- date_palm_vil %>% 
  dplyr::select(-Tree_type) %>% 
  mutate(Tree_type = "Date palm")

# community survey summary
cs.total <- fruit_survey %>% 
  group_by(Village_ID, Center_lat, Center_long) %>% 
  summarise(num_survey = n()) %>% 
  left_join(fruit_survey %>% dplyr::select(Village_ID, case_control_group)) %>% 
  distinct()

# replace some missing values
cs.total[cs.total$Village_ID == "2033",]$Center_lat <- 22.726322
cs.total[cs.total$Village_ID == "2033",]$Center_long <- 92.080936
cs.total[cs.total$Village_ID == "3037",]$Center_lat <- 23.640984962679507
cs.total[cs.total$Village_ID == "3037",]$Center_long <- 89.60317562244902

# fruit camera village summary
f.total <- fruit_data_vil %>% 
  group_by(Village_ID) %>% 
  summarize(f.num_tree = n_distinct(Tree_number),
            f.num_visits = sum(Number_of_visits),
            f.avg_cont_dur = mean(ifelse(DurContT > 0,
                                         DurContT, NA), na.rm = T), 
            f.cum_cont_dur = sum(ifelse(DurContT > 0,
                                        DurContT, NA), na.rm = T)) %>%  
  mutate(f.yn_visit = case_when(f.num_visits > 0 ~ 1, TRUE ~ 0))
f.total$f.num_trees_w_visits <- fruit_data_vil %>%
  group_by(Village_ID, Tree_number) %>%
  summarize(f.tree_visits = sum(Number_of_visits),
            f.trees_w_visits = sum(f.tree_visits > 0)) %>%
  group_by(Village_ID) %>%
  summarize(f.num_trees_w_visits = sum(f.trees_w_visits > 0)) %>%
  pull(f.num_trees_w_visits)

# date palm camera villages summary
dp.total <- date_palm_vil %>% 
  group_by(Village_ID) %>% 
  summarize(dp.num_tree = n_distinct(Tree_number), 
            dp.num_visits = sum(Number_of_visits), 
            dp.avg_cont_dur = mean(ifelse(DurContT > 0, 
                                          DurContT, NA), na.rm = T), 
            dp.cum_cont_dur = sum(ifelse(DurContT > 0, 
                                         DurContT, NA), na.rm = T)) %>% 
  mutate(dp.yn_visit = case_when(dp.num_visits > 0 ~ 1, TRUE ~ 0))
dp.total$dp.num_trees_w_visits <- date_palm_vil %>%
  group_by(Village_ID, Tree_number) %>%
  summarize(dp.tree_visits = sum(Number_of_visits),
            dp.trees_w_visits = sum(dp.tree_visits > 0)) %>%
  group_by(Village_ID) %>%
  summarize(dp.num_trees_w_visits = sum(dp.trees_w_visits > 0)) %>%
  pull(dp.num_trees_w_visits)

vil.total <- full_join(cs.total, dp.total, by="Village_ID") 
vil.total <- full_join(vil.total, f.total, by="Village_ID") 
vil.total <- vil.total %>% 
  mutate(dp.yn = case_when(is.na(dp.num_tree) ~ 0,
                           TRUE ~ 1), 
         f.yn = case_when(is.na(f.num_tree) ~ 0,
                          TRUE ~ 1), 
         cs.yn = 1) %>% 
  mutate(Trees_observed = case_when((dp.yn == 0 & f.yn == 0) ~ "No trees", 
                                    (dp.yn == 1 & f.yn == 0) ~ "Date palm only", 
                                    (dp.yn == 0 & f.yn == 1) ~ "Fruit tree only",
                                    TRUE ~ "Both"))

vil.total <- vil.total %>%
  mutate(yn_visits = case_when(f.yn_visit == 1 & (dp.yn_visit == 0 | is.na(dp.yn_visit)) ~ "fruit",
                               dp.yn_visit == 1 & (f.yn_visit == 0 | is.na(f.yn_visit)) ~ "date palm",
                               f.yn_visit == 1 & dp.yn_visit == 1 ~ "both",
                               Trees_observed == "No trees" ~ "no trees",
                               TRUE ~ "no visits")) %>%
  replace_na(list(dp.num_tree = 0, f.num_tree = 0, dp.num_trees_w_visits = 0, f.num_trees_w_visits = 0)) %>%
  mutate(total_trees = dp.num_tree + f.num_tree,
         total_trees_w_visits = dp.num_trees_w_visits + f.num_trees_w_visits)

# create long table version
vil.total_long <- vil.total %>% 
  subset(select = -c(num_survey, dp.num_tree, f.num_tree)) %>% 
  gather(col_type, col_yn, dp.yn:cs.yn, factor_key = TRUE)

vil.total$case_control_group_f = factor(vil.total$case_control_group, levels=c("case", "control, near", "control, far"))
vil.total_long$case_control_group_f = factor(vil.total_long$case_control_group, levels=c("case", "control, near", "control, far"))

# dataframe with all bat visit data
date_palm_vil1 <- date_palm_vil %>% 
  dplyr::select(-Date_of_shaving) %>% 
  mutate(dp_or_fruit = "Date palm")

fruit_data_vil1 <- fruit_data_vil %>% 
  mutate(dp_or_fruit = "Fruit")

all_data <- rbind(date_palm_vil1, fruit_data_vil1) %>% 
  left_join(vil.total %>% dplyr::select(Village_ID, Trees_observed, yn_visits))

all_data$case_control_group_f = factor(all_data$case_control_group, levels=c("case", "control, near", "control, far"))

rm(date_palm_vil1)
rm(fruit_data_vil1)

#  Total villages included in study, by village type
vil.total_long %>% 
  group_by(case_control_group_f) %>% 
  summarize(num_vil = n_distinct(Village_ID)) %>% 
  mutate(total_vil = sum(num_vil))

# number of villages with each type of data collection
vil.total_long %>% 
  group_by(col_type) %>% 
  summarize(vil_observed = sum(col_yn)) %>% 
  mutate(vil_not_observed = 206 - vil_observed,
         total_vil = 206, 
         "perc_vil" = vil_observed/206*100)

save.image(file = "data/visit_analysis.RData")
