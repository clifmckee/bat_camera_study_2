### Code for extracting data from bat visits to date palm trees

# Load libraries
library(readxl)
library(dplyr)
library(tibble)
library(lubridate)
library(tidyr)
library(here)

# Check working directory
here()

# Create column names for date palm data
palm_names <- c("Village_ID",
                "Tree_number",
                "Tree_type",
                "Date_of_observation",
                "Date_of_shaving",
                "Camera_number",
                "Tree_coordinates_east",
                "Tree_coordinates_west",
                "In_H",
                "In_M",
                "In_S",
                "Out_H",
                "Out_M",
                "Out_S",
                "DurStayT",
                "DurContT",
                "Way",
                "Con_place",
                "P_NP",
                "Comments",
                "Number_of_visits",
                "Number_of_stays",
                "Number_of_contaminations",
                "AM_PM")
# Find all file names in year 1 date palm directory
year1_palm_path <- "./Data/Village case control tree data - year 1/Date palm trees"
year1_palm <- list.files(path = year1_palm_path,
                         pattern = "xls")

# Make a blank dataframe for year 1
palm_data_1 <- setNames(data.frame(matrix(ncol = length(palm_names), nrow = 0)),
                        palm_names)

# Loop through all year 1 trees and extract data
for(i in 1:length(year1_palm)){
  palm_dat1 <- read_xls(path = paste0(year1_palm_path, "/", year1_palm[i]),
                   col_names = palm_names,
                   sheet = "Data Extraction",
                   skip = 9) %>%
    filter(rowMeans(is.na(.)) < 1)
  palm_data_1 <- rbind(palm_data_1, palm_dat1)
}

# Find all file names in year 2 date palm directory
year2_palm_path <- "./Data/Village case control tree data - year 2/Date palm trees"
year2_palm <- list.files(path = year2_palm_path,
                         pattern = "xls")

# Make a blank dataframe for year 2
palm_data_2 <- setNames(data.frame(matrix(ncol = length(palm_names), nrow = 0)),
                        palm_names)

# Loop through all year 2 trees and extract data
for(i in 1:length(year2_palm)){
  palm_dat2 <- read_xls(path = paste0(year2_palm_path, "/", year2_palm[61]),
                   col_names = palm_names,
                   sheet = "Data Extraction",
                   skip = 9) %>%
    filter(rowMeans(is.na(.)) < 1)
  palm_data_2 <- rbind(palm_data_2, palm_dat2)
}

# Combine years
palm_data <- rbind(palm_data_1, palm_data_2) %>%
  separate(col = Date_of_observation,
           into = c("Day", "Month", "Year"),
           remove = FALSE) %>%
  mutate(Day = as.numeric(Day),
         Month = as.numeric(Month),
         Year = case_when(Year == 11 ~ 2011,
                          Year == 12 ~ 2012,
                          Year == 13 ~ 2013,
                          Year == 2011 ~ 2011,
                          Year == 2012 ~ 2012,
                          Year == 2013 ~ 2013))

## Additional data checks

checks <- palm_data %>% 
  mutate(issues1 = case_when(is.na(Tree_number) ~ 1, TRUE ~ 0))

checks <- checks %>% 
  mutate(issues2 = case_when(is.na(Number_of_visits) ~ 1, TRUE ~ 0))                         

checks <- checks %>% 
  mutate(issues3 = case_when(is.na(Number_of_stays) ~ 1, TRUE ~ 0))  

checks <- checks %>% 
  mutate(issues4 = case_when(is.na(Number_of_contaminations) ~ 1, TRUE ~ 0))  

final_issues <-  checks %>% 
  filter(issues1 == 1 | issues2 == 1 | issues3 == 1 | issues4 == 1)

write.csv(final_issues, here("Data","date_palm_issues.csv"))

# Check that the number of visits = 1 if duration of stay > 0
flagged <-palm_data %>% 
  filter((is.na(Number_of_visits) | Number_of_visits==0) & DurStayT>0)
# 56 observations

# Replace number of visits as 1 instead of 0
palm_data_update <- palm_data %>% 
  mutate(Number_of_visits = case_when(
    (is.na(Number_of_visits) | Number_of_visits==0) & DurStayT>0 ~ 1, 
    TRUE~Number_of_visits))

# Check that the number of stays = 1 if duration of stay > 1
flagged_1 <- palm_data %>% 
  filter((is.na(Number_of_stays) | Number_of_stays==0) & DurStayT>1)
# 11 observations

# Replace number of stays as 1 instead of 0
palm_data_update <- palm_data_update %>% 
  mutate(Number_of_stays = case_when(
    (is.na(Number_of_stays) | Number_of_stays==0) & DurStayT>1 ~ 1, 
    TRUE~Number_of_stays))


# Check that the number of visits = 1 if the number of stays = 1
flagged_2 <- palm_data_update %>% 
  filter((is.na(Number_of_visits) | Number_of_visits==0) & Number_of_stays==1)
# 0 observations

# Check that the number of contaminations = 1 only if the number of stays = 1
flagged_3 <- palm_data_update %>% 
  filter(Number_of_contaminations==1 & Number_of_stays==0)
# 411 observations

# Replace number of contamination as 0 instead of 1
palm_data_update <- palm_data_update %>% 
  mutate(Number_of_contaminations = case_when(
    Number_of_contaminations==1 & Number_of_stays==0 ~ 0, 
    TRUE~Number_of_contaminations))

# Check that duration of contamination is never larger than duration of stay
flagged_4 <- palm_data_update %>% 
  filter(DurContT>DurStayT)
# 2 observations

# Replace the duration of contamination with the duration of stay
palm_data_update <- palm_data_update %>% 
  mutate(DurContT = case_when(
    DurContT>DurStayT ~ DurStayT, 
    TRUE~DurContT))

# final check for NA values
palm_data_update %>%
  select(Number_of_visits, Number_of_stays, Number_of_contaminations, DurContT, DurStayT) %>% 
  summarise_all(funs(sum(is.na(.))))

flagged_5 <- palm_data_update %>% 
  filter(is.na(Number_of_stays))

palm_data_update <- palm_data_update %>% 
  mutate(Number_of_stays = case_when(
    DurStayT==1 & is.na(Number_of_stays) ~ 0, 
    TRUE~Number_of_stays))

flagged_5 <- palm_data_update %>% 
  filter(is.na(Number_of_contaminations))

palm_data_update <- palm_data_update %>% 
  mutate(Number_of_contaminations = case_when(
    DurContT>1 & is.na(Number_of_contaminations) ~ 1, 
    DurContT==0 & is.na(Number_of_contaminations) ~ 0,
    TRUE~Number_of_contaminations))

# save fruit tree visit data
saveRDS(palm_data_update, file= here("Data", "date_palm_visit_data.RDS"))
