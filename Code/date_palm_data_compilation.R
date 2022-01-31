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

# save fruit tree visit data
saveRDS(palm_data, file= here("Data", "date_palm_visit_data.RDS"))
