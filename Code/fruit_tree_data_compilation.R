### Code for extracting data from bat visits to fruit trees

# Load libraries
library(readxl)
library(dplyr)
library(tibble)
library(lubridate)
library(tidyr)
library(here)

# Check working directory
here()

# Create column names for fruit tree data
fruit_names <- c("Village_ID",
                "Tree_number",
                "Tree_type",
                "Date_of_observation",
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

# Find all file names in year 1 fruit tree directory
year1_fruit_path <- "./Data/Village case control tree data - year 1/Fruit trees"
year1_fruit <- list.files(path = year1_fruit_path,
                         pattern = "xls")

# Make a blank dataframe for year 1
fruit_data_1 <- setNames(data.frame(matrix(ncol = length(fruit_names), nrow = 0)),
                        fruit_names)

# Loop through all year 1 trees and extract data
for(i in 1:length(year1_fruit)){
  fruit_dat1 <- read_xls(path = paste0(year1_fruit_path, "/", year1_fruit[i]),
                   col_names = fruit_names,
                   sheet = "Data Extraction",
                   skip = 9) %>%
    filter(rowMeans(is.na(.)) < 1)
  fruit_data_1 <- rbind(fruit_data_1, fruit_dat1)
}

# Find all file names in year 2 fruit tree directory
year2_fruit_path <- "./Data/Village case control tree data - year 2/Fruit trees"
year2_fruit <- list.files(path = year2_fruit_path,
                         pattern = "xls")

# Make a blank dataframe for year 2
fruit_data_2 <- setNames(data.frame(matrix(ncol = length(fruit_names), nrow = 0)),
                        fruit_names)

# Loop through all year 2 trees and extract data
for(i in 1:length(year2_fruit)){
  fruit_dat2 <- read_xls(path = paste0(year2_fruit_path, "/", year2_fruit[i]),
                   col_names = fruit_names,
                   sheet = "Data Extraction",
                   skip = 9) %>%
    filter(rowMeans(is.na(.)) < 1)
  fruit_data_2 <- rbind(fruit_data_2, fruit_dat2)
}

# Combine years
fruit_data <- rbind(fruit_data_1, fruit_data_2) %>%
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
