## Load packages
library(haven)
library(tidyverse)
library(ggthemes)
library(broom)
library(rgdal)
library(geosphere)
library(rgeos)
library(here)

# Check working directory
here()

## Read in data
# Case/control village household survey data
survey <- read_dta("./Data/130527_CommunityResidentSurvey.dta")
# Village geographic data
villages <- read_dta("./Data/140408_NipahRiskFactor_VillageCenter_GISDATA_3Jul_using.dta")
# Village center geographic coordinates
village_centers <- readRDS("./Data/villagecenter.RDS")
# Read in Bangladesh shapefile for administrative districts
bd2 <- readOGR(dsn = "./Data/bgd_adm_bbs_20201113_SHP",
               layer = "bgd_admbnda_adm2_bbs_20201113")

## Manipulate data
# Reduce resolution of Bangladesh polygon and convert to data frame
bd2_fort <- tidy(gSimplify(bd2, tol = 0.001))

# Fix some incorrect village ID numbers
survey[survey$dataid == "108704",]$q1_6 <- "1087"
survey[survey$dataid == "304509",]$q1_6 <- "3045"
survey[survey$dataid == "105803",]$q1_6 <- "1058"
survey[survey$dataid == "105806",]$q1_6 <- "1058"

# Correct village survey data
survey_correct <- survey %>%
  # Remove duplicated rows (village 1052 had duplicated participants)
  # For example, `dataid` 105201 showed up twice with two different interview dates
  # The only thing that was different was the interview date, all other recorded data was identical
  filter(!duplicated(survey$dataid)) %>%
  # Correct village ID
  mutate(correct_villid = as.numeric(substring(dataid, 1, 4))) %>%
  # Make new column for case/control group
  mutate(group = substring(correct_villid, 1, 1),
         case_control_group = case_when(group == 1 ~ "control, near",
                                        group == 2 ~ "control, far",
                                        group == 3 ~ "case"))

# Join village geographic data and village center geographic coordinates
villages_correct <- full_join(villages, village_centers, by = c("villid" = "ID"), keep.all = TRUE)

# Join survey and village center data
survey_villages <- full_join(survey_correct, villages_correct, by = c("correct_villid" = "villid"), keep.all = TRUE)

### Example plots ###

ggplot(survey_villages, aes(x = case_control_group, y = log10(q3_1))) +
  geom_boxplot() +
  labs(x = "Case/control group", y = "Number of date palm trees")

survey_villages %>%
  drop_na(q3_1) %>%
  ggplot(aes(x = q3_1)) +
  geom_histogram() +
  facet_wrap(~case_control_group)

survey_villages %>%
  group_by(case_control_group) %>%
  summarize(proportion_households = mean(q3_5, na.rm = TRUE))

survey_villages %>%
  group_by(group) %>%
  summarize(proportion_households = mean(q5_frt1_5_4, na.rm = TRUE))

survey_villages %>%
  group_by(group) %>%
  summarize(proportion_households = mean(q6_3_3, na.rm = TRUE))

ggplot(survey_villages, aes(x = group, y = q6_3_3)) +
  geom_boxplot() +
  labs(x = "Case/control group", y = "Number of banana trees")

## Additional data checks

# Check that villages only have one village type listed
survey_villages_1 <- survey_villages %>% 
  select(q1_6,case_control_group) %>% 
  rename(Village_ID = q1_6)

survey_villages_1 <- survey_villages_1 %>% 
  transform(survey_villages_1, Village_ID = as.numeric(Village_ID))

survey_villages_1 <- survey_villages_1[!duplicated(survey_villages_1), ] 

survey_villages_1 <- na.omit(survey_villages_1)

survey_villages_1 <- survey_villages_1 %>% 
  mutate("dup" = duplicated(survey_villages_1$Village_ID))

# village 2045 has two different village types listed --> RESOLVED
# village 2087 has two different village types listed --> RESOLVED
# village 3058 has two different village types listed --> RESOLVED

# save community resident fruit survey
saveRDS(survey_villages, file = here("Data", "community_resident_fruit_survey.RDS"))
