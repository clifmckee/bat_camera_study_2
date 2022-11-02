
# PREAMBLE ----------------------------------------------------------------
library(here)
library(knitr)
library(rmarkdown)
here()

rm(list = ls())

# DATA CLEANING SCRIPTS ---------------------------------------------------
clean_date_palm <- TRUE # toggle ON/OFF for importation and cleaning of date palm tree visit data
if(clean_date_palm){
  source("./Code/date_palm_data_compilation.R")
  rmarkdown::render("./Code/bat_date_palm_visits_EDA.Rmd", "html_document")
}

clean_fruit <- TRUE # toggle ON/OFF for importation and cleaning of fruit tree visit data
if(clean_fruit){
  source("./Code/fruit_tree_data_compilation.R")
  rmarkdown::render("./Code/bat_fruit_visits_EDA.Rmd", "html_document")
}

clean_community_survey <- TRUE # toggle ON/OFF for importation and cleaning of community resident survey data
if(clean_community_survey){
  rmarkdown::render("./Code/community_resident_fruit_survey_EDA.Rmd", "html_document")
}

# ANALYSIS AND FIGURES ----------------------------------------------------
render_presentation <- TRUE # toggle ON/OFF for running Kelly's analysis notebook
if(render_presentation){
  rmarkdown::render("./Code/to_present.Rmd", "html_document")
}

# COMPILE DATA ------------------------------------------------------------
compile_data <- TRUE # toggle ON/OFF for importation and cleaning of community resident survey data
if(compile_data){
  source("./Code/visit_analysis.R")
}

# COMPILE FIGURES ---------------------------------------------------------
compile_figs <- TRUE # toggle ON/OFF for creating figures
if(compile_figs){
  
  # clear environment
  rm(list = ls())
  
  # load data
  load(here("Data", "visit_analysis.RData"))
  
  # figure scripts
  source("./Code/tree_mapping_code.R")
  source("./Code/tree_visit_summary_code.R")
  source("./Code/tree_type_visits_comparison.R")
  source("./Code/fruit_consumption_survey_results.R")
}
