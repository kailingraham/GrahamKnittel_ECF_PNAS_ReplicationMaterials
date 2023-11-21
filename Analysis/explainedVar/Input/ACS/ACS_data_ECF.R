# The Roosevelt Project
# Distributed Effects of Climate Policy: A Machine Learning Approach
# Author(s): Green and Knittel

# Code: Saraf (ssaraf@mit.edu), edited by Kailin Graham (kgraham1@mit.edu)
# Technical specifications: R version 4.1.1 (Mac OS Monterey 12.6)
# Last Updated: 09/26/2022

#-------------------------------------------------------------------------------
#
# In this code:
# (1) Load data from American Community Survey
#
#-------------------------------------------------------------------------------
# Clear all before running code
rm(list = ls())

# Set seed for replication
set.seed(092022)

# Creating a packages repository
packages <- c("tidycensus", "tidyverse", "fastDummies", "sf", "units", "foreign", 
              "glmnet", "reshape2", "survey", "haven", "glinternet", 
              "sampleSelection", "data.table", "dplyr", "tidyr", "stringr", 
              "magrittr", "readr", "srvyr", "ggplot2", "rvest", "xml2", "viridis")


# Install ALL packages (Use this step if not already installed)
# lapply(packages, install.packages)

# Load ALL packages
lapply(packages, library, character.only = TRUE)

install.packages("openxlsx")
library(writexl)
library(readxl)

# Set working directory
#setwd("~/Dropbox (MIT)/Project_roosevelt/")

#-------------------------------------------------------------------------------
#
# FIPS Code Data for US states and counties 
#
# (Sourced from ``tidycensus`` package in R)
# Dimensions: 3247 rows and 5 columns
#
#-------------------------------------------------------------------------------
# Extract built-in dataset
data("fips_codes")

# Rename and remove duplicate dataframe
FIPS <- fips_codes %>% rename_all(., .funs = toupper) %>%
  mutate(COUNTY = str_replace_all(COUNTY, "Parish", "County")) %>%
  mutate(COUNTY = str_replace_all(COUNTY, "Borough", "County")) %>%
  mutate(COUNTY = str_replace_all(COUNTY, "city", "County")) %>%
  mutate(COUNTY = str_replace_all(COUNTY, "Census Area", "County")) %>%
  mutate(COUNTY = str_replace_all(COUNTY, "La Salle", "LaSalle"))

 rm(fips_codes)

# List of states from FIPS data-set
states_all <- as.character(FIPS %>% select(c(1)) %>% unique() %>% slice_head(n = 51) %>% pull(STATE))

#-------------------------------------------------------------------------------
#
# American Community Survey (ACS) (2020)
# 5-Year survey data from 2016-2020 on population demographic, income, and
# vehicle use 
#
# References:
# Table IDs: https://www.census.gov/programs-surveys/acs/data/data-tables/table-ids-explained.html
# Table Lists: https://www.census.gov/programs-surveys/acs/technical-documentation/table-shells.2020.html#list-tab-JA0S0NE51ITOL8Y409
#
#-------------------------------------------------------------------------------
# Census API Key (Required the first time only)
#census_api_key("1b5678a1a374e17f9c32c8981fbe81d8f437edc7", install = TRUE)

# Run to reload R environment with API Key without restarting it. (Required the first time only)
#readRenviron("~/.Renviron")

# Census API Key in R environment
Sys.getenv("1b5678a1a374e17f9c32c8981fbe81d8f437edc7")

# Load data on variables in ACS and create lists with relevant variables
### CHANGE TALBEIDS OF INTEREST HERE ###
vars_characters <- 'B01003_001|B06009_|B19001_|B19013_|B24031|B23025_|B06004H_|B17025_|B05006_'
vars_subject <- 'S2403'
vars_profile <- 'DP05'

vars_characters_data <- load_variables(2020, "acs5", cache = TRUE) %>% 
  filter(grepl(vars_characters, name))
vars_characters_list <- as.character(vars_characters_data %>% pull(name))

vars_subject_data <- load_variables(2020, "acs5/subject", cache = TRUE) %>% 
  filter(grepl(vars_subject, name))
vars_subject_list <- as.character(vars_subject_data %>% pull(name))

vars_profile_data <- load_variables(2020, "acs5/profile", cache = TRUE) %>% 
  filter(grepl(vars_profile, name))
vars_profile_list <- as.character(vars_profile_data %>% pull(name))

vars_county_list <- c(vars_characters_list, vars_subject_list, vars_profile_list)

#-------------------------------------------------------------------------------
#
# ACS County Level Data
#
#-------------------------------------------------------------------------------
# Load ACS ``county`` level data for all states and relevant variables
ACS_COUNTY <- get_acs(geography = "county", variables = vars_county_list, year = 2020, state = states_all, output = "wide") %>% 
  select(-ends_with("M"))

# Read in specific variables of interest and their column names from Excel file
# write_xlsx(ACS_COUNTY, "ACS_COUNTY.xlsx")
variable_name <- read_excel("C:/Users/kaili/OneDrive - Massachusetts Institute of Technology/CEEPR/Carbon footprint project/ACS/Data/ACS/ACS_var_names_crosswalk.xlsx")

# Assign column names to variables of interest
ACS_COUNTY_CLEAN <- ACS_COUNTY
setnames(ACS_COUNTY_CLEAN, new = t(variable_name[,'col_name_final']),
         old = t(variable_name[,'col_name_old']), skip_absent = TRUE)
#colnames(ACS_COUNTY) <- t(variable_name[,'col_name_final'])

# Drop columns without assignmed column names (i.e. that are not variables of interest)
ACS_COUNTY_CLEAN <- ACS_COUNTY_CLEAN  %>%  select(-starts_with(c("B0", "B1", "B2", "DP", "S0", "S1", "S2", "C0", "C1"))) #%>% separate(NAME, c("COUNTY", "STATE"), ", ")

# Check if NA values present
colSums(is.na(ACS_COUNTY_CLEAN))

#-------------------------------------------------------------------------------
#
# Save ACS data
#
#-------------------------------------------------------------------------------

write.csv(ACS_COUNTY_CLEAN, "C:/Users/kaili/OneDrive - Massachusetts Institute of Technology/CEEPR/Carbon footprint project/ACS/Data/ACS/ACS_COUNTY_CLEAN.csv")
print("Done")
