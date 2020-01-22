# load libraries
library(dplyr)
library(lubridate)
library(readxl)
library(cleaninginspectoR)
library(readr)

`%notin%` <- Negate(`%in%`)
# read utf8 csv
raw_data <- read.csv("data_cleaning/raw/AFG1904_Emergency SNFI & Winterisation Assessment - all versions - False - 2019-12-30-09-51-49.csv", stringsAsFactors = F, encoding = 'UTF-8', na.strings = c("", "NA"))
# correct the first column name
raw_data <- raw_data %>%
  dplyr::rename(
    start = X.U.FEFF.start
  ) 

# read cleaning log
cleaning_log <- read_excel("data_cleaning/cleaning_log/reach_afg_shelter2019_cleaninglog3_AT.xlsx") %>% 
  filter(Column != "entire entry")

# read cleaning log and get invalid interviews
cleaning_log_invalid_interviews <- read_excel("data_cleaning/cleaning_log/reach_afg_shelter2019_cleaninglog3.xlsx") %>% 
  filter(Column == "entire entry")


# creat a columns for interview duration 
raw_data <- raw_data %>% 
  mutate(
    start = ymd_hms(start), 
    end = ymd_hms(end),
    inter_duration = as.POSIXct(end) - as.POSIXct(start),
    Valid = case_when(
      inter_duration > 20 & consent == "yes" ~ "Valid",
      TRUE ~ "Deleted"
    )
  )   
  
# Apply cleaning log on raw data
for (rowi in 1:nrow(cleaning_log)){
  uuid_i <- cleaning_log$uuid[rowi]
  var_i <- cleaning_log$Column[rowi]
  old_i <- cleaning_log$`Old Value`[rowi]
  new_i <- cleaning_log$`New Value`[rowi]
  cat("\014")
  print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  raw_data[raw_data$X_uuid == uuid_i, var_i] <- new_i
}


# short interviews
short_interviews <- raw_data %>% 
  filter(Valid == "Deleted")

# Get invalid interviews uuids from cleaning long
invalid_int_uuid <- cleaning_log_invalid_interviews$uuid

# filter out invalid interviews
clean_data <- raw_data %>% 
  filter(X_uuid %notin% invalid_int_uuid & Valid == "Valid") %>% 
  select(-c(inter_duration,Valid))


# Data inspection
inspection_result <-  inspect_all(clean_data, uuid.column.name = "X_uuid" )
write_excel_csv(inspection_result, "data_cleaning/data_cleaning_output/Shelter_unhcr_2019_Outliers.csv")

# write clean data 
write_excel_csv(clean_data, "input/data/clean/Shelter_unhcr_2019_clean_data.csv")
write_excel_csv(clean_data, "data_cleaning/data_cleaning_output//Shelter_unhcr_2019_clean_data.csv")
# write short interviews
write_excel_csv(short_interviews, "data_cleaning/data_cleaning_output//Shelter_unhcr_2019_short_intervies.csv")

 



