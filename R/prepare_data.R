# Author: Sayed Farhad Nabizada
# Date: 19/Dec/2019

# load libraries
library(tidyverse)
library(lubridate)
library(parallel)
library(readxl)


# Takes in data and a list of vars in the format
# c("var1", "var2", "var3", "var4") and spits out 
# the sum of the row ignoring missing values
row_sum <- function(data, vars) {
  rowSums(data[vars], na.rm = TRUE)
  
}


# load data, creates strata and cluster id vars
data <- read.csv("input/data/clean/Shelter_unhcr_2019_clean_data.csv", stringsAsFactors = F, 
                na.strings = c("", "NA"))

# column names without groups
colname_df <- read.csv("input/col_names/colnames.csv")
names(data) <- colname_df$names_without_groups


data <- data %>%
  mutate(
    # households by strata
    displacement_status = case_when(
      area_origin == "no" ~ "idps",
      area_origin == "yes" & damage == "yes" ~ 'natural_disaster_affected',
      area_origin == "yes" & damage == "no" ~ "host",
      TRUE ~ "Unknown"
    ),
    strata = stringr::str_c(province, "_", displacement_status),
    cluster_id = str_c(displacement_status, "_", province, "_", village)
  )

# hh adults
hh_adults_vars <- c(
  "male_18_49",
  "male_50_64",
  "female_18_49",
  "female_50_64"
)

data$hh_adults <- row_sum(data, hh_adults_vars)


# new indicators
data <- data %>% 
  mutate(
    hoh_gender_final = case_when(
      resp_hoh == "yes" ~ resp_gender,
      TRUE ~ hoh_gender
    ),
    # female heads-of-household
    female_hhh = case_when(
      (resp_gender == "female" & resp_hoh == "yes") | hoh_gender == "female" ~ "yes",
      TRUE ~ "no"
    ),
    
    # elderly heads-of-household
    elderly_hhh = case_when(
      (resp_age >= 65 & resp_hoh == "yes") | hoh_age >= 65 ~ "yes",
      TRUE ~ "no"
    ),
    
    # hhs which contain elderly members
    hh_contain_elderly = case_when(
      male_65_over > 0 | female_65_over > 0 ~ "yes",
      TRUE ~ "no"
    ),
    # households containing children under 5
    hh_contain_child_under_5 = case_when(
      male_less_1 > 0 | female_less_1 > 0 | male_1_4 > 0 | female_1_4 > 0 ~ "yes",
      TRUE ~ "no"
    ),
    # households with female or child head of household, and containing no adult male
    female_child_hoh_no_adult = case_when(
      ((((resp_gender == "female" & resp_hoh == "yes") | hoh_gender == "female") | 
        hoh_age <= 18 ) & (male_18_49 == 0 | female_18_49 == 0)) & cash_flow.remittances == 0 ~ "yes",
      TRUE ~ "no"
    ),
    # dependancy ratio
    dep_ratio_score = case_when(
      hh_adults == 0 ~ 0,
      TRUE ~ household_size / hh_adults
    ),
    # households with dependancy ratio of 8 or more
    dep_ration_8_or_more = case_when(
      dep_ratio_score >= 8 ~ "yes",
      TRUE ~ "no"
    ),
    # households with no adult male of working age or adult working woman. *
    hhs_no_adult_male_working = case_when(
      male_18_49 == 0 & male_50_64 == 0 & breadwinner == 0 ~ "yes",
      TRUE ~ "no"
    ),
    # households without any source of livelihood or income generating activities
    hhs_no_src_livelihood = case_when(
      breadwinner == 0 ~ "yes",
      TRUE ~ "no"
    ),
    # households relying on casual labour by one member
    hh_relying_on_casual_labour = case_when(
      income_source == "unskilled" ~ "yes",
      TRUE ~ "no"
    ),
    # households displaced outside their province of origin
    displaced_outside_prov_org = case_when(
      province != displace_province ~ "yes",
      displacement_status == "host" ~ NA_character_,
      TRUE ~ "no"
    ),
    # # hosueholds living in unsafe shelter
    hhs_living_unsafe_shelter = case_when(
      shelter_type == "tent" | shelter_type == "makeshift"| shelter_type == "collective_centre" |
        shelter_type == "open_space" | shelter_type == "unfinished" | shelter_type == "damaged_house" ~ "yes",
      TRUE ~ "no"
    ),
    # households reported as badly or fully damaged
    report_bad_full_dmg_shelter = case_when(
      shelter_damage_overall == "bad" | shelter_damage_overall == "destroyed" ~ "yes",
      TRUE ~ "no"
    ), 
    # households reported with badly or fully damaged roofs
    report_bad_full_dmg_roof = case_when(
      shelter_damage_roof == "bad" | shelter_damage_roof == "destroyed" ~ "yes",
      TRUE ~ "no"
    ),
    #  households reported with badly or fully damaged walls
    report_bad_full_dmg_walls = case_when(
      shelter_damage_walls == "bad" | shelter_damage_walls == "destroyed" ~ "yes",
      TRUE ~ "no"
    ),
    # hosueholds reporting having an insecure tenure agreement
    insecure_tenure_agreement = case_when(
      tenure == "rental_verbal" |  tenure == "none" ~ "yes",
      TRUE ~ "no"
    ),
    # non-displaced households hosting IDPs, by type of shelter
    hosting_idps_shelter_type = case_when(
      hosting == "yes" ~ paste0("hosting in: ",shelter_type),
      TRUE ~ NA_character_
    ),
     # households residing with or hosting other households
    hhs_hosted = case_when(
      arrangement == "hosted" | hosting == "yes" ~ "yes",
      TRUE ~ "no"
    ),
    # households reporting facing challenges with ESNFI aid
    facing_challenge_esnfi = case_when(
      material_barriers.accessing == 1 | material_barriers.receiving == 1 | material_barriers.using == 1 ~ "yes",
      material_barriers.none == 1 ~ "no",
      is.na(material_barriers) ~ NA_character_
    ),
    # households relying on borrowing, begging or social charity for coping
    relying_on_borrow_beg_charity = case_when(
      material_coping.loans == 1 | material_coping.borrow_materials == 1 | material_coping.scavenge == 1 ~ "yes", 
      material_coping.sell_household == 1 | material_coping.sell_land == 1 | material_coping.reduce_expenses == 1 | 
        material_coping.reduce_food == 1 | material_coping.divert_money == 1 | material_coping.sell_assets == 1 |
        material_coping.adult_labour == 1 | material_coping.child_labour == 1 | material_coping.none == 1 |
        material_coping.other == 1 ~ "no",
      is.na(material_coping) ~ NA_character_
    ),
    # households reporting challenges in accessing shelter of NFI material at markets in the last 3 months
    challenge_access_shelter_nfi = case_when(
      material_access == "yes" | nfi_access == "yes" ~ "yes",
      material_access == "no" | nfi_access == "no" ~ "no",
      TRUE ~ NA_character_
    ),
    # households with poor asset holdings
    poor_asset_holdings = case_when(
    nfi_own_pots == "no" & nfi_own_fuel == "no" & nfi_own_jacket == "no" & nfi_own_shoes == "no" &
      nfi_own_caps == "no" & nfi_own_gloves == "no" ~ "yes",
    TRUE ~ "no"
    ),
    # households reporting ownership of all key NFI
    own_all_key_nfi = case_when(
      nfi_own_mattress == "yes" & nfi_own_pots == "yes" & nfi_own_containers == "yes" &
        nfi_own_solarlamp == "yes" & nfi_own_fuel == "yes" & nfi_own_tarpaulin == "yes" &
        nfi_own_jacket == "yes" & nfi_own_shoes == "yes" & nfi_own_caps == "yes" & 
        nfi_own_gloves == "yes" ~ "yes",
      TRUE ~ "no"
    ),
    # households reporting ownership of no key NFI 
    own_no_key_nfi = case_when(
      nfi_own_mattress == "no" & nfi_own_pots == "no" & nfi_own_containers == "no" &
        nfi_own_solarlamp == "no" & nfi_own_fuel == "no" & nfi_own_tarpaulin == "no" &
        nfi_own_jacket == "no" & nfi_own_shoes == "no" & nfi_own_caps == "no" & 
        nfi_own_gloves == "no" ~ "yes",
      TRUE ~ "no"
    ),
    # households reporting ownership of all key winter clothing
    own_all_key_winter_clothing = case_when(
      nfi_own_jacket == "yes" & nfi_own_shoes == "yes" & nfi_own_caps == "yes" &
        nfi_own_gloves == "yes" ~ "yes",
      TRUE ~ "no"
    ),
    # households reporting ownership of no key winter clothing
    own_no_key_winter_clothing = case_when(
      nfi_own_jacket == "no" & nfi_own_shoes == "no" & nfi_own_caps == "no" &
        nfi_own_gloves == "no" ~ "yes",
      TRUE ~ "no"
    ),
    # households reporting fuel as unavailable in their nearest market
    fuel_unavailable_nearst_market = case_when(
      market_find_fuel == "impossible" ~ "yes", 
      TRUE ~ "no"
    ),
    # hh reporting main items as available in market
    main_items_available_in_market = case_when(
      (market_find_timber == "easy" | market_find_timber == "possible" | market_find_timber == "difficult") &
        (market_find_glass == "easy" | market_find_glass == "possible" | market_find_glass == "difficult") &
        (market_find_fuel == "easy" | market_find_fuel == "possible" | market_find_fuel == "difficult") &
        (market_find_blanket == "easy" | market_find_blanket == "possible" | market_find_blanket == "difficult") &
        (market_find_coat == "easy" | market_find_coat == "possible" | market_find_coat == "difficult")  ~ "yes",
      TRUE ~ "no"
    ),
    # hh reporting ESNFI as a top 3 priority need
    esnfi_as_priority_need = case_when(
      priority_need.winter == 1 | priority_need.shelter == 1 | priority_need.nfi == 1 ~ "yes",
      TRUE ~ "no"
    ),
    # household members proportion
    hh_size_prop = case_when(
      household_size == 1 ~ "1",
      household_size == 2 ~ "2",
      household_size == 3 ~ "3",
      household_size == 4 ~ "4",
      household_size == 5 ~ "5",
      household_size == 6 ~ "6",
      household_size == 7 ~ "7",
      household_size == 8 ~ "8",
      household_size == 9 ~ "9",
      household_size > 9 ~ "10+",
      TRUE ~ NA_character_
    ),
    # disability proportion
    disability_prop = case_when(
      disability == 0 ~ "0",
      disability == 1 ~ "1",
      disability == 2 ~ "2",
      disability > 2 ~ "3+",
      TRUE ~ NA_character_
    ),
    # pregnant proportion
    pregnant_prop = case_when(
      pregnant == 0 ~ "0",
      pregnant == 1 ~ "1",
      pregnant == 2 ~ "2",
      pregnant > 2 ~ "3+",
      TRUE ~ NA_character_
    ), 
    # breastfeeding proportion
    breastfeeding_prop = case_when(
      breastfeeding == 0 ~ "0",
      breastfeeding == 1 ~ "1",
      breastfeeding == 2 ~ "2",
      breastfeeding > 2 ~ "3+",
      TRUE ~ NA_character_
    ),
    # breadwinner proportion
    breadwinner_prop = case_when(
      breadwinner == 0 ~ "0",
      breadwinner == 1 ~ "1",
      breadwinner == 2 ~ "2",
      breadwinner > 2 ~ "3+",
      TRUE ~ NA_character_
    ),
    # living space proportion
    living_space_prop = case_when(
      living_space == 0 ~ "0",
      living_space == 1 ~ "1",
      living_space == 2 ~ "2",
      living_space == 3 ~ "3",
      living_space == 4 ~ "4",
      living_space > 4 ~ "5+",
      TRUE ~ NA_character_
      
    )
  )

# ESNFI severity

data <- data %>% 
  mutate(
    female_child_hoh_no_adult_class = case_when(
      female_child_hoh_no_adult == "yes" ~ 3,
      female_child_hoh_no_adult == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    dep_ration_8_or_more_class = case_when(
      dep_ration_8_or_more == "yes" ~ 3,
      dep_ration_8_or_more == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    hhs_no_adult_male_working_class = case_when(
      hhs_no_adult_male_working == "yes" ~ 2,
      hhs_no_adult_male_working == "no" ~ 0
    ),
    elderly_hhh_class = case_when(
      elderly_hhh == "yes" ~ 3,
      elderly_hhh == "no" ~ 0
    ),
    poor_asset_holdings_class = case_when(
      poor_asset_holdings == "yes" ~ 1,
      poor_asset_holdings == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    hhs_hosted_class = case_when(
      hhs_hosted == "yes" ~ 1,
      hhs_hosted == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    hhs_living_unsafe_shelter_class = case_when(
      hhs_living_unsafe_shelter == "yes" ~ 3,
      hhs_living_unsafe_shelter == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    
    relying_on_borrow_beg_charity_class = case_when(
      relying_on_borrow_beg_charity == "yes" ~ 3,
      relying_on_borrow_beg_charity == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    hh_relying_on_casual_labour_class = case_when(
      hh_relying_on_casual_labour == "yes" ~ 1,
      hh_relying_on_casual_labour == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    hhs_no_src_livelihood_class = case_when(
      hhs_no_src_livelihood == "yes" ~ 2,
      hhs_no_src_livelihood == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    # female_hhh_class = case_when(
    #   female_hhh == "yes" ~ 3,
    #   female_hhh == "no" ~ 0,
    #   TRUE ~ NA_real_
    # ),
    disability_prop_class = case_when(
      disability_prop == "0" ~ 0,
      disability_prop == "1" ~ 2,
      disability_prop == "2" ~ 2,
      disability_prop == "3+" ~ 2,
      TRUE ~ NA_real_
    )
  )

# esnfi vul indicator vars
esnfi_vul_indicator_vars <- c(
  "female_child_hoh_no_adult_class",
  "dep_ration_8_or_more_class",
  "hhs_no_adult_male_working_class",
  "elderly_hhh_class",
  "poor_asset_holdings_class",
  "hhs_hosted_class",
  "hhs_living_unsafe_shelter_class",
  "relying_on_borrow_beg_charity_class",
  "hh_relying_on_casual_labour_class",
  "hhs_no_src_livelihood_class",
  "disability_prop_class"
)

data$esnfi_vul_indicator_score <- row_sum(data, esnfi_vul_indicator_vars)

data <- data %>% 
  mutate(
    esnfi_vul_indicator = case_when(
      esnfi_vul_indicator_score <= 8 ~ "Not_Sufficiently_Vulnerable",
      esnfi_vul_indicator_score > 8 & esnfi_vul_indicator_score < 17 ~ "Vulnerable",
      esnfi_vul_indicator_score > 16 ~ "Most_Vulnerable"
    )
  )

# demogarphics 
data$perc_male_less_1 <- (data$male_less_1  / data$household_size) * 100 
data$perc_female_less_1 <- (data$female_less_1 / data$household_size) * 100
data$perc_male_1_4 <- (data$male_1_4 / data$household_size) *100
data$perc_female_1_4 <- (data$female_1_4 / data$household_size) * 100
data$perc_male_5_15 <- (data$male_5_15 / data$household_size) * 100
data$perc_female_5_15 <- (data$female_5_15 / data$household_size) * 100
data$perc_male_16_17 <- (data$male_16_17 / data$household_size) * 100
data$perc_female_16_17 <- (data$female_16_17 / data$household_size) * 100
data$perc_male_18_49 <- (data$male_18_49 / data$household_size) * 100
data$perc_female_18_49 <- (data$female_18_49 / data$household_size) * 100
data$perc_male_50_64 <- (data$male_50_64 / data$household_size) * 100
data$perc_female_50_64 <- (data$female_50_64 / data$household_size) * 100
data$perc_male_65_over <- (data$male_65_over / data$household_size) * 100
data$perc_female_65_over <- (data$female_65_over / data$household_size) * 100


# disaggregation vars: 
data$strata_hoh_gender <- paste0(data$strata,"_",data$hoh_gender_final)
data$strata_resp_gender <- paste0(data$strata,"_",data$resp_gender)



write.csv(data, "input/data/recoded/shelter_2019_recoded_data.csv", row.names = F)
