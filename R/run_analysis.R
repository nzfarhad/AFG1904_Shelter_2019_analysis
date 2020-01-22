
source("R/functions/small_function.r")
source("R/functions/surveymodule.r")
source("R/functions/graph_function.R")

session<-"Shelter_2019"
lang="english"


library(dplyr);library(scales);library(readxl);library(srvyr)

##########################################################################################
# get the data
data<-read.csv("input/data/recoded/shelter_2019_recoded_data.csv",check.names=F)
names(data) <- gsub(x = names(data), pattern = "\\.", replacement = "/")

# Load Analysis Plan
log<-read_excel("input/analysisplans/analysis_plan_all_indicators_ordered.xlsx")
# log<-read_excel("input/analysisplans/analysis_plan_Jan_20_2020.xlsx")
log$analysis_key<-paste0(log$xi,"/",log$yi)


questions<-read_excel("input/questionaire/reach_afg_esnfw_kobo_v4_08.12.19.xlsx", sheet = 'survey')
choices<-read_excel("input/questionaire/reach_afg_esnfw_kobo_v4_08.12.19.xlsx", sheet = 'choices')

sampling_frame<-read.csv("input/samplingframe/shelter_samplingframe.csv")


data<-create_weights(sampling_frame, data)



# write.csv(data,"data_w_weight.csv")


# load the parameters
analysis.params<-parametres(data,log,questions=questions,choices=choices,lang=lang)
check_param(analysis.params)


##########################################################################################
# process the results
results<-lapply(seq_along(analysis.params$xi),boom_rmd,params=analysis.params)
check_create_directory("RDS")
saveRDS(results,sprintf("RDS/results_for_request_Jan_20_%s_%s.RDS",lang,session))


##########################################################################################
# extract the data results
data_results<- lapply(results, function(x){x$data}) %>% do.call(rbind.fill,.)
write.csv(data_results,sprintf("results/AnalysisResults_Request_Jan_20_%s_%s.csv",lang,session))



##########################################################################################
# create data merge
source("R/functions/function_data_merge.R")
check_create_directory("datamerge")

mergelevel<-unique(log$yi)

lapply(mergelevel,function(mergelevel,data_results){
  torank <- NULL
  # torank <- c("cash_flow", "repairs_unable")
  # extract results for choosen level
  res<-data_results[data_results$yi==mergelevel,]
  datamerge<-create_the_data_merge(results=res,label="variable",vartorank=torank)
  datamerge$level<-mergelevel
  return(datamerge)
},data_results=data_results) %>% bind_rows -> datamerge

write.csv(datamerge,sprintf("datamerge/datamerge_Request_Jan_20_%s_%s.csv",lang,session))


# income weighted statistics
# Overall 
income_summary_overall <- data %>%
  as_survey(weights = c(weight)) %>%
  summarize(
    income_mean = survey_mean(income, na.rm = T, vartype = NULL),
    income_median = survey_median(income, na.rm = T, vartype = NULL),
    income = survey_quantile(income, quantile = c(0.25, 0.75), vartype = NULL, na.rm = T)
  ) %>% mutate(level = "Overall") %>% select(level, income_mean:income_q75 )

income_min <- min(data$income, na.rm = T)
income_max <- max(data$income, na.rm = T)

income_summary_overall <- cbind(income_summary_overall, income_min, income_max)


# by province
income_summary_by_prov <- data %>%
  as_survey(weights = c(weight)) %>%
  group_by(level = province) %>% 
  summarize(
            income_mean = survey_mean(income, na.rm = T, vartype = NULL),
            income_median = survey_median(income, na.rm = T, vartype = NULL),
            income = survey_quantile(income, quantile = c(0.25, 0.75), vartype = NULL, na.rm = T)
            )
# min max
income_summary_by_prov_min_max <- data %>%
  group_by(province) %>%
  summarise(income_min = min(income, na.rm = T),
            income_max = max(income, na.rm = T)) %>% select(-c(province))

# bind
income_summary_by_prov <- cbind(income_summary_by_prov, income_summary_by_prov_min_max)


# by population group  
income_summary_by_pop_group <- data %>%
  as_survey(weights = c(weight)) %>%
  group_by(level =displacement_status) %>% 
  summarize(
    income_mean = survey_mean(income, na.rm = T, vartype = NULL),
    income_median = survey_median(income, na.rm = T, vartype = NULL),
    income = survey_quantile(income, quantile = c(0.25, 0.75), vartype = NULL, na.rm = T)
  )

# min max
income_summary_by_pop_group_min_max <- data %>%
  group_by(displacement_status) %>%
  summarise(income_min = min(income, na.rm = T),
            income_max = max(income, na.rm = T)) %>% select(-c(displacement_status)) 

# bind
income_summary_by_pop_group <- cbind(income_summary_by_pop_group, income_summary_by_pop_group_min_max)



# by population strata 
income_summary_by_strata <- data %>%
  as_survey(weights = c(weight)) %>%
  group_by(level = strata) %>% 
  summarize(
    income_mean = survey_mean(income, na.rm = T, vartype = NULL),
    income_median = survey_median(income, na.rm = T, vartype = NULL),
    income = survey_quantile(income, quantile = c(0.25, 0.75), vartype = NULL, na.rm = T)
  )

# min max
income_summary_by_strata_min_max <- data %>%
  group_by(strata) %>%
  summarise(income_min = min(income, na.rm = T),
            income_max = max(income, na.rm = T)) %>% select(-c(strata)) 

# bind
income_summary_by_strata <- cbind(income_summary_by_strata, income_summary_by_strata_min_max)



# by population strata and hoh gender
income_summary_by_strata_hoh_gender <- data %>%
  as_survey(weights = c(weight)) %>%
  group_by(level = strata_hoh_gender) %>% 
  summarize(
    income_mean = survey_mean(income, na.rm = T, vartype = NULL),
    income_median = survey_median(income, na.rm = T, vartype = NULL),
    income = survey_quantile(income, quantile = c(0.25, 0.75), vartype = NULL, na.rm = T)
    
  )

 # min max
income_summary_by_strata_hoh_gender_min_max <- data %>%
  group_by(strata_hoh_gender) %>%
  summarise(income_min = min(income, na.rm = T),
            income_max = max(income, na.rm = T)) %>% select(-c(strata_hoh_gender)) 
  
 # bind
income_summary_by_strata_hoh_gender <- cbind(income_summary_by_strata_hoh_gender, income_summary_by_strata_hoh_gender_min_max)




# bind income dfs
income_summ_statics <- rbind(income_summary_overall, 
                             income_summary_by_prov,
                             income_summary_by_pop_group,
                             income_summary_by_strata,
                             income_summary_by_strata_hoh_gender)




write.csv(income_summ_statics, "income_summ_statics_min_max_included.csv", row.names = F )


# rent_cost weighted statistics
# Overall 
rent_cost_summary_Overall <- data %>%
  as_survey(weights = c(weight)) %>%
  summarize(
    rent_cost_mean = survey_mean(rent_cost, na.rm = T, vartype = NULL),
    rent_cost_median = survey_median(rent_cost, na.rm = T, vartype = NULL),
    rent_cost = survey_quantile(rent_cost, quantile = c(0.25, 0.75), vartype = NULL, na.rm = T)
  ) %>% mutate(level = "Overall") %>% select(level, rent_cost_mean:rent_cost_q75 )

rent_cost_min <- min(data$rent_cost, na.rm = T)
rent_cost_max <- max(data$rent_cost, na.rm = T)

rent_cost_summary_Overall <- cbind(rent_cost_summary_Overall, rent_cost_min, rent_cost_max)


# Rent_cost by province  weighted statistics
rent_cost_summary_province <- data %>%
  as_survey(weights = c(weight)) %>%
  group_by(level = province) %>%
  summarize(
    rent_cost_mean = survey_mean(rent_cost, na.rm = T, vartype = NULL),
    rent_cost_median = survey_median(rent_cost, na.rm = T, vartype = NULL),
    rent_cost = survey_quantile(rent_cost, quantile = c(0.25, 0.75), vartype = NULL, na.rm = T)
  ) 

# min max
rent_cost_summary_province_min_max <- data %>%
  group_by(province) %>%
  summarise(rent_cost_min = min(rent_cost, na.rm = T),
            rent_cost_max = max(rent_cost, na.rm = T)) %>% select(-c(province)) 

# bind
rent_cost_summary_province <- cbind(rent_cost_summary_province, rent_cost_summary_province_min_max)

# bind rent_cost dfs
rent_cost_summ_statics <- rbind(rent_cost_summary_Overall, rent_cost_summary_province )


write.csv(rent_cost_summ_statics, "rent_cost_summ_statics.csv", row.names = F )



