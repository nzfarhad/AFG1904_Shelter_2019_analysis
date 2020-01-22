source("R/functions/small_function.r")
source("R/functions/surveymodule.r")
source("R/functions/graph_function.R")

session<-"Shelter_2019"
lang="english"


library(dplyr);library(scales);library(readxl)

##########################################################################################
# get the data
data<-read.csv("input/data/recoded/shelter_2019_recoded_data.csv",check.names=F)
names(data) <- gsub(x = names(data), pattern = "\\.", replacement = "/")


# Load analysis plan
log<-read_excel("input/analysisplans/analysis_plan_ranked.xlsx")
# log<-read_excel("input/analysisplans/analysis_plan_hoh_gender.xlsx") 
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
saveRDS(results,sprintf("RDS/results_ranked_hoh_disagg_and_overall_%s_%s.RDS",lang,session))


##########################################################################################
# extract the data results
data_results<- lapply(results, function(x){x$data}) %>% do.call(rbind.fill,.)
write.csv(data_results,sprintf("results/AnalysisResults_ranked_hoh_disagg_and_overall_%s_%s.csv",lang,session))



##########################################################################################
# ranked vars for factsheets
ranked_vars <- c(
  "displace_push",
  "shelter_damage_cause",
  "repairs_unable",
  "eviction_why",
  "type_assistance",
  "material_coping",
  "shelter_coping",
  "information_source",
  "fuel_source",
  "priority_need",
  "winter_cope_how",
  "priority_first",
  "priority_second",
  "priority_third",
  "nfi_prefer",
  "shelter_prefer",
  "winter_prefer",
  "cash_spend",
  "cash_prefer_nfi"
)

# Ranked Vars for hoh gender disagg
ranked_vars_hoh_gender <- c(
  "resp_hoh",
  "tazkera",
  "income",
  "cash_flow",
  "income_source",
  "shelter_type",
  "shelter_damage",
  "shelter_damage_cause",
  "shelter_damage_overall",
  "repairs",
  "repairs_unable",
  "arrangement",
  "tenure",
  "tenure_validity",
  "rent_cost",
  "hosting",
  "hosting_relationship",
  "hosting_when",
  "hosted_relationship",
  "hosted_pay",
  "afford_rent",
  "rent_change",
  "eviction",
  "eviction_why",
  "eviction_fear",
  "eviction_fear_why",
  "unsafe_shelter",
  "unsafe_shelter_why",
  "assistance",
  "type_assistance",
  "shelter_form_assist",
  "cash_shelter_assist",
  "material_barriers",
  "material_access_barrier",
  "material_receipt_barrier",
  "material_use_barrier",
  "material_coping",
  "shelter_access",
  "shelter_access_why",
  "shelter_coping",
  "material_access",
  "material_access_why",
  "nfi_access",
  "nfi_access_why",
  "fuel_source",
  "shelter_construct",
  "priority_need",
  "winter_cope",
  "winter_cope_how",
  "priority_first",
  "priority_second",
  "priority_third",
  "nfi_prefer",
  "shelter_prefer",
  "winter_prefer",
  "cash_spend",
  "cash_prefer_nfi"
)



# create data merge
source("R/functions/function_data_merge.R")
check_create_directory("datamerge")

mergelevel<-unique(log$yi)

lapply(mergelevel,function(mergelevel,data_results){
  torank <- ranked_vars_hoh_gender
  # torank <- c("cash_flow", "repairs_unable")
  # extract results for choosen level
  res<-data_results[data_results$yi==mergelevel,]
  datamerge<-create_the_data_merge(results=res,label="variable",vartorank=torank)
  datamerge$level<-mergelevel
  return(datamerge)
},data_results=data_results) %>% bind_rows -> datamerge

write.csv(datamerge,sprintf("datamerge/datamerge_ranked_hoh_gender_and_overall_%s_%s.csv",lang,session))

