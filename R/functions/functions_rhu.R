
getthedata<-function(clean_files){
  # need to change the tab name to "main"
  lapply(clean_files,function(x){read_excel(x,"main")}) %>% do.call(rbind.fill,.)->hh
  lapply(clean_files,function(x){read_excel(x,"HH_info")}) %>% do.call(rbind.fill,.)->ind
  
  convcol<-read.csv("../assets/headingconvertion.csv")
  
  for (j in 1:nrow(convcol)){
    hh[[ch(convcol$after[j])]]<-paste0(hh[[ch(convcol$after[j])]] %>% car::recode(.,"NA=''"),hh[[ch(convcol$before[j])]]%>% car::recode(.,"NA=''"))
  }
  hh<-hh[,-grep(paste(convcol$before,collapse="|"),names(hh))]
  return(list(hh=hh,ind=ind))
}





impl_clean<-function(data,uuid,dclean,uuid_log,qmname,newval,oldval,action,implemented,othermain,othertextvar,newcategory){
  for (k in 1:nrow(dclean))
  {
    Taction<-dclean[[action]][k]
    
    if(Taction=="note"){
      
    } else if(Taction=="remove_all"){
      data<-data[which(!data[[uuid]]%in%dclean[[uuid_log]][k]),]
      
    } else if(Taction=="recode_all"){
      data[[dclean[[qmname]][k]]][data[[dclean[[qmname]][k]]]==dclean[[oldval]][k]]<-dclean[[newval]][k]
      
    } else if(dclean[[implemented]][k]=="yes"){
      
      if(Taction=="translate and recoded" )
      {
        X<-as.character(dclean[[uuid_log]][k])
        Y<-as.character(dclean[[othermain]][k])
        val<-dclean[[newcategory]][k]
        valold<-dclean[[othertextvar]][k]
        data[,which(names(data)==Y)]<-as.character(data[,which(names(data)==Y)])
        data[which(data[[uuid]]==X),which(names(data)==Y)]<-as.character(val)
        #print(paste0(val,"___",valold))
      }
      
      X<-as.character(dclean[[uuid_log]][k])
      Y<-as.character(dclean[[qmname]][k])
      val<-dclean[[newval]][k]
      valold<-dclean[[oldval]][k]
      data[,which(names(data)==Y)]<-as.character(data[,which(names(data)==Y)])
      data[which(data[[uuid]]==X),which(names(data)==Y)]<-as.character(val)
      #print(paste0(val,"___",valold))
    }
  }
  return(data)
}

prepare_data<-function(list_data,dclean){
  
  hh<-list_data$hh
  ind<-list_data$ind
  
  hh<-hh[which(!(hh$rhu_user=="family" & hh$rhu_as_familly=="no" & hh$rhu_alternative_use=="transit_centre")),]
  hh<-hh[,which(!names(hh)%in%c("s1", "s2","s3","challenges_0" ,"challenges_1"   ,"challenges_2" ,"benefit_0","benefit_1","benefit_2"))]
  
  
  names(hh)<-gsub("impermeability","rainproof",names(hh))
  
  dclean<-dclean[which(dclean$was_done=="yes"),]
  
  # read cleaning log and implement changes
 
  hh<-impl_clean(
    data=hh,
    uuid="_uuid",
    dclean=dclean,
    uuid_log="uuid",
    qmname="var",
    newval="new_value",
    oldval="old_value",
    action="action",
    implemented="was_done",
    othermain="othermain",
    othertextvar="othertextvar",
    newcategory="proposed category"
  )
  
   # coerce date to text
  hh$today<-as.character(hh$today)
  
  # prepare the dataset 
  hh<- hh%>% lapply(rec_missing)%>% as.data.frame(check.names=F) %>% class_assess
  ind<- ind%>% lapply(rec_missing)%>% as.data.frame(check.names=F) %>% class_assess
  
  # create a child uuid
  uptid<-function(pr,ch){
    a <- pr[["_uuid"]][match(ch[["_parent_index"]],pr[["_index"]])]
    return(a)
  }
  
  ## function create age groups
  AgeGroups<-function(x,age) {
    cut(coerc(x), age,include.lowest = TRUE, right = TRUE, ordered_result = FALSE)
  }
  
  # creat uuid parent for the child table
  # 
  # ind$parent_uuid<-uptid(hh,ind)
  # ind$uuid<-paste0(ind$parent_uuid,"_",ind[["_index"]])

  ind$parent_uuid<-ind[["_submission__uuid"]]
  
  # count the loop numbers 
  table(ind$parent_uuid) %>% as.data.frame() -> counthh
  names(counthh)<-c("parent_uuid","loopcount")
  hh<- merge(hh,counthh, by.x="_uuid",by.y="parent_uuid",all.x=T)
  hh$checkloop<-(hh$household_size==hh$loopcount)
  
  # adding the respondent to the list of individual
  # if the loop count lower than h size en the loop is not complete. 
  forind<-hh[which(hh$checkloop==FALSE),c(
    "resp_name",
    "resp_sex",
    "reps_age",
    "_uuid"
  )]
  names(forind)<-c("name","sexe","age","parent_uuid")
  ind<-rbind.fill(ind,forind)
  

  ##################################################
  ## aggregate child table indicators to the main table 
  
  # age groups
  age<-c(0,1,5,12,18,60,Inf)
  ind$age_gr<-age_groups(ind$age,age)
  ind$age_gr_sexe<-paste0(ind$sexe," ",ind$age_gr)
  age_table<-dcast(ind, parent_uuid~age_gr_sexe)
  hh<- merge(hh,age_table, by.x="_uuid",by.y="parent_uuid",all.x=T)
  
  # is hhh of household
  ind$hh_head <-ind$hh_head %>% ch
  ind$hh_head[ind$hh_head=="yes"]<-"is_hhh"
  ind$hh_head[ind$hh_head=="no"]<-"is_not_hhh"
  
  # attach parent info to ind table
  
  ind<-merge(ind,hh[c("country","site_name","_uuid")],by.x="parent_uuid",by.y="_uuid",all.x=T)
  
  is_hhh<-dcast(ind, parent_uuid~hh_head)
  hh<- merge(hh,is_hhh, by.x="_uuid",by.y="parent_uuid",all.x=T)
  
  # more household head issue
  hh$check_more_than1HHH<-hh$is_hhh>1
  hh$check_no_HHH<-hh$is_hhh==0
  
  # check if benefit are the same than challenges
  rhu_spec<-c(
    "thermal_comfort",
    "privacy",
    "wind_resistance",
    "ventilation",
    "lighting_system",
    "feeling_safe",
    "living_space",
    "adaptability"
  )
  
  # compare challenges and benefits - if any are the same = TRUE
  lapply(rhu_spec,
         function(y,hh){
           hh[[paste0("benefit_rhu/",y)]]== hh[[paste0("challenges_rhu/",y)]]
         },hh=hh) %>% do.call(cbind,.) %>% as.data.frame-> same_answers
  
  names(same_answers)<-rhu_spec
  hh$check_benef_challenge<-same_answers%>% apply(.,1,sum)
  # that is super high! NEED TO ADD A CONSTRAINT
  
  # recode likert scale from 1 to 5
  var_likert<-c("thermal_confort","living_space","privacy","ventilation","lighting","wind_resistance","prefer_rhu","overall")
  
  for(x in var_likert){
    
    hh[[x]]<-car::recode(hh[[x]],"
                         'very_dissatisfied'='1_very_dissatisfied';
                         'dissatisfied'='2_dissatisfied';
                         'neutral'='3_neutral';
                         'satisfied'='4_satisfied';'very_satisfied'='5_very_satisfied';'prefer_not_to_answer'='0_prefer_not_to_answer'")
  }
  
  # get survey time
  hh$surveytime<-check_lenght(hh$start, hh$end)
  
  # get percentage of blank cells
  hh$blank_numb<-check_blanks(hh)
  
  # get density the shelter is 17.5sqm
  hh$density<-17.5/hh$household_size
  
  # recode the shelter types
  shelter_recode<-c("shelter_wish1","shelter_wish2","shelter_wish3","type_shelter_origine","type_shelter_sameloc")
    
  for (x in shelter_recode) {
    hh[[x]]<-recode(hh[[x]],
    "'rhu'='RHU';                                                                                
    'concrete_house'='Concrete house';
    'a_unchr_tent'='UNHCR tent';
    'a_non-unhcr_tent'='Non UNHCR tent';
    'emergency_shelter_(using_shelter_kit_material_e.g._plastic_sheeting_&_rope_tc.)'='Emergency shelter';
    'emergency_shelter'='Emergency shelter';
    'dontknow'='Do not know';
    'other_transitional_shelter_(collective_centre,_other_type_of_prefabricated_shelter)'='Other transitional shelter';
    'other_transitional_shelter'='Other transitional shelter';
    'a_unchr_tent_'='UNHCR tent';
    'a_non-unhcr_tent_provided_by_goverment_or_other_agency_'='Non UNHCR tent';
    'local_shelter_solution_(specifiy)'=NA"
    )
  }

  names(hh)<-recode(names(hh),"'upgrade_rhu_type/new_opening_for_ventillation'='upgrade_rhu_type/new_opening_for_ventilation';
  'upgrade_rhu_type/additional_plastic_sheeting_for_shade'='upgrade_rhu_type/additional_plastic_sheeting'")
  
  hh$feeling_safe_all<-hh$feeling_safe
  
  hh[,c("shelter_wish1","shelter_wish2","shelter_wish3")]<-hh[,c("shelter_wish1","shelter_wish2","shelter_wish3")] %>% apply(.,2,function(x){car::recode(x,"'other'=''")}) %>% apply(.,1,function(x){paste(x, collapse="-_-")}) %>% 
      gsub("-_--_-","-_-",.) %>% 
    strsplit(.,split="-_-") %>% lapply(function(x){
      c(x,rep(NA,3-length(x))) %>% car::recode(.,"''=NA")
    }) %>%  do.call(rbind,.)

  hh[["rhu_alternative_use"]][hh[["rhu_user"]]=="family"&(
      hh[["rhu_alternative_use"]]=="medical_centre"|
        hh[["rhu_alternative_use"]]=="education_facility"|
        hh[["rhu_alternative_use"]]=="staff_accomodation"|
        hh[["rhu_alternative_use"]]=="collective_centre")]<-"other"
  
  hh$rhu_usage<-ifelse(hh[["rhu_user"]]=="family" & is.na(hh[["rhu_alternative_use"]]),"Used by displaced population as living space",
          ifelse(hh[["rhu_user"]]=="family",paste0("Used by displaced population as ",hh[["rhu_alternative_use"]]),
                 ifelse(hh[["rhu_user"]]!="family",paste0("Used by ",hh[["rhu_user"]]," as ",hh[["rhu_alternative_use"]]),"")
          ))

  challenge_benefit<-names(hh)[c(grep("challenges_rhu/",names(hh)),grep("benefit_rhu/",names(hh)))]
  
  var_to_recode<-c("feeling_safe",var_likert,challenge_benefit)
  for(x in var_to_recode){
    hh[[x]][hh$rhu_usage!="Used by displaced population as living space"|is.na(hh$rhu_usage)]<-NA
  }
  
  hh$lighting_system_working<-recode(hh$lighting_system_working,"NA='no'")
  
  
  hh$household_status<-recode(hh$household_status,
    "'asylum_seeker other'='asylum_seekers';
    'asylum_seeker temporary_residence'='asylum_seekers';
    'asylum_seeker'='asylum_seekers';
    'idp returnee'='returnees';
    'idp temporary_residence'='IDPs';
    'idp'='IDPs';
    'returnee'='returnees';
    'refugee'='refugees';
    'refugee asylum_seeker'='refugees';
    'refugee idp returnee'='refugees'") 
  
  hh<-hh[,-(grep("household_status/",names(hh)))]
  
  hh$last_shelter<-ifelse(hh$live_in_other_shelter_sameloc=="yes",ch(hh$type_shelter_sameloc),ch(hh$type_shelter_origine))
  
  hh$important_characteristc_1<-hh$important_characteristc_1 %>% tolower
  hh$important_characteristc_2<-hh$important_characteristc_2 %>% tolower
  
  for (x in c("important_characteristc_1","important_characteristc_2")) {
    hh[[x]]<-recode(hh[[x]],"'none'='Not sure'")
  }
  
  
  
  hh$ligh_here_working<-ifelse(hh$lighting_system=="yes" & hh$lighting_system_working=="yes","Solar light present and working",
                               ifelse(hh$lighting_system=="yes" & hh$lighting_system_working=="no","Solar light present but NOT working","No solar light"))
  
  
  names(hh)<-recode(names(hh),"'challenges_rhu/thermal_comfort'='challenges_rhu/lack of thermal_comfort';
  'challenges_rhu/privacy'='challenges_rhu/lack of privacy';
  'challenges_rhu/wind_resistance'='challenges_rhu/lack of wind_resistance';
  'challenges_rhu/ventilation'='challenges_rhu/lack of ventilation';
  'challenges_rhu/lighting_system'='challenges_rhu/lack of lighting_system';
  'challenges_rhu/feeling_safe'='challenges_rhu/feeling_unsafe';
  'challenges_rhu/living_space'='challenges_rhu/lack of living_space';
  'challenges_rhu/adaptability'='challenges_rhu/lack of adaptability';
  'challenges_rhu/other'='challenges_rhu/other';
  'challenges_rhu/impermeability'='challenges_rhu/Not rainproof';
  'challenges_rhu/none'='challenges_rhu/none';
  'challenges_rhu/dontknow'='challenges_rhu/Do not know'")
  
  hh$padlock_who<-recode(hh$padlock_who,"'purchased_by_unhcr'='provided by UNHCR'")


  
  
  # export in .csv
  write.csv(hh,"../processed/parent_table.csv")
  write.csv(ind,"../processed/child_table.csv")
  
  
  return(list(hh=hh,ind=ind))
  }

check_the_data<-function(hh){
  #######################################################
  #• extract others
  apply(hh[,grep("_other$",names(hh))],2,function(x) {
    na.omit(x) %>% length
  })%>% as.data.frame -> table_other
  
  ######################################################
  # count the others
  
  names(table_other)<-"freq"
  table_other$var<-rownames(table_other)
  high_other<-rownames(table_other)[which(table_other[,1]> 0)]
  
  
  # wite a list of CSV
  lapply(high_other,function(x,hh){na.omit(hh[c(x,"_uuid")]) %>% unique},hh=hh)   %>% 
    do.call(rbind.fill, .) %>% 
    melt(id="_uuid") %>% na.omit %>% write.csv("list of OTHER.csv")
  
}




plot_for_data_merge<-function(hh, sites){

  hh %>%  class_assess->hh

  dbf<-hh[hh$site_names==which_sites,]
  
  ### histogram displacement
  p2<-ggplot(dbf, aes(duration_displacement)) +
    geom_bar(aes(y = (..count..)/sum(..count..)*100),color='white',fill='#0071bb')+
      theme_minimal()+ 
      xlab("Duration (months)") + 
      ylab("% Households") 
  p2
  ggsave(sprintf("../datamerge/figures/duration_displacement_%s.png",which_sites),p2,width = 4, height = 2)
  
  
  
  p1<-ggplot(dbf, aes(duration_live_rhu)) +
      geom_bar(aes(y = (..count..)/sum(..count..)*100),color='white',fill='#0071bb',width=1)+
      theme_minimal()+ 
      xlab("Duration (months)") + 
      ylab("% Households") 
  pl
  ggsave(sprintf("../datamerge/figures/duration_live_rhu_%s.png",which_sites),pl,width = 4, height = 2)
  
  
  
  ### graph for likert scale
  require(RColorBrewer)
  
  var_likert<-c("thermal_confort","living_space","privacy","ventilation","lighting","wind_resistance","prefer_rhu","overall")
  for(x in var_likert){
    dbf[[x]]<-car::recode(dbf[[x]],"
    'very_dissatisfied'='1_very_dissatisfied';
    'dissatisfied'='2_dissatisfied';
    'neutral'='3_neutral';
    'satisfied'='4_satisfied';'very_satisfied'='5_very_satisfied';'prefer_not_to_answer'='0_prefer_not_to_answer'")
  }
  
  data.likert<-lapply(var_likert,function(x,dbf){
    table(dbf[[x]])
  },dbf=dbf) %>% do.call(bind_rows,.) %>% apply(.,2,function(x) car::recode(x,"NA=0")) %>% 
    apply(.,1,function(x){x/sum(x,na.rm=T)*100}) %>% t %>% as.data.frame
  
  
  data.likert$id<-var_likert
  mydata<-data.likert %>% melt()
  mydata$id<-proper(mydata$id)
  mydata<-mydata[mydata$id%in%c("Thermal confort","Living space","Privacy","Ventilation","Lighting","Wind resistance"),]
  p<-plotlikert(mydata)
  p
  
  ggsave(sprintf("../datamerge/figures/likert_%s.png",which_sites),p,width = 4, height = 2.5)
  
  
  ## heatmaps
  # graph to produce = needs to be splitted by sites


 log<-read_excel("../datamerge/analysis_plan/analysis_plan_datamerge.xlsx")
 log<-log[which(log$graph_for_datamerge=="yes"),]
 
 params<-parametres(dbf,log)
 check_param(params)
 
 # here should a site names in files names
 
 # should it use the results instead ?
 
  results_quartiers<-do.call(rbind,sapply(seq_along(params$xi),boum,params=params,graph_on=TRUE,loop=which_sites))
  
}











