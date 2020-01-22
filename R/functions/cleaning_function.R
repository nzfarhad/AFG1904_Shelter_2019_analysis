require(igraph)
library(tidyverse)
library(readxl)

## function to create unique look code
uptid<-function(pr,ch){
  a <- pr[["_uuid"]][match(ch[["_parent_index"]],pr[["_index"]])]
  return(a)
}

## function create age groups
age_groups<-function(x,age) {
  cut(coerc(x), age,include.lowest = TRUE, right = TRUE, ordered_result = FALSE)
}

# box plot
bplot<-function(data,y,x=NULL) {
  ggplot(data, aes(x=x,y=y)) +  geom_boxplot() + coord_flip()
}

# short for as character
ch<-function(x) {as.character(x)}

prep_mydata<-function(x) {
  ch(tolower(rec_missing(x)))
}
as.df<-function(x){as.data.frame(x,stringsAsFactors = F, check.names = F)}

	
	check_lenght<-function(start,end){
		interval <-strptime(end,"%Y-%m-%d T %H:%M:%S")-strptime(start,"%Y-%m-%d T %H:%M:%S")
		return(interval)
}


check_blanks<- function(data){
	apply(data, 1,function(x) {length(which(is.na(x)))})/ncol(data)*100
}
	
	
		
	data_check<-function(dbs,uuid,titl,enum){
		ct=0
		dbs<-class_assess(dbs)
		date_u<-humanTime()
		
		if(!any(  list.files()=="data_check")){dir.create("data_check")}
		wb <- loadWorkbook(sprintf("data_check/data_checks_%s_%s.xlsx",titl,date_u), create = TRUE)

		createSheet(wb, name = "outliers")
		createSheet(wb, name = "others")
			
		names(dbs)<-tolower(names(dbs))
		
		oth<-which((str_detect(names(dbs),"_other$")|str_detect(names(dbs),"_autre$")|str_detect(names(dbs)," other$"))&( sapply(dbs,class)=="factor"|sapply(dbs,class)=="character"))
		
		
		for (k in oth){
			oth<-which((str_detect(names(dbs),"autre$")|str_detect(names(dbs),"other$"))&( sapply(dbs,class)=="factor"|sapply(dbs,class)=="character"))
		
			index<-k
			rad_min<-8
			rad_max<-1
			
			if(index-rad_min<=0){index_min<-1}else{index_min<-index-rad_min}
			search_rad<-names(dbs)[c((index-1):(index_min),(index+1):(index+rad_max))]
			
			d<-levenshteinSim(
				gsub("other","",names(dbs)[index]),
				search_rad
				)
				
			matchs<-search_rad[which(d==max(d)&d>0.9)]
			
			if(length(matchs)==0){colname<-names(dbs)[index-1]}else{colname<-matchs}
		
			if(length(which(!is.na(dbs[,k])))/length(dbs[,k])>.02){
				ct=ct+1
				ds<-cbind(
						dbs[[uuid]],
						dbs[[enum]],
						rep(names(dbs)[k],nrow(dbs)),
						as.character(dbs[,k]),
						rep(colname,nrow(dbs)),
						dbs[colname],
						rep("others: to check if could be recoded",nrow(dbs))
				)
				
				names(ds)<-c("uuid","enumerator","question.name","old.value","if other text entry","other text var","comments")
				
				ds<-ds[!is.na(ds$question.name)&!is.na(ds$old.value),]
				
				if(ct==1){
					outother<-ds
				}else {
					outother<-rbind(outother,ds)
				}
				writeWorksheet(wb, ds, sheet = "others", startRow = getLastRow(wb,"others")+1, startCol = 2)
			}
		}
		
		############################3############################3############################3####
		## check on outliers
		num<-which(sapply(dbs,class)=="integer"|sapply(dbs,class)=="numeric")
		
		ct=0
		for (l in num){
			lowerq = quantile(dbs[,l],na.rm=T)[2]
			upperq = quantile(dbs[,l],na.rm=T)[4]
			iqr = upperq - lowerq

				ti<-3
				extreme.threshold.upper = (iqr * ti) + upperq + 1 
				extreme.threshold.lower = lowerq - (iqr * ti) - 1
				outl<-which(dbs[,l]>extreme.threshold.upper )
				if(length(outl)>0){
					ct=ct+1
					doth<-data.frame(
						as.character(dbs[[uuid]][outl]),
						as.character(dbs[[enum]][outl]),
						rep(names(dbs)[l],length(outl)),
						as.character(dbs[,l][outl]),
						rep("NA",length(outl)),
						rep("NA",length(outl)),
						rep("outlier: to check",length(outl))
					)
					names(doth)<-c("uuid","enumerator","question.name","old.value","if other text entry","other text var","comments")
					if(ct==1){
					outoutliers<-doth
				}else {
					outoutliers<-rbind(outoutliers,doth)
				}
				}
								
				
		}
		writeWorksheet(wb, outoutliers, sheet = "outliers", startRow = getLastRow(wb,"outliers")+1, startCol = 2)
		saveWorkbook(wb)
		return(rbind(outother,outoutliers))
	}
	
	
	
	

# check the cleaning, implement instruction from an excel sheet
cleaning_check<-function(cl,db){
  
  db<-lapply(db,as.character) %>% as.df
  
  # for all the cells:
  for (i in 1:nrow(cl)){
    
    if(is.na(cl$cond2_var[i])){
      lo<-which(ch(db[[cl$cond1_var[i]]])==cl$cond1_ch[i])
    } else {
      lo<-which(ch(db[[cl$cond1_var[i]]])==cl$cond1_ch[i] &  ch(db[[cl$cond2_var[i]]])==cl$cond2_ch[i])
    }
    
    old.value<-db[[cl$change1_var[i]]][lo] %>% ch
    db[[cl$change1_var[i]]][lo]<-cl$change1_ch[i]
    new.value<-db[[cl$change1_var[i]]][lo]  %>% ch
    
    clog<-data.frame( 
      uuid=db[["_uuid"]][lo],
      question.name=rep(cl$change1_var[i],length(lo)),
      old.value=old.value,
      new.value=new.value,
      issue=rep(cl$comment[i],length(lo))
    )  
    
    if(!is.na(cl$change2_var[i])){
      old.value<-db[[cl$change2_var[i]]][lo]
      db[[cl$change2_var[i]]][lo]<-cl$change2_ch[i]
      
      new.value<-db[[cl$change2_var[i]]][lo]
      
      clog2<-data.frame( 
        uuid=db[["_uuid"]][lo],
        question.name=rep(cl$change2_var[i],length(lo)),
        old.value=old.value,
        new.value=new.value,
        issue=rep(cl$comment[i],length(lo))
      )  
      clog<-rbind(clog,clog2)
      
    }
    if(i == 1) {log_cl<-clog} else {log_cl<-rbind(log_cl, clog)}
  }
  return(list(log_cl,db))
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




	
	
	log_check<-function(data,uuid,dclean,uuid_log,qmname,newval,oldval,titl){
		
		data<-class_assess(data)
		date_u<-humanTime()
		
		if(!any(  list.files()=="data_check")){dir.create("data_check")}
		wb <- loadWorkbook(sprintf("data_check/logbook_checks_%s_%s.xlsx",titl,date_u), create = TRUE)
		
		
		# remove caps from headings
		names(data)<-tolower(names(data))
		names(dclean)<-tolower(names(dclean))
		
		# recode missing values to consistent format

		XLConnect::createSheet(wb, name = "log")
		# INPUT TRUE if a column describing in a cleaning log
		change_yn<-FALSE

		# remove caps to questions names from log
		dclean[[qmname]]<-tolower(dclean[[qmname]])
		dclean[[newval]]<-tolower(dclean[[newval]])
		dclean[[oldval]]<-tolower(dclean[[oldval]])
			
		# add additional column in the cleaning log template for checks
		dclean$check_impl<-rep(NA,nrow(dclean))
		dclean$comments<-rep(NA,nrow(dclean))
		dclean$valueindata<-rep(NA,nrow(dclean))
		
		# read cleaning log
		for (k in 1:nrow(dclean))
		{
		  X<-as.character(dclean[[uuid_log]][k])
		  Y<-as.character(dclean[[qmname]][k])
		  val<-dclean[[newval]][k]
		  valold<-dclean[[oldval]][k]
		  check<-NA
		  
		ref<-tolower(as.character(data[which(data[[uuid]]==X),which(names(data)==Y)]))
			
		if(length(ref)==0){
			if(length(which(data[[uuid]]==X))==0&length(which(names(data)==Y))==0){
				dclean$comments[k]<-"colonne name and uuid are wrong or do not exist or missing"
			}else if(length(which(data[[uuid]]==X))==0){
				dclean$comments[k]<-"uuid is wrong or is missing or does not exist"
			}else if(length(which(names(data)==Y))==0){
				dclean$comments[k]<-"colonne name is wrong or does not exist or is missing"
			} 
		} else {
		  
				if(is.na(ref)|is.na(val)){
					check<-is.na(val)&is.na(ref)
					valdata<-ref
				}else{				
						check<-ref==val
						valdata<-ref
				}
		  
			if(length(check)>0){
				dclean$check_impl[k]<-check
				dclean$valueindata[k]<-valdata
				if(!check){dclean$comments[k]<-"New value does not match the dataset"}
			}
		  
			}
		}
		
		XLConnect::writeWorksheet(wb, dclean, sheet = "log", startRow =1, startCol = 1)
		XLConnect::saveWorkbook(wb)
		return(dclean)
	
	}
	