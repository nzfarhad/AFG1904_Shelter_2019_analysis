
plot_pyr<-function(df,sexe,level,color="#0071bb"){
  
  
  if(length(color)==1){color=rep(color,2)}
  
	df$perc<-df$value/sum(df$value)*100
	df$pop<-df$perc
	df$pop[df[[sexe]]==level[1]]<-df$perc[df[[sexe]]==level[1]]*-1
	df$memb_sexe<-df[[sexe]]
 
	fsize=5
	font_family="Arial Narrow"
 
	n1 <- ggplot(df, aes(x = variable, y = pop, fill = memb_sexe )) + 
	geom_bar(subset = .(memb_sexe == level[1]), stat = "identity") + 
	geom_bar(subset = .(memb_sexe == level[2]), stat = "identity") + 
	geom_text(aes(label = paste0(formatC(round(abs(df$perc),0), digits = 0, format = "f"),"%")),position = position_dodge(width = 0),family=font_family,size = round(fsize*0.8,0),color="#42423E")+
	coord_flip() + 
	scale_fill_manual(guide = guide_legend(reverse=TRUE),values=color)+ 
	theme_bw()

	n1<-n1+theme_pander()+
		theme(
			 legend.key.width = unit(2, "cm"),
			 legend.key.height = unit(1, "cm"),
			 legend.title=element_text(family=font_family,size=round(fsize*2,0),face="plain",color="#42423E"),
			 legend.text=element_text(family=font_family,size=round(fsize*2,0),color="#42423E"),
			 axis.text.x=element_blank(),
			 axis.text.y=element_text(family=font_family,size=round(fsize*2,0),color="#42423E"),
			 axis.title.x = element_blank(),
			 panel.grid=element_blank(),
			 #axis.text=element_blank(),
			 axis.ticks=element_blank(),
			 axis.title.y = element_blank()
		)
	n1
}



heat_map<-function(
    datag,
		disa="Var2",
		variable="Var1",
		value="Freq",
		count="count",
		valid_n="valid_n",
		
		type="count",
		nb="no",
		
		
		fsize=4,
		font_family="Arial Narrow",
		H_col="#c97b83",
		smult=FALSE,
		order=TRUE,
		col_revert=FALSE
	){
  
	# here can percentage
	#names(datag)[1]<-"disa"
	
	datag$disa<-datag[[disa]] %>% sapply(wrapit,n=20)
	datag$variable<-datag[[variable]] %>% sapply(wrapit,n=20)
	datag$value<-datag[[value]]
	datag$count<-datag[[count]]
	datag$valid_n<-datag[[valid_n]]
	
	
	has_CI<-all(c("CI_lw","CI_up") %in% names(datag))
	
	datag$value<-coerc(datag$value)
	datag$variable<-factor(datag$variable,levels=rev(levels(factor(datag$variable))))
	
	if(order==TRUE){
	  if(length(unique(datag$disa))==1){
	    cdata <- datag
	    ordered<-cdata[with(cdata, order(-value, variable)), ]$variable
	  }else{
	    cdata <- plyr::ddply(datag, "variable", summarise, mean = mean(value,na.rm=TRUE))
	    ordered<-cdata[with(cdata, order(-mean, variable)), ]$variable
	  } 
	}
	
	p <- ggplot(datag, aes(x=datag$disa, y=datag$variable)) 
	p<-p+geom_tile(aes(fill = datag$value),colour = "white")
	if(has_CI){
	  if(nb=="yes"){
	    p<-p+geom_text(aes(label=paste(round(datag$value,2)," [",round(datag$CI_lw,2)," , ",round(datag$CI_up,2),"]",sep="")), family=font_family,size = fsize,color="#42423E")
	  }else{
	    p<-p+geom_text(aes(label=paste(round(datag$value,0),"% [",round(datag$CI_lw,0)," , ",round(datag$CI_up,0),"]",sep="")), family=font_family,size = fsize,color="#42423E")
	  }
	}else{ # if no cl
	  if(nb=="yes"){
	    p<-p+geom_text(aes(label=paste(round(datag$value,2),sep="")), family=font_family,size = fsize,color="#42423E")
	  } else if (type=="count"){
	    p<-p+geom_text(aes(label=paste(round(datag$value,0),sep="")), family=font_family,size = fsize,color="#42423E")
	  } else if (type=="count_perc"){
	    p<-p+geom_text(aes(label=paste(round(datag$value,0),"%"," (",round(datag$count,0),"/",round(datag$valid_n,0),")",sep="")), family=font_family,size = fsize,color="#42423E")
	  }else{
	    p<-p+geom_text(aes(label=paste(round(datag$value,0),"%",sep="")), family=font_family,size = fsize,color="#42423E")
	  }
	}
	
	
	if(order){
	  p<-p+scale_y_discrete(limits=rev(ordered))
	}
	
	p<-p+scale_fill_gradient(low = "white",high = H_col )+
	  scale_x_discrete(expand = c(0, 0)) +
	  xlab("") + 
	  ylab("") 
	
	
	p.bot<-p+ theme(panel.grid.major = element_blank(),
	                panel.grid.minor = element_blank(),
	                panel.border = element_rect(fill=NA,color="white", size=0.5, linetype="solid"),
	                axis.line = element_blank(),
	                axis.ticks = element_blank(),
	                panel.background = element_rect(fill="white"),
	                plot.background = element_rect(fill="white"),
	                legend.position = "none", 
	                axis.text.x = element_blank(),
	                plot.margin = unit(c(1,0,0,0), "cm"),
	                axis.text.y=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"))
	
	p.top   <-  p.bot + theme(
	  axis.text.x = element_text(family=font_family,size=round(fsize*3.4,0)),
	  axis.text.y = element_text(family=font_family,size=round(fsize*3.4,0),color="white")
	)  + coord_cartesian(ylim = c(0,0))
	
	
	p.bot<-p+ theme(panel.grid.major = element_blank(),
	                panel.grid.minor = element_blank(),
	                panel.border = element_rect(fill=NA,color="white", size=0.5, linetype="solid"),
	                axis.line = element_blank(),
	                axis.ticks = element_blank(),
	                panel.background = element_rect(fill="white"),
	                plot.background = element_rect(fill="white"),
        legend.position = "none", 
        axis.text.x = element_blank(),
        plot.margin = unit(c(1,0,0,0), "cm"),
        axis.text.y=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"))

	p.top   <-  p.bot + theme(
		axis.text.x = element_text(family=font_family,size=round(fsize*3.4,0)),
		axis.text.y = element_text(family=font_family,size=round(fsize*3.4,0),color="white")
		)  + coord_cartesian(ylim = c(0,0))
		
	require(gtable)
	#Extract Grobs
	g1<-ggplotGrob(p.top)
	g2<-ggplotGrob(p.bot)
	#Bind the tables
	g<-gtable:::rbind_gtable(g1, g2, "first")
	#Remove a row between the plots
	g <- gtable_add_rows(g, unit(-1.25,"cm"), pos=nrow(g1))
	#draw
	panels <- g$layout$t[grep("panel", g$layout$name)]
	g$heights[panels] <- unit.c(unit(0,"null"),unit(2,"null"))
	grid.newpage()
	grid.draw(g)
	return(g)
	#print(p)
}



pie<-function(gr,
              sdonut=0,
              fsize=5,
              font_family="Arial",
              variable="variable",
              value="value",
              rowname="rowname",
              col_ramp="REACH blue",
              col_revert=F
){
  
  if(all(c("yi_lab","xi_lab") %in% names(gr))){
    labx<-unique(gr$yi_lab)
    laby<-unique(gr$xi_lab) 
  } else {
    labx<-unique(gr$yi) %>% proper
    laby<-unique(gr$xi) %>% proper
  }
  
  gr$variable<-gr[[variable]]  %>% sapply(wrapit,n=20)
  gr$value<-gr[[value]]
  gr$rowname<-gr[[rowname]]  %>% sapply(wrapit,n=20)
  
  gpsum<-(by(gr$value,gr$rowname,sum, simplify=FALSE) %>% unlist %>% as.data.frame)
  names(gpsum)<-"gpsum"
  gpsum$rowname<-row.names(gpsum)
  
  gr<-merge(gr,gpsum,by="rowname",all.x=T)
  
  gr$fraction = gr$value / gr$gpsum
  gr = gr[order(gr$rowname,gr$fraction), ]
  
  
  gr$ymax <- do.call(c, tapply(gr$fraction, 
                                   gr$rowname, 
                                   FUN=cumsum,simplify=F))
  
  gr$ymin<-gr$ymax - gr$fraction
    gr$pos<- do.call(c, tapply(gr$fraction, 
                             gr$rowname, 
                             FUN=function(x){cumsum(x)-x/2},simplify=F))
    
    colorsc<-color_ramp(gr$variable,col_ramp,col_revert)
    
	p2 = ggplot(gr, aes(fill=variable, ymax=ymax, ymin=ymin, xmax=100, xmin=sdonut)) +
			 geom_rect(colour="white",size=0.5) +
			 coord_polar(theta="y") +
			 xlim(c(0, 100)) +
		   facet_wrap( ~ rowname, ncol=2) + 
			 geom_text(aes(x=mean(c(sdonut,100)), y=pos, label = paste(round(value,1),"%",sep="")), size=round(fsize*0.8,0), family=font_family)  +
	     guides(fill = guide_legend(title = wrapit(laby,30))) + 
	      theme_minimal() +
				theme(
				 legend.key.width = unit(2, "cm"),
				 legend.key.height = unit(1, "cm"),
				 legend.title=element_text(family=font_family,size=round(fsize*2,0),face="plain",color="#42423E"),
				 legend.text=element_text(family=font_family,size=round(fsize*2,0),color="#42423E"),
				 axis.text.x=element_blank(),
				 axis.text.y=element_blank(),
				 axis.title.x = element_blank(),
				 panel.grid=element_blank(),
				 #axis.text=element_blank(),
				 axis.ticks=element_blank(),
				 axis.title.y = element_blank()
			) + scale_fill_manual(values=colorsc)
	return(p2)
}
	
	

bar_chart<-function(datag,variable="Var2",disa="Var1",value="value",nb="no",
  fsize=5,toorder=F,font_family="Arial Narrow",H_col="#c97b83",col_ramp="REACH red",col_revert=F,flip=T){
	
  smult=FALSE
  
  has_CI<-all(c("CI_lw","CI_up") %in% names(datag))
  
  datag$disa<-datag[[disa]] %>% sapply(wrapit,n=20)
  datag$variable<-datag[[variable]] %>% sapply(wrapit,n=20)
  datag$value<-datag[[value]]
  
  if(flip){scaletoreverse<-TRUE} else{scaletoreverse<-FALSE}
  
  if(toorder==TRUE){
    if(length(unique(datag$disa))==1){
      cdata <- datag
      ordered<-cdata[with(cdata, order(-value, variable)), ]$variable
    }else{
      cdata <- plyr::ddply(datag, "variable", summarise, mean = mean(value,na.rm=TRUE))
      ordered<-cdata[with(cdata, order(-mean, variable)), ]$variable
    } 
  }
  
  if(all(c("yi_lab","xi_lab") %in% names(datag))){
    labx<-unique(datag$yi_lab)
    laby<-unique(datag$xi_lab) 
  } else {
    labx<-unique(datag$yi) %>% proper
    laby<-unique(datag$xi) %>% proper
  }
  
  datag$variable<-lapply(datag$variable,wrapit) %>% unlist

		if(!smult & nb=="yes" & length(unique(datag$variable))==1 ){
			typ<-"int"
			if(flip){
				datag$disa<-factor(datag$disa,levels=rev(levels(factor(datag$disa))))
			}
			
			colorsc<-color_ramp(datag$variable,col_ramp,col_revert)
			p<-ggplot(data=datag, aes(x=disa, y=value))
			p<-p+geom_bar(fill=colorsc,stat="identity",position = position_dodge(width = 0.9), alpha=.9)
			
		}else if(!smult & nb=="yes" & length(unique(datag$variable))>1 ){
			typ<-"barHy"
			 colorsc<-color_ramp(datag$variable,col_ramp,col_revert)
			if(flip){
				datag$disa<-factor(datag$disa,levels=rev(levels(factor(datag$disa))))
			}
			p<-ggplot(data=datag, aes(x=datag$disa, y=datag$value, fill=datag$variable))
			p<-p+geom_bar(stat="identity",position = position_dodge(width = 0.9), alpha=.9)
				
		# }else if(!smult & !(any(levels(as.factor(datag[,2]))=="Oui")) & !(any(levels(as.factor(datag[,2]))=="yes")) & !(any(levels(as.factor(datag[,2]))=="oui")) ){
		# 	typ<-"barHy"
		# 	if(flip){
		# 		datag$disa<-factor(datag$disa,levels=rev(levels(factor(datag$disa))))
		# 		datag$variable<-factor(datag$variable,levels=rev(levels(factor(datag$variable))))
		# 	}
		# 	p<-ggplot(data=datag, aes(x=datag$disa, y=datag$value, fill=datag$variable))
		# 	p<-p+geom_bar(stat="identity",position = position_dodge(width = 0.9), alpha=.9)
		# 		
		}else{
				typ<-"barV"
				colorsc<-color_ramp(datag$variable,col_ramp,col_revert)
				p<-ggplot(data=datag, aes(x=datag$disa, y=datag$value, fill=datag$variable))
				p<-p+geom_bar(stat="identity",position = position_dodge(width = 0.9), alpha=.9)
				if(toorder==TRUE){
					p<-p+scale_x_discrete(limits=rev(ordered))
				}else{
					p<-p+scale_x_discrete()
				}
		}
	
	
		if(has_CI & typ!="facet_barV"){
			p<-p+geom_errorbar(aes(ymin=datag$CI_lw, ymax=datag$CI_up),position = position_dodge(width = 0.9), alpha=.7,width=.5,color="#42423E")
		}
		
		if(typ=="int"){
			p<-p+geom_text(aes(label = formatC(round(datag$value,2),digits = 2,format = "f")),position = position_dodge(width = 0.75),family=font_family,size = round(fsize*1.2,0),color="#42423E")
		}else if(typ=="barV"){
			if(nb=="yes"){
				p<-p+geom_text(aes(label = formatC(round(datag$value,2), digits = 2, format = "f")),position = position_dodge(width = 0.75),family=font_family,size = round(fsize*1.2,0),color="#42423E")+
				scale_fill_manual(guide = guide_legend(reverse=scaletoreverse),wrapit(laby,30),values=rev(colorsc))
			}else{
				p<-p+geom_text(aes(label = paste0(formatC(round(datag$value,1), digits = 1, format = "f"),"%")),position = position_dodge(width = 0.75),family=font_family,size = round(fsize*1.2,0),color="#42423E")+
				scale_fill_manual(guide = guide_legend(reverse=scaletoreverse),wrapit(laby,30),values=rev(colorsc))
			}
		}else{
			if(nb=="yes"){
				p<-p+geom_text(aes(label = formatC(round(datag$value,2), digits = 2, format = "f")),position = position_dodge(width = 0.75),family=font_family,size = round(fsize*1.2,0),color="#42423E")+
				scale_fill_manual(guide = guide_legend(reverse=scaletoreverse),wrapit(laby,30),values=rev(colorsc))
			}else{
				p<-p+geom_text(aes(label = paste0(formatC(round(datag$value,1), digits = 1, format = "f"),"%")),position = position_dodge(width = 0.75),family=font_family,size = round(fsize*1.2,0),color="#42423E")+
				scale_fill_manual(guide = guide_legend(reverse=scaletoreverse),wrapit(laby,30),values=rev(colorsc))
			}
		}
		
		if(toorder==TRUE){
			p<-p+scale_x_discrete(limits=rev(ordered))
			p<-p+scale_y_continuous(breaks=NULL)
		}else{
			p<-p+scale_y_continuous(breaks=NULL)
		}
  
    p<-p+theme_minimal()+
		theme(
			legend.key.width = unit(2, "cm"),
			legend.key.height = unit(1, "cm"),
			legend.title=element_text(family=font_family,size=round(fsize*3.4,0),face="plain",color="#42423E"),
            legend.text=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
			axis.text.x=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
            axis.text.y=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
			axis.title.x = element_blank(),axis.title.y = element_blank()
      )+labs(x = wrapit(labx,30))
	  

	if(flip){
		p<-p+coord_flip()
	}
  return(p)
}


# graphic messy function0
graph_crosst_sc<-function(datag,labx,laby,nb,fsize,is_ordered,ordeR,font_family,type_graph,flip,H_col,smult,col_ramp,col_revert){
	
	# here can be percentage
	fct<-100
	has_CI<-all(c("CI_lw","CI_up") %in% names(datag))
	names(datag)[1]<-"disa"
	
	if(has_CI){
	  if(nb=="yes"){
			datag$CI_lw<-coerc(datag$CI_lw)
			datag$CI_up<-coerc(datag$CI_up)
	  }else{
	    datag$CI_lw<-coerc(datag$CI_lw)*fct
	    datag$CI_up<-coerc(datag$CI_up)*fct
	  }
	}
	
	if(nb=="yes"){
		 datag$value<-coerc(datag$value)
	}else{
		 datag$value<-coerc(datag$value)*fct
	}
		
	datag$varible<-lapply(datag$varible,wrapit) %>% unlist
	datag$disa<-lapply(datag$disa,wrapit) %>% unlist
	
  colorsc<-color_ramp2(datag,nb,col_ramp,col_revert,H_col,smult,NULL)


  if(type_graph=="Heat map"){
		typ<-"heat"
		
		datag$variable<-factor(datag$variable,levels=rev(levels(factor(datag$variable))))
		p <- ggplot(datag, aes(x=datag$disa, y=datag$variable)) 
		p<-p+geom_tile(aes(fill = datag$value),colour = "white")
		if(has_CI){
			if(nb=="yes"){
				p<-p+geom_text(aes(label=paste(round(datag$value,2)," (",round(datag$CI_lw,2)," , ",round(datag$CI_up,2),")",sep="")), family=font_family,size = fsize,color="#42423E")
			}else{
				p<-p+geom_text(aes(label=paste(round(datag$value,1),"% (",round(datag$CI_lw,1)," , ",round(datag$CI_up,1),")",sep="")), family=font_family,size = fsize,color="#42423E")
			}
		}else{
			if(nb=="yes"){
				p<-p+geom_text(aes(label=paste(round(datag$value,2),sep="")), family=font_family,size = fsize,color="#42423E")
			} else {
				p<-p+geom_text(aes(label=paste(round(datag$value,1),"%",sep="")), family=font_family,size = fsize,color="#42423E")
			}
		}
		if(is_ordered==TRUE){
			p<-p+scale_y_discrete(limits=rev(ordeR))
		}
	
		p<-p+scale_fill_gradient(low = "white",high = H_col )+
		 scale_x_discrete(expand = c(0, 0)) +
		 xlab("") + 
		 ylab("") 
		
  }else{
		
		if(smult & nb=="No"){
			typ<-"barVmult"
			if(flip){
				datag$disa<-factor(datag$disa,levels=rev(levels(factor(datag$disa))))
			}
			p<-ggplot(data=datag, aes(x=datag$variable, y=datag$value, fill=datag$disa))
			tp<-labx;labx<-laby;laby<-tp
			p<-p+geom_bar(stat="identity",position = position_dodge(width = 0.9), alpha=.9)
			p<-p+scale_x_discrete(limits=rev(ordeR))
			
		} else if(any(names(datag)%in%"facet")){
			typ<-"facet_barV"
			datag$disa<-as.character(datag$disa)
			datag$facet<-as.character(datag$facet)
			datag$variable<-as.character(datag$variable)
			if(flip){
				datag$variable<-factor(datag$variable,levels=rev(levels(factor(datag$variable))))
			}
			
			tp<-labx;labx<-laby;laby<-tp
			
			if(length(unique(datag$disa))==1){
				p<-ggplot(data=datag, aes(x=datag$variable, y=datag$value, fill=facet))#,group=facet))
				p<-p+geom_bar(stat="identity",position = position_dodge(width = 0.9), alpha=.9)
			} else {
				p<-ggplot(data=datag, aes(x=datag$variable, y=datag$value, fill=disa))#,group=facet))
				p<-p+geom_bar(stat="identity",position = position_dodge(width = 0.9), alpha=.9)
			}
			
			if(has_CI){
					p<-p+geom_errorbar(aes(ymin=CI_lw, ymax=CI_up),position = position_dodge(width = 0.9), alpha=.7,width=.5,color="#42423E")
			}
			
			if(length(unique(datag$disa))!=1){
				p<-p+facet_wrap(~ facet)
			}
			
		} else if(!smult & nb=="yes" & length(unique(datag$variable))==1 ){
			typ<-"int"
			if(flip){
				datag$disa<-factor(datag$disa,levels=rev(levels(factor(datag$disa))))
			}
			p<-ggplot(data=datag, aes(x=disa, y=value))
			p<-p+geom_bar(fill=colorsc,stat="identity",position = position_dodge(width = 0.9), alpha=.9)
			
		}else if(!smult & nb=="yes" & length(unique(datag$variable))>1 ){
			typ<-"barHy"
			 colorsc<-color_ramp2(datag,"no",col_ramp,col_revert,H_col,smult,NULL)
			if(flip){
				datag$disa<-factor(datag$disa,levels=rev(levels(factor(datag$disa))))
			}
			p<-ggplot(data=datag, aes(x=datag$disa, y=datag$value, fill=datag$variable))
			p<-p+geom_bar(stat="identity",position = position_dodge(width = 0.9), alpha=.9)
				
		}else if(!smult & !(any(levels(as.factor(datag[,2]))=="Oui")) & !(any(levels(as.factor(datag[,2]))=="yes")) & !(any(levels(as.factor(datag[,2]))=="oui")) ){
			typ<-"barHy"
			if(flip){
				datag$disa<-factor(datag$disa,levels=rev(levels(factor(datag$disa))))
				datag$variable<-factor(datag$variable,levels=rev(levels(factor(datag$variable))))
			}
			p<-ggplot(data=datag, aes(x=datag$disa, y=datag$value, fill=datag$variable))
			p<-p+geom_bar(stat="identity",position = position_dodge(width = 0.9), alpha=.9)
				
		}else{
				typ<-"barV"
				p<-ggplot(data=datag, aes(x=datag$variable, y=datag$value, fill=datag$disa))
				tp<-labx;labx<-laby;laby<-tp
				p<-p+geom_bar(stat="identity",position = position_dodge(width = 0.9), alpha=.9)
				if(is_ordered==TRUE){
					p<-p+scale_x_discrete(limits=rev(ordeR))
				}else{
					p<-p+scale_x_discrete()
				}
		}
	
	
		if(ncol(datag)>=5 & typ!="facet_barV"){
			p<-p+geom_errorbar(aes(ymin=datag$CI_lw, ymax=datag$CI_up),position = position_dodge(width = 0.9), alpha=.7,width=.5,color="#42423E")
		}
		
		# if(!smult & nb=="yes" & length(unique(datag$variable))==1 ){
			# p<-p+geom_errorbar(aes(ymin=datag$CI_lw, ymax=datag$CI_up),position = position_dodge(width = 0.9), alpha=.7,width=.5,color="#42423E")
		# }
		
		if(typ=="facet_barV"){
			p<-p+geom_text(aes(label = paste0(formatC(round(datag$value,1), digits = 1, format = "f"),"%")),position = position_dodge(width = 0.75),family=font_family,size = round(fsize*1.2,0),color="#42423E")+
			scale_fill_manual(guide = guide_legend(reverse=TRUE),wrapit(laby,10),values=rev(colorsc))			
		}else if(typ=="int"){
			p<-p+geom_text(aes(label = formatC(round(datag$value,2),digits = 2,format = "f")),position = position_dodge(width = 0.75),family=font_family,size = round(fsize*1.2,0),color="#42423E")
		}else if(typ=="barV"){
			if(nb=="yes"){
				p<-p+geom_text(aes(label = formatC(round(datag$value,2), digits = 2, format = "f")),position = position_dodge(width = 0.75),family=font_family,size = round(fsize*1.2,0),color="#42423E")+
				scale_fill_manual(guide = guide_legend(reverse=FALSE),wrapit(laby,10),values=rev(colorsc))
			}else{
				p<-p+geom_text(aes(label = paste0(formatC(round(datag$value,1), digits = 1, format = "f"),"%")),position = position_dodge(width = 0.75),family=font_family,size = round(fsize*1.2,0),color="#42423E")+
				scale_fill_manual(guide = guide_legend(reverse=FALSE),wrapit(laby,10),values=rev(colorsc))
			}
		}else{
			if(nb=="yes"){
				p<-p+geom_text(aes(label = formatC(round(datag$value,2), digits = 2, format = "f")),position = position_dodge(width = 0.75),family=font_family,size = round(fsize*1.2,0),color="#42423E")+
				scale_fill_manual(guide = guide_legend(reverse=FALSE),wrapit(laby,10),values=rev(colorsc))
			}else{
				p<-p+geom_text(aes(label = paste0(formatC(round(datag$value,1), digits = 1, format = "f"),"%")),position = position_dodge(width = 0.75),family=font_family,size = round(fsize*1.2,0),color="#42423E")+
				scale_fill_manual(guide = guide_legend(reverse=FALSE),wrapit(laby,10),values=rev(colorsc))
			}
		}
		
		if(is_ordered==TRUE){
			p<-p+scale_x_discrete(limits=rev(ordeR))
			p<-p+scale_y_continuous(breaks=NULL)
		}else{
			p<-p+scale_y_continuous(breaks=NULL)
		}
		
  }
  if(typ=="barVmult"){
    p<-p+theme_pander()+
		theme(
			legend.position = c(0.8,0.2),
			legend.key.width = unit(2, "cm"),
			legend.key.height = unit(1, "cm"),
			legend.title=element_text(family=font_family,size=round(fsize*3.4,0),face="plain",color="#42423E"),
            legend.text=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
			axis.text.x=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
            axis.text.y=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
			axis.title.x = element_blank(),
			axis.title.y = element_blank()
      )+labs(x = wrapit(labx,10))
	  
	}else if(typ=="facet_barV"){
    p<-p+theme_pander()+
		theme(
			legend.key.width = unit(2, "cm"),
			legend.key.height = unit(1, "cm"),
			legend.title=element_text(family=font_family,size=round(fsize*3.4,0),face="plain",color="#42423E"),
            legend.text=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
			axis.text.x=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
            axis.text.y=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
			axis.title.x = element_blank(),axis.title.y = element_blank(),
			strip.text.x = element_text(family=font_family,size=round(fsize*3.4,0),face="plain",color="#42423E"),
			strip.text.y = element_text(family=font_family,size=round(fsize*3.4,0),face="plain",color="#42423E")
    )+labs(x = wrapit(labx,10))
	  
	}else if(typ!="heat"){
    p<-p+theme_pander()+
		theme(
			legend.key.width = unit(2, "cm"),
			legend.key.height = unit(1, "cm"),
			legend.title=element_text(family=font_family,size=round(fsize*3.4,0),face="plain",color="#42423E"),
            legend.text=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
			axis.text.x=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
            axis.text.y=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
			axis.title.x = element_blank(),axis.title.y = element_blank()
      )+labs(x = wrapit(labx,10))
	  
  }else{
    p.bot<-p+ theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA,color="white", size=0.5, linetype="solid"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="white"),
        plot.background = element_rect(fill="white"),
        legend.position = "none", 
        axis.text.x = element_blank(),
        plot.margin = unit(c(1,0,0,0), "cm"),
        axis.text.y=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"))

	p.top   <-  p.bot + theme(
		axis.text.x = element_text(family=font_family,size=round(fsize*3.4,0)),
		axis.text.y = element_text(family=font_family,size=round(fsize*3.4,0),color="white")
		)  + coord_cartesian(ylim = c(0,0))
		
	require(gtable)
	#Extract Grobs
	g1<-ggplotGrob(p.top)
	g2<-ggplotGrob(p.bot)
	#Bind the tables
	g<-gtable:::rbind_gtable(g1, g2, "first")
	#Remove a row between the plots
	g <- gtable_add_rows(g, unit(-1.25,"cm"), pos=nrow(g1))
	#draw
	panels <- g$layout$t[grep("panel", g$layout$name)]
	g$heights[panels] <- unit.c(unit(0,"null"),unit(2,"null"))
	grid.newpage()
	grid.draw(g)
	
  
  }
  if(typ!="heat"){
	if(flip){
		p<-p+coord_flip()
	}
  }
  if(typ!="heat"){
   print(p)
  }
}


graph_line<-function(datag,laby,fsize,font_family,H_col,nb,col_ramp,col_revert){
	
	
	names(datag)[1]<-"disa"
	
	if(ncol(datag)>=5){
		names(datag)[4:5]<-c("CI_lw","CI_up")
		
		datag$CI_lw<-as.numeric(as.character(datag$CI_lw))
		datag$CI_up<-as.numeric(as.character(datag$CI_up))
	}
	
		datag$value<-as.numeric(as.character(datag$value))
		
	
	if(nb=="yes"){
	
		colorsc<-color_ramp2(datag,nb,col_ramp,col_revert,H_col,FALSE,NULL)
		datag$disa<-factor(datag$disa,levels=levels(factor(datag$disa)))
		p<-ggplot(data=datag, aes(x=datag$disa, y=datag$value,group=1))
		p<-p+geom_ribbon(aes(ymin = datag$CI_lw, ymax = datag$CI_up), fill = H_col,alpha=0.05) 
		p<-p+geom_path(color=H_col,stat="identity",position = position_dodge(width = 0.9), alpha=.9,size=1.5)	
			
		p<-p+geom_errorbar(aes(ymin=datag$CI_lw, ymax=datag$CI_up),position = position_dodge(width = 0.9), alpha=.7,width=.5,color="#42423E")
		p<-p+geom_text(aes(label = paste0(formatC(round(datag$value,2), digits = 1, format = "f"))),position = position_dodge(width = 0.75),family=font_family,size = round(fsize*0.8,0),color="#42423E")+
				scale_fill_manual(guide = guide_legend(reverse=TRUE),wrapit(laby,10),values=colorsc)+
				scale_y_continuous(breaks=NULL)
				
		
		p<-p+theme_pander()+
		theme(
			 legend.key.width = unit(2, "cm"),
			 legend.key.height = unit(1, "cm"),
			 legend.title=element_text(family=font_family,size=round(fsize*3.4,0),face="plain",color="#42423E"),
			 legend.text=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
			 axis.text.x=element_text(family=font_family,size=round(fsize*3,0),color="#42423E"),
			 axis.text.y=element_text(family=font_family,size=round(fsize*3,0),color="#42423E"),
			 axis.title.x = element_blank(),
			 axis.title.y = element_blank()
		)
				
		p<-p+labs(x = wrapit(laby,30))
	
	} else {
	
		

		if(any(names(datag)%in%"facet")){
			typ<-"facet_barV"
			datag<-class_assess(datag)
			datag$variable<-as.factor(as.character(datag$variable))
			datag$facet<-as.factor(as.character(datag$facet))
			
		
			if(length(unique(datag$disa))!=1){
				p<-ggplot(data=datag, aes(x=disa, y=value, colour=datag$variable, group=datag$variable))
				colorsc<-color_ramp2(datag,"no",col_ramp,col_revert,H_col,TRUE,"variable")
			} else {
				p<-ggplot(data=datag, aes(x=datag$variable, y=value, colour=facet,  group=facet))
				colorsc<-color_ramp2(datag,"no",col_ramp,col_revert,H_col,TRUE,"facet")
			}
			
			p<-p+geom_line(size=0.8)
						
			if(ncol(datag)>=5){
				p<-p+geom_ribbon(aes(ymin = CI_lw, ymax = CI_up) ,alpha=0.1,fill = "grey70",colour="white") 
			}
			
			p<-p+scale_color_manual(guide = guide_legend(reverse=FALSE),wrapit(laby,10),values=colorsc)
			
			if(length(unique(datag$disa))!=1){
				p<-p+facet_wrap(~ facet)
			}
			
		} else {
		
			datag$variable<-as.numeric(as.character(datag$variable))
			colorsc<-color_ramp2(datag,nb,col_ramp,col_revert,H_col,FALSE,"disa")
		
			p<-ggplot(data=datag, aes(x=datag$variable, y=datag$value, colour=datag$disa,group=datag$disa))
			p<-p+geom_line(size=0.8)
			if(ncol(datag)>=5){
				p<-p+geom_ribbon(aes(ymin = datag$CI_lw, ymax = datag$CI_up) ,alpha=0.1,fill = "grey70",colour="white") 
			}
			
			p<-p+scale_color_manual(guide = guide_legend(reverse=FALSE),wrapit(laby,10),values=colorsc)
		}
		
		p<-p+theme_bw()+
		theme(
			 legend.key.width = unit(2, "cm"),
			 legend.key.height = unit(1, "cm"),
			 legend.title=element_text(family=font_family,size=round(fsize*3.4,0),face="plain",color="#42423E"),
			 legend.text=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
			 axis.text.x=element_text(family=font_family,size=round(fsize*3,0),color="#42423E"),
			 axis.text.y=element_text(family=font_family,size=round(fsize*3,0),color="#42423E"),
			 panel.border = element_blank(),
			 panel.grid.major.x = element_blank() ,
			 # panel.grid.minor = element_blank(),
			 #panel.grid.major = element_blank(),
			 axis.title.x = element_blank(),
			 axis.title.y = element_blank()
		)
	
	}
	
	  
	print(p)
	  
}



make_hist<-function(db,disa=NULL,level=NULL){
  if(is.null(disa)){
    dbf<-db
  } else {
    dbf<-db[which(db[[disa]]==level),]
  }
  
  dbf<-dbf[which(!is.na(dbf$duration_displacement)),]

if(range(dbf$duration_displacement,na.rm=T)[2]-range(dbf$duration_displacement,na.rm=T)[1]>20){
  dbf$duration_displacement<-dbf$duration_displacement %>% cut(pretty(dbf$duration_displacement,10), include.lowest = T)
}
### histogram displacement
p2<-ggplot(dbf, aes(duration_displacement)) +
  geom_bar(aes(y = (..count..)/sum(..count..)*100),color='white',fill='#0071bb',width=0.4)+
  theme_minimal()+ 
  xlab("Duration displacement (months)") + 
  ylab("% Households") 
ggsave(sprintf("figures/duration_displacement_%s.png",level),p2,width = 4.4, height = 2.2)
}


plot_for_data_merge<-function(hh, which_sites){

  hh %>%  class_assess->hh

  db<-hh[hh$site_name==which_sites & hh$rhu_usage=="Used by displaced population as living space",]
  dbf<-db
  if(range(dbf$duration_displacement,na.rm=T)[2]-range(dbf$duration_displacement,na.rm=T)[1]>20){
    dbf$duration_displacement<-dbf$duration_displacement %>% cut(pretty(dbf$duration_displacement,10), include.lowest = T)
  }
  ### histogram displacement
  p2<-ggplot(dbf, aes(duration_displacement)) +
    geom_bar(aes(y = (..count..)/sum(..count..)*100),color='white',fill='#0071bb',width=0.4)+
      theme_minimal()+ 
      xlab("Duration displacement (months)") + 
      ylab("% Households") 
  ggsave(sprintf("../datamerge/figures/duration_displacement_%s.png",which_sites),p2,width = 4.4, height = 2.2)
  
  
  if(range(dbf$duration_live_rhu,na.rm=T)[2]-range(dbf$duration_live_rhu,na.rm=T)[1]>20){
    dbf$duration_live_rhu<-dbf$duration_live_rhu %>% cut(pretty(dbf$duration_live_rhu,10), include.lowest = T)
  }
  
  dbf<-data.frame(duration_live_rhu=na.omit(dbf$duration_live_rhu))
  
  p1<-ggplot(dbf, aes(duration_live_rhu)) +
      geom_bar(aes(y = (..count..)/sum(..count..)*100),color='white',fill='#0071bb',width=0.8)+
      theme_minimal() +  
      xlab("Duration in RHU (months)") + 
      ylab("% displaced population") 
  
  ggsave(sprintf("../datamerge/figures/duration_live_rhu_%s.png",which_sites),p1,width = 4.4, height = 2.2)
  
  
  ### graph for likert scale
  require(RColorBrewer)
  dblikert<-db
  
  theorder<-c("0_prefer_not_to_answer","1_very_dissatisfied","2_dissatisfied","3_neutral","3_neutral_down","5_very_satisfied","4_satisfied","3_neutral_up")
  
  var_likert<-c("thermal_confort","living_space","privacy","ventilation","lighting","wind_resistance","prefer_rhu","overall")
  for(x in var_likert){
    dblikert[[x]]<-car::recode(dblikert[[x]],"
    'very_dissatisfied'='1_very_dissatisfied';
    'dissatisfied'='2_dissatisfied';
    'neutral'='3_neutral';
    'satisfied'='4_satisfied';'very_satisfied'='5_very_satisfied';'prefer_not_to_answer'='0_prefer_not_to_answer'")
    dblikert[[x]]<-factor( dblikert[[x]],levels=theorder)
  }
  
  try({data.likert<-lapply(var_likert,function(x,dblikert){
    table(dblikert[[x]])
  },dblikert=dblikert) %>% do.call(bind_rows,.) %>% apply(.,2,function(x) car::recode(x,"NA=0")) %>% 
    apply(.,1,function(x){x/sum(x,na.rm=T)*100}) %>% t %>% as.data.frame
  })
  
  if(exists("data.likert")){  
    data.likert$id<-var_likert
    mydata<-data.likert %>% melt()
    mydata$id<-proper(mydata$id)
    mydata$id[mydata$id=="Overall"]<-"OVERALL"
    
    var_order<-c("Thermal confort","Living space","Privacy","Ventilation","Lighting","Wind resistance")
    
    mydata1<-mydata[mydata$id%in% var_order,]
    mydata1$id<-as.factor(  mydata1$id)
    mydata1$id <- factor(mydata1$id,rev(levels(mydata1$id)[match(var_order,levels(mydata1$id))]))
  
    p<-plotlikert(mydata1)
    ggsave(sprintf("../datamerge/figures/satisfaction_%s.png",which_sites),p,width = 4, height = 3)
  }
}


make_the_log<-function(log,data,var,index,uuid,old,reason,implemented="yes"){
  rbind(log,data.frame(
    uuid=data[[uuid]][index] %>% ch,
    old.value=old%>% ch,
    new.value=data[[var]][index]%>% ch,
    question.name=var%>% ch,
    comments=reason%>% ch,
    implemented=implemented %>% ch,
    stringsAsFactors=F
  ))
}



