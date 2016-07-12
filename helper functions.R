library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotflow)
library(scales)
library(randomcoloR)
library(ggthemes)
library(stringr)
#library(plyr)

library(tidyr)
library(directlabels)


plot_data<-function(df,text_type){
  tempt<-df %>%
    ggplot(aes(x=factor(year_col),y=count,group=term, color=term))+
    geom_line(alpha=.8)+
    guides(color=FALSE)+
    #scale_color_discrete(guide=guide_legend(title="Search Term"))+
    geom_text(aes(label=ifelse(count>0,count,"")),color="black",vjust=-.6)+
    labs(title=paste("# Search term hits in SIOP",text_type,"by year"))+
    
    theme(panel.background=element_blank(),
          axis.title=element_blank(),
          axis.text.y=element_blank())
  
  direct.label(tempt,list(first.qp,hjust=-.5,vjust=1))
}

run_search<-function(search_terms,text_type,siop){
  ###This returns a df ready for graphing word counts by year by term###
  
  #find search column
  ind<-match(text_type,names(siop))
  text_col<-siop[,ind]
  year_col<-siop$year
  
  count<-1
  
  #get counts for each search term
  for (term in search_terms){
    
    text_count<-str_count(text_col,term)
    #let's make sure only counts max 1 per presentation
    test_count<-ifelse(text_count>=1,1,0)
    
    if(count==1){
      outdf<-cbind(year_col,text_count)
    } else{
      outdf<-cbind(outdf,text_count)
    }
    count<-count+1
  }
  
  outdf<-as.data.frame(outdf)
  names(outdf)[2:(1+length(search_terms))]<-gsub(" ","_",search_terms)
  
  
  long_df<-outdf %>%
    gather(key="term",value="count",-year_col) %>%
    group_by(term, year_col) %>%
    summarise(count=sum(count))
  long_df$term<-factor(long_df$term)
  return(long_df)
}
