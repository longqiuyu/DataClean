# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
library(lubridate)
translate <- function(data,code,output){
  data[substr(data,1,nchar(code)) %in% code]=output
  data
}

#Extend function Translate to a vector, and interpret to "Other" for codes not mentioned
recode <- function(data,code_list,output_list,other="Other"){
  data=as.character(data)
  for (i in 1:length(code_list)){
    data=translate(data,code_list[i],output_list[i])
  }
  data[!data %in% output_list]=other
  data
}


set_level <- function(data,base=default){
  data <- as.character(data)
  default=names(which.max(table(data)))
  relevel(as.factor(data),ref=base)
}

duplicates <- function(dataFrame,var){
  temp=dataFrame[duplicated(dataFrame[var]) | duplicated(dataFrame[var],fromLast = TRUE),]
  temp=temp[order(temp[var]),]
  temp
}

deduplicates <- function(dataFrame,var,all=FALSE){
  if (all) return(dataFrame[!(duplicated(dataFrame[var])|duplicated(dataFrame[var],fromLast = TRUE)),])
  if (!all) return(dataFrame[!duplicated(dataFrame[var]),])
}

get_next_Tue<-function(date){
  date=as.Date(date)
  dow=wday(date)
  if(dow<=3) return(date+3-dow)
  if(dow>3) return(date+10-dow)
}

date_to_ymd <- function(date){
  y=year(date)
  m=month(date)
  d=day(date)
  if (m<10) m=paste('0',m,sep='')
  if (d<10) d=paste('0',d,sep='')
  return(paste(y,m,d,sep=''))
}

get_historical_status <- function(stsp_history, SUB_RRN, date){
  if (class(date)!="Date") stop("input 'date' is not Date format")
  temp_history=stsp_history[stsp_history$SUB_RRN==SUB_RRN,]
  temp_history=temp_history[temp_history$STSP_EFF_DT<=date,]
  temp_history=temp_history[order(temp_history$STSP_EFF_DT,decreasing = TRUE),]
  if (dim(temp_history)[1]==0) return ('Active')
  if (temp_history$STSP_TYPE_CD[1]=="P") return ('Former')
  if (temp_history$STSP_TYPE_CD[1]=="S") return ('Active')
}
