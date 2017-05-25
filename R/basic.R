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
