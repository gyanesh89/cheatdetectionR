library(tidyverse)
library(purrr)
library(stringdist)
answercopy<-function(rdf,itr_rdf,n=10000){
common_correct=matrix(rep(NA,n*n),nrow=n,ncol=1)
common_incorrect_sameoptions=matrix(rep(NA,n*n),nrow=n,ncol=n)
common_correct=matrix(rep(NA,n*n),nrow=n,ncol=n)
common_incorrect_differentoptions=matrix(rep(n*n),nrow=n,ncol=n)
differentoutcomes=matrix(rep(NA,n*n),nrow=n,ncol=n)
x=1
for(r in sort(unique(rdf$centno) )){
  test1=rdf %>% dplyr::select(-sid,-centno)
  test2=itr_rdf %>% dplyr::select(-sid,-centno,-p_scor)
  counter<-rdf%>% filter(centno==r)
for(i in unique(counter$sid)){
  for(j in unique(counter$sid)){
    cat("going",r," ",x)
common_correct[i,j]=sum(test2[i,]==1 & test2[j,]==1)
common_incorrect_sameoptions[i,j]=sum(test1[i,]==test1[j,])-common_correct[i,j]
common_incorrect_differentoptions[i,j]=sum(test2[i,]==0 & test2[j,]==0)-common_incorrect_sameoptions[i,j]
differentoutcomes=sum(test2[i,]!=test2[j,])
x=x+1
cat("\n")
}
}
}
}
