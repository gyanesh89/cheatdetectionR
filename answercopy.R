#' @export
answercopy<-function(rdf,itr_rdf){
n=length(rdf)-2
itr_rdf[is.na(itr_rdf)] <- 0
common_correct=matrix(rep(NA,n*n),nrow=n,ncol=n)
common_incorrect_sameoptions=matrix(rep(NA,n*n),nrow=n,ncol=n)
common_correct=matrix(rep(NA,n*n),nrow=n,ncol=n)
common_incorrect_differentoptions=matrix(rep(NA,n*n),nrow=n,ncol=n)
differentoutcomes=matrix(rep(NA,n*n),nrow=n,ncol=n)
x=1
for(r in sort(unique(rdf$centno) )){
  test1=rdf %>% dplyr::select(-sid,-centno)
  test2=itr_rdf %>% dplyr::select(-sid,-centno,-p_scor)
  counter<-rdf%>% filter(centno==r)
for(i in unique(counter$sid)){
  for(j in unique(counter$sid)){
    cat("going",r," ",x)
common_correct[i,j]=sum(test2[i,]==1 & test2[j,]==1,na.rm=T)
common_incorrect_sameoptions[i,j]=sum(test1[i,]==test1[j,],na.rm=T)-common_correct[i,j]
common_incorrect_differentoptions[i,j]=sum(test2[i,]==0 & test2[j,]==0,na.rm=T)-common_incorrect_sameoptions[i,j]
differentoutcomes[i,j]=sum(test2[i,]!=test2[j,],na.rm=T)
x=x+1
cat("\n")
}
}
}
common_correct=as.data.frame(common_correct) %>% as_tibble()
common_incorrect_sameoptions=as.data.frame(common_incorrect_sameoptions) %>% as_tibble()
common_incorrect_differentoptions=as.data.frame(common_incorrect_differentoptions) %>% as_tibble()
differentoutcomes=as.data.frame(differentoutcomes) %>% as_tibble()
return(list(common_correct=common_correct,common_incorrect_differentoptions=common_incorrect_differentoptions,
            common_incorrect_sameoptions=common_incorrect_sameoptions,differentoptions=differentoutcomes))
}
