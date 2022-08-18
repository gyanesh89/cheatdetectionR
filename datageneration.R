#' @export
datagen<-function(n=100,no_centres=10,no_question=100){
sid=seq(1,n,1)
centno=sample.int(no_centres,n,replace=T)
prelim_rdf=matrix(sample.int(6,n*no_question,replace = T),nrow=n,ncol=no_question) %>% as_tibble()
prelim_rdf=prelim_rdf %>% replace_with_na_all(condition = ~.x ==6)
prelim_rdf %>% mutate(sid=sid,centno=centno)->rdf
answer_key=matrix(sample.int(5,no_question,replace=T),ncol=no_question) %>% as_tibble()
itr_rdf<-prelim_rdf
for (i in seq(1,n)){
  cat("Processing Candidate",i,"/n")
  itr_rdf[i,]<-(answer_key==prelim_rdf[i,])
}
itr_rdf<-itr_rdf %>% mutate(sid=sid,centno=centno)
itr_rdf<-itr_rdf %>% rowwise() %>% mutate(p_scor = sum(c_across(starts_with("V")),na.rm=T))
return(list(rdf=rdf,itr_rdf=itr_rdf))
}

