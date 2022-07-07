library(tidyverse)
n=10000
no_centres=100
no_question=100
datagen<-function(n=10000,no_centres=100,no_question=100){
sid=seq(1,n,1)
centno=sample.int(no_centres,n,replace=T)
prelim_rdf=matrix(sample.int(5,n*no_question,replace = T),nrow=n,ncol=no_question) %>% as_tibble()
prelim_rdf %>% mutate(sid=sid,centno=centno)->rdf
answer_key=matrix(sample.int(5,no_question,replace=T),ncol=no_question) %>% as_tibble()
itr_rdf<-prelim_rdf
for (i in seq(1,n)){
  cat("Processing Candidate",i,"/n")
  itr_rdf[i,]<-(answer_key==prelim_rdf[i,])
}
itr_rdf<-itr_rdf %>% mutate(sid=sid,centno=centno)
itr_rdf<-itr_rdf %>% rowwise() %>% mutate(p_scor = sum(c_across(starts_with("V"))))
usethis::use_data(rdf,itr_rdf,overwrite = T,internal = T)
}

