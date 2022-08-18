#' @export
tableconverter<-function(rdf,itr_rdf,common_correct,common_incorrect_sameoptions,common_incorrect_differentoptions,differentoptions){
cc=0
cis=0
sid1=0
sid2=0
cid=0
d=0
centno=0
kindex=0
df=tibble(cc,cis,cid,d,sid1,sid2,centno,kindex)
n=length(rdf)-2
itr_rdf[is.na(itr_rdf)] <- 0
x=1
for(r in sort(unique(rdf$centno) )){
  test1=rdf %>% dplyr::select(-sid,-centno)
  test2=itr_rdf %>% dplyr::select(-sid,-centno,-p_scor)
  counter<-rdf%>% filter(centno==r)
  for(i in unique(counter$sid)){
    for(j in unique(counter$sid)){
      cat("going",r," ",x)
      cc=as.integer(common_correct[i,j])
      cis=as.integer(common_incorrect_sameoptions[i,j])
      cid=as.integer(common_incorrect_differentoptions[i,j])
      d=as.integer(differentoptions[i,j])
      sid1=i
      sid2=j
      centno=r
      kindex=optimal_k_index_response_similarity(itr_rdf,1:100,tibble(copier_id=i,source_id=j))$k.index
      x=x+1
      y=tibble(cc,cis,cid,d,sid1,sid2,centno,kindex)
      df=df %>% bind_rows(y)
      cat("\n")
    }
  }
}
df=df %>% filter(sid1!=sid2)

return(df)
}
