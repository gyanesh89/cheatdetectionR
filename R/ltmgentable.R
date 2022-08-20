#' @export
ltmgentable1<-function(itr_rdf){
  sid1=0
  sid2=0
  centno=0
  kindex=0
  df=tibble(sid1,sid2,centno,kindex)
  x=1
  for(r in sort(unique(itr_rdf$center) )){
    counter<-itr_rdf %>% filter(center==r)
    for(i in unique(counter$sid)){
      for(j in unique(counter$sid)){
        cat("going",r," ",x)
        sid1=i
        sid2=j
        centno=r
        kindex=optimal_k_index_response_similarity(itr_rdf,1:20,tibble(copier_id=i,source_id=j))$k.index
        x=x+1
        y=tibble(sid1,sid2,centno,kindex)
        df=df %>% bind_rows(y)
        cat("\n")
      }
    }
  }
  df=df %>% filter(sid1!=sid2)

  return(df)
}
