
itempreknow<-function(itr_rdf){

  itr_rdf %>%
    group_by(centno) %>%
    summarise(avgscor = mean(p_scor, na.rm = T)) %>%
    ungroup() -> center_score_avgs
  names(itr_rdf) <- str_replace(names(itr_rdf), "V", "qno")

  itr_rdf %>%
    gather(key = "qno", value = "val", -centno) %>%
    group_by(centno, qno) %>%
    summarise(avg_item_score = mean(val, na.rm = TRUE),
              n_resp = n()) %>%
    ungroup() %>%
    inner_join(center_score_avgs) -> center_question_total_data

  center_question_total_data %>%
    ggplot(aes(avgscor, avg_item_score)) +
    geom_point(aes(alpha = n_resp)) +
    scale_alpha_continuous(range = c(0.01, 0.1)) +
    facet_wrap(~ qno)
}
