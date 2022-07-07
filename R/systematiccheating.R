
# This script contains code for detecting cheating at a very high level view. This creates a SD vs Mean scores plot at center level.
systematiccheating<-function(itr_rdf){
itr_rdf %>%
  group_by(centno) %>%
  summarise(n = n(),
            avgscor = mean(p_scor, na.rm = T),
            sdscor = sd(p_scor, na.rm = T)) -> center_scores

center_scores %>%
  ggplot(aes(avgscor, sdscor)) +
  geom_point(aes(alpha = n)) +
  labs(title = "Looking for systematic cheating... anything in the bottom right corner?")
}
