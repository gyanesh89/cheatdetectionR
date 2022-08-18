
# This script contains person fit analysis code. Here we are using person fit analyis for detecting abberance.
#' @export
abberrantresponse<-function(itr_rdf,cutoff=1){
itr_rdf[is.na(itr_rdf)]=0
itr_rdf %>% dplyr::select(-c(sid,centno,p_scor))->test1
PerFit::r.pbis(test1) -> pfstat_rpbis
PerFit::C.Sato(test1) -> pfstat_csato
PerFit::Ht(test1) -> pfstat_ht
PerFit::A.KB(test1) -> pfstat_akb
PerFit::E.KB(test1) -> pfstat_ekb
PerFit::D.KB(test1) -> pfstat_dkb
PerFit::cutoff(pfstat_rpbis, Blvl = 0.0001)$Cutoff -> cutoff_rpbis
PerFit::cutoff(pfstat_csato, Blvl = 0.0001)$Cutoff -> cutoff_csato
PerFit::cutoff(pfstat_ht, Blvl = 0.0001)$Cutoff -> cutoff_ht
PerFit::cutoff(pfstat_akb, Blvl = 0.0001)$Cutoff -> cutoff_akb
PerFit::cutoff(pfstat_ekb, Blvl = 0.0001)$Cutoff -> cutoff_ekb
PerFit::cutoff(pfstat_dkb, Blvl = 0.0001)$Cutoff -> cutoff_dkb
aberrant_stats_raw <- tibble(
  rpbis = pfstat_rpbis$PFscores$PFscores,
  csato = pfstat_csato$PFscores$PFscores,
  ht = pfstat_ht$PFscores$PFscores,
  akb = pfstat_akb$PFscores$PFscores,
  ekb = pfstat_ekb$PFscores$PFscores,
  dkb = pfstat_dkb$PFscores$PFscores
)
aberrant_stats_flagged <- aberrant_stats_raw %>%
  mutate(
    rpbis_flag = rpbis <= cutoff_rpbis,
    csato_flag = csato >= cutoff_csato,
    ht_flag = ht <= cutoff_ht,
    akb_flag = akb <= cutoff_akb,
    ekb_flag = ekb <= cutoff_ekb,
    dkb_flag = dkb >= cutoff_dkb,
  )
aberrant_stats_flagged %>%
  group_by(rpbis_flag, csato_flag, ht_flag, akb_flag, ekb_flag, dkb_flag) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(num_flags = rpbis_flag + csato_flag + ht_flag + akb_flag + ekb_flag + dkb_flag) -> flag_groups
itr_rdf %>% bind_cols(aberrant_stats_flagged)-> new_test
new_test %>%
  mutate(num_flags = rpbis_flag + csato_flag + ht_flag + akb_flag + ekb_flag + dkb_flag) -> sus_students
sus_students<-sus_students %>% filter(num_flags>=cutoff)
bind_rows(sus_students) %>%
  arrange(centno, sid) %>%
  group_by(centno) %>%
  summarise(n_stu = n_distinct(sid)) -> center_aber_counts
itr_rdf %>%
  count(centno) %>%
  rename(tot_stu = n) %>%
  inner_join(center_aber_counts) %>%
  mutate(pct = round(n_stu / tot_stu * 100, 2)) %>%
  arrange(desc(pct)) -> center_aber_stats
return(center_aber_stats=center_aber_stats)
}
