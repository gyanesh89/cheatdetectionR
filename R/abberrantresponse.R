
# This script contains person fit analysis code. Here we are using person fit analyis for detecting abberance.
abberantresponse <-function(itr_rdf,list_of_subjects,column_name){
expand.grid(
  subj = list_of_subjects,
  booklet = unique(itr_rdf[,column_name])) %>%
  mutate(splitkey = paste0(subj, "_", booklet)) -> subj_booklet_combinations

lapply(subj_booklet_combinations$splitkey, function(x) {

  # browser()

  cur_subj <- strsplit(x, "_")[[1]][1]
  cur_booklet <- strsplit(x, "_")[[1]][2]

  cat("\nProcessing subject", cur_subj, "and booklet", cur_booklet, "\n")

  cat("Generating the subject data frames...\n")

  res <- get_subj_itr(itr_rdf, cur_subj, cur_booklet, main_subj_qnums[[cur_subj]])

  # res$itr_subj_rdf

  cat("Calculating pfstats...\n")

  r.pbis(res$itr_subj_mat) -> pfstat_rpbis
  C.Sato(res$itr_subj_mat) -> pfstat_csato
  Ht(res$itr_subj_mat) -> pfstat_ht
  A.KB(res$itr_subj_mat) -> pfstat_akb
  E.KB(res$itr_subj_mat) -> pfstat_ekb
  D.KB(res$itr_subj_mat) -> pfstat_dkb

  cat("Calculating cutoffs...\n")

  cutoff(pfstat_rpbis, Blvl = 0.0001)$Cutoff -> cutoff_rpbis
  cutoff(pfstat_csato, Blvl = 0.0001)$Cutoff -> cutoff_csato
  cutoff(pfstat_ht, Blvl = 0.0001)$Cutoff -> cutoff_ht
  cutoff(pfstat_akb, Blvl = 0.0001)$Cutoff -> cutoff_akb
  cutoff(pfstat_ekb, Blvl = 0.0001)$Cutoff -> cutoff_ekb
  cutoff(pfstat_dkb, Blvl = 0.0001)$Cutoff -> cutoff_dkb


  aberrant_stats_raw <- tibble(
    rpbis = pfstat_rpbis$PFscores$PFscores,
    csato = pfstat_csato$PFscores$PFscores,
    ht = pfstat_ht$PFscores$PFscores,
    akb = pfstat_akb$PFscores$PFscores,
    ekb = pfstat_ekb$PFscores$PFscores,
    dkb = pfstat_dkb$PFscores$PFscores
  )

  cat("Generating flags...\n")

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

  res$itr_subj_rdf %>%
    bind_cols(aberrant_stats_flagged) -> itr_pfstat_rdf

  itr_pfstat_rdf %>%
    mutate(num_flags = rpbis_flag + csato_flag + ht_flag + akb_flag + ekb_flag + dkb_flag) -> sus_students

  list("cur_subj" = cur_subj, "cur_booklet" = cur_booklet, "sus_students" = sus_students, "flag_groups" = flag_groups)

}) -> res_raw

lapply(res_raw, function(x) {

  # browser()

  x$sus_students %>%
    filter(num_flags >= 5) %>%
    dplyr::select(sid, centno) %>%
    mutate(subject = x$cur_subj,
           booklet = x$cur_booklet,
           aberrant = TRUE)

}) -> res_nflags


bind_rows(res_nflags) %>%
  arrange(centno, sid) %>%
  spread(subject, aberrant)

bind_rows(res_nflags) %>%
  arrange(centno, sid) %>%
  group_by(centno) %>%
  summarise(n_stu = n_distinct(sid)) -> center_aber_counts

rdf %>%
  count(centno) %>%
  rename(tot_stu = n) %>%
  inner_join(center_aber_counts) %>%
  mutate(pct = round(n_stu / tot_stu * 100, 2)) %>%
  arrange(desc(pct)) -> center_aber_stats


hist(center_aber_stats$pct, breaks = 30)
boxplot(center_aber_stats$pct)


bind_rows(res_nflags) %>%
  arrange(centno, sid) %>%
  count(sid) %>%
  sample_n(20)
# filter(n == 3)

# interesting ids
# 508887

get_candidate_responses(rdf, itr_rdf, itemavgs_rdf, 18) %>%
  viz_candidate_responses(nrow = 1, hide_true_labs = F)

# % of candidates passed

itr_rdf %>%
  mutate(pass = if_else(p_scor > 90, "pass", "fail")) %>%
  group_by(centno, pass) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  spread(pass, n) %>%
  mutate(pct_pass = pass / (pass + fail)) -> pct_pass_fail

hist(pct_pass_fail$pct_pass)

pct_pass_fail %>%
  mutate(region = str_sub(centno, 1, 4)) %>%
  group_by(region) %>%
  summarise(n_cent = n(),
            avg_rate = mean(pct_pass)) %>%
  View()
}
