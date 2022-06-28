answercopy<-function(rdf,itr_rdf,column_name){
rdf_center_split <- split(rdf, rdf[,column_name])
itr_rdf_center_split <- split(itr_rdf, itr_rdf[,column_name])

random_cents <- sample(1:length(rdf_center_split), size = 5, replace = F)

rdf_center_split_subset <- rdf_center_split[random_cents]
itr_rdf_center_split_subset <- itr_rdf_center_split[random_cents]

a = Sys.time()
pmap_dfr(list(rdf_center_split, itr_rdf_center_split, seq_along(rdf_center_split)),
         function(x, y, i) {

           cat("Processing center", x$centno[1], "\n")
           cat(i, "out of", length(rdf_center_split), "(", round(i / length(rdf_center_split) * 100, 2), "% progress )", "\n")

           # x <- rdf_center_split[[1]]
           # y <- itr_rdf_center_split[[1]]

           stopifnot(nrow(x) == nrow(y))

           # x$splitkey <- paste0(x$book_code, x$lang1, x$lang1code, x$lang2, x$lang2code)
           # y$splitkey <- paste0(y$book_code, y$lang1, y$lang1code, y$lang2, y$lang2code)

           ### main paper stats

           x_split <- split(x, paste0(x$book_code,'_',x$psubtype))
           y_split <- split(y, paste0(x$book_code,'_',x$psubtype))

           map2(x_split, y_split, function(p, q) {


             p %>%
               select(sid, res1:res90) %>%
               gather(key = "qno", value = "mcqopt", -sid) -> sid_resps

             q %>%
               select(sid, res1:res90) %>%
               gather(key = "qno", value = "outcome", -sid) -> sid_outcomes

             get_unique_sid_combinations(p$sid) %>%
               as_tibble() %>%
               mutate(sid1 = as.integer(sid1), sid2 = as.integer(sid2)) %>%
               expand(nesting(sid1, sid2), qno = paste0("res", 1:90)) %>%
               inner_join(sid_resps, by = c("sid1" = "sid", "qno" = "qno")) %>%
               rename(mcqopt1 = mcqopt) %>%
               inner_join(sid_resps, by = c("sid2" = "sid", "qno" = "qno")) %>%
               rename(mcqopt2 = mcqopt) %>%
               inner_join(sid_outcomes, by = c("sid1" = "sid", "qno" = "qno")) %>%
               rename(outcome1 = outcome) %>%
               inner_join(sid_outcomes, by = c("sid2" = "sid", "qno" = "qno")) %>%
               rename(outcome2 = outcome) %>%
               mutate(type = case_when(
                 outcome1 == 1 & outcome2 == 1 ~ "common_correct",
                 outcome1 == 0 & outcome2 == 0 & mcqopt1 == mcqopt2 ~ "common_incorrect",
                 outcome1 == 0 & outcome2 == 0 & mcqopt1 != mcqopt2 ~ "common_incorrect_diffans",
                 TRUE ~ "different_outcomes"
               )) %>%
               count(sid1, sid2, type) %>%
               mutate(paper_type = "main")

           }) %>%
             bind_rows() %>%
             drop_na() -> main_stats

           ### language 1 stats

           x$l1_splitkey <- paste0(x$lang1, x$lang1code)
           x_split <- split(x, x$l1_splitkey)

           y$l1_splitkey <- paste0(y$lang1, y$lang1code)
           y_split <- split(y, y$l1_splitkey)

           map2(x_split, y_split, function(p, q) {

             p %>%
               select(sid, res91:res120) %>%
               gather(key = "qno", value = "mcqopt", -sid) -> sid_resps

             q %>%
               select(sid, res91:res120) %>%
               gather(key = "qno", value = "outcome", -sid) -> sid_outcomes

             get_unique_sid_combinations(p$sid) %>%
               as_tibble() %>%
               mutate(sid1 = as.integer(sid1), sid2 = as.integer(sid2)) %>%
               expand(nesting(sid1, sid2), qno = paste0("res", 91:120)) %>%
               inner_join(sid_resps, by = c("sid1" = "sid", "qno" = "qno")) %>%
               rename(mcqopt1 = mcqopt) %>%
               inner_join(sid_resps, by = c("sid2" = "sid", "qno" = "qno")) %>%
               rename(mcqopt2 = mcqopt) %>%
               inner_join(sid_outcomes, by = c("sid1" = "sid", "qno" = "qno")) %>%
               rename(outcome1 = outcome) %>%
               inner_join(sid_outcomes, by = c("sid2" = "sid", "qno" = "qno")) %>%
               rename(outcome2 = outcome) %>%
               mutate(type = case_when(
                 outcome1 == 1 & outcome2 == 1 ~ "common_correct",
                 outcome1 == 0 & outcome2 == 0 & mcqopt1 == mcqopt2 ~ "common_incorrect",
                 outcome1 == 0 & outcome2 == 0 & mcqopt1 != mcqopt2 ~ "common_incorrect_diffans",
                 TRUE ~ "different_outcomes"
               )) %>%
               count(sid1, sid2, type) %>%
               mutate(paper_type = "lang1")

           }) %>%
             bind_rows() %>%
             drop_na() -> l1_stats

           # language 2
           x$l2_splitkey <- paste0(x$lang2, x$lang2code)
           x_split <- split(x, x$l2_splitkey)

           y$l2_splitkey <- paste0(y$lang2, y$lang2code)
           y_split <- split(y, y$l2_splitkey)

           map2(x_split, y_split, function(p, q) {

             p %>%
               select(sid, res121:res150) %>%
               gather(key = "qno", value = "mcqopt", -sid) -> sid_resps

             q %>%
               select(sid, res121:res150) %>%
               gather(key = "qno", value = "outcome", -sid) -> sid_outcomes

             get_unique_sid_combinations(p$sid) %>%
               as_tibble() %>%
               mutate(sid1 = as.integer(sid1), sid2 = as.integer(sid2)) %>%
               expand(nesting(sid1, sid2), qno = paste0("res", 121:150)) %>%
               inner_join(sid_resps, by = c("sid1" = "sid", "qno" = "qno")) %>%
               rename(mcqopt1 = mcqopt) %>%
               inner_join(sid_resps, by = c("sid2" = "sid", "qno" = "qno")) %>%
               rename(mcqopt2 = mcqopt) %>%
               inner_join(sid_outcomes, by = c("sid1" = "sid", "qno" = "qno")) %>%
               rename(outcome1 = outcome) %>%
               inner_join(sid_outcomes, by = c("sid2" = "sid", "qno" = "qno")) %>%
               rename(outcome2 = outcome) %>%
               mutate(type = case_when(
                 outcome1 == 1 & outcome2 == 1 ~ "common_correct",
                 outcome1 == 0 & outcome2 == 0 & mcqopt1 == mcqopt2 ~ "common_incorrect",
                 outcome1 == 0 & outcome2 == 0 & mcqopt1 != mcqopt2 ~ "common_incorrect_diffans",
                 TRUE ~ "different_outcomes"
               )) %>%
               count(sid1, sid2, type) %>%
               mutate(paper_type = "lang2")

           }) %>%
             bind_rows() %>%
             drop_na() -> l2_stats

           bind_rows(main_stats, l1_stats, l2_stats) %>%
             spread(type, n, fill = 0) %>%
             mutate(common_total = common_correct + common_incorrect,
                    keep = case_when(paper_type == 'main' ~ common_total>=45,
                                     paper_type == 'lang1' ~ common_total>=15,
                                     paper_type == 'lang2' ~ common_total>=15)) %>%
             filter(keep) %>%
             select(-keep) %>%
             mutate(across(.cols = c(starts_with('common_'), different_outcomes), .fns = as.integer))

         }) -> res_common_corrects_incorrects
b = Sys.time()
b-a
save(res_common_corrects_incorrects,file = 'scripts/algorithm_results/july2019_p2/july2019_p2_results.Rdata')

stopifnot(exists('raw_answer_copying_results_path'))

load(raw_answer_copying_results_path)

res_common_corrects_incorrects %>%
  mutate(keep_2 = case_when(paper_type == 'main' ~ common_total>=75,
                            paper_type == 'lang1' ~ common_total>=25,
                            paper_type == 'lang2' ~ common_total>=25)) %>%
  filter(common_incorrect>=(common_incorrect_diffans+different_outcomes), keep_2) %>%
  left_join(rdf %>%
              select(sid, centno), by = c('sid1' = 'sid')) %>%
  select(-keep_2) -> suspected_answer_copying_student_pairs

rdf %>%
  select(sid, res1:res90) %>%
  filter(sid %in% (filter(suspected_answer_copying_student_pairs, paper_type == 'main') %$% unique(sid1))) %>%
  pivot_longer(cols = c(-sid), names_to = 'qno', values_to = 'mcqopt') %>%
  group_by(sid) %>%
  count(mcqopt) %>%
  ungroup() %>%
  filter(n>75) %$% unique(sid) -> main_paper_sids_with_garbage_resp

rdf %>%
  select(sid, res91:res120) %>%
  filter(sid %in% (filter(suspected_answer_copying_student_pairs, paper_type == 'lang1') %$% unique(sid1))) %>%
  pivot_longer(cols = c(-sid), names_to = 'qno', values_to = 'mcqopt') %>%
  group_by(sid) %>%
  count(mcqopt) %>%
  ungroup() %>%
  filter(n>22) %$% unique(sid) -> lang1_paper_sids_with_garbage_resp

rdf %>%
  select(sid, res121:res150) %>%
  filter(sid %in% (filter(suspected_answer_copying_student_pairs, paper_type == 'lang2') %$% unique(sid1))) %>%
  pivot_longer(cols = c(-sid), names_to = 'qno', values_to = 'mcqopt') %>%
  group_by(sid) %>%
  count(mcqopt) %>%
  ungroup() %>%
  filter(n>22) %$% unique(sid) -> lang2_paper_sids_with_garbage_resp

suspected_answer_copying_student_pairs %>%
  mutate(remove = case_when(paper_type == 'main' & sid1 %in% main_paper_sids_with_garbage_resp ~ T,
                            paper_type == 'lang1' & sid1 %in% lang1_paper_sids_with_garbage_resp ~ T,
                            paper_type == 'lang2' & sid1 %in% lang2_paper_sids_with_garbage_resp ~ T,
                            TRUE ~ F)) %>%
  filter(!remove) %>%
  select(-remove) -> suspected_answer_copying_student_pairs_2
suspected_answer_copying_student_pairs_2 %>%
  mutate(keep = case_when(paper_type %in% c('lang1','lang2') ~ (common_total>=28 & common_incorrect>=6) |
                            (common_incorrect>=0.5*common_correct & common_incorrect>=2*(common_incorrect_diffans+different_outcomes)),
                          paper_type == 'main' ~ T)) %>%
  filter(keep) %>%
  select(-keep) %>%
  mutate(exam = str_replace(paper_name, '_', ' ') %>% str_to_title()) -> suspected_answer_copying_student_pairs_3

suspected_answer_copying_student_pairs_3 %>%
  group_by(centno) %>%
  summarise(suspected_students = union(sid1, sid2) %>% length()) %>%
  full_join(rdf %>%
              count(centno, name = 'total_students')) %>%
  replace_na(replace = list(suspected_students = 0)) %>%
  mutate(perc_suspects = 100*(suspected_students/total_students),
         exam = str_replace(paper_name, '_', ' ') %>% str_to_title()) -> center_level_info
suspected_answer_copying_student_pairs_3 %>%
  write_csv(file = file.path(getActiveProject(),'scripts','algorithm_results', paper_name, 'suspected_student_pairs.csv'))

center_level_info %>%
  write_csv(file = file.path(getActiveProject(),'scripts','algorithm_results', paper_name, 'center_level_metrics.csv'))

rdf %>%
  filter(sid %in% (suspected_answer_copying_student_pairs_3 %$% union(sid1, sid2))) %>%
  mutate(exam = str_replace(paper_name, '_', ' ') %>% str_to_title()) %>%
  write_csv(file = file.path(getActiveProject(),'scripts','algorithm_results', paper_name, 'suspected_students_rdf.csv'))

itr_rdf %>%
  filter(sid %in% (suspected_answer_copying_student_pairs_3 %$% union(sid1, sid2))) %>%
  mutate(exam = str_replace(paper_name, '_', ' ') %>% str_to_title()) %>%
  write_csv(file = file.path(getActiveProject(),'scripts','algorithm_results', paper_name, 'suspected_students_itr_rdf.csv'))

itemavgs_rdf %>%
  mutate(exam = str_replace(paper_name, '_', ' ') %>% str_to_title()) %>%
  write_csv(file = file.path(getActiveProject(),'scripts','algorithm_results', paper_name, 'itemavgs.csv'))
}
