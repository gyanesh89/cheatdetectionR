#' @export
optimal_k_index_response_similarity <- function(data, item.loc, suspect_pairs){

  n_items = length(item.loc)

  data %>%
    mutate(across(.cols = item.loc,.fns = ~(.x == 0)),
           row_id = sid) %>%
    column_to_rownames(var = 'row_id') %>%
    dplyr::select(sid, item.loc) -> is_response_incorrect_df

  data %>%
    mutate(across(.cols = item.loc,.fns = is.na),
           row_id = sid) %>%
    column_to_rownames(var = 'row_id') %>%
    dplyr::select(sid,item.loc) %>%
    as.data.table() -> is_response_na_df

  colnames(is_response_na_df) <- c('sid', paste0('V',1:n_items))
  setkey(is_response_na_df,sid)

  is_response_incorrect_df %>%
    mutate(n_incorrect = rowSums(is_response_incorrect_df[,2:(n_items+1)], na.rm = TRUE)) %>%
    dplyr::select(sid, n_incorrect) -> n_incorrect_df

  is_response_incorrect_df <- as.data.table(is_response_incorrect_df)
  colnames(is_response_incorrect_df) <- c('sid', paste0('V',1:n_items))
  setkey(is_response_incorrect_df,sid)

  n_incorrect_df %>%
    group_by(n_incorrect) %>%
    group_split() %>%
    map_dfr(function(df){

      out_df <- expand_grid(copier_id = df$sid,
                            subgroup_id = df$sid)
      out_df

    }) %>%
    lazy_dt() -> subgroup_df

  suspect_pairs %>%
    mutate(chunk = ceiling(row_number()/2000)) %>%
    split(.$chunk) -> pairs_list

  cols <- paste0('V',1:n_items)

  map_dfr(pairs_list, function(pairs){

    print(paste0('Progress:',100*as.numeric(pairs$chunk[1])/length(pairs_list), '%'))

    lazy_dt(pairs) %>%
      left_join(subgroup_df, by = 'copier_id') %>%
      as_tibble() %>%
      mutate(common_incorrect = rowSums((is_response_incorrect_df[subgroup_id,..cols] & is_response_incorrect_df[source_id,..cols]),na.rm = TRUE) +
               rowSums((is_response_na_df[subgroup_id,..cols] & is_response_na_df[source_id,..cols]))) %>%
      group_by(copier_id,source_id) %>%
      summarise(avg_common_incorrect = mean(common_incorrect), .groups = 'drop') %>%
      left_join(n_incorrect_df %>%
                  rename(w_s=n_incorrect), by = c('source_id' = 'sid')) %>%
      mutate(m = rowSums((is_response_incorrect_df[copier_id,..cols] & is_response_incorrect_df[source_id,..cols]),na.rm = TRUE) +
               rowSums((is_response_na_df[copier_id,..cols] & is_response_na_df[source_id,..cols])),
             p = avg_common_incorrect/w_s,
             k.index = 1-pbinom(m-1,w_s,p)) %>%
      dplyr::select(copier_id,source_id,k.index) -> output

    output

  }) -> res

  return(res=res)
}
