# Functions for source in other codes
# less "polution" in main codes
# created for use in our data

# PRS value adjustment
adjust_model <- function(data_list) {
  # data must contain the dfs with
  # variables adjustment
  plyr::join_all(
    data_list,
    by = "IID",
    type = "inner"
   )
  new_PRS <- glm(PRS_child ~ sex:PC20, data = var_for_ajustment[, -"IID"])
  return(new_PRS)
}

# Calculate the prevalence
calc_prev <- function(data, n, column_name, wave) {
  # column_name and wave must be give as char
  df <-
    select(data, all_of(column_name), wave) %>%
    mutate(quantile_n = ntile(!!sym(column_name), n)) %>%
    select(quantile_n, wave)
  # !! bang-bang == unquote for dplyr
  # sym() converts a char to symbol
  p <-
    as.data.frame(cbind(1:n, table(df$quantile_n, df[[wave]]))) %>%
    rename("ntile" = 1, "Control" = 2, "Case" = 3) %>%
    mutate(
      prev = Case / (Control + Case),
      ntile = factor(ntile)) %>%
    select(-Control, -Case)
  return(p)
}
