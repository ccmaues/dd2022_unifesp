# Functions for source in other codes
# and datas that are standard in the analysis
# less "polution" in main codes
# created for use in our data

pacman::p_load(
  glue, dplyr, ggplot2, remotes, tidyr,
  envalysis, extrafont, glue, ggthemr, data.table
)

#font_import()
loadfonts()

# check system
if (Sys.info()["sysname"] == "Linux") {
  Path <- "/media/santorolab/C207-3566"
  font <- "Ubuntu Condensed"
} else {
  Path <- "D:"
  font <- "Arial Narrow"
}
pheno <- readRDS(glue("{Path}/objects_R/cass_BHRC_Mod_All_Phenotypes_29-02-2024.RDS"))
ages <-	readRDS(glue("{Path}/objects_R/cass_BHRC_Age_Imputed_26-02-2024.RDS")) %>%
  rename(age_W0 = W0, age_W1 = W1, age_W2 = W2)
state <- readRDS(glue("{Path}/objects_R/cass_BHRC_STATE.RDS"))
sex <- readRDS(glue("{Path}/objects_R/cass_BHRC_sex.RDS"))
pc <- readRDS(glue("{Path}/objects_R/cass_BHRC_PC20.RDS"))
prs <- readRDS(glue("{Path}/objects_R/cass_BHRC_PRS.RDS"))

# PRS value adjustment with all variables
adjust_model <- function(data_list) {
  # data must contain the dfs with
  # variables adjustment
  var_for_ajustment <-
    plyr::join_all(
      data_list,
      by = "IID",
      type = "inner"
    )
  # The + indicates multiple predictors
  new_PRS <-
    glm(PRS ~ sex + age_W0 + age_W1 + age_W2 +
    popID + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
    PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
    PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20,
    data = var_for_ajustment)
  return(new_PRS)
}


# PRS value adjustment withou age
adjust_model_v2 <- function(data_list) {
  # data must contain the dfs with
  # variables adjustment
  var_for_ajustment <-
    plyr::join_all(
      data_list,
      by = "IID",
      type = "inner"
    )
  # The + indicates multiple predictors
  new_PRS <-
    glm(PRS ~ sex +
    popID + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
    PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
    PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20,
    data = var_for_ajustment)
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
