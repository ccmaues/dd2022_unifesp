pacman::p_load(
  glue, dplyr, ggplot2, remotes, envalysis,
  extrafont, glue, ggthemr, patchwork, tidyr
)

# check system
if (Sys.info()["sysname"] == "Linux") {
  path <- "/media/santorolab/C207-3566"
  font <- "Ubuntu Condensed"
} else {
  path <- "D:"
  font <- "Arial Narrow"
}

file_list <-
  list.files(
  "PRS_database/Final_Scores_PRSCS",
  pattern = "\\.profile$"
)

prs_values <- list()
for (file in file_list) {
  file_path <- file.path(glue("{path}/PRS_database/Final_Scores_PRSCS/"), file)
  file_name <- gsub("^PRSCS_(.*?)_Score\\.profile$", "\\1", file)
  data <-
    data.table::fread(file_path, header = TRUE) %>%
    select(IID, PRSCS_zscore) %>%
    rename(PRS = PRSCS_zscore)
  print(head(data))
  prs_values[[file_name]] <- data
}

state <- readRDS(glue("{path}/objects_R/cass_BHRC_STATE.RDS"))
sex <- readRDS(glue("{path}/objects_R/cass_BHRC_sex.RDS"))
pc <- readRDS(glue("{path}/objects_R/cass_BHRC_PC20.RDS"))

new_PRS_values <- list()
for (phenotype in names(prs_values)) {
  model_data <-
    plyr::join_all(
    list(prs_values[[phenotype]], sex, state, pc),
    by = "IID",
    type = "inner"
  )
  new_values <-
    glm(PRS ~ sex:PC20, data = model_data)
  new_PRS_values[[phenotype]] <-
    data.frame(
    IID = model_data$IID,
    PRS = new_values$residuals
  )
}

final_PRS <- new_PRS_values[[1]][['IID']]
for (name in names(new_PRS_values)) {
  if (i > 1) {
    final_PRS <-
      data.frame(cbind(final_PRS, new_PRS_values[[name]][[i]]))
  }
}
colnames(final_PRS) <- c("IID", names(new_PRS_values))
head(final_PRS)

saveRDS(
  final_PRS, glue("{path}/objects_R/cass_BHRC_PRS_values_ajusted_15-02-2023.RDS")
)


# (1) Verificar a correlação entre prs (para 11 doenças
# psiquiátricas) levando em consideração a comorbidade
# de doenças nos indivíduos com mais de um diagnóstico
# no período;