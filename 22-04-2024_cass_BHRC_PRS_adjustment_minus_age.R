source("functions_to_source.R")

file_list <-
  list.files(
  glue("{Path}/PRS_database/Final_Scores_PRSCS"),
  pattern = "\\.profile$"
)

prs_values <- list()
for (file in file_list) {
  file_path <- file.path(glue("{Path}/PRS_database/Final_Scores_PRSCS/"), file)
  file_name <- gsub("^PRSCS_(.*?)_Score\\.profile$", "\\1", file)
  data <-
    data.table::fread(file_path, header = TRUE) %>%
    select(IID, PRSCS_zscore) %>%
    rename(PRS = PRSCS_zscore)
  prs_values[[file_name]] <- data
}

# ajustar pra fazer a regressão e os testes de modelo
# fazer PRSCS com as mesmas bases, porém
# com burn-ins e interactions diferente (omitzado)

new_PRS <- NULL
for(df_name in names(prs_values)) {
  data <- prs_values[[df_name]]
  glm_result <- adjust_model_v2(list(data, sex, state, pc))
  samples <- glm_result$data$IID
  values <- glm_result$residuals
  new_PRS[[df_name]] <- data.frame(IID = samples, PRS = values)
}

final_PRS <- plyr::join_all(new_PRS, by = "IID", type = "inner")
colnames(final_PRS)[2:ncol(final_PRS)] <- names(prs_values)

str(final_PRS)
str(prs)
saveRDS(
  final_PRS,
  glue("{Path}/objects_R/cass_BHRC_PRS_minus_age.RDS")
)
