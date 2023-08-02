setwd("F:/")
pacman::p_load(dplyr, glue)

ext_dir <- "F:/0_external_files"
obj_dir <- "F:/objects_R"
prs_data <- "F:/PRS_database"

adhd_pheno <- readRDS(glue("{obj_dir}/cass_BHRC_ADHD_to_join_PRS.RDS"))
PRSCS <- data.table::fread(
    glue("{prs_data}/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile")) %>%
    select(IID, PRSCS_zscore) %>%
    rename(PRSCS = PRSCS_zscore)
PRSice2 <- data.table::fread(
    glue("{prs_data}/Final_Scores_PRSice/PRS_Score_ADHD.all_score")) %>%
    select(IID, Pt_1) %>%
    rename(PRSice2 = Pt_1)
adhd_data <- plyr::join_all(
    list(adhd_pheno, PRSCS, PRSice2),
    by = "IID", type = "inner"
)

saveRDS(adhd_data, glue("{obj_dir}/cass_BHRC_ADHD_data.RDS"))
