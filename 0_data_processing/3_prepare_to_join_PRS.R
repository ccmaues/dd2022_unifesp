setwd("F:/")
pacman::p_load(dplyr, glue)

ext_dir <- "F:/0_external_files"
obj_dir <- "F:/objects_R"

adhd <- readRDS(glue("{obj_dir}/cass_BHRC_ADHD_mod_pheno.RDS"))[[1]] %>%
    mutate(across(2:4, factor))
pc20 <- readRDS(glue("{obj_dir}/cass_BHRC_PC20.RDS"))
ages <- readRDS(glue("{obj_dir}/cass_BHRC_ages.RDS"))
ancestry <- readRDS(glue("{obj_dir}/cass_BHRC_ADMIXTURE.RDS"))
# the object above has less 1 sample... Which one?
sex <- readRDS(glue("{obj_dir}/cass_BHRC_sex.RDS"))

for_PRS_join <- plyr::join_all(
    list(adhd, pc20, ages, ancestry, sex),
    by = "IID", type = "inner")

saveRDS(for_PRS_join, glue("{obj_dir}/cass_BHRC_ADHD_to_join_PRS.RDS"))