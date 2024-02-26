pacman::p_load(
  glue, dplyr, ggplot2, remotes, envalysis,
  extrafont, glue, ggthemr, patchwork, tidyr
)

# check system
if (Sys.info()["sysname"] == "Linux") {
  Path <- "/media/santorolab/C207-3566"
} else {
  Path <- "D:"
}

# Phenotype modification
og_pheno <-
  readRDS(glue("{Path}/0_external_files/dawba_20200526.rds")) %>%
  rename(wave = redcap_event_name, IID = subjectid) %>%
  mutate(wave =
    case_when(
      wave == "wave0_arm_1" ~ "W0",
      wave == "wave1_arm_1" ~ "W1",
      wave == "wave2_arm_1" ~ "W2"
    ))
og_pheno$IID <- gsub("^", "C", og_pheno$IID)
saveRDS(og_pheno, glue("{Path}/objects_R/cass_BHRC_phenotype.RDS"))

# State modification (can't open...?)
# admixture <- readRDS(glue("{Path}/0_external_files/admixture.rds")) %>%


# Sex modification
sex <-
  data.table::fread(
    glue("{Path}/0_external_files/BHRC_Probands_Final.fam")
  ) %>%
  select(-V1, -V3, -V4, -V6) %>%
  rename(IID = V2, sex = V5) %>%
  mutate(
    sex = factor(case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female")
    )
  )
saveRDS(sex, glue("{Path}/objects_R/cass_BHRC_sex.RDS"))

# PCs modification
pcs <-
  data.table::fread(
    glue("{Path}/0_external_files/BHRC_Probands_Final_pca20.eigenvec")
  ) %>%
  select(-V1)
colnames(pcs) <- c("IID", paste("PC", 1:20, sep = ""))
saveRDS(pcs, glue("{Path}/objects_R/cass_BHRC_PC20.RDS"))
