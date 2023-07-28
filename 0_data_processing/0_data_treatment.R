setwd("F:/")
pacman::p_load(dplyr, dplyr, tidyr, glue)

ext_dir <- "F:/0_external_files"
obj_dir <- "F:/objects_R"

### 1. Read files
## Dictionary with phenotype variable names and descriptions
dic <- readxl::read_excel(
    glue("{ext_dir}/BHRC_Dict_of_Variables_W0W1W2.xlsx")) %>% # Load
    select("Variable / Field Name", `Field Type`) %>% # Select columns
    rename(
        "var_name" = "Variable / Field Name", # Rename columns
        "var_description" = "Field Type"
    )

saveRDS(dic, glue("{obj_dir}/cass_BHRC_dict_of_var.RDS"))

## Get genotyped samples IDs:
n_to_use <- data.table::fread(
    glue("{ext_dir}/BHRC_Probands_Final.fam")
    ) %>%
    rename(IID = V2, sex = V5) %>%
    select(IID, sex)

n_to_use$sex <- factor(ifelse(n_to_use$sex == 2, "Female", "Male"))

saveRDS(n_to_use, glue("{obj_dir}/cass_BHRC_sex.RDS"))

## Phenotype status per sample
phenotype <- readRDS(
    glue("{ext_dir}/dawba_20200526.rds")) %>%
    rename(IID = subjectid, wave = redcap_event_name) %>%
    mutate(
        wave = gsub("wave0_arm_1", "W0", wave),
        wave = gsub("wave1_arm_1", "W1", wave),
        wave = gsub("wave2_arm_1", "W2", wave),
        IID = gsub("^", "C", IID)
    )
phenotype <- filter(phenotype, IID %in% n_to_use$IID) # Select rows
saveRDS(phenotype, glue("{obj_dir}/cass_BHRC_phenotype.RDS"))

## Ancestry data per sample:
admixture <- read.table(
    glue("{ext_dir}/admixture"),
    header = FALSE)[, -c(6:9)]
colnames(admixture) <- c("AMR", "AFR", "EUR", "popID", "IID")
filtered_admixture <- subset(admixture, popID %in% c("BRA_RS", "BRA_SP"))
filtered_admixture$popID <- factor(filtered_admixture$popID)
saveRDS(filtered_admixture, glue("{obj_dir}/cass_BHRC_ADMIXTURE.RDS"))

## Ages at each wave
ages <- readxl::read_excel(
    glue("{ext_dir}/Age_BHRCS.xlsx")) %>%
    rename(IID = subjectid) %>%
    mutate(
        IID = gsub("^", "C", IID),
        W0_Age = round(W0_Age),
        W1_Age = round(W1_Age),
        W2_Age = round(W2_Age)
    )
saveRDS(ages, glue("{obj_dir}/cass_BHRC_ages.RDS"))

## Principal Components calculated REFAZER COM NOVO QC
pc20 <- data.table::fread(
    glue("{ext_dir}/BHRC_Probands_Final_pca20.eigenvec")) %>%
    select_at(vars(paste0("V", 2:22, sep = ""))) %>%
    rename("IID" = V2)

colnames(pc20)[2:21] <- paste0("PC", 1:21, sep = "")

saveRDS(pc20,  glue("{obj_dir}/cass_BHRC_PC20.RDS"))
