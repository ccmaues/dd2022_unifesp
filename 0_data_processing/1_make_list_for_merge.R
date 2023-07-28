pacman::p_load(dplyr)

setwd("F:/")
pacman::p_load(dplyr, tidyr, glue)

obj_dir <- "F:/objects_R"

dic <- readRDS(glue("{obj_dir}/cass_BHRC_dict_of_var.RDS"))
phenotype <- readRDS(glue("{obj_dir}/cass_BHRC_phenotype.RDS"))

variables <- colnames(phenotype[, c(-1, -2, -3)])
all_pIDS <- filter(dic, var_name %in% variables)
any_pvarIDS <- filter(all_pIDS, grepl("any", var_name, ignore.case = TRUE)) # for substituition

all_pIDS$new_pIDS <- ifelse(grepl("anx", all_pIDS$var_description, ignore.case = TRUE), "dcanyanx",
    ifelse(grepl("adhd|hyp", all_pIDS$var_description, ignore.case = TRUE), "dcanyhk",
    ifelse(grepl("mdd|dep", all_pIDS$var_description, ignore.case = TRUE), "dcanydep",
    ifelse(grepl("social", all_pIDS$var_description, ignore.case = TRUE), "dcanyso",
    ifelse(grepl("conduct|oppositional", all_pIDS$var_description, ignore.case = TRUE), "dcanycd",
    ifelse(grepl("Attach disorder", all_pIDS$var_description, ignore.case = TRUE), "dcanyat", "")
                )
            )
        )
    )
)

colnames(all_pIDS) <- c("Old variable names", "Variable description", "New variable name")

data.table::fwrite(all_pIDS, "F:/1_produced_files/cass_BHRC_new_pheno_codes.csv")