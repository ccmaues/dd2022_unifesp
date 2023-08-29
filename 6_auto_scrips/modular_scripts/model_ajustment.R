#!/usr/bin/env Rscript
if (!require(pacman)) {
  message("
Loading necessary packages... It may take a while.
  ")
  install.packages(pacman, dependencies = TRUE)
  library(pacman, character.only = TRUE)
}

p_load(dplyr, glue, docopt)

doc <- "
Description:
  This script will take the given variables and correct the PRS model with them.

Usage:
  model_ajustment.R [--input] [--output] [--pheno] [--vars]

Options:
  -i, --input FILE    # PRS file with scores [ file.profile | file.all_score ]
  -o, --output FILE   # Output file names
  -p, --pheno FILE    # RDS file with variables [ file.RDS ]
  -v, --vars FILE     # Column names to be used in file.RDS [ var1,var2,var3 ] 
"

args <- docopt(doc)
input <- args[["--input"]]
output <- args[["--output"]]
pheno <- args[["--pheno"]]
vars <- args[["--vars"]]

opt_path <- dirname(output)
opt_name <- basename(output)
file_name <- basename(input)

var_names <- gsub(",", "", vars)
vars <- unlist(strsplit(vars, ","))
vars <- c(vars, paste("PC", 1:20, sep = ""))

variables <- readRDS(pheno)
for_use <- select(variables, IID, all_of(vars))
prs_values <- data.table::fread(input) %>%
  rename_at(vars(matches("PRSCS_zscore|Pt_1")), ~ "PRS") %>%
  select(IID, PRS)

## Join data
for_ajustment <- inner_join(for_use, prs_values, by = "IID")

## Use Generalized Linear Model
# it depends on the PRS distribution model
ajusted <- glm(PRS ~ . - IID, data = for_ajustment, family = gaussian)
final_model <- data.frame(ajusted$xlevels$IID, ajusted$residuals)
colnames(final_model) <- c("IID", "PRS_residuals")
final_model$IID <- as.character(final_model$IID)
## Save new R object with ajusted PRS model
saveRDS(final_model, glue("{opt_path}/{opt_name}_PRS_ajusted.RDS"))