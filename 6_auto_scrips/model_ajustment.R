#!/usr/bin/env Rscript
if (!require(pacman)) {
  message("
Loading necessary packages... It may take a while.
  ")
  install.packages(pacman, dependencies = TRUE)
  library(pacman, character.only = TRUE)
}

p_load(plyr, glue, docopt)

doc <- "
Description:
  This script will take the given variables and correct the PRS model with them.

Usage:
  my_script.R [--input FILE] [--output FILE] [--pheno FILE] [--vars FILE]

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

variables <- readRDS(pheno)
for_use <- select(variables, all_of(vars))
prs_values <- data.table::fread(input) %>%
  rename_at(vars(matches("PRSCS_zscore|Pt_1")), ~ "PRS") %>%
  select(IID, PRS)

## Join data

## Use Generalized Linear Model

## Save new R object with ajusted PRS model