#!/usr/bin/env Rscript
if (!require(pacman)) {
  message("
Loading necessary packages... It may take a while.
  ")
  install.packages(pacman, dependencies = TRUE)
  library(pacman, character.only = TRUE)
}

p_load(
  psych, dplyr, glue, ggplot2,
  tidyr, patchwork, docopt,
  ggthemr, stringr
)

doc <- "
Usage:
  stat_description.R [--input] [--output] [--pheno] [--vars]

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

message("
Choosen arguments:
    INPUT: glue("{file_name}")
    OUTPUT: glue("{opt_name}")
    PHENOTYPE: glue("{pheno}")
    Variables: glue("{vars}")
")