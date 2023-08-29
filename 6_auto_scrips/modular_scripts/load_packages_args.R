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
  -v, --vars LIST     # Column names to be used in file.RDS [ var1,var2,var3 ]
"

args <- docopt(doc)
input <- args[["--input"]]
output <- args[["--output"]]
pheno <- args[["--pheno"]]
vars <- args[["--vars"]]

opt_path <- dirname(output)
opt_name <- basename(output)
file_name <- basename(input)
pheno_name <- basename(pheno)

if(!is.na(vars) && !is.null(vars)) {
  var_names <- gsub(",", "", vars) # must keep
  vars <- unlist(strsplit(vars, ","))
  for_use <- select(readRDS(pheno), all_of(vars))
} else {
  vars <- "PRS"
  var_names <- "PRS"
  for_use <- data.table::fread(input) %>%
    rename_at(vars(matches("PRSCS_zscore|Pt_1")), ~"PRS") %>%
    select(IID, PRS)
}

message("
-------------------------------------------------

  Chosen arguments:

    INPUT:       ", file_name, "
    OUTPUT:      ", opt_name, "
    PHENOTYPE:   ", pheno_name, "
    VARIABLES:   ", vars, "

-------------------------------------------------
")

## Load PRS
prs_values <- data.table::fread(input) %>%
  rename_at(vars(matches("PRSCS_zscore|Pt_1")), ~"PRS") %>%
  select(IID, PRS)

## Set tool name
if (str_detect(file_name, "_Score.profile")) {
  name_tool <- gsub("_Score", "", file_name) %>%
    gsub(".profile", "", .)
} else if (str_detect(file_name, "_.all_score")) {
  name_tool <- gsub("_Score_", "", file_name) %>%
    gsub(".all_score", "", .)
} else {
  name_tool <- "Unknown"
}

source("functions.R") # Load functions for plots and tests
source("make_tests.R") # Make data transformation for tests
source("make_plots.R")
source("save_files.R")