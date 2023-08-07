#!/usr/bin/env Rscript
library(docopt)
library(glue)
library(dplyr)
doc <- "
Usage:
  my_script.R [--input FILE] [--output FILE] [--pheno FILE] [--vars FILE]

Options:
  -i, --input FILE         # PRS file with scores [ file.profile | file.all_score ]
  -o, --output FILE        # Output file names
  -p, --pheno FILE         # RDS file with variables [ file.RDS ]
  -v, --vars FILE          # Column names to be used in file.RDS [ var1,var2,var3 ] 
"

args <- docopt(doc)
input <- args[["--input"]]
output <- args[["--output"]]
pheno <- args[["--pheno"]]
vars <- args[["--vars"]]

vars <- unlist(strsplit(vars, ","))
# message("
# ------------------------------
# Loading necessary packages...
# ------------------------------"
# )
# pacman::p_load(
#     psych, dplyr, glue, ggplot2,
#     tidyr, patchwork, optparser
#     ) # add ggthemr later

message(glue("This is the {input} file"))
message(glue("This is the {output} file"))
message(glue("This is the {pheno} path"))
message(glue("This is the {vars} path"))

variables <- readRDS(input)
for_use <- select(variables, all_of(vars))

## Count N in var
lapply(vars, function(col_name) {
  as.data.frame(table(for_use[[col_name]])) %>%
  rename(!!col_name := Freq)
})

## Test normality

## Make plots
#ggthemr("fresh")