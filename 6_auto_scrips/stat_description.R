#!/usr/bin/env Rscript
library(docopt)
library(glue)
doc <- "
Usage:
  my_script.R [--input FILE] [--tool TOOL] [--output FILE] [--var-path FILE]

Options:
  -i, --input FILE         # PRS file with scores [ file.profile | file.all_score ]
  -t, --tool TOOL          # Type of tool used [ PRSice2 | PRSCS ]
  -o, --output FILE        # Output file names
  -p, --var-path FILE      # RDS file with variables [ file.RDS ]
  -v, --vars               # Column names to be used in file.RDS [ var1,var2,var3 ] 
"

args <- docopt(doc)
input <- args[["--input"]]
tool <- args[["--tool"]]
output <- args[["--output"]]
var_path <- args[["--var-path"]]
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
message(glue("This is the {tool} file"))
message(glue("This is the {output} file"))
message(glue("This is the {var_path} path"))
message(glue("This is the {vars} path"))

#variables <- readRDS(glue("{var}"))
#ggthemr("fresh")

## Count N in var

## Test normality

## Make plots