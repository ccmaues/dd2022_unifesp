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
result_list <- lapply(vars, function(col_name) {
  opt <- as.data.frame(table(for_use[[col_name]])) %>%
    rename(!!col_name := Freq)
  opt
})
plyr::join_all(result_list, by = "Var1", type = "inner") %>%
  rename(Type = Var1) %>%
  data.table::fwrite(.,
    glue("{output}.tsv",
    quote = FALSE, sep = "\t",
    row.names = FALSE, col.names = TRUE))

if (file.exists(glue("{output}.tsv"))) {
  message("
  First file written
  ")
} else {
  message("
  Problem with writting the N file...
  ")
}

## Test normality
norm_test <- select_if(for_use, is.numeric)
if (ncol(norm_test) != 0) {
  message("make norm test")
} else {
  message("
  Given variables are not numeric.
  No necessity for normality test...
  ")
}

## Make plots
#ggthemr("fresh")