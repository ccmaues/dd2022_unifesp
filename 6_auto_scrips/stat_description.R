#!/usr/bin/env Rscript
# message("
# ------------------------------
# Loading necessary packages...
# ------------------------------"
# )
# might have problems
# pacman::p_load(
#     psych, dplyr, glue, ggplot2,
#     tidyr, patchwork, optparser
#     ) # add ggthemr later
library(argparse)
library(glue)
parser <- ArgumentParser(
    description = "Script for statistical description of a variable"
    )
parser$add_argument(
    "--input",
    dest = "input_file",
    help = "INPUT file name with or without path [MUST BE RDS FORMAT]")
parser$add_argument(
    "--output",
    dest = "opt_file",
    default = "output_stat",
    help = "OUTPUT file name [MULTIPLE OUTPUT FORMATS]"
    )
parser$add_argument(
    "--output-dir",
    dest = "opt_path",
    help = "OUTPUT path"
)

args <- parser$parse_args()
input_file <- args$input_file
opt_file <- args$opt_file
opt_dir <- args$opt_path

message(glue("This is the {input_file} file"))
message(glue("This is the {opt_file} file"))
message(glue("This is the {opt_dir} path"))

#vars <- readRDS(glue("{obj_dir}/{input_file}.RDS"))
#ggthemr("fresh")