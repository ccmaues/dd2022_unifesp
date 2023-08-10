#!/usr/bin/env Rscript
library(docopt)
library(glue)
library(dplyr, quietly = TRUE)
doc <- "
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

var_names <- vars
vars <- unlist(strsplit(vars, ","))

# pacman::p_load(
#     psych, dplyr, glue, ggplot2,
#     tidyr, patchwork, optparser
#     ) # add ggthemr later

variables <- readRDS(pheno)
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
    glue("{output}_{var_names}_N.tsv",
    quote = FALSE, sep = "\t",
    row.names = FALSE, col.names = TRUE))

## Test normality
prs_values <- data.table::fread(input) %>%
  rename_at(vars(matches("PRSCS_zscore|Pt_1")), ~ "PRS") %>%
  select(IID, PRS)
str(prs_values)

## fix with PCs LATER
library(psych)
library(stringr)

if (str_detect(file_name, "_Score.profile")) {
  name_tool <- gsub("_Score", "", file_name) %>%
    gsub(".profile", "", .)
} else if (str_detect(file_name, "_.all_score")) {
  name_tool <- gsub("_Score_", "", file_name) %>%
    gsub(".all_score", "", .)
} else {
  name_tool <- "Unknown"
}

make_describe_df <- function(data) {
  opt <- data.frame(
  Tool = name_tool,
  Variable = data$vars,
  N = data$n,
  SD = data$sd,
  Median = data$median,
  Min = data$min,
  Max = data$max,
  Range = data$range,
  Skew = data$skew,
  Kurtosis = data$kurtosis,
  SE = data$se
  )
  return(opt)
}

make_test_df <- function(data) {
  opt <- data.frame(
    Tool = name_tool,
    Statistic = data$statistic,
    P_value = data$p.value,
    Method = "Anderson-Darling Test"
  )
  return(opt)
}


num_test <- select_if(for_use, is.numeric)
if (ncol(num_test) != 0) {
    for (i in 1:ncol(for_use)) {
      if (names(for_use)[i] != "PRS") {
        col_name <- names(for_use)[i]
        fcolumn <- levels(for_use[[i]])
        for_sep <- data.frame(for_use[[i]], for_use[["PRS"]])
        colnames(for_sep) <- c(col_name, "PRS")
        for (level in fcolumn) {
          subset_data <- for_sep %>%
            filter(!!sym(col_name) == level)
          new_df_name <- paste(col_name, level, sep = "_")
          subset_dfs[[new_df_name]] <- subset_data
        }
      }
    }
    description <- list()
    test <- list()
    subset_names <- names(subset_dfs)

    for (name in subset_names) {
      data <- subset_dfs[[name]]
      d <- psych::describe(data$PRS)
      t <- nortest::ad.test(data$PRS)

      description[[name]] <- make_describe_df(d)
      test[[name]] <- make_test_df(t)
    }
} else {
  norm_test <- select(variables, IID, all_of(vars)) %>%
    inner_join(., prs_values, by = "IID") %>%
    select(., all_of(vars), PRS)
    subset_dfs <- list()
    for (i in 1:ncol(norm_test)) {
      if (names(norm_test)[i] != "PRS") {
        col_name <- names(norm_test)[i]
        fcolumn <- levels(norm_test[[i]])
        for_sep <- data.frame(norm_test[[i]], norm_test[["PRS"]])
        colnames(for_sep) <- c(col_name, "PRS")
        for (level in fcolumn) {
        subset_data <- for_sep %>%
            filter(!!sym(col_name) == level)
          new_df_name <- paste(col_name, level, sep = "_")
          subset_dfs[[new_df_name]] <- subset_data
        }
      }
    }
    description <- list()
    test <- list()
    subset_names <- names(subset_dfs)

    for (name in subset_names) {
        data <- subset_dfs[[name]]
        d <- psych::describe(data$PRS)
        t <- nortest::ad.test(data$PRS)

      description[[name]] <- make_describe_df(d)
      test[[name]] <- make_test_df(t)
    }
}

description_df <- do.call(rbind, description)
test_df <- do.call(rbind, test)

data.table::fwrite(description_df,
  glue("{output}_{var_names}_{name_tool}_describe.tsv"),
  quote = FALSE, sep = ",",
  row.names = TRUE, col.names = TRUE
)
data.table::fwrite(test_df,
  glue("{output}_{var_names}_{name_tool}_norm_test.tsv"),
  quote = FALSE, sep = ",",
  row.names = TRUE, col.names = TRUE
)

## Make plots
#ggthemr("fresh")