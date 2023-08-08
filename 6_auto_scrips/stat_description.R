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
    glue("{output}.tsv",
    quote = FALSE, sep = "\t",
    row.names = FALSE, col.names = TRUE))

# if (file.exists(glue("{output}.tsv"))) {
#   message("
#   First file written.
#   ")
# } else {
#   message("
#   Problem with writting the N file...
#   ")
# }

## Test normality
prs_values <- data.table::fread(input) %>%
  rename_at(vars(matches("PRSCS_zscore|Pt_1")), ~ "PRS") %>%
  select(IID, PRS)
str(prs_values)

## fix with PCs LATER
library(psych)
library(stringr)

if (str_detect(file_name, "_Score.profile")) {
  name <- gsub("_Score", "", file_name) %>%
    gsub(".profile", "", .)
} else if (str_detect(file_name, "_.all_score")) {
  name <- gsub("_Score_", "", file_name) %>%
    gsub(".all_score", "", .)
} else {
  name <- "NAME"
  # message("
  # Not able to identify the file name type... Using default.
  # ")
}

num_test <- select_if(for_use, is.numeric)
if (ncol(num_test) != 0) {
  # message("
  # making norm test with the given variables...
  # ")
  lapply(vars, function(col_name) {
    d <- describe(for_use[[col_name]])
    rownames(d) <- name
    t <- nortest::ad.test(for_use[[col_name]])
    r <- data.frame(
      Statistic = t$statistic,
      P_value = t$p.value,
      Method = "Anderson-Darling Test",
      row.names = name
    )
    # if (file.exists(glue("{output}_describe.tsv"))) {
    #   message("
    #   Describe file written.
    #   ")
    # } else {
    #   message("
    #   Problem with writting the describe file...
    #   ")
    # }

    # if (file.exists(glue("{output}_norm_test.tsv"))) {
    #   message("
    #   Norm test file written.
    #   ")
    # } else {
    #   message("
    #   Problem with writting the norm test file...
    #   ")
    # }
  })
} else {
  # message("
  # Given variables are not numeric.
  # Using them as categorical for PRS values...
  # ")
  norm_test <- select(variables, IID, all_of(vars)) %>%
    inner_join(., prs_values, by = "IID") %>%
    select(., all_of(vars), PRS)

    subset_dfs <- list()
    for (i in 1:ncol(norm_test)) {
      if (names(norm_test)[i] != "PRS") {
        col_name <- names(norm_test)[i]
        fcolumn <- levels(norm_test[[i]])
        nlevels <- nlevels(norm_test[[i]])
        for_sep <- data.frame(norm_test[[i]], norm_test[["PRS"]])
        colnames(for_sep) <- c(col_name, "PRS")
        for (level in nlevels) {
          subset_data <- group_split(for_sep, {{ col_name }})
          print(level)
          print(head(subset_data))
          new_df_name <- paste(col_name, level, sep = "_")
          subset_dfs[[new_df_name]] <- subset_data
        }
      }
    }
    d <- describe(norm_test[[col_name]])
    rownames(d) <- name
    d
    # data.table::fwrite(d,
    # glue("{output}_describe.tsv",
    # quote = FALSE, sep = "\t",
    # row.names = TRUE, col.names = TRUE))
    t <- nortest::ad.test(norm_test[[col_name]])
    r <- data.frame(
      Statistic = t$statistic,
      P_value = t$p.value,
      Method = "Anderson-Darling Test",
      row.names = name
    )
}
data.table::fwrite(d,
glue("{output}_describe.tsv",
quote = FALSE, sep = "\t",
row.names = TRUE, col.names = TRUE))
data.table::fwrite(r,
glue("{output}_norm_test.tsv",
quote = FALSE, sep = "\t",
row.names = TRUE, col.names = TRUE))
## Make plots
#ggthemr("fresh")