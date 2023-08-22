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

message("
#### Describing variables. . .
")
## Count N in var
result_list <- lapply(vars, function(col_name) {
  opt <- as.data.frame(table(for_use[[col_name]])) %>%
    rename(!!col_name := Freq)
  opt
})
plyr::join_all(result_list, by = "Var1", type = "inner") %>%
  rename(Type = Var1) %>%
  data.table::fwrite(.,
    glue("{opt_path}/{opt_name}_{var_names}_N.tsv"),
    quote = FALSE, sep = ",",
    row.names = FALSE, col.names = TRUE
  )

if (file.exists(glue("{opt_path}/{opt_name}_{var_names}_N.tsv"))) {
  print("Variable(s) N sample written!")
} else {
  print("Problem saving file with N sample.")
}

## Test normality
prs_values <- data.table::fread(input) %>%
  rename_at(vars(matches("PRSCS_zscore|Pt_1")), ~"PRS") %>%
  select(IID, PRS)

## fix with PCs LATER
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

description <- list()
test <- list()
num_test <- select_if(for_use, is.numeric)
message("
#### Making normality test for variables . . .
")
if (ncol(num_test) != 0) {
  subset_dfs <- list()
  for (i in 1:ncol(for_use)) {
    for_sep <- data.frame(for_use[[i]])
    col_name <- colnames(for_use)[i]
    colnames(for_sep) <- col_name
    subset_dfs[[col_name]] <- for_sep
    subset_names <- names(subset_dfs)
    for (name in subset_names) {
      data <- subset_dfs[[name]]
      d <- psych::describe(data[[glue("{name}")]])
      t <- nortest::ad.test(data[[glue("{name}")]])
      description[[name]] <- make_describe_df(d)
      test[[name]] <- make_test_df(t)
    }
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
        subset_dfs[[new_df_name]] <- subset_data %>%
          select(PRS)
      }
    }
  }
  subset_names <- names(subset_dfs)
  for (name in subset_names) {
    data <- subset_dfs[[name]]
    d <- psych::describe(data$PRS)
    t <- nortest::ad.test(data$PRS)
    description[[name]] <- make_describe_df(d)
    test[[name]] <- make_test_df(t)
  }
}
subset_dfs <- lapply(subset_dfs, function(df) {
  colnames(df) <- "value"
  return(df)
})

description_df <- do.call(rbind, description)
test_df <- do.call(rbind, test)

data.table::fwrite(description_df,
  glue("{opt_path}/{opt_name}_{var_names}_{name_tool}_describe.tsv"),
  quote = FALSE, sep = ",",
  row.names = TRUE, col.names = TRUE
)
if (file.exists(
  glue("{opt_path}/{opt_name}_{var_names}_{name_tool}_describe.tsv")
)) {
  print("Description written!")
} else {
  print("Problem saving file with variable description.")
}
data.table::fwrite(test_df,
  glue("{opt_path}/{opt_name}_{var_names}_{name_tool}_norm_test.tsv"),
  quote = FALSE, sep = ",",
  row.names = TRUE, col.names = TRUE
)
if (file.exists(
  glue("{opt_path}/{opt_name}_{var_names}_{name_tool}_norm_test.tsv")
)) {
  print("Normal distribution test DONE!")
} else {
  print("Problem saving file with distribution test.")
}

## Make QQ plots
message("
#### Making Q-Q plots!
")
ggthemr("fresh")
qqplots <- list()
subset_names <- names(subset_dfs)

make_qq <- function(data) {
  opt <- list()
  for (i in 1:length(data)) {
    df <- data[[i]]
    ptitle <- subset_names[[i]]
    opt[[ptitle]] <-
      ggplot(df, aes(sample = value)) +
      geom_qq() +
      labs(
        title = ptitle,
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
      )
  }
  return(opt)
}

qqplots <- make_qq(subset_dfs)

library(patchwork)
plots <- wrap_plots(qqplots)
ggsave(
  glue("{opt_path}/{opt_name}_{var_names}_QQplot.png"),
  plots,
  device = "png"
)
if (file.exists(
  glue("{opt_path}/{opt_name}_{var_names}_QQplot.png")
)) {
  print("QQ plot saved!")
} else {
  print("Problem saving file the QQ plot.")
}

## Make density plots
message("
#### Making Density plots!
")
if (ncol(num_test) == 0) {
  for_plot <- select(variables, all_of(vars), IID) %>%
    inner_join(., prs_values, by = "IID") %>%
    rename("gp" = vars) %>%
    select(-IID)
} else {
  if(grepl("Age", vars)) {
    temp <- for_use %>%
      tidyr::pivot_longer(
      contains("Age"),
      names_to = "wave",
      values_to = "gp") %>%
      na.omit
    temp$wave <- gsub("_Age", "", temp$wave)
    for_plot <- list(
      W0 = filter(temp, wave == "W0") %>% select(-wave, -IID),
      W1 = filter(temp, wave == "W1") %>% select(-wave, -IID),
      W2 = filter(temp, wave == "W2") %>% select(-wave, -IID))
  } else {
    for_plot <- select(variables, all_of(vars), IID) %>%
      inner_join(., prs_values, by = "IID") %>%
      select(all_of(vars), PRS) %>%
      mutate(gp = case_when(
        EUR > 0.7 ~ "EUR",
        AMR > 0.7 ~ "AMR",
        AFR > 0.7 ~ "AFR",
        TRUE ~ "Other"
      )) %>%
      select(PRS, gp)
  }
}
make_plot <- function(data) {
  if(class(for_plot) == "list") {
    tnames <- names(for_plot)
    plot_list <- list()
    for(i in 1:length(for_plot)) {
      data <- for_plot[[i]]
      p <- ggplot(
        for_plot[[1]],
        aes(x = PRS, fill = factor(gp), color = factor(gp))) +
        geom_bar(alpha = 0.6, position = "dodge") +
        labs(
          x = "PRS scale",
          title = glue("PRS values distribution by age at {tnames[i]}")
        )
      plot_list[[i]] <- p
      }
      library(patchwork)
      plot <- wrap_plots(plot_list, ncol = 1)
  } else { # aqui
    opt <-
      ggplot(data, aes(x = PRS, fill = gp, color = gp)) +
      geom_density(alpha = 0.4) +
      labs(
        x = "PRS scale",
        title = glue("PRS values distribution at {vars}"))
  return(opt)
  }
}
plot <- make_plot(for_plot)
ggsave(
  glue("{opt_path}/{opt_name}_{var_names}_Denplot.png"),
  plot,
  device = "png"
)
if (file.exists(
  glue("{opt_path}/{opt_name}_{var_names}_Denplot.png")
)) {
  print("Density plot saved!")
} else {
  print("Problem saving file the density plot.")
}
## add part in which i set groups for numbers

message(glue("\n
#### Outputs saved at:
{opt_path}
"))