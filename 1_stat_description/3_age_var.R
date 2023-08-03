setwd("F:/")
pacman::p_load(psych, dplyr, glue, ggplot2, ggthemr, tidyr, patchwork)

obj_dir <- "F:/objects_R"
plot_dir <- "F:/plots_R"
tables_dir <- "F:/tables_R"

vars <- readRDS(glue("{obj_dir}/cass_BHRC_ADHD_data.RDS"))
ggthemr("fresh")

## Age (ANOVA?)
W0 <- as.data.frame(table(vars$W0_Age)) %>%
    rename(., "Age" = Var1, "W0" = Freq)
W1 <- as.data.frame(table(vars$W1_Age)) %>%
    rename(., "Age" = Var1, "W1" = Freq)
W2 <- as.data.frame(table(vars$W2_Age)) %>%
    rename(., "Age" = Var1, "W2" = Freq)

data.table::fwrite(
    W0,
    glue("{tables_dir}/cass_nAgeW0_BHRC.tsv"),
    sep = "\t"
)
data.table::fwrite(
    W1,
    glue("{tables_dir}/cass_nAgeW1_BHRC.tsv"),
    sep = "\t"
)
data.table::fwrite(
    W2,
    glue("{tables_dir}/cass_nAgeW2_BHRC.tsv"),
    sep = "\t"
)