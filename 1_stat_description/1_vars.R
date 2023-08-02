setwd("F:/")
pacman::p_load(psych, dplyr, glue, ggplot2, ggthemr, tidyr, patchwork)

ext_dir <- "F:/1_stat_description"
obj_dir <- "F:/objects_R"
plot_dir <- "F:/plots_R"
tables_dir <- "F:/tables_R"

vars <- readRDS(glue("{obj_dir}/cass_BHRC_ADHD_data.RDS"))

ggthemr("fresh")
## PRS data
# PRSice2
qqplot1 <- ggplot(vars, aes(sample = PRSice2)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
        ) +
    labs(
        title = "PRSice2 ADHD",
        x = "Theoretical Quantiles (Standard Normal)",
        y = "Observed Quantiles"
    )
dPRSice2 <- describe(vars$PRSice2)
rownames(dPRSice2) <- "PRSice2"
tPRSice2 <- nortest::ad.test(vars$PRSice2)
rPRSice2 <- data.frame(Statistic = tPRSice2$statistic,
                        p_value = tPRSice2$p.value,
                        method = "Anderson-Darling Test",
                        row.names = "PRSice2")

# PRS-CS
qqplot2 <- ggplot(vars, aes(sample = PRSCS)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
        ) +
    labs(
        title = "PRSCS ADHD",
        x = "Theoretical Quantiles (Standard Normal)",
        y = "Observed Quantiles"
    )
dPRSCS <- describe(vars$PRSCS)
rownames(dPRSCS) <- "PRSCS"
tPRSCS <- nortest::ad.test(vars$PRSCS)
rPRSCS <- data.frame(Statistic = tPRSCS$statistic,
                    p_value = tPRSCS$p.value,
                    method = "Anderson-Darling Test",
                    row.names = "PRSCS")

# Save output
rbind(rPRSice2, rPRSCS) %>%
#    knitr::kable(format = "markdown", row.names = TRUE) %>%
    data.table::fwrite(
        glue("{tables_dir}/cass_normalityTest_PRS_noChange.tsv"),
        sep = "\t",
        row.names = TRUE
    )
rbind(dPRSice2, dPRSCS) %>%
#    knitr::kable(format = "markdown", row.names = TRUE) %>%
    data.table::fwrite(
        glue("{tables_dir}/cass_description_PRS_noChange.tsv"),
        sep = "\t",
        row.names = TRUE
    )
qqplot <- qqplot1 + qqplot2
ggsave(
    glue("{plot_dir}/cass_PRS_qqplot_noChange.png"),
    qqplot,
    height = 15,
    width = 25,
    unit = "cm")

## ADHD phenotype
as.data.frame(table(vars$W0)) %>%
    rename(., "Phenotype" = Var1, "N" = Freq) %>%
    knitr::kable()
as.data.frame(table(vars$W1)) %>%
    rename(., "Phenotype" = Var1, "N" = Freq) %>%
    knitr::kable()
as.data.frame(table(vars$W2)) %>%
    rename(., "Phenotype" = Var1, "N" = Freq) %>%
    knitr::kable()

## Sex
as.data.frame(table(vars$sex)) %>%
    rename(., "Sex" = Var1, "N" = Freq) %>%
    knitr::kable()

## Age
as.data.frame(table(vars$W0_Age)) %>%
    rename(., "Age" = Var1, "N" = Freq) %>%
    knitr::kable()
as.data.frame(table(vars$W1_Age)) %>%
    rename(., "Age" = Var1, "N" = Freq) %>%
    knitr::kable()
as.data.frame(table(vars$W2_Age)) %>%
    rename(., "Age" = Var1, "N" = Freq) %>%
    knitr::kable()

## State
as.data.frame(table(vars$popID)) %>%
    rename(., "State" = Var1, "N" = Freq) %>%
    knitr::kable()

## Ancestrality
describe(vars$AMR) %>% knitr::kable()
describe(vars$AFR) %>% knitr::kable()
describe(vars$EUR) %>% knitr::kable()
