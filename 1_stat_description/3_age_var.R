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
# check age normality
qqplot8 <- ggplot(man, aes(sample = PRSice2)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSice2 ADHD age W0",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )

qqplot9 <- ggplot(man, aes(sample = PRSice2)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSice2 ADHD age W1",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )

qqplot10 <- ggplot(man, aes(sample = PRSice2)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSice2 ADHD age W2",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )

qqplot11 <- ggplot(man, aes(sample = PRSCS)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSCS ADHD age W0",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )

qqplot12 <- ggplot(man, aes(sample = PRSCS)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSCS ADHD age W1",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )

qqplot13 <- ggplot(man, aes(sample = PRSCS)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSCS ADHD age W2",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )

qqplot_AGE <- qqplot8 + qqplot9 + qqplot10 +
    qqplot11 + qqplot12 + qqplot13

dPRSCS_fem <- describe(fem$PRSCS)
rownames(dPRSCS_fem) <- "PRSCS"
tPRSCS_fem <- nortest::ad.test(fem$PRSCS)
rPRSCS_fem <- data.frame(
    Statistic = tPRSCS_fem$statistic,
    p_value = tPRSCS_fem$p.value,
    method = "Anderson-Darling Test",
    row.names = "PRSCS fem"
)