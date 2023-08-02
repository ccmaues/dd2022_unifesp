setwd("F:/")
pacman::p_load(psych, dplyr, glue, ggplot2, ggthemr, tidyr, patchwork)

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
        x = "Theoretical Quantiles",
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
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )
dPRSCS <- describe(vars$PRSCS)
rownames(dPRSCS) <- "PRSCS"
tPRSCS <- nortest::ad.test(vars$PRSCS)
rPRSCS <- data.frame(Statistic = tPRSCS$statistic,
                    p_value = tPRSCS$p.value,
                    method = "Anderson-Darling Test",
                    row.names = "PRSCS")

# Save PRS output
norm_test_PRS <- rbind(rPRSice2, rPRSCS)
data.table::fwrite(
    norm_test_PRS,
    glue("{tables_dir}/cass_normalityTest_PRS_noChange.tsv"),
    sep = "\t",
    row.names = TRUE
)
desc_PRS <- rbind(dPRSice2, dPRSCS)
data.table::fwrite(
    desc_PRS,
    glue("{tables_dir}/cass_description_PRS_noChange.tsv"),
    sep = "\t",
    row.names = TRUE
)
qqplot_PRS <- qqplot1 + qqplot2
ggsave(
    glue("{plot_dir}/cass_PRS_qqplot_noChange.png"),
    qqplot_PRS,
    height = 15,
    width = 25,
    unit = "cm")

## ADHD phenotype
n1 <- as.data.frame(table(vars$W0)) %>%
    rename(., "Phenotype" = Var1, "W0" = Freq)
n2 <- as.data.frame(table(vars$W1)) %>%
    rename(., "Phenotype" = Var1, "W1" = Freq)
n3 <- as.data.frame(table(vars$W2)) %>%
    rename(., "Phenotype" = Var1, "W2" = Freq)

# Save adhd output
adhd_desc <- plyr::join_all(list(n1, n2, n3), type = "inner", by = "Phenotype")

data.table::fwrite(
    adhd_desc,
    glue("{tables_dir}/cass_nTime_ADHD.tsv"),
    sep = "\t"
)
