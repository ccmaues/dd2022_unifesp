setwd("F:/")
pacman::p_load(psych, dplyr, glue, ggplot2, ggthemr, tidyr, patchwork)

obj_dir <- "F:/objects_R"
tables_dir <- "F:/tables_R"

vars <- readRDS(glue("{obj_dir}/cass_BHRC_ADHD_data.RDS"))
ggthemr("fresh")

## Ancestrality
eur <- describe(vars$EUR)
rownames(eur) <- "EUR"
amr <- describe(vars$AMR)
rownames(amr) <- "AMR"
afr <- describe(vars$AFR)
rownames(afr) <- "AFR"

rbind(eur, amr, afr) %>%
    data.table::fwrite(
        .,
        glue("{tables_dir}/cass_nAncestrality_BHRC.tsv"),
        sep = "\t"
    )

qqplot11 <- ggplot(vars, aes(sample = EUR)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "EUR",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )
dEUR <- describe(vars$EUR)
rownames(dEUR) <- "EUR"
tEUR <- nortest::ad.test(vars$EUR)
rEUR <- data.frame(
    Statistic = tEUR$statistic,
    p_value = tEUR$p.value,
    method = "Andescon-Darling Test",
    row.names = "EUR"
)

qqplot12 <- ggplot(vars, aes(sample = AMR)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "AMR",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )
dAMR <- describe(vars$AMR)
rownames(dAMR) <- "AMR"
tAMR <- nortest::ad.test(vars$AMR)
rAMR <- data.frame(
    Statistic = tAMR$statistic,
    p_value = tAMR$p.value,
    method = "Andescon-Darling Test",
    row.names = "AMR"
)

qqplot13 <- ggplot(vars, aes(sample = AFR)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "AFR",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )
dAFR <- describe(vars$AFR)
rownames(dAFR) <- "AFR"
tAFR <- nortest::ad.test(vars$AFR)
rAFR <- data.frame(
    Statistic = tAFR$statistic,
    p_value = tAFR$p.value,
    method = "Andescon-Darling Test",
    row.names = "AFR"
)

qqplot_ANC <- qqplot11 + qqplot12 + qqplot13
ggsave(
    glue("{plot_dir}/cass_PRS_ancestrality_qqplot_noChange.png"),
    qqplot_ANC,
    height = 15,
    width = 40,
    unit = "cm"
)

norm_PRS_test_sex <- rbind(rEUR, rAFR, rAMR)
data.table::fwrite(
    norm_PRS_test_sex,
    glue("{tables_dir}/cass_normalityTest_PRS_ancestrality_noChange.tsv"),
    sep = "\t",
    row.names = TRUE
)
