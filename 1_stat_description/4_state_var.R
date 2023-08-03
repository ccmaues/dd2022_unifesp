setwd("F:/")
pacman::p_load(psych, dplyr, glue, ggplot2, ggthemr, tidyr, patchwork)

obj_dir <- "F:/objects_R"
plot_dir <- "F:/plots_R"
tables_dir <- "F:/tables_R"

vasc <- readRDS(glue("{obj_dir}/cass_BHRC_ADHD_data.RDS"))
ggthemr("fresh")

## State
sp <- filter(vasc, popID == "BRA_SP") %>%
    select(PRSice2, PRSCS)
sc <- filter(vasc, popID == "BRA_sc") %>%
    select(PRSice2, PRSCS)

n_state <- as.data.frame(table(vasc$popID)) %>%
    rename("State" = Var1, "N" = Freq)

data.table::fwrite(
    n_state,
    glue("{tables_dir}/cass_nState_BHRC.tsv"),
    sep = "\t"
)

# verify normality
qqplot7 <- ggplot(sp, aes(sample = PRSice2)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSice2 SP",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )
dPRSice2_sp <- describe(sp$PRSice2)
rownames(dPRSice2_sp) <- "PRSice2"
tPRSice2_sp <- nortest::ad.test(sp$PRSice2)
rPRSice2_sp <- data.frame(
    Statistic = tPRSice2_sp$statistic,
    p_value = tPRSice2_sp$p.value,
    method = "Andescon-Darling Test",
    row.names = "PRSice2 SP"
)
qqplot8 <- ggplot(sp, aes(sample = PRSCS)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSCS SP",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )
dPRSCS_sp <- describe(sp$PRSCS)
rownames(dPRSCS_sp) <- "PRSCS"
tPRSCS_sp <- nortest::ad.test(sp$PRSCS)
rPRSCS_sp <- data.frame(
    Statistic = tPRSCS_sp$statistic,
    p_value = tPRSCS_sp$p.value,
    method = "Andescon-Darling Test",
    row.names = "PRSCS SP"
)

qqplot9 <- ggplot(sc, aes(sample = PRSice2)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSice2 sc",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )
dPRSice2_sc <- describe(sc$PRSice2)
rownames(dPRSice2_sc) <- "PRSice2"
tPRSice2_sc <- nortest::ad.test(sc$PRSice2)
rPRSice2_sc <- data.frame(
    Statistic = tPRSice2_sc$statistic,
    p_value = tPRSice2_sc$p.value,
    method = "Andescon-Darling Test",
    row.names = "PRSice2 sc"
)
qqplot10 <- ggplot(sc, aes(sample = PRSCS)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSCS sc",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )
dPRSCS_sc <- describe(sc$PRSCS)
rownames(dPRSCS_sc) <- "PRSCS"
tPRSCS_sc <- nortest::ad.test(sc$PRSCS)
rPRSCS_sc <- data.frame(
    Statistic = tPRSCS_sc$statistic,
    p_value = tPRSCS_sc$p.value,
    method = "Andescon-Darling Test",
    row.names = "PRSCS sc"
)

qqplot_STATE <- qqplot7 + qqplot8 + qqplot9 + qqplot10
ggsave(
    glue("{plot_dir}/cass_PRS_state_qqplot_noChange.png"),
    qqplot_STATE,
    height = 15,
    width = 40,
    unit = "cm"
)
norm_PRS_test_sex <- rbind(rPRSice2_sp, rPRSCS_sp, rPRSice2_sc, rPRSCS_sc)
data.table::fwrite(
    norm_PRS_test_sex,
    glue("{tables_dir}/cass_normalityTest_PRS_state_noChange.tsv"),
    sep = "\t",
    row.names = TRUE
)
