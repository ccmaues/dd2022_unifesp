setwd("F:/")
pacman::p_load(psych, dplyr, glue, ggplot2, ggthemr, tidyr, patchwork)

obj_dir <- "F:/objects_R"
plot_dir <- "F:/plots_R"
tables_dir <- "F:/tables_R"

vars <- readRDS(glue("{obj_dir}/cass_BHRC_ADHD_data.RDS"))
ggthemr("fresh")

## Sex
sex_BHRC <- as.data.frame(table(vars$sex)) %>%
    rename(., "Sex" = Var1, "N" = Freq)
data.table::fwrite(
    sex_BHRC,
    glue("{tables_dir}/cass_nSex_BHRC.tsv"),
    sep = "\t"
)
fem <- filter(vars, sex == "Female") %>%
    select(PRSice2, PRSCS)

man <- filter(vars, sex == "Male") %>%
    select(PRSice2, PRSCS)

# verify normality
qqplot3 <- ggplot(fem, aes(sample = PRSCS)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSCS ADHD female",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )
dPRSCS_fem <- describe(fem$PRSCS)
rownames(dPRSCS_fem) <- "PRSCS"
tPRSCS_fem <- nortest::ad.test(fem$PRSCS)
rPRSCS_fem <- data.frame(
    Statistic = tPRSCS_fem$statistic,
    p_value = tPRSCS_fem$p.value,
    method = "Anderson-Darling Test",
    row.names = "PRSCS fem"
)
qqplot4 <- ggplot(fem, aes(sample = PRSice2)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSice2 ADHD female",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )
dPRSice2_fem <- describe(fem$PRSice2)
rownames(dPRSice2_fem) <- "PRSice2"
tPRSice2_fem <- nortest::ad.test(fem$PRSice2)
rPRSice2_fem <- data.frame(
    Statistic = tPRSice2_fem$statistic,
    p_value = tPRSice2_fem$p.value,
    method = "Anderson-Darling Test",
    row.names = "PRSice2 fem"
)

qqplot5 <- ggplot(man, aes(sample = PRSCS)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSCS ADHD male",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )
dPRSCS_man <- describe(man$PRSCS)
rownames(dPRSCS_man) <- "PRSCS"
tPRSCS_man <- nortest::ad.test(man$PRSCS)
rPRSCS_man <- data.frame(
    Statistic = tPRSCS_man$statistic,
    p_value = tPRSCS_man$p.value,
    method = "Anderson-Darling Test",
    row.names = "PRSCS man"
)
qqplot6 <- ggplot(man, aes(sample = PRSice2)) +
    stat_qq() +
    geom_abline(
        intercept = 0, slope = 1,
        linetype = "dashed", color = "red"
    ) +
    labs(
        title = "PRSice2 ADHD male",
        x = "Theoretical Quantiles",
        y = "Observed Quantiles"
    )
dPRSice2_man <- describe(man$PRSice2)
rownames(dPRSice2_man) <- "PRSice2"
tPRSice2_man <- nortest::ad.test(man$PRSice2)
rPRSice2_man <- data.frame(
    Statistic = tPRSice2_man$statistic,
    p_value = tPRSice2_man$p.value,
    method = "Anderson-Darling Test",
    row.names = "PRSice2 man"
)
qqplot_SEX <- qqplot3 + qqplot4 + qqplot5 + qqplot6
ggsave(
    glue("{plot_dir}/cass_PRS_sex_qqplot_noChange.png"),
    qqplot_SEX,
    height = 15,
    width = 40,
    unit = "cm"
)
norm_PRS_test_sex <- rbind(rPRSice2_fem, rPRSCS_fem, rPRSice2_man, rPRSCS_man)
data.table::fwrite(
    norm_PRS_test_sex,
    glue("{tables_dir}/cass_normalityTest_PRS_sex_noChange.tsv"),
    sep = "\t",
    row.names = TRUE
)
desc_PRS_sex <- rbind(dPRSice2_fem, dPRSCS_fem, dPRSice2_man, dPRSCS_man)
data.table::fwrite(
    desc_PRS_sex,
    glue("{tables_dir}/cass_description_PRS_sex_noChange.tsv"),
    sep = "\t",
    row.names = TRUE
)
# Sex test bias (t-test)
test_PRSice_sex <- t.test(fem$PRSice2, man$PRSice2)
test_PRSCS_sex <- t.test(fem$PRSCS, man$PRSCS)

t.test_result_sex <- data.frame(
    t = test_PRSice_sex$statistic,
        df = test_PRSice_sex$parameter,
        p = test_PRSice_sex$p.value,
        row.names = "PRSice2"
)
t.test_result_sex <- rbind(t.test_result_sex, data.frame(
    t = test_PRSCS_sex$statistic,
    df = test_PRSCS_sex$parameter,
    p = test_PRSCS_sex$p.value,
    row.names = "PRSCS2"
))
data.table::fwrite(
    t.test_result_sex,
    glue("{tables_dir}/cass_TTest_sex_PRS_noChange.tsv"),
    sep = "\t",
    row.names = TRUE
)
