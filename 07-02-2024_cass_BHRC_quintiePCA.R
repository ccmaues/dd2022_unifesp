pacman::p_load(
    glue, dplyr, ggplot2, remotes, envalysis,
    extrafont, glue, ggthemr, patchwork, tidyr
    )

#font_import()
loadfonts()

# check system
if (Sys.info()["sysname"] == "Linux") {
  path <- "/media/santorolab/C207-3566"
  font <- "Ubuntu Condensed"
} else {
  path <- "D:"
  font <- "Arial Narrow"
}

# Data for models ajustment and plot
pheno <- readRDS(glue("{path}/objects_R/cass_BHRC_ADHD_modOnlyInCases_18-12-2023.RDS"))
ages <- readRDS(glue("{path}/objects_R/cass_BHRC_Age_Imputed_19-12-2023.RDS")) %>%
    rename(age_W0 = W0, age_W1 = W1, age_W2 = W2)
prs <- data.table::fread(glue("{path}/PRS_database/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile")) %>%
    select(IID, PRSCS_zscore) %>%
    rename(prs = PRSCS_zscore)
state <- readRDS(glue("{path}/objects_R/cass_BHRC_STATE.RDS"))
sex <- readRDS(glue("{path}/objects_R/cass_BHRC_sex.RDS"))
pc <- readRDS(glue("{path}/objects_R/cass_BHRC_PC20.RDS"))

# Model ajustment
var_for_ajustment <- plyr::join_all(
    list(prs, sex, state, pc),
    by = "IID",
    type = "inner"
)
new_values <- glm(prs ~ sex:PC20, data = var_for_ajustment)

new_prs <- data.frame(
    IID = var_for_ajustment$IID,
    prs = new_values$residuals
)

data_adhd <- plyr::join_all(
    list(new_prs, sex, pheno, ages, state, pc), type = "inner", by = "IID"
)
data_adhd$ADHD_Quintile <- ntile(data_adhd$prs, 5)

ggthemr("fresh")
data_adhd <- mutate(data_adhd,
    ADHD_Quintile = case_when(
    ADHD_Quintile == 1 ~ "1st",
    ADHD_Quintile == 2 ~ "2nd",
    ADHD_Quintile == 3 ~ "3rd",
    ADHD_Quintile == 4 ~ "4th",
    ADHD_Quintile == 5 ~ "5th"
    )) %>%
    rename(Sex = sex)
data_adhd$ADHD_Quintile <- factor(data_adhd$ADHD_Quintile, levels = c("1st", "2nd", "3rd", "4th", "5th"))

# Prevalence plot:
ggplot(data_adhd,
    aes(PC1, PC2,
    color = ADHD_Quintile,
    fill = ADHD_Quintile,
    shape = Sex)) +
    geom_point(size = 5, alpha = 0.5) +
    theme_publish() +
    labs(
        title = glue("BHRC ADHD prevalence data\nN = {nrow(data_adhd)}"),
        subtitle = "PC values from genetic data",
        y = "Prevalence %",
        x = "PRS Quintile",
        caption = "Plotted by:\nCássia Maués Cuóco - cuoco@unifesp.br"
    ) +
    scale_shape_manual(values = c("Female" = 24, "Male" = 22)) +
    scale_fill_manual(values = c("1st" = "#6dc26d", "2nd" = "#009dff", "3rd" = "#b44545", "4th" = "orange", "5th" = "#7d45a0")) +
    scale_color_manual(values = c("1st" = "#6dc26d", "2nd" = "#009dff", "3rd" = "#b44545", "4th" = "orange", "5th" = "#7d45a0")) +
    theme(
            text = element_text(family = font),
            legend.position = "top",
            legend.text = element_text(size = 25),
            legend.title = element_blank(),
            axis.text = element_text(size = 25),
            axis.title = element_text(size = 30, face = "bold"),
            plot.title = element_text(size = 30),
            plot.subtitle = element_text(size = 25, face = "italic"),
            plot.caption = element_text(size = 20, hjust = 0.5)
        )
