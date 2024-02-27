pacman::p_load(
  glue, dplyr, ggplot2, remotes,
  envalysis, extrafont, glue, ggthemr
)

#font_import()
loadfonts()

# check system
if (Sys.info()["sysname"] == "Linux") {
  Path <- "/media/santorolab/C207-3566"
  font <- "Ubuntu Condensed"
} else {
  Path <- "D:"
  font <- "Arial Narrow"
}

# Data for models ajustment and plot
pheno <-
	readRDS(glue("{Path}/objects_R/cass_BHRC_Mod_All_Phenotypes_26-02-2024.RDS"))$dcanyhk
ages <-
	readRDS(glue("{Path}/objects_R/cass_BHRC_Age_Imputed_26-02-2024.RDS")) %>%
  rename(age_W0 = W0, age_W1 = W1, age_W2 = W2)
prs <-
	data.table::fread(glue("{Path}/PRS_database/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile")) %>%
  select(IID, PRSCS_zscore) %>%
  rename(prs = PRSCS_zscore)
state <- readRDS(glue("{Path}/objects_R/cass_BHRC_STATE.RDS"))
sex <- readRDS(glue("{Path}/objects_R/cass_BHRC_sex.RDS"))
pc <- readRDS(glue("{Path}/objects_R/cass_BHRC_PC20.RDS"))

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
  list(new_prs, pheno, ages, state, sex, pc), type = "inner", by = "IID"
)

data_adhd$ADHD_Decile <- ntile(data_adhd$prs, 10)
library(patchwork)

# Decile: 1st, 5th, and 8th
for_plot <-
  data_adhd %>%
  mutate(
    color_deciles = case_when(
    ADHD_Decile == 1 ~ "1st",
    ADHD_Decile == 5 ~ "5th",
    ADHD_Decile == 8 ~ "8th",
    .default = "Other"),
  Conversion = case_when(
    W0 == 0 & W1 == 0 & W2 == 0 ~ "none",
    W0 == 0 & W1 == 0 & W2 == 2 ~ "in W2",
    W0 == 0 & W1 == 2 ~ "in W1",
    W0 == 2 ~ "in W0"),
  dentro = ifelse(
    PC1 > -0.04 &
    PC2 > -0.06, TRUE, FALSE)) %>%
  filter(ADHD_Decile == 1 | ADHD_Decile == 8 | ADHD_Decile == 5) %>%
  rename(Sex = sex, Deciles = color_deciles)

# PCA Decile
ggplot(for_plot,
  aes(PC1, PC2,
    color = Deciles,
    shape = Sex,
    fill = Deciles,
    label = Conversion)) +
  geom_point(size = 6, alpha = 0.5) +
  geom_text(
    aes(label = ifelse(!dentro, as.character(Conversion), '')),
    hjust = 1.3,
    vjust = 0,
    size = 5,
    show.legend = FALSE
    ) +
  labs(
    title = "BHRC data PCA of 1st and 8th deciles (ADHD)",
    subtitle = glue("N = {nrow(for_plot1)}"),
    caption = "27-02-2024_cass_BHRC_DecileADHDInvestigation.R"
  ) +
  scale_shape_manual(values = c("Female" = 24, "Male" = 22)) +
  scale_color_manual(values = c("1st" = "#387cb4", "8th" = "#ff9100", "5th" = "#cd75df", "other" = "#ececec")) +
  scale_fill_manual(values = c("1st" = "#387cb4", "8th" = "#ff9100", "5th" = "#cd75df", "other" = "#ececec")) +
  theme_publish() +
  theme(
    text = element_text(family = font),
    axis.title = element_text(size = 20),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    plot.caption = element_text(size = 20),
    plot.title = element_text(size = 25),
    plot.subtitle = element_text(size = 20)
)

p1 <- for_plot %>%
    group_by(Sex, Deciles) %>%
    summarise(N = n()) %>%
    group_by(Deciles) %>%
    mutate(Proportion = N / sum(N)) %>%
    ggplot(aes(Deciles, Proportion, fill = Sex)) +
    geom_bar(
        position = "fill",  # Stack bars to 100%
        stat = "identity",
        alpha = 0.95,
        width = 0.7
    ) +
    geom_text(
        aes(label = N, y = Proportion),  # Use Proportion for y-position
        position = position_fill(vjust = 0.5),  # Position text in the middle of each bar
        size = 7,
        fontface = "italic"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Female" = "#eb922b", "Male" = "#3eacb6")) +
    theme(
        text = element_text(family = font, color = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 25, vjust = 10, face = "bold", color = "black"),
        axis.line.x = element_line(linewidth = 1),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = "bold"),
        legend.title.position = "top"
    )
p2 <- for_plot %>%
    group_by(Conversion, Deciles) %>%
    summarise(N = n()) %>%
    group_by(Deciles) %>%
    mutate(Proportion = N / sum(N)) %>%
    ggplot(aes(Deciles, Proportion, fill = Conversion)) +
    geom_bar(
        position = "fill",
        stat = "identity",
        alpha = 0.95,
        width = 0.7
    ) +
    geom_text(
        aes(label = N, y = Proportion),
        position = position_fill(vjust = 0.5),
        size = 7,
        fontface = "italic"
    ) +
    theme_minimal() +
    labs(
        title = "BHRC ADHD Deciles description",
        subtitle = glue("N = {nrow(for_plot1)}"),
        caption = "27-02-2024_cass_BHRC_DecileADHDInvestigation.R"
    ) +
    theme(
        text = element_text(family = font, color = "black"),
        axis.title.x = element_text(size = 25, face = "bold", vjust = 5, color = "black"),
        axis.text.x = element_text(size = 25, vjust = 10, face = "bold", color = "black"),
        axis.line.x = element_line(linewidth = 1),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = "bold"),
        legend.title.position = "top",
        plot.title = element_text(size = 35, face = "bold", hjust = 1.2),
        plot.subtitle = element_text(size = 30, hjust = -1.5),
        plot.caption = element_text(size = 20, hjust = 0.5, vjust = 4)
    )
p3 <- for_plot %>%
    mutate(popID = case_when(
        popID == "BRA_SP" ~ "São Paulo",
        popID == "BRA_RS" ~ "Rio Grande do Sul"
    )) %>%
    rename(State = popID) %>%
    group_by(State, Deciles) %>%
    summarise(N = n()) %>%
    group_by(Deciles) %>%
    mutate(Proportion = N / sum(N)) %>%
    ggplot(aes(Deciles, Proportion, fill = State)) +
    geom_bar(
        position = "fill",
        stat = "identity",
        alpha = 0.95,
        width = 0.7
    ) +
    geom_text(
        aes(label = N, y = Proportion),
        position = position_fill(vjust = 0.5),
        size = 7,
        fontface = "italic"
    ) +
    theme_minimal() +
    theme(
        text = element_text(family = font, color = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 25, vjust = 10, face = "bold", color = "black"),
        axis.line.x = element_line(linewidth = 1),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = "bold"),
        legend.title.position = "top"
    )
p1 + p2 + p3
