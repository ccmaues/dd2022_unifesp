# Prevalência separando as pessoas por idade na W0
pacman::p_load(
  glue, dplyr, ggplot2, RColorBrewer,
  remotes, envalysis, raincloudplots,
  extrafont, glue, ggthemr
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

age <-
  readRDS(glue("{Path}/objects_R/cass_BHRC_Age_Imputed_26-02-2024.RDS"))

mean_values <- summarize(
  age,
  across(W0:W2, mean, na.rm = TRUE)
) %>%
tidyr::pivot_longer(
  cols = everything(),
  names_to = "wave",
  values_to = "mean_values"
)

forplot <- tidyr::pivot_longer(
  age, cols = W0:W2, names_to = "wave", values_to = "age"
)

ggthemr("fresh")

ggplot(forplot, aes(x = wave, y = age, fill = wave)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    width = 0.6,
    .width = 0,
    justification = -0.2,
    point_colour = NA,
    alpha = 0.9
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.color = "#992727",
    outlier.shape = 21,
    outlier.size = 3,
    color = "black",
    linewidth = 1,
    position = position_dodge(width = 0.75),
    coef = 1,
    notch = FALSE,
    alpha = 0.9
  ) +
  gghalves::geom_half_point(
    size = 2,
    side = "l",
    range_scale = 0.4,
    alpha = 0.1,
    shape = 21
  ) +
  geom_text(
    data = mean_values,
    color = "white",
    size = 8,
    aes(
      x = wave,
      y = mean_values,
      label = sprintf("~ %.1f years", mean_values),
      family = font,
      fontface = "bold"
    ),
    position = position_dodge(width = 0.75),
    vjust = -3,
    size = 5) +
  # scale_fill_manual(values = wave_colors2) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  coord_flip() +
  labs(
    title = "Age distribution of BHRC probands from 2010 - 2020",
    subtitle = glue("N = {nrow(age)}"),
    caption = "Data: cass_BHRC_Age_Imputed_26-02-2024.RDS
    Code: 27-02-2024_cass_BHRC_ageDistributionAll_plot.R",
    y = "AGE (yr)"
  ) +
  theme_publish() +
  theme(
    text = element_text(family = font),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 25),
    axis.title.x = element_text(
      face = "bold",
      size = 25,
      hjust = 1.06,
      vjust = 7
    ),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = "plain", size = 20),
    axis.text.y = element_text(face = "bold", hjust = 10, vjust = -2, size = 25),
    legend.position = "none",
    plot.caption = element_text(size = 19, vjust = 10),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_line(
      color = "#616161",
      linetype = "dashed"
    ),
    plot.margin = unit(c(0, 1.5, 16, 1), "cm")
  )
