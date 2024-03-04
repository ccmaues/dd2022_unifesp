source("functions_to_source.R")

# Phenotype ADHD
pADHD <-
  pheno %>%
  select(IID, wave, dcanyhk) %>%
  pivot_wider(
    names_from = wave,
    values_from = dcanyhk)

# Values ADHD
vADHD <-
  prs %>%
  select(IID, ADHD) %>%
  rename(PRS = ADHD)

# Covariables for regression correction
cADHD <-
  plyr::join_all(
    list(sex, ages, state, pc),
    by = "IID",
    type = "inner"
  )

# data from ADHD
dADHD <-
  plyr::join_all(
  list(pADHD, vADHD, cADHD),
  by = "IID",
  type = "inner"
)


# Quintiles
for_plot <-
  dADHD %>%
  mutate(
    Deciles = ntile(PRS, 10),
    Deciles = case_when(
    Deciles == 1 ~ "1st",
    Deciles == 2 ~ "2nd",
    Deciles == 3 ~ "3rd",
    Deciles == 4 ~ "4th",
    Deciles == 5 ~ "5th",
    Deciles == 6 ~ "6rd",
    Deciles == 7 ~ "7th",
    Deciles == 8 ~ "8th",
    Deciles == 9 ~ "9th",
    Deciles == 10 ~ "10th"
    ),
    Quintiles = ntile(PRS, 5),
    Quintiles = case_when(
    Quintiles == 1 ~ "1st",
    Quintiles == 2 ~ "2nd",
    Quintiles == 3 ~ "3rd",
    Quintiles == 4 ~ "4th",
    Quintiles == 5 ~ "5th"
    ),
    Conversion = case_when(
    W0 == 0 & W1 == 0 & W2 == 0 ~ "control",
    W0 == 0 & W1 == 0 & W2 == 2 ~ "in W2",
    W0 == 0 & W1 == 2 ~ "in W1",
    W0 == 2 ~ "in W0")) %>%
  select(sex, popID, Conversion, Deciles, Quintiles, PC1, PC2)

# PCA Quintile
ggplot(for_plot,
  aes(PC1, PC2,
    color = Quintiles,
    shape = sex,
    fill = Quintiles,
    label = Conversion)) +
  geom_point(size = 6, alpha = 0.5) +
  labs(
    Title = glue("BHRC ADHD PCA data by quintile and sex - N = {nrow(for_plot)}"),
    subtitle = "PRS adjusted to sex, state, age (in all waves), and 20 first PCs",
    caption = "04-03-2024_cass_BHRC_QuintileADHDInvestigation.R"
  ) +
  scale_shape_manual(values = c("Female" = 24, "Male" = 22)) +
  scale_fill_manual(values = c("1st" = "#6dc26d", "2nd" = "#009dff", "3rd" = "#b44545", "4th" = "orange", "5th" = "#7d45a0")) +
  scale_color_manual(values = c("1st" = "#6dc26d", "2nd" = "#009dff", "3rd" = "#b44545", "4th" = "orange", "5th" = "#7d45a0")) +
  theme_publish() +
  theme(
    # text = element_text(family = font),
    # axis.title = element_text(size = 20),
    # axis.text.y = element_blank(),
    # axis.text.x = element_blank(),
    # axis.ticks = element_blank(),
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20),
    # plot.caption = element_text(size = 20),
    # plot.title = element_text(size = 25, vjust = 0.5),
    # plot.subtitle = element_text(size = 20)
)

# PCA Quintile
ggplot(for_plot,
  aes(PC1, PC2,
    color = Quintiles,
    shape = Sex,
    fill = Quintiles,
    label = Conversion)) +
  geom_point(size = 6, alpha = 0.5) +
  labs(
    title = "BHRC data PCA of quintiles (ADHD)",
    subtitle = glue("N = {nrow(for_plot)}"),
    caption = "28-02-2024_cass_BHRC_QuintileADHDInvestigation.R"
  ) +
  scale_shape_manual(values = c("Female" = 24, "Male" = 22)) +
  scale_color_manual(
    values = c(
      "1st" = "#387cb4", "2nd" = "#ff9100",
      "3th" = "#db74df", "4th" = "#ace676",
      "5th" = "#56ccc6", "6th" = "#d86f6f",
      "7th" = "#51a589", "8th" = "#7051b8",
      "9th" = "#c8df74", "10th" = "#dfbd74")) +
  scale_fill_manual(values = c(
      "1st" = "#387cb4", "2nd" = "#ff9100",
      "3th" = "#db74df", "4th" = "#ace676",
      "5th" = "#56ccc6", "6th" = "#d86f6f",
      "7th" = "#51a589", "8th" = "#7051b8",
      "9th" = "#c8df74", "10th" = "#dfbd74")) +
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
    plot.subtitle = element_text(size = 20))

# Quintiles composition
ggthemr("fresh")
# Conversion time
p1 <- for_plot %>%
  group_by(Conversion, Quintiles) %>%
  summarise(N = n()) %>%
  group_by(Quintiles) %>%
  mutate(Proportion = N / sum(N)) %>%
  ggplot(aes(Quintiles, Proportion, fill = Conversion)) +
  geom_bar(
    position = "fill",
    stat = "identity",
    alpha = 0.95,
    width = 0.7) +
  geom_text(aes(
    label = N, y = Proportion),
    position = position_fill(vjust = 0.5),
    size = 7,
    fontface = "italic") +
    theme_minimal() +
  labs(
    title = "BHRC ADHD Quintiles composition",
    subtitle = glue("N = {nrow(for_plot)}")) +
  theme(
    text = element_text(family = font, color = "black"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 20, hjust = 0.5, vjust = 2))

# Sex
p2 <- for_plot %>%
  group_by(Sex, Quintiles) %>%
  summarise(N = n()) %>%
  group_by(Quintiles) %>%
  mutate(Proportion = N / sum(N)) %>%
  ggplot(aes(Quintiles, Proportion, fill = Sex)) +
  geom_bar(
    position = "fill",
    stat = "identity",
    alpha = 0.95,
    width = 0.7) +
  geom_text(aes(
    label = N, y = Proportion),
    position = position_fill(vjust = 0.5),
    size = 7,
    fontface = "italic") +
    theme_minimal() +
  theme(
    text = element_text(family = font, color = "black"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    plot.title = element_text(size = 35, face = "bold"),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 20, hjust = 0.5, vjust = 2))

# State
p3 <- for_plot %>%
  group_by(popID, Quintiles) %>%
  summarise(N = n()) %>%
  group_by(Quintiles) %>%
  mutate(Proportion = N / sum(N)) %>%
  ggplot(aes(Quintiles, Proportion, fill = popID)) +
  geom_bar(
    position = "fill",
    stat = "identity",
    alpha = 0.95,
    width = 0.7) +
  geom_text(aes(
    label = N, y = Proportion),
    position = position_fill(vjust = 0.5),
    size = 7,
    fontface = "italic") +
  scale_fill_manual(values = c("São Paulo" = "#699b7f", "Rio Grande do Sul" = "#994e6d")) +
  labs(caption = "28-02-2024_cass_BHRC_QuintileADHDInvestigation.R") +
    theme_minimal() +
  theme(
    text = element_text(family = font, color = "black"),
    axis.title.x = element_text(size = 25, face = "bold", color = "black"),
    axis.text.x = element_text(size = 25, face = "bold", color = "black"),
    axis.line.x = element_line(linewidth = 1),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    plot.title = element_text(size = 35, face = "bold"),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 20, hjust = 0.5, vjust = 1))

library(patchwork)
p1 / p2 / p3
