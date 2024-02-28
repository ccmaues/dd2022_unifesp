pacman::p_load(
  glue, dplyr, ggplot2, remotes,
  envalysis, extrafont, glue, ggthemr,
  tidyr, patchwork
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

colors <- c("#65ADC2", "#233B43", "#E84646")

# Data for models ajustment and plot
ages <-
	readRDS(glue("{Path}/objects_R/cass_BHRC_Age_Imputed_26-02-2024.RDS")) %>%
  rename(age_W0 = W0, age_W1 = W1, age_W2 = W2)
state <-
  readRDS(glue("{Path}/objects_R/cass_BHRC_STATE.RDS")) %>%
  mutate(
    popID = case_when(
      popID == "BRA_SP" ~ "São Paulo",
      popID == "BRA_RS" ~ "Rio Grande do Sul"))
sex <- readRDS(glue("{Path}/objects_R/cass_BHRC_sex.RDS"))
pheno <- readRDS(glue("{Path}/objects_R/cass_BHRC_Mod_all_Phenotypes_26-02-2024.RDS"))$dcanyhk

for_plot <-
  plyr::join_all(list(ages, sex, state, pheno), type = "inner", by = "IID") %>%
  mutate(
    Conversion = case_when(
      W0 == 0 & W1 == 0 & W2 == 0 ~ "none",
      W0 == 0 & W1 == 0 & W2 == 2 ~ "in W2",
      W0 == 0 & W1 == 2 ~ "in W1",
      W0 == 2 ~ "in W0"),
    across(where(is.numeric), round, digits = 0)) %>%
  select(-W0, -W1, -W2, -IID)

ggthemr("fresh")

p1 <-
  for_plot %>%
  select(Conversion, age_W0) %>%
  rename(Age = age_W0) %>%
  group_by(Conversion, Age) %>%
  summarise(N = n()) %>%
  group_by(Age) %>%
  mutate(Proportion = N / sum(N)) %>%
  ggplot(aes(Age, Proportion, fill = Conversion)) +
  geom_bar(
    position = "fill",
    stat = "identity",
    alpha = 0.95,
    width = 0.7) +
  scale_x_continuous(n.breaks = 9) +
  geom_text(aes(
    label = N, y = Proportion),
    position = position_fill(vjust = 0.5),
    size = 7,
    fontface = "italic") +
    theme_minimal() +
  labs(
    title = "BHRC ADHD prev by age - conversion time",
    subtitle = glue("N = {nrow(for_plot)}")) +
  theme(
    text = element_text(family = font, color = "black"),
    axis.text.x = element_text(size = 25, face = "bold"),
    axis.line.x = element_line(linewidth = 1),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 20, hjust = 0.5, vjust = 2))

p2 <-
  for_plot %>%
  select(Conversion, age_W1) %>%
  rename(Age = age_W1) %>%
  group_by(Conversion, Age) %>%
  summarise(N = n()) %>%
  group_by(Age) %>%
  mutate(Proportion = N / sum(N)) %>%
  ggplot(aes(Age, Proportion, fill = Conversion)) +
  geom_bar(
    position = "fill",
    stat = "identity",
    alpha = 0.95,
    width = 0.7) +
  scale_x_continuous(n.breaks = 10) +
  geom_text(aes(
    label = N, y = Proportion),
    position = position_fill(vjust = 0.5),
    size = 7,
    fontface = "italic") +
    theme_minimal() +
  theme(
    text = element_text(family = font, color = "black"),
    axis.text.x = element_text(size = 25, face = "bold"),
    axis.line.x = element_line(linewidth = 1),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 20, hjust = 0.5, vjust = 2))

p3 <-
  for_plot %>%
  select(Conversion, age_W2) %>%
  rename(Age = age_W2) %>%
  group_by(Conversion, Age) %>%
  summarise(N = n()) %>%
  group_by(Age) %>%
  mutate(Proportion = N / sum(N)) %>%
  ggplot(aes(Age, Proportion, fill = Conversion)) +
  geom_bar(
    position = "fill",
    stat = "identity",
    alpha = 0.95,
    width = 0.7) +
  scale_x_continuous(n.breaks = 11) +
  geom_text(aes(
    label = N, y = Proportion),
    position = position_fill(vjust = 0.5),
    size = 7,
    fontface = "italic") +
    theme_minimal() +
  labs(caption = "28-02-2024_cass_BHRC_PrevBtAgeComp.R") +
  theme(
    text = element_text(family = font, color = "black"),
    axis.text.x = element_text(size = 25, face = "bold"),
    axis.line.x = element_line(linewidth = 1),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 20, hjust = 0.5, vjust = 2))

p1 / p2 / p3

# Sex
p1 <-
  for_plot %>%
  select(sex, age_W0) %>%
  rename(Age = age_W0) %>%
  group_by(sex, Age) %>%
  summarise(N = n()) %>%
  group_by(Age) %>%
  mutate(Proportion = N / sum(N)) %>%
    ggplot(aes(Age, Proportion, fill = sex)) +
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
  scale_x_continuous(n.breaks = 9) +
  theme_minimal() +
  labs(
    title = "BHRC ADHD prev by age - sex composition",
    subtitle = glue("N = {nrow(for_plot)}")) +
  theme(
    text = element_text(family = font, color = "black"),
    axis.text.x = element_text(size = 25, face = "bold"),
    axis.line.x = element_line(linewidth = 1),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 20, hjust = 0.5, vjust = 2))

p2 <-
  for_plot %>%
  select(sex, age_W1) %>%
  rename(Age = age_W1) %>%
  group_by(sex, Age) %>%
  summarise(N = n()) %>%
  group_by(Age) %>%
  mutate(Proportion = N / sum(N)) %>%
    ggplot(aes(Age, Proportion, fill = sex)) +
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
  scale_x_continuous(n.breaks = 10) +
  theme_minimal() +
  theme(
    text = element_text(family = font, color = "black"),
    axis.text.x = element_text(size = 25, face = "bold"),
    axis.line.x = element_line(linewidth = 1),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 20, hjust = 0.5, vjust = 2))

p3 <-
  for_plot %>%
  select(sex, age_W2) %>%
  rename(Age = age_W2) %>%
  group_by(sex, Age) %>%
  summarise(N = n()) %>%
  group_by(Age) %>%
  mutate(Proportion = N / sum(N)) %>%
    ggplot(aes(Age, Proportion, fill = sex)) +
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
  scale_x_continuous(n.breaks = 11) +
  theme_minimal() +
  labs(caption = "28-02-2024_cass_BHRC_PrevBtAgeComp.R") +
  theme(
    text = element_text(family = font, color = "black"),
    axis.text.x = element_text(size = 25, face = "bold"),
    axis.line.x = element_line(linewidth = 1),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 20, hjust = 0.5, vjust = 2))

p1 / p2 / p3

# State
p1 <-
  for_plot %>%
  select(popID, age_W0) %>%
  rename(Age = age_W0) %>%
  group_by(popID, Age) %>%
  summarise(N = n()) %>%
  group_by(Age) %>%
  mutate(Proportion = N / sum(N)) %>%
    ggplot(aes(Age, Proportion, fill = popID)) +
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
  scale_x_continuous(n.breaks = 9) +
  theme_minimal() +
  labs(
    title = "BHRC ADHD prev by age - state composition",
    subtitle = glue("N = {nrow(for_plot)}")) +
  theme(
    text = element_text(family = font, color = "black"),
    axis.text.x = element_text(size = 25, face = "bold"),
    axis.line.x = element_line(linewidth = 1),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 20, hjust = 0.5, vjust = 2))

p2 <-
  for_plot %>%
  select(sex, age_W1) %>%
  rename(Age = age_W1) %>%
  group_by(sex, Age) %>%
  summarise(N = n()) %>%
  group_by(Age) %>%
  mutate(Proportion = N / sum(N)) %>%
    ggplot(aes(Age, Proportion, fill = sex)) +
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
  scale_x_continuous(n.breaks = 10) +
  theme_minimal() +
  theme(
    text = element_text(family = font, color = "black"),
    axis.text.x = element_text(size = 25, face = "bold"),
    axis.line.x = element_line(linewidth = 1),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 20, hjust = 0.5, vjust = 2))

p3 <-
  for_plot %>%
  select(sex, age_W2) %>%
  rename(Age = age_W2) %>%
  group_by(sex, Age) %>%
  summarise(N = n()) %>%
  group_by(Age) %>%
  mutate(Proportion = N / sum(N)) %>%
    ggplot(aes(Age, Proportion, fill = sex)) +
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
  scale_x_continuous(n.breaks = 11) +
  theme_minimal() +
  labs(caption = "28-02-2024_cass_BHRC_PrevBtAgeComp.R") +
  theme(
    text = element_text(family = font, color = "black"),
    axis.text.x = element_text(size = 25, face = "bold"),
    axis.line.x = element_line(linewidth = 1),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 20, hjust = 0.5, vjust = 2))

p1 / p2 / p3
