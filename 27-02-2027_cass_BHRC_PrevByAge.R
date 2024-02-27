pacman::p_load(
  glue, dplyr, ggplot2, remotes,
  envalysis, extrafont, glue, ggthemr,
  tidyr
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

data_adhd <-
  plyr::join_all(list(new_prs, pheno, ages), type = "inner", by = "IID")

# Prevalence calculation (10 and 5) and separate groups by age in W0
data_adhd$ADHD_Quintile <- ntile(data_adhd$prs, 5)
data_adhd <- select(data_adhd, -IID, -prs)

# Age
for_p0 <- table(round(data_adhd$age_W0), data_adhd$W0)
pW0 <-
  as.data.frame(cbind(rownames(for_p0), for_p0)) %>%
  rename(Age = V1, Control0 = 2, Case0 = 3) %>%
  mutate(
    across(Control0:Case0, as.numeric),
    p0 = Case0 / (Case0 + Control0),
    Age = as.numeric(Age)) %>%
  select(-Control0, -Case0)
p0_data <- c(
  mean = mean(pW0$p0), sd = sd(pW0$p0),
  se = sd(pW0$p0)/sqrt(length(pW0)),
  lower = t.test(pW0)$conf.int[1],
  upper = t.test(pW0)$conf.int[2])

for_p1 <- table(round(data_adhd$age_W1), data_adhd$W1)
pW1 <-
  as.data.frame(cbind(rownames(for_p1), for_p1)) %>%
  rename(Age = V1, Control1 = 2, Case1 = 3) %>%
  mutate(
    across(Control1:Case1, as.numeric),
    p1 = Case1 / (Case1 + Control1),
    Age = as.numeric(Age)) %>%
  select(-Control1, -Case1)
p1_data <- c(
  mean = mean(pW1$p1), sd = sd(pW1$p1),
  se = sd(pW1$p1)/sqrt(length(pW1)),
  lower = t.test(pW1)$conf.int[1],
  upper = t.test(pW1)$conf.int[2])

for_p2 <- table(round(data_adhd$age_W2), data_adhd$W2)
pW2 <-
  as.data.frame(cbind(rownames(for_p2), for_p2)) %>%
  rename(Age = V1, Control2 = 2, Case2 = 3) %>%
  mutate(
    across(Control2:Case2, as.numeric),
    p2 = Case2 / (Case2 + Control2),
    Age = as.numeric(Age)) %>%
  select(-Control2, -Case2)
p2_data <- c(
  mean = mean(pW2$p2), sd = sd(pW2$p2),
  se = sd(pW2$p2)/sqrt(length(pW2)),
  lower = t.test(pW2)$conf.int[1],
  upper = t.test(pW2)$conf.int[2])

ggthemr("fresh")

# O problema de sobreposição de idades entre as waves AINDA
# é um troço que tá me fodendo e eu não sei como ajeitar
# Fazer um gráfico pra cada wave por idade
# outro exemplo: http://www.sthda.com/english/wiki/wiki.php?id_contents=7924

w0 <- ggplot(pW0, aes(x = Age, y = p0 * 100)) +
  stat_smooth(
    method = "lm",
    formula = y ~ x,
    geom = "smooth",
    se = FALSE,
    color = "#ff0000ef",
    linetype = "dashed",
    linewidth = 0.5) +
  geom_point(size = 10, color = "#65ADC2") +
  geom_line(linewidth = 3, color = "#65ADC2", alpha = 0.7) +
  geom_errorbar(
    aes(x = Age, 
    ymin = (p0 * 100) - p0_data[4],
    ymax = (p0 * 100) + p0_data[5]),
    position = position_dodge(0.05),
    width = 0.3, linewidth = 1.5,
    alpha = 0.6, color = "#65ADC2") +
  scale_x_continuous(n.breaks = 9) +
  scale_y_continuous(n.breaks = 10, limits = c(-5, 34)) +
  theme_publish() +
  labs(
    title = "BHRC ADHD data prevalence by year in each wave",
    subtitle = "W0",
    y = "Prevalence %") +
  theme(
    text = element_text(family = font),
    plot.background = element_rect(fill = "#ffffff"),
    axis.title.y = element_blank(),
    axis.text = element_text(face = "bold", size = 20),
    legend.text = element_text(face = "italic", size = 20),
    axis.title.x = element_blank(),
    plot.subtitle = element_text(face = "bold", size = 25),
    plot.title = element_text(face = "bold", size = 30))

w1 <- ggplot(pW1, aes(x = Age, y = p1 * 100)) +
  stat_smooth(
    method = "lm",
    formula = y ~ x,
    geom = "smooth",
    se = FALSE,
    color = "#ff0000ef",
    linetype = "dashed",
    linewidth = 0.5) +
  geom_point(size = 10, color = "#233B43") +
  geom_line(linewidth = 3, color = "#233B43", alpha = 0.7) +
  geom_errorbar(
    aes(x = Age, 
    ymin = (p1 * 100) - p1_data[4],
    ymax = (p1 * 100) + p1_data[5]),
    position = position_dodge(0.05),
    width = 0.3, linewidth = 1.5,
    alpha = 0.6, color = "#233B43") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10, limits = c(-5, 34)) +
  theme_publish() +
  labs(subtitle = "W1", y = "Prevalence %") +
  theme(
    text = element_text(family = font),
    plot.background = element_rect(fill = "#ffffff"),
    axis.title.y = element_text(face = "bold", size = 25),
    axis.title.x = element_blank(),
    axis.text = element_text(face = "bold", size = 20),
    legend.text = element_text(face = "italic", size = 20),
    plot.subtitle = element_text(face = "bold", size = 25))

w2 <- ggplot(pW2, aes(x = Age, y = p2 * 100)) +
  stat_smooth(
    method = "lm",
    formula = y ~ x,
    geom = "smooth",
    se = FALSE,
    color = "#ff0000ef",
    linetype = "dashed",
    linewidth = 0.5) +
  geom_point(size = 10, color = "#E84646") +
  geom_line(linewidth = 3, color = "#E84646", alpha = 0.7) +
  geom_errorbar(
    aes(x = Age,
    ymin = (p2 * 100) - p2_data[4],
    ymax = (p2 * 100) + p2_data[5]),
    position = position_dodge(0.05),
    width = 0.3, linewidth = 1.5,
    alpha = 0.5, color = "#E84646") +
  scale_x_continuous(n.breaks = 11) +
  scale_y_continuous(n.breaks = 10, limits = c(-5, 34)) +
  theme_publish() +
  labs(
    subtitle = "W2",
    x = "Age(yr)",
    caption = "27-02-2024_cass_BHRC_PrevByAge.R") +
  theme(
    text = element_text(family = font),
    plot.background = element_rect(fill = "#ffffff"),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(face = "bold", size = 20),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    plot.caption = element_text(size = 20),
    plot.subtitle = element_text(face = "bold", size = 25))

final <- w0 / w1 / w2
final
library(patchwork)
colors <- c("#65ADC2", "#233B43", "#E84646")
