pacman::p_load(
  glue, dplyr, ggplot2, remotes,
  envalysis, extrafont, glue, ggthemr
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
pheno <-
	readRDS(glue("{path}/objects_R/cass_BHRC_mod_All_phenotypes_26-02-2024.RDS"))$dcanyhk
ages <-
	readRDS(glue("{path}/objects_R/cass_BHRC_Age_Imputed_26-02-2024.RDS")) %>%
  rename(age_W0 = W0, age_W1 = W1, age_W2 = W2)
prs <-
	data.table::fread(glue("{path}/PRS_database/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile")) %>%
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
  list(new_prs, pheno, ages), type = "inner", by = "IID"
)

# Prevalence calculation (10 and 5) and separate groups by age in W0
data_adhd$ADHD_Decile <- ntile(data_adhd$prs, 10)
data_adhd$ADHD_Quintile <- ntile(data_adhd$prs, 5)
data_adhd$Age_sep <- ntile(data_adhd$age_W0, 4)

#> table(round(data_adhd$age_W0))
#  6   7   8   9  10  11  12  13  14
#  2 129 218 243 262 225 175 129  64

### 1st
adhd <- filter(data_adhd, Age_sep == 1)
# Decile
pW0_decile <- as.data.frame(cbind(1:10, table(adhd$ADHD_Decile, adhd$W0)))
colnames(pW0_decile) <- c("Decil", "Control0", "Case0")
pW0_decile$p0 <- pW0_decile$Case0 / (pW0_decile$Case0 + pW0_decile$Control0)
pW0_decile$Decil <- factor(pW0_decile$Decil)

pW1_decile <- as.data.frame(cbind(1:10, table(adhd$ADHD_Decile, adhd$W1)))
colnames(pW1_decile) <- c("Decil", "Control1", "Case1")
pW1_decile$p1 <- pW1_decile$Case1 / (pW1_decile$Case1 + pW1_decile$Control1)
pW1_decile$Decil <- factor(pW1_decile$Decil)

pW2_decile <- as.data.frame(cbind(1:10, table(adhd$ADHD_Decile, adhd$W2)))
colnames(pW2_decile) <- c("Decil", "Control2", "Case2")
pW2_decile$p2 <- pW2_decile$Case2 / (pW2_decile$Case2 + pW2_decile$Control2)
pW2_decile$Decil <- factor(pW2_decile$Decil)

for_plot1 <-
  plyr::join_all(
  list(pW0_decile, pW1_decile, pW2_decile),
    by = "Decil",
    type = "inner"
  ) %>%
  select(Decil, p0, p1, p2) %>%
  tidyr::pivot_longer(
    cols = starts_with("p"),
    names_to = "wave",
    values_to = "prevalence"
  )
for_plot1$wave <- gsub("^p", "W", for_plot1$wave)

# Quintile
pW0_quintile <- as.data.frame(cbind(1:5, table(adhd$ADHD_Quintile, adhd$W0)))
colnames(pW0_quintile) <- c("Quintile", "Control0", "Case0")
pW0_quintile$p0 <- pW0_quintile$Case0 / (pW0_quintile$Case0 + pW0_quintile$Control0)
pW0_quintile$Quintile <- factor(pW0_quintile$Quintile)

pW1_quintile <- as.data.frame(cbind(1:5, table(adhd$ADHD_Quintile, adhd$W1)))
colnames(pW1_quintile) <- c("Quintile", "Control1", "Case1")
pW1_quintile$p1 <- pW1_quintile$Case1 / (pW1_quintile$Case1 + pW1_quintile$Control1)
pW1_quintile$Quintile <- factor(pW1_quintile$Quintile)

pW2_quintile <- as.data.frame(cbind(1:5, table(adhd$ADHD_Quintile, adhd$W2)))
colnames(pW2_quintile) <- c("Quintile", "Control2", "Case2")
pW2_quintile$p2 <- pW2_quintile$Case2 / (pW2_quintile$Case2 + pW2_quintile$Control2)
pW2_quintile$Quintile <- factor(pW2_quintile$Quintile)

for_plot2 <-
  plyr::join_all(
  list(pW0_quintile, pW1_quintile, pW2_quintile),
    by = "Quintile",
    type = "inner"
  ) %>%
  select(Quintile, p0, p1, p2) %>%
  tidyr::pivot_longer(
    cols = starts_with("p"),
    names_to = "wave",
    values_to = "prevalence"
  )

for_plot2$wave <- gsub("^p", "W", for_plot2$wave)

### 2nd
adhd <- filter(data_adhd, Age_sep == 2)
# Decile
pW0_decile <- as.data.frame(cbind(1:10, table(adhd$ADHD_Decile, adhd$W0)))
colnames(pW0_decile) <- c("Decil", "Control0", "Case0")
pW0_decile$p0 <- pW0_decile$Case0 / (pW0_decile$Case0 + pW0_decile$Control0)
pW0_decile$Decil <- factor(pW0_decile$Decil)

pW1_decile <- as.data.frame(cbind(1:10, table(adhd$ADHD_Decile, adhd$W1)))
colnames(pW1_decile) <- c("Decil", "Control1", "Case1")
pW1_decile$p1 <- pW1_decile$Case1 / (pW1_decile$Case1 + pW1_decile$Control1)
pW1_decile$Decil <- factor(pW1_decile$Decil)

pW2_decile <- as.data.frame(cbind(1:10, table(adhd$ADHD_Decile, adhd$W2)))
colnames(pW2_decile) <- c("Decil", "Control2", "Case2")
pW2_decile$p2 <- pW2_decile$Case2 / (pW2_decile$Case2 + pW2_decile$Control2)
pW2_decile$Decil <- factor(pW2_decile$Decil)

for_plot3 <-
  plyr::join_all(
  list(pW0_decile, pW1_decile, pW2_decile),
    by = "Decil",
    type = "inner"
  ) %>%
  select(Decil, p0, p1, p2) %>%
  tidyr::pivot_longer(
    cols = starts_with("p"),
    names_to = "wave",
    values_to = "prevalence"
  )
for_plot3$wave <- gsub("^p", "W", for_plot3$wave)

# Quintile
pW0_quintile <- as.data.frame(cbind(1:5, table(adhd$ADHD_Quintile, adhd$W0)))
colnames(pW0_quintile) <- c("Quintile", "Control0", "Case0")
pW0_quintile$p0 <- pW0_quintile$Case0 / (pW0_quintile$Case0 + pW0_quintile$Control0)
pW0_quintile$Quintile <- factor(pW0_quintile$Quintile)

pW1_quintile <- as.data.frame(cbind(1:5, table(adhd$ADHD_Quintile, adhd$W1)))
colnames(pW1_quintile) <- c("Quintile", "Control1", "Case1")
pW1_quintile$p1 <- pW1_quintile$Case1 / (pW1_quintile$Case1 + pW1_quintile$Control1)
pW1_quintile$Quintile <- factor(pW1_quintile$Quintile)

pW2_quintile <- as.data.frame(cbind(1:5, table(adhd$ADHD_Quintile, adhd$W2)))
colnames(pW2_quintile) <- c("Quintile", "Control2", "Case2")
pW2_quintile$p2 <- pW2_quintile$Case2 / (pW2_quintile$Case2 + pW2_quintile$Control2)
pW2_quintile$Quintile <- factor(pW2_quintile$Quintile)

for_plot4 <- 
  plyr::join_all(
  list(pW0_quintile, pW1_quintile, pW2_quintile),
    by = "Quintile",
    type = "inner"
  ) %>%
  select(Quintile, p0, p1, p2) %>%
  tidyr::pivot_longer(
    cols = starts_with("p"),
    names_to = "wave",
    values_to = "prevalence"
  )
for_plot4$wave <- gsub("^p", "W", for_plot4$wave)

### 3rd
adhd <- filter(data_adhd, Age_sep == 3)
# Decile
pW0_decile <- as.data.frame(cbind(1:10, table(adhd$ADHD_Decile, adhd$W0)))
colnames(pW0_decile) <- c("Decil", "Control0", "Case0")
pW0_decile$p0 <- pW0_decile$Case0 / (pW0_decile$Case0 + pW0_decile$Control0)
pW0_decile$Decil <- factor(pW0_decile$Decil)

pW1_decile <- as.data.frame(cbind(1:10, table(adhd$ADHD_Decile, adhd$W1)))
colnames(pW1_decile) <- c("Decil", "Control1", "Case1")
pW1_decile$p1 <- pW1_decile$Case1 / (pW1_decile$Case1 + pW1_decile$Control1)
pW1_decile$Decil <- factor(pW1_decile$Decil)

pW2_decile <- as.data.frame(cbind(1:10, table(adhd$ADHD_Decile, adhd$W2)))
colnames(pW2_decile) <- c("Decil", "Control2", "Case2")
pW2_decile$p2 <- pW2_decile$Case2 / (pW2_decile$Case2 + pW2_decile$Control2)
pW2_decile$Decil <- factor(pW2_decile$Decil)

for_plot5 <- plyr::join_all(
    list(pW0_decile, pW1_decile, pW2_decile),
    by = "Decil",
    type = "inner"
) %>%
select(Decil, p0, p1, p2) %>%
tidyr::pivot_longer(
    cols = starts_with("p"),
    names_to = "wave",
    values_to = "prevalence"
)
for_plot5$wave <- gsub("^p", "W", for_plot5$wave)

# Quintile
pW0_quintile <- as.data.frame(cbind(1:5, table(adhd$ADHD_Quintile, adhd$W0)))
colnames(pW0_quintile) <- c("Quintile", "Control0", "Case0")
pW0_quintile$p0 <- pW0_quintile$Case0 / (pW0_quintile$Case0 + pW0_quintile$Control0)
pW0_quintile$Quintile <- factor(pW0_quintile$Quintile)

pW1_quintile <- as.data.frame(cbind(1:5, table(adhd$ADHD_Quintile, adhd$W1)))
colnames(pW1_quintile) <- c("Quintile", "Control1", "Case1")
pW1_quintile$p1 <- pW1_quintile$Case1 / (pW1_quintile$Case1 + pW1_quintile$Control1)
pW1_quintile$Quintile <- factor(pW1_quintile$Quintile)

pW2_quintile <- as.data.frame(cbind(1:5, table(adhd$ADHD_Quintile, adhd$W2)))
colnames(pW2_quintile) <- c("Quintile", "Control2", "Case2")
pW2_quintile$p2 <- pW2_quintile$Case2 / (pW2_quintile$Case2 + pW2_quintile$Control2)
pW2_quintile$Quintile <- factor(pW2_quintile$Quintile)

for_plot6 <-
  plyr::join_all(
    list(pW0_quintile, pW1_quintile, pW2_quintile),
    by = "Quintile",
    type = "inner"
  ) %>%
  select(Quintile, p0, p1, p2) %>%
  tidyr::pivot_longer(
    cols = starts_with("p"),
    names_to = "wave",
    values_to = "prevalence"
  )
for_plot6$wave <- gsub("^p", "W", for_plot6$wave)

# 4th: x >= 14
adhd <- filter(data_adhd, Age_sep == 4)
# Decile
pW0_decile <- as.data.frame(cbind(1:10, table(adhd$ADHD_Decile, adhd$W0)))
colnames(pW0_decile) <- c("Decil", "Control0", "Case0")
pW0_decile$p0 <- pW0_decile$Case0 / (pW0_decile$Case0 + pW0_decile$Control0)
pW0_decile$Decil <- factor(pW0_decile$Decil)

pW1_decile <- as.data.frame(cbind(1:10, table(adhd$ADHD_Decile, adhd$W1)))
colnames(pW1_decile) <- c("Decil", "Control1", "Case1")
pW1_decile$p1 <- pW1_decile$Case1 / (pW1_decile$Case1 + pW1_decile$Control1)
pW1_decile$Decil <- factor(pW1_decile$Decil)

pW2_decile <- as.data.frame(cbind(1:10, table(adhd$ADHD_Decile, adhd$W2)))
colnames(pW2_decile) <- c("Decil", "Control2", "Case2")
pW2_decile$p2 <- pW2_decile$Case2 / (pW2_decile$Case2 + pW2_decile$Control2)
pW2_decile$Decil <- factor(pW2_decile$Decil)

for_plot7 <-
  plyr::join_all(
    list(pW0_decile, pW1_decile, pW2_decile),
    by = "Decil",
    type = "inner"
  ) %>%
  select(Decil, p0, p1, p2) %>%
  tidyr::pivot_longer(
    cols = starts_with("p"),
    names_to = "wave",
    values_to = "prevalence"
  )
for_plot7$wave <- gsub("^p", "W", for_plot7$wave)

# Quintile
pW0_quintile <- as.data.frame(cbind(1:5, table(adhd$ADHD_Quintile, adhd$W0)))
colnames(pW0_quintile) <- c("Quintile", "Control0", "Case0")
pW0_quintile$p0 <- pW0_quintile$Case0 / (pW0_quintile$Case0 + pW0_quintile$Control0)
pW0_quintile$Quintile <- factor(pW0_quintile$Quintile)

pW1_quintile <- as.data.frame(cbind(1:5, table(adhd$ADHD_Quintile, adhd$W1)))
colnames(pW1_quintile) <- c("Quintile", "Control1", "Case1")
pW1_quintile$p1 <- pW1_quintile$Case1 / (pW1_quintile$Case1 + pW1_quintile$Control1)
pW1_quintile$Quintile <- factor(pW1_quintile$Quintile)

pW2_quintile <- as.data.frame(cbind(1:5, table(adhd$ADHD_Quintile, adhd$W2)))
colnames(pW2_quintile) <- c("Quintile", "Control2", "Case2")
pW2_quintile$p2 <- pW2_quintile$Case2 / (pW2_quintile$Case2 + pW2_quintile$Control2)
pW2_quintile$Quintile <- factor(pW2_quintile$Quintile)

for_plot8 <-
  plyr::join_all(
    list(pW0_quintile, pW1_quintile, pW2_quintile),
    by = "Quintile",
    type = "inner"
  ) %>%
  select(Quintile, p0, p1, p2) %>%
  tidyr::pivot_longer(
    cols = starts_with("p"),
    names_to = "wave",
    values_to = "prevalence"
  )
for_plot8$wave <- gsub("^p", "W", for_plot8$wave)


# for the plotting part
new_x_axis_5 <- c("1st", "2nd", "3rd", "4th", "5th")
new_x_axis_10 <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")

ggthemr("fresh")
library(patchwork)
p1 <-
  ggplot(for_plot1, aes(x = Decil)) +
  geom_line(
    aes(
    x = Decil,
    y = prevalence * 100,
    color = wave,
    group = wave),
  linewidth = 1,
  linetype = "dashed") +
  geom_point(aes(x = Decil, y = prevalence * 100, color = wave), size = 5) +
  scale_x_discrete(labels = new_x_axis_10) +
  scale_y_continuous(n.breaks = 10, limits = c(1, 50)) +
  theme_publish() +
  labs(
    title = glue("BHRC ADHD prevalence data\nN = {nrow(data_adhd)}"),
    subtitle = "min: 5.8 yr\nmax: 8.57 yr",
    y = "Prevalence %",
    x = "PRS Decile") +
  theme(
    legend.position = "top",
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

p2 <-
  ggplot(for_plot3, aes(x = Decil)) +
  geom_line(
  aes(
    x = Decil,
    y = prevalence * 100,
    color = wave,
    group = wave),
  linewidth = 1,
  linetype = "dashed") +
  geom_point(aes(x = Decil, y = prevalence * 100, color = wave), size = 5) +
  scale_x_discrete(labels = new_x_axis_10) +
  scale_y_continuous(n.breaks = 10, limits = c(1, 50)) +
  theme_publish() +
  labs(
    subtitle = "min: 8.57 yr\nmax: 10.08 yr",
    y = "Prevalence %",
    x = "PRS Decile") +
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none")

p3 <-
  ggplot(for_plot5, aes(x = Decil)) +
  geom_line(
    aes(
      x = Decil,
      y = prevalence * 100,
      color = wave,
      group = wave),
    linewidth = 1,
    linetype = "dashed") +
  geom_point(aes(x = Decil, y = prevalence * 100, color = wave), size = 5) +
  scale_x_discrete(labels = new_x_axis_10) +
  scale_y_continuous(n.breaks = 10, limits = c(1, 50)) +
  theme_publish() +
  labs(
    subtitle = "min: 10.09 yr\nmax: 11.53 yr",
    y = "Prevalence %",
    x = "PRS Decile") +
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none")

p4 <-
  ggplot(for_plot7, aes(x = Decil)) +
  geom_line(
    aes(
      x = Decil,
      y = prevalence * 100,
      color = wave,
      group = wave),
    linewidth = 1,
    linetype = "dashed") +
  geom_point(aes(x = Decil, y = prevalence * 100, color = wave), size = 5) +
  scale_x_discrete(labels = new_x_axis_10) +
  scale_y_continuous(n.breaks = 10, limits = c(1, 50)) +
  theme_publish() +
  labs(
    subtitle = "min: 11.54 yr\nmax: 14.33 yr",
    y = "Prevalence %",
    x = "PRS Decile",
    caption = "27-02-2024_cass_BHRC)PrevByW0AgeSepADHD_plot.R") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none")

p5 <-
  ggplot(for_plot2, aes(x = Quintile)) +
  geom_line(
  aes(
    x = Quintile,
    y = prevalence * 100,
    color = wave,
    group = wave),
  linewidth = 1,
  linetype = "dashed") +
  geom_point(aes(x = Quintile, y = prevalence * 100, color = wave), size = 5) +
  scale_x_discrete(labels = new_x_axis_5) +
  scale_y_continuous(n.breaks = 10) +
  theme_publish() +
  labs(
    title = glue("BHRC ADHD prevalence data\nN = {nrow(data_adhd)}"),
    subtitle = "min: 5.83 yr\nmax: 8.57 yr",
    y = "Prevalence %",
    x = "PRS Quintile") +
  theme(
    legend.position = "top",
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

p6 <-
  ggplot(for_plot4, aes(x = Quintile)) +
  geom_line(
    aes(
      x = Quintile,
      y = prevalence * 100,
      color = wave,
      group = wave),
  linewidth = 1,
  linetype = "dashed") +
  geom_point(aes(x = Quintile, y = prevalence * 100, color = wave), size = 5) +
  scale_x_discrete(labels = new_x_axis_5) +
  scale_y_continuous(n.breaks = 10) +
  theme_publish() +
  labs(
    subtitle = "min: 8.57 yr\nmax: 10.08 yr",
    y = "Prevalence %",
    x = "PRS Quintile") +
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )
p7 <-
  ggplot(for_plot6, aes(x = Quintile)) +
  geom_line(
    aes(
      x = Quintile,
      y = prevalence * 100,
      color = wave,
      group = wave),
    linewidth = 1,
    linetype = "dashed") +
  geom_point(aes(x = Quintile, y = prevalence * 100, color = wave), size = 5) +
  scale_x_discrete(labels = new_x_axis_5) +
  scale_y_continuous(n.breaks = 10) +
  theme_publish() +
  labs(
    subtitle = "min: 10.09 yr\nmax: 11.53 yr",
    y = "Prevalence %",
    x = "PRS Quintile") +
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none")

p8 <-
  ggplot(for_plot8, aes(x = Quintile)) +
  geom_line(
    aes(
      x = Quintile,
      y = prevalence * 100,
      color = wave,
      group = wave
    ),
    linewidth = 1,
    linetype = "dashed") +
  geom_point(aes(x = Quintile, y = prevalence * 100, color = wave), size = 5) +
  scale_x_discrete(labels = new_x_axis_5) +
  scale_y_continuous(n.breaks = 10) +
  theme_publish() +
  labs(
    subtitle = "min: 11.54 yr\nmax: 14.33 yr",
    y = "Prevalence %",
    x = "PRS Quintile",
    caption = "27-02-2024_cass_BHRC)PrevByW0AgeSepADHD_plot.R"
  ) +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none")

p1 / p2 / p3 / p4
p5 / p6 / p7 / p8
