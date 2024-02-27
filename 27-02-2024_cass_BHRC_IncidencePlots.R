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

# I need to get the dates in which they were diagnosed and put into a data.frame
# Datas para geração de gráficos -------------------------
dates <-
  readRDS(glue("{Path}/0_external_files/Ota_149BHRC_2023_01_15.rds")) %>%
  select(ident, redcap_event_name, birth_date_original, d_date) %>%
  rename(wave = redcap_event_name, birth = birth_date_original) %>%
  mutate(
    wave = case_when(
      wave == "wave0_arm_1" ~ "W0",
      wave == "wave1_arm_1" ~ "W1",
      wave == "wave2_arm_1" ~ "W2"
    )
  )
espelho <-
  readRDS("D:/0_external_files/Lucas_Keep2190_BHRCS.rds") %>%
  select(ident, IID) %>%
  mutate(ident = as.double(ident))
pheno <-
	readRDS(glue("{Path}/objects_R/cass_BHRC_Mod_All_Phenotypes_26-02-2024.RDS"))$dcanyhk
ages <-
	readRDS(glue("{Path}/objects_R/cass_BHRC_Age_Imputed_26-02-2024.RDS")) %>%
  rename(age_W0 = W0, age_W1 = W1, age_W2 = W2) %>%
  pivot_longer(
    cols = starts_with("age_W"),
    names_to = "wave",
    values_to = "age"
  ) %>%
  mutate(age = round(age, digits = 0))
ages$wave <- gsub("age_", "", ages$wave)
sex <- readRDS(glue("{Path}/objects_R/cass_BHRC_sex.RDS"))

# devtools::install_github("reconhub/incidence")

# 1. Mudar de ident para IID
codf <-
  inner_join(dates, espelho, by = "ident") %>%
  select(-ident) %>%
  na.omit()
all_waves <- filter(data.frame(table(codf$IID)), Freq == 3)
# 2. Pegar datas de avaliação
eval <-
  select(codf, IID, d_date) %>%
  filter(IID %in% all_waves$Var1)
# 3. Pegar fenótipos que SURGIRAM
conv_wave <-
  pheno %>%
  mutate(
    conversion_wave = case_when(
      W0 == 2 ~ "W0",
      W0 == 0 & W1 == 2 ~ "W1",
      W0 == 0 & W1 == 0 & W2 == 2 ~ "W2",
      W0 == 0 & W1 == 0 & W2 == 0 ~ "Control")) %>%
  select(IID, conversion_wave) %>%
  rename(wave = conversion_wave)
# 4. Associar fenótipo a datas
conv_date <-
  eval %>%
  mutate(
    wave = case_when(
    d_date <= as.Date("2011-12-31") ~ "W0",
    d_date >= as.Date("2012-01-01") & d_date <= as.Date("2014-12-31") ~ "W1",
    d_date >= as.Date("2015-01-01") ~ "W2")) %>%
  inner_join(., conv_wave, by = c("IID", "wave")) %>%
  inner_join(., sex, by = "IID") %>%
  inner_join(., ages, by = c("IID", "wave")) %>%
  select(-IID)
conv_W0 <-
  filter(conv_date, wave == "W0") %>%
  select(-wave)
conv_W1 <-
  filter(conv_date, wave == "W1") %>%
  select(-wave)
conv_W2 <-
  filter(conv_date, wave == "W2") %>%
  select(-wave)

# Plotar gráfico ------------------------------------------------
library(incidence)
# fazer grupo com idade ao invés de sexo
i0 <-
  incidence(
    as.Date(conv_W0$d_date),
    interval = "1 month",
    groups = conv_W0$age)

plot(i0, border = "white", stack = TRUE)

i1 <-
  incidence(
    as.Date(conv_W1$d_date),
    interval = "1 month",
    groups = conv_W1$age)

plot(i1, border = "white", stack = TRUE)

# posso fazer um zoom nos aglomerados pra melhorar o visual
i2 <-
  incidence(
    as.Date(conv_W2$d_date),
    interval = "1 month",
    groups = conv_W2$age)

plot(i2, border = "white", stack = TRUE)
# https://cran.r-project.org/web/packages/incidence/vignettes/overview.html previsao de incidência