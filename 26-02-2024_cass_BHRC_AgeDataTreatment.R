pacman::p_load(
  ggplot2, dplyr, data.table,
  tidyr, readxl, glue,
  ggthemr
)

# check system
if (Sys.info()["sysname"] == "Linux") {
  Path <- "/media/santorolab/C207-3566"
} else {
  Path <- "D:"
}

# Use original RDS file with ages (it is not rounded)
age <- read_excel("0_external_files/Age_BHRCS.xlsx") %>%
  select(subjectid, W0_Age) %>%
  rename(IID = subjectid)
age$IID <- gsub("^", "C", age$IID)

# d_date = Dom Interview Date N = 1.594
# p_date1 = Psych. Section 1 Interview Date
# p_date2 = Psych. Section 2 Interview Date

espelho <-
  readRDS("0_external_files/Lucas_Keep2190_BHRCS.rds") %>%
  select(IID, ident)
espelho$ident <- as.numeric(espelho$ident)
ota <-
  readRDS("0_external_files/Ota_149BHRC_2023_01_15.rds") %>%
  select(ident, birth_date, d_date) %>%
  na.omit() %>%
  group_by(ident) %>%
  filter(n() == 3)
head(ota)

getDiffDates <-
  function(data) {
    df <-
    data %>%
      group_by(ident) %>%
      mutate(
        w0w1 = difftime(median(d_date), min(d_date), units = "days") / 365,
        w1w2 = difftime(max(d_date), median(d_date), units = "days") / 365) %>%
      ungroup() %>%
      select(ident, w0w1, w1w2) %>%
      unique()
  return(df)
}

diff_time_waves <-
  getDiffDates(ota) %>%
  inner_join(., espelho, by = "ident") %>%
  select(IID, w0w1, w1w2)

# join data and impute age
imputed_age <- inner_join(diff_time_waves, age, by = "IID") %>%
  rename(W0 = W0_Age) %>%
  mutate(W1 = W0 + w0w1, W2 = W1 + w1w2) %>%
  select(IID, W0, W1, W2)
imputed_age$W1 <- as.double(imputed_age$W1)
imputed_age$W2 <- as.double(imputed_age$W2)

saveRDS(
  imputed_age,
  glue("{Path}/objects_R/cass_BHRC_Age_Imputed_26-02-2024.RDS")
)
