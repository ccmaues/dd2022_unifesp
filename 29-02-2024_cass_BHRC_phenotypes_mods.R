pacman::p_load(
  glue, dplyr, ggplot2, remotes, envalysis,
  extrafont, glue, ggthemr, patchwork, tidyr
)

# check system
if (Sys.info()["sysname"] == "Linux") {
  path <- "/media/santorolab/C207-3566"
} else {
  path <- "D:"
}

# dcanyanx_pgc = panic/ago, GAD, SoPh, and Spph
# dcpanic/dcagor, dcgena, dcsoph, and dcspph

phenos <-
  readRDS(glue("{path}/objects_R/cass_BHRC_phenotype.RDS")) %>%
  select(
    IID, wave, dcanyhk, dceat, dcpdd,
    dcmania, dcmadep, dcocd, dcptsd,
    dcpsych, dctic, dcpanic, dcagor,
    dcgena, dcsoph, dcspph
  )

# pre-process the PGC variable (make as one) for
# everdisorder processing
pgc_processing <-
  select(phenos, dcpanic, dcagor, dcgena, dcsoph, dcspph) %>%
  mutate(dcanyanx_pgc = rowSums(., na.rm = FALSE)) %>%
  select(dcanyanx_pgc)

# get it toguether for processing
phenotypes <-
  select(phenos, IID, wave, dcanyhk,
  dceat, dcpdd, dcmania, dcmadep,
  dcocd, dcptsd, dcpsych, dctic) %>%
  bind_cols(., pgc_processing)

##  note: no variable for AD (alzheimer)

## For each phenotype in the data, let's do...
## prepare the data for processing (i.e. separate
## the phenotypes into individual data.frames)
## and remove controls that have left the study
for_ever_disorder <- list()
true_controls <- list()
processed_data <- list()

for (name_column in colnames(phenotypes)[3:ncol(phenotypes)]) {
  print(name_column)
  pre_processing <-
    phenotypes %>%
    select(IID, wave, all_of(name_column)) %>%
    pivot_wider(names_from = wave, values_from = all_of(name_column)) %>%
    select(IID, W0, W1, W2)

  # get controls that came in all waves
  true_controls[[name_column]] <-
    pre_processing %>%
    mutate(sum = rowSums(select(., -IID), na.rm = FALSE)) %>% # Keep NAs
    filter(sum == 0) %>%
    select(-sum)

  # get people that have something
  cases_identification <-
    pre_processing %>%
    mutate(sum = rowSums(select(., -IID), na.rm = TRUE)) %>% # Remove NAs
    filter(sum != 0)

  # get the rest of the IIDs
  IIDs_to_keep <- c(true_controls[[name_column]]$IID, cases_identification$IID)

  # start processing
  for_ever_disorder <-
    pre_processing %>%
    filter(IID %in% IIDs_to_keep)

  # Ever disorder case treatment
  for_ever_disorder$W1[is.na(for_ever_disorder$W1)] <- 0
  for_ever_disorder$W2[is.na(for_ever_disorder$W2)] <- 0
  for_ever_disorder$sum <- rowSums(for_ever_disorder[2:4])

  # create new output
  df_new <- data.frame(
    IID = character(),
    W0 = numeric(),
    W1 = numeric(),
    W2 = numeric(),
    stringsAsFactors = FALSE
  )

  # Get people that were flagged as cases in all waves (no = 6)
  case1 <-
    for_ever_disorder %>%
    filter(sum == 6) %>%
    select(-sum)
    print(nrow(case1))

  if(nrow(case1) == 0) {
    print("No rows in case1")
  } else {
  df_new <-
    rbind(df_new, data.frame(IID = case1$IID, W0 = 2, W1 = 2, W2 = 2))
  }

  # Get people that were flagged as case in W0
  case2 <-
    for_ever_disorder %>%
    filter(W0 == 2) %>%
    filter(!IID %in% df_new$IID)

  if(nrow(case2) == 0) {
    print("No rows in case2")
  } else {
  df_new <-
    rbind(df_new, data.frame(IID = case2$IID, W0 = 2, W1 = 2, W2 = 2))
  }

  # Get people that were flagged as case in W1
  case3 <-
    for_ever_disorder %>%
    filter(W1 == 2) %>%
    filter(!IID %in% df_new$IID)

  if(nrow(case3) == 0) {
    print("No rows in case3")
  } else {
  df_new <-
    rbind(df_new, data.frame(IID = case3$IID, W0 = 0, W1 = 2, W2 = 2))
  }

  # Get people that are case in W2 (no people with case here - too many NAs)
  if (nrow(filter(for_ever_disorder, W2 == 2)) > 0) {
    case4 <-
      for_ever_disorder %>%
      filter(W2 == 2) %>%
      filter(!IID %in% df_new$IID)
    df_new <- rbind(
      df_new,
      data.frame(IID = case4$IID, W0 = 0, W1 = 0, W2 = 2))
  }
  processed_data[[name_column]] <- rbind(df_new, true_controls[[name_column]])
}

# Make dcany from new data
final_processing <-
  purrr::imap(processed_data, function(data, name){
  data %>%
    tidyr::pivot_longer(
      cols = starts_with("W"),
      names_to = "wave",
      values_to = name)
})

# To save the IIDs sequence
f1 <-
  Reduce(function(x, y) inner_join(x, y, by = c("IID", "wave")), final_processing)

# Get the new column to the old data.frame
final_data <-
  select(f1, -IID, -wave) %>%
  mutate(dcany = rowSums(.)) %>%
  select(dcany) %>%
  bind_cols(., f1) %>%
  mutate(
    dcany = case_when(
      dcany != 0 ~ 2,
      dcany == 0 ~ 0)) %>%
  select(
    IID, wave, dcany, dcanyhk, dceat,
    dcpdd, dcmania, dcmadep, dcocd,
    dcptsd, dcpsych, dctic, dcanyanx_pgc)

saveRDS(
  final_data,
  glue("{path}/objects_R/cass_BHRC_modOnlyInCases_All_phenotypes_29-02-2024.RD.RDS")
)
