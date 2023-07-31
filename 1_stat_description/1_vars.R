setwd("F:/")
pacman::p_load(psych, dplyr, glue, ggplot2, ggthemr, tidyr)

ext_dir <- "F:/1_stat_description"
obj_dir <- "F:/objects_R"

vars <- readRDS(glue("{obj_dir}/cass_BHRC_ADHD_to_join_PRS.RDS"))
str(vars)

## ADHD phenotype
as.data.frame(table(vars$W0)) %>%
    rename(., "Phenotype" = Var1, "N" = Freq) %>%
    knitr::kable()
as.data.frame(table(vars$W1)) %>%
    rename(., "Phenotype" = Var1, "N" = Freq) %>%
    knitr::kable()
as.data.frame(table(vars$W2)) %>%
    rename(., "Phenotype" = Var1, "N" = Freq) %>%
    knitr::kable()

## Sex
as.data.frame(table(vars$sex)) %>%
    rename(., "Sex" = Var1, "N" = Freq) %>%
    knitr::kable()

## Age
as.data.frame(table(vars$W0_Age)) %>%
    rename(., "Age" = Var1, "N" = Freq) %>%
    knitr::kable()
as.data.frame(table(vars$W1_Age)) %>%
    rename(., "Age" = Var1, "N" = Freq) %>%
    knitr::kable()
as.data.frame(table(vars$W2_Age)) %>%
    rename(., "Age" = Var1, "N" = Freq) %>%
    knitr::kable()

## State
as.data.frame(table(vars$popID)) %>%
    rename(., "State" = Var1, "N" = Freq) %>%
    knitr::kable()

## Ancestrality
describe(vars$AMR) %>% knitr::kable()
describe(vars$AFR) %>% knitr::kable()
describe(vars$EUR) %>% knitr::kable()
