setwd("F:/")
dir("objects_R/")
pacman::p_load(ggplot2, dplyr, data.table, tidyr)
library(readxl)

dictonary <- read_excel("0_external_files/BHRC_Dict_of_Variables_W0W1W2.xlsx")
BHRC_pheno <- readRDS("0_external_files/dawba_20200526.rds")
str(BHRC_pheno)
adhd <- select(BHRC_pheno, subjectid, redcap_event_name, dcanyhk) %>%
    rename(IID = subjectid, wave = redcap_event_name, ADHD_pheno = dcanyhk)
adhd$wave <- gsub("wave0_arm_1", "W0", adhd$wave)
adhd$wave <- gsub("wave1_arm_1", "W1", adhd$wave)
adhd$wave <- gsub("wave2_arm_1", "W2", adhd$wave)
adhd$IID <- gsub("^", "C", adhd$IID)
str(adhd)
# Retirar controles que não vieram nas outras waves
# (tentar também só com aqueles que vieram)
coW0 <- filter(adhd, wave == "W0" & ADHD_pheno == 0) %>%
    na.omit()
coW1 <- filter(adhd, wave == "W1" & ADHD_pheno == 0) %>%
    na.omit()
coW2 <- filter(adhd, wave == "W2" & ADHD_pheno == 0) %>%
    na.omit()
str(coW0)
lapply(list(coW0, coW1, coW2), str)
coIIDs <- c(coW0$IID, coW1$IID, coW2$IID)
coIIDs_InAllWaves <- filter(as.data.frame(table(coIIDs)), Freq == 3)
str(coIIDs_InAllWaves)
# Preparar dado pra imputação de fenótipo
forEverDisorder <- pivot_wider(adhd, names_from = wave, values_from = ADHD_pheno) %>%
    select(IID, W0, W1, W2) %>%
    filter(!IID %in% coIIDs_InAllWaves$coIIDs)

# Ever disorder de ADHD (somente casos são tratados)
process_data <- function(data) {
    df <- data
    df$W1[is.na(df$W1)] <- 0
    df$W2[is.na(df$W2)] <- 0
    df$sum <- rowSums(df[2:4])

    df_new <- data.frame(
            IID = character(),
            W0 = numeric(),
            W1 = numeric(),
            W2 = numeric(),
            stringsAsFactors = FALSE
            )

    # Get all cases (no = 6)
    if (nrow(filter(df, sum == 6)) > 0) {
        case1 <- filter(df, sum == 6)
        df_new <- bind_rows(
            df_new,
            data.frame(IID = case1$IID, W0 = 2, W1 = 2, W2 = 2)
            )
    }

    # Get people that are case in W0
    case2 <- filter(df, W0 == 2) %>%
        filter(!IID %in% df_new$IID)
    df_new <- bind_rows(
        df_new,
        data.frame(IID = case2$IID, W0 = 2, W1 = 2, W2 = 2)
        )

    # Get people that are case in W1
    case3 <- filter(df, W1 == 2) %>%
        filter(!IID %in% df_new$IID)
    df_new <- bind_rows(
        df_new,
        data.frame(IID = case3$IID, W0 = 0, W1 = 2, W2 = 2))

    # Get people that are case in W2 (no people with case here - too many NAs)
    if (nrow(filter(df, W2 == 2)) > 0) {
        case4 <- filter(df, W2 == 2) %>%
            filter(!IID %in% df_new$IID)
        df_new <- bind_rows(
            df_new,
            data.frame(IID = case4$IID, W0 = 0, W1 = 0, W2 = 2)
            )
    }
    return(df_new)
}

cases_newADHDpheno <- process_data(forEverDisorder)
control_newADHDpheno <- filter(adhd, IID %in% coIIDs_InAllWaves$coIIDs) %>%
    pivot_wider(., names_from = wave, values_from = ADHD_pheno)
newADHDpheno <- rbind(cases_newADHDpheno, control_newADHDpheno)
# saveRDS(newADHDpheno, "objects_R/cass_BHRC_ADHD_modOnlyInCases_18-12-2023.RDS")
# Não usar imputação de idade
age <- readRDS("objects_R/cass_BHRC_ages.RDS")
forPlot_AGE <- pivot_longer(age,
    cols = W0_Age:W2_Age,
    names_to = "wave",
    values_to = "ages")
forPlot_AGE$wave <- gsub("_Age", "", forPlot_AGE$wave)
# fazer gráfico de idade por wave (violin + boxplot + point)
library(ggthemr)
ggthemr("fresh")
# ggplot(forPlot_AGE, aes(wave, ages, group = wave, fill = wave)) +
#     geom_violin(alpha = 0.9) +
#     geom_jitter(
#         width = 0.1,
#         shape = 18,
#         color = "#3d3d3d",
#         alpha = 0.3
#         ) +
#     stat_boxplot(
#         geom = "errorbar",
#         color = "black",
#         width = 0.2,
#         linewidth = 1
#         ) +
#     geom_boxplot(
#         outlier.colour = "red",
#         outlier.shape = 1,
#         width = 0.3,
#         linewidth = 1
#         ) +
#     labs(
#         title = "BHRC probands's age distribuition by the waves",
#         subtitle = "Without age imputation",
#         caption = "Cassia M Cuoco - ccmaues@gmail.com"
#         ) +
#     ylab("Age (yr)") +
#     xlab("Wave") +
#     ylim(c(5.5, 22)) +
#     scale_y_continuous(n.breaks = 15) +
#     theme(
#         title = element_text(face = "bold", color = "black"),
#         axis.text = element_text(face = "bold", color = "black"),
#         plot.subtitle = element_text(face = "italic"),
#         plot.caption = element_text(face = "italic"),
#         legend.title = element_text(face = "bold", color = "black"),
#         panel.grid.major.y = element_line(color = "#7272724c"),
#         panel.grid.major.x =  element_blank()
#         )  +
#     annotate("text", x = "W0", y = 15,
#         label = "N = 2,511", vjust = 0, fontface = "bold") +
#     annotate("text", x = "W1", y = 19,
#         label = "N = 2,010", vjust = 0, fontface = "bold") +
#     annotate("text", x = "W2", y = 22, 
#         label = "N = 1,928", vjust = 0, fontface = "bold")

# ggsave(
#     "age_distribuition_BHRC.png",
#     device = "png", units = "cm",
#     width = 20, height = 20
#     )
# clusterizar os grupos de idade (7 + 8 anos; 9 + 10; 11 + 12; 13 + 14)
newPhenoPlusAge <- left_join(newADHDpheno, age, by = "IID")
str(newPhenoPlusAge)
# gráfico daqueles com info de TDHA - DAWBA
forPlot_AGE2 <- select(
    newPhenoPlusAge, W0_Age, W1_Age, W2_Age
    ) %>%
    pivot_longer(
    cols = W0_Age:W2_Age,
    names_to = "wave",
    values_to = "ages"
    )
forPlot_AGE2$wave <- gsub("_Age", "", forPlot_AGE2$wave)
# ggplot(forPlot_AGE2, aes(wave, ages, group = wave, fill = wave)) +
#     geom_violin(alpha = 0.9) +
#     geom_jitter(
#         width = 0.1,
#         shape = 18,
#         color = "#3d3d3d",
#         alpha = 0.3
#     ) +
#     stat_boxplot(
#         geom = "errorbar",
#         color = "black",
#         width = 0.2,
#         linewidth = 1
#     ) +
#     geom_boxplot(
#         outlier.colour = "red",
#         outlier.shape = 1,
#         width = 0.3,
#         linewidth = 1
#     ) +
#     labs(
#         title = "BHRC probands's age distribuition by the waves",
#         subtitle = "Without age imputation",
#         caption = "Cassia M Cuoco - ccmaues@gmail.com"
#     ) +
#     ylab("Age (yr)") +
#     xlab("Wave") +
#     ylim(c(5.5, 22)) +
#     scale_y_continuous(n.breaks = 15) +
#     theme(
#         title = element_text(face = "bold", color = "black"),
#         axis.text = element_text(face = "bold", color = "black"),
#         plot.subtitle = element_text(face = "italic"),
#         plot.caption = element_text(face = "italic"),
#         legend.title = element_text(face = "bold", color = "black"),
#         panel.grid.major.y = element_line(color = "#7272724c"),
#         panel.grid.major.x = element_blank()
#     ) +
#     annotate("text", x = "W0", y = 15,
#         label = "N = 1,762", vjust = 0, fontface = "bold") +
#     annotate("text", x = "W1", y = 19,
#         label = "N = 1,711", vjust = 0, fontface = "bold") +
#     annotate("text", x = "W2", y = 22, 
#         label = "N = 1,700", vjust = 0, fontface = "bold")

# ggsave(
#     "age_distribuition_BHRC_withPhenotypeDAWBA.png",
#     device = "png", units = "cm",
#     width = 20, height = 20
# )

# identificar qual são as colunas de W1 e W2
dates <- readRDS("0_external_files/dates.rds") %>%
    select(ident, redcap_event_name, birth_date, p_date1, p_date2)
str(dates)
# Juntar com as idades da W0 e fazer a imputação
espelho <- readRDS("0_external_files/Lucas_Keep2190_BHRCS.rds") %>%
    select(IID, ident)
espelho$ident <- as.numeric(espelho$ident)
ota <- readRDS("0_external_files/Ota_149BHRC_2023_01_15.rds") %>%
    select(ident, birth_date, d_date) %>%
    na.omit() %>%
    group_by(ident) %>%
    filter(n() == 3)

str(ota)
# d_date = Dom Interview Date
# p_date1 = Psych. Section 1 Interview Date
# p_date2 = Psych. Section 2 Interview Date
# n = 1594
# filter by people that have Freq as 3

getDiffDates <- function(data) {
    df <- data %>%
        group_by(ident) %>%
        mutate(
            w0w1 = difftime(
                median(d_date),
                min(d_date),
                units = "days"
            ) / 365.25,
            w1w2 = difftime(
                max(d_date),
                median(d_date),
                units = "days"
            ) / 365.25
        ) %>%
        ungroup() %>%
        select(ident, w0w1, w1w2) %>%
        unique()
    return(df)
}

diffTimeWaves <- getDiffDates(ota) %>%
    inner_join(., espelho, by = "ident") %>%
    select(IID, w0w1, w1w2)

# Use original RDS file with ages (it is no rounded)
ageOG <- read_excel("0_external_files/Age_BHRCS.xlsx") %>%
    select(subjectid, W0_Age) %>%
    rename(IID = subjectid)
ageOG$IID <- gsub("^", "C", ageOG$IID)

# join data and impute age
imputedAge <- inner_join(diffTimeWaves, ageOG, by = "IID") %>%
    rename(W0 = W0_Age) %>%
    mutate(W1 = W0 + w0w1, W2 = W1 + w1w2) %>%
    select(IID, W0, W1, W2)
imputedAge$W1 <- as.double(imputedAge$W1)
imputedAge$W2 <- as.double(imputedAge$W2)

# saveRDS(imputedAge, "cass_BHRC_Age_Imputed_19-12-2023.RDS")
forPlot_AGE <- pivot_longer(imputedAge,
    cols = W0:W2,
    names_to = "wave",
    values_to = "ages")
# ggplot(forPlot_AGE, aes(wave, ages, group = wave, fill = wave)) +
#     geom_violin(
#         alpha = 0.6,
#         aes(color = wave)) +
#     geom_jitter(
#         width = 0.1,
#         shape = 18,
#         color = "#3d3d3d",
#         alpha = 0.3
#         ) +
#     stat_boxplot(
#         geom = "errorbar",
#         color = "black",
#         width = 0.2,
#         linewidth = 1
#         ) +
#     geom_boxplot(
#         outlier.colour = "red",
#         outlier.shape = 1,
#         width = 0.3,
#         linewidth = 1
#         ) +
#     labs(
#         title = "BHRC probands's age distribuition by the waves",
#         subtitle = "With age imputation - N = 1,450",
#         caption = "Cássia M Cuóco - ccmaues@gmail.com"
#         ) +
#     ylab("Age (yr)") +
#     xlab("Wave") +
#     ylim(c(5.5, 22)) +
#     scale_y_continuous(n.breaks = 15) +
#     theme(
#         title = element_text(face = "bold", color = "black"),
#         axis.text = element_text(face = "bold", color = "black"),
#         plot.subtitle = element_text(face = "italic"),
#         plot.caption = element_text(face = "italic"),
#         legend.title = element_text(face = "bold", color = "black"),
#         panel.grid.major.y = element_line(color = "#7272724c"),
#         panel.grid.major.x =  element_blank()
#         )
# ggsave(
#     "BHRC_Age_wave_distribuitionOnlyInAllWaves.png",
#     device = "png", units = "cm",
#     width = 20, height = 20)

# fazer, a partir daqui, um sem imputação de idade e um com imputação de idade
t <- inner_join(newADHDpheno, imputedAge, by = "IID")
