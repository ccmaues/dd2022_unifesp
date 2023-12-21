setwd("F:/")
pacman::p_load(ggplot2, dplyr, data.table, tidyr, readxl, patchwork)
dir("objects_R/")

# Modified phenotype: 19-12-2023 (Co Freq != 3 removed and only Ca imputed)
adhd <- readRDS("objects_R/cass_BHRC_ADHD_modOnlyInCases_18-12-2023.RDS")

# PRS values from Lucas Ito
prs <- fread("PRS_database/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile") %>%
    select(IID, PRSCS_zscore) %>%
    rename(PRS = PRSCS_zscore)

# PC values from samples in PRS
pca <- readRDS("objects_R/cass_BHRC_PC20.RDS")
colnames(pca) <- c("IID", paste("PC", 1:20, sep = ""))

# State from the samples
state <- readRDS("objects_R/cass_BHRC_STATE.RDS")
state$popID <- factor(state$popID)

# Sex from the samples
sex <- readRDS("objects_R/cass_BHRC_sex.RDS")

# Age imputated from evaluation dates
ageMOD <- readRDS("objects_R/cass_BHRC_Age_Imputed_19-12-2023.RDS")

# Age without imputation
age <- read_excel("0_external_files/Age_BHRCS.xlsx") %>%
    rename(IID = subjectid)
age$IID <- gsub("^", "C", age$IID)

forAjustment <- plyr::join_all(
    list(prs, pca, state, sex),
    type = "inner", by = "IID"
    )
str(forAjustment)

# PRS model ajustment
values <- glm(PRS ~ PC1:PC20 + popID + sex, data = forAjustment)
ajustedPRS <- data.frame(forAjustment$IID, values$residuals) %>%
    rename(IID = forAjustment.IID, PRS = values.residuals)

workData <- plyr::join_all(
    list(ajustedPRS, adhd, age, ageMOD),
    type = "inner", by = "IID")
colnames(workData)[3:11] <- c(
    "phenoW0", "phenoW1", "phenoW2",
    "ageW0", "ageW1", "ageW2",
    "modAgeW0", "modAgeW1", "modAgeW2"
    )
workData<- mutate(workData, ADHD_Decile = ntile(PRS, 10))
str(workData)

# prevalence calculus filtered by waves of age
# in the FIRST wave without age imputation
# until 8 yr
g1 <- filter(workData, ageW0 <= 8)
# until 10 yr
g2 <- filter(workData, ageW0 <= 10)
# until 12 yr
g3 <- filter(workData, ageW0 <= 12)
# until 14 yr
g4 <- filter(workData, ageW0 <= 14)

# Group 1
pw0g1 <- as.data.frame(cbind(1:10, table(g1$ADHD_Decile, g1$phenoW0)))
colnames(pw0g1) <- c("Decil", "adhd0", "w0")
pw0g1$pw0 <- pw0g1$w0 / (pw0g1$adhd0 + pw0g1$w0)
pw0g1$Decil <- factor(pw0g1$Decil)

pw1g1 <- as.data.frame(cbind(1:10, table(g1$ADHD_Decile, g1$phenoW1)))
colnames(pw1g1) <- c("Decil", "adhd0", "w1")
pw1g1$pw1 <- pw1g1$w1 / (pw1g1$adhd0 + pw1g1$w1)
pw1g1$Decil <- factor(pw1g1$Decil)

pw2g1 <- as.data.frame(cbind(1:10, table(g1$ADHD_Decile, g1$phenoW2)))
colnames(pw2g1) <- c("Decil", "adhd0", "w2")
pw2g1$pw2 <- pw2g1$w2 / (pw2g1$adhd0 + pw2g1$w2)
pw2g1$Decil <- factor(pw2g1$Decil)

g1Prevalence <- plyr::join_all(
    list(pw0g1, pw1g1, pw2g1), type = "inner",
    by = "Decil"
    ) %>%
    select(Decil, pw0, pw1, pw2)

forPlotg1 <- pivot_longer(
    g1Prevalence, cols = pw0:pw2,
    names_to = "waves", values_to = "prevalence"
)
forPlotg1$waves <- gsub("pw", "W", forPlotg1$waves)

# Group 2
pw0g2 <- as.data.frame(cbind(1:10, table(g2$ADHD_Decile, g2$phenoW0)))
colnames(pw0g2) <- c("Decil", "adhd0", "w0")
pw0g2$pw0 <- pw0g2$w0 / (pw0g2$adhd0 + pw0g2$w0)
pw0g2$Decil <- factor(pw0g2$Decil)

pw1g2 <- as.data.frame(cbind(1:10, table(g2$ADHD_Decile, g2$phenoW1)))
colnames(pw1g2) <- c("Decil", "adhd0", "w1")
pw1g2$pw1 <- pw1g2$w1 / (pw1g2$adhd0 + pw1g2$w1)
pw1g2$Decil <- factor(pw1g2$Decil)

pw2g2 <- as.data.frame(cbind(1:10, table(g2$ADHD_Decile, g2$phenoW2)))
colnames(pw2g2) <- c("Decil", "adhd0", "w2")
pw2g2$pw2 <- pw2g2$w2 / (pw2g2$adhd0 + pw2g2$w2)
pw2g2$Decil <- factor(pw2g2$Decil)

g2Prevalence <- plyr::join_all(
    list(pw0g2, pw1g2, pw2g2),
    type = "inner",
    by = "Decil"
    ) %>%
    select(Decil, pw0, pw1, pw2)
forPlotg2 <- pivot_longer(
    g2Prevalence,
    cols = pw0:pw2,
    names_to = "waves", values_to = "prevalence"
)
forPlotg2$waves <- gsub("pw", "W", forPlotg2$waves)

# Group 3
pw0g3 <- as.data.frame(cbind(1:10, table(g3$ADHD_Decile, g3$phenoW0)))
colnames(pw0g3) <- c("Decil", "adhd0", "w0")
pw0g3$pw0 <- pw0g3$w0 / (pw0g3$adhd0 + pw0g3$w0)
pw0g3$Decil <- factor(pw0g3$Decil)

pw1g3 <- as.data.frame(cbind(1:10, table(g3$ADHD_Decile, g3$phenoW1)))
colnames(pw1g3) <- c("Decil", "adhd0", "w1")
pw1g3$pw1 <- pw1g3$w1 / (pw1g3$adhd0 + pw1g3$w1)
pw1g3$Decil <- factor(pw1g3$Decil)

pw2g3 <- as.data.frame(cbind(1:10, table(g3$ADHD_Decile, g3$phenoW2)))
colnames(pw2g3) <- c("Decil", "adhd0", "w2")
pw2g3$pw2 <- pw2g3$w2 / (pw2g3$adhd0 + pw2g3$w2)
pw2g3$Decil <- factor(pw2g3$Decil)

g3Prevalence <- plyr::join_all(
    list(pw0g3, pw1g3, pw2g3),
    type = "inner",
    by = "Decil"
) %>%
    select(Decil, pw0, pw1, pw2)
forPlotg3 <- pivot_longer(
    g3Prevalence,
    cols = pw0:pw2,
    names_to = "waves", values_to = "prevalence"
)
forPlotg3$waves <- gsub("pw", "W", forPlotg3$waves)

# Group 4
pw0g4 <- as.data.frame(cbind(1:10, table(g4$ADHD_Decile, g4$phenoW0)))
colnames(pw0g4) <- c("Decil", "adhd0", "w0")
pw0g4$pw0 <- pw0g4$w0 / (pw0g4$adhd0 + pw0g4$w0)
pw0g4$Decil <- factor(pw0g4$Decil)

pw1g4 <- as.data.frame(cbind(1:10, table(g4$ADHD_Decile, g4$phenoW1)))
colnames(pw1g4) <- c("Decil", "adhd0", "w1")
pw1g4$pw1 <- pw1g4$w1 / (pw1g4$adhd0 + pw1g4$w1)
pw1g4$Decil <- factor(pw1g4$Decil)

pw2g4 <- as.data.frame(cbind(1:10, table(g4$ADHD_Decile, g4$phenoW2)))
colnames(pw2g4) <- c("Decil", "adhd0", "w2")
pw2g4$pw2 <- pw2g4$w2 / (pw2g4$adhd0 + pw2g4$w2)
pw2g4$Decil <- factor(pw2g4$Decil)

g4Prevalence <- plyr::join_all(
    list(pw0g4, pw1g4, pw2g4),
    type = "inner",
    by = "Decil"
) %>%
    select(Decil, pw0, pw1, pw2)
forPlotg4 <- pivot_longer(
    g3Prevalence,
    cols = pw0:pw2,
    names_to = "waves", values_to = "prevalence"
)
forPlotg3$waves <- gsub("pw", "W", forPlotg3$waves)

forPlotNoAgeMod <- list(
    g1 = forPlotg1,
    g2 = forPlotg2,
    g3 = forPlotg3,
    g4 = forPlotg4
)

# Prevalence plot for 4 groups without age imputation
library(ggthemr)
ggthemr("fresh")
plots <- list()
opt <- list()
types <- c("<= 8 yr at W0", "<= 10 yr at W0", "<= 12 yr at W0", "<= 14 yr at W0")
subtitle <- c("N = X", "N = x", "N = x", "N = x")
new_x_axis <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")

make_prev_plot <- function(data) {
    for (i in 1:length(data)) {
        df <- data[[i]]
        p1 <- ggplot(df, aes(x = Decil)) +
            geom_line(aes(
                x = Decil, y = prevalence * 100, color = waves,
                group = waves), size = 2, linetype = "dashed"
                ) +
            geom_point(aes(
                x = Decil, y = prevalence * 100,
                color = waves), size = 5
                ) +
            scale_y_continuous(n.breaks = 15) +
            scale_x_discrete(labels = new_x_axis) +
            labs(y = "Prevalence %", x = "PRS Decile") +
            ggtitle(paste(types[i]), paste(subtitle[i])) +
            ylim(c(5,55)) +
            theme(
                legend.position = "right",
                axis.title =
                    element_text(color = "#000000", face = "bold", size = 10),
                axis.text =
                    element_text(color = "#000000", face = "bold", size = 10),
                panel.grid.major =
                    element_line(color = "#cecccc", linetype = "dotted"),
                panel.grid.minor =
                    element_line(color = "#cecccc", linetype = "dotted"),
                plot.title =
                    element_text(color = "#000000", face = "bold.italic"),
                plot.subtitle =
                    element_text(color = "#000000", face = "bold.italic"),
                legend.text =
                    element_text(color = "#000000", face = "bold", size = 10),
                legend.title =
                    element_text(color = "#000000", face = "bold", size = 10)
            )
        opt[[i]] <- p1
    }
    return(opt)
}

plotsPrev <- make_prev_plot(forPlotNoAgeMod)
plotOpt <- plotsPrev[[1]] / plotsPrev[[2]] / plotsPrev[[3]] / plotsPrev[[4]]

ggsave(
    "BHRC_ADHD_prevalenceByW0Age.png",
    plotOpt,
    device = "png", units = "cm",
    width = 30, height = 25
)
# fazer agora com:
# Co faltante tirado
# everdisorder
# imputação de idade
# somente W0, W1, W2
# Incidência

workData2 <- plyr::join_all(
    list(ajustedPRS, adhd,ageMOD),
    type = "inner", by = "IID")
colnames(workData2)[3:8] <- c(
    "phenoW0", "phenoW1", "phenoW2",
    "modAgeW0", "modAgeW1", "modAgeW2"
    )
workData2 <- mutate(workData2, ADHD_Decile = ntile(PRS, 10))
str(workData2)
# prevalence calculus filtered by waves of age
# in the FIRST wave with age imputation
# until 8 yr
g1 <- filter(workData2, modAgeW0 <= 8)
# until 10 yr
g2 <- filter(workData2, modAgeW0 <= 10)
# until 12 yr
g3 <- filter(workData2, modAgeW0 <= 12)
# until 14 yr
g4 <- filter(workData2, modAgeW0 <= 14)

# Group 1
pw0g1 <- as.data.frame(cbind(1:10, table(g1$ADHD_Decile, g1$phenoW0)))
colnames(pw0g1) <- c("Decil", "adhd0", "w0")
pw0g1$pw0 <- pw0g1$w0 / (pw0g1$adhd0 + pw0g1$w0)
pw0g1$Decil <- factor(pw0g1$Decil)

pw1g1 <- as.data.frame(cbind(1:10, table(g1$ADHD_Decile, g1$phenoW1)))
colnames(pw1g1) <- c("Decil", "adhd0", "w1")
pw1g1$pw1 <- pw1g1$w1 / (pw1g1$adhd0 + pw1g1$w1)
pw1g1$Decil <- factor(pw1g1$Decil)

pw2g1 <- as.data.frame(cbind(1:10, table(g1$ADHD_Decile, g1$phenoW2)))
colnames(pw2g1) <- c("Decil", "adhd0", "w2")
pw2g1$pw2 <- pw2g1$w2 / (pw2g1$adhd0 + pw2g1$w2)
pw2g1$Decil <- factor(pw2g1$Decil)

g1Prevalence <- plyr::join_all(
    list(pw0g1, pw1g1, pw2g1), type = "inner",
    by = "Decil"
    ) %>%
    select(Decil, pw0, pw1, pw2)

forPlotg1 <- pivot_longer(
    g1Prevalence, cols = pw0:pw2,
    names_to = "waves", values_to = "prevalence"
)
forPlotg1$waves <- gsub("pw", "W", forPlotg1$waves)

# Group 2
pw0g2 <- as.data.frame(cbind(1:10, table(g2$ADHD_Decile, g2$phenoW0)))
colnames(pw0g2) <- c("Decil", "adhd0", "w0")
pw0g2$pw0 <- pw0g2$w0 / (pw0g2$adhd0 + pw0g2$w0)
pw0g2$Decil <- factor(pw0g2$Decil)

pw1g2 <- as.data.frame(cbind(1:10, table(g2$ADHD_Decile, g2$phenoW1)))
colnames(pw1g2) <- c("Decil", "adhd0", "w1")
pw1g2$pw1 <- pw1g2$w1 / (pw1g2$adhd0 + pw1g2$w1)
pw1g2$Decil <- factor(pw1g2$Decil)

pw2g2 <- as.data.frame(cbind(1:10, table(g2$ADHD_Decile, g2$phenoW2)))
colnames(pw2g2) <- c("Decil", "adhd0", "w2")
pw2g2$pw2 <- pw2g2$w2 / (pw2g2$adhd0 + pw2g2$w2)
pw2g2$Decil <- factor(pw2g2$Decil)

g2Prevalence <- plyr::join_all(
    list(pw0g2, pw1g2, pw2g2),
    type = "inner",
    by = "Decil"
    ) %>%
    select(Decil, pw0, pw1, pw2)
forPlotg2 <- pivot_longer(
    g2Prevalence,
    cols = pw0:pw2,
    names_to = "waves", values_to = "prevalence"
)
forPlotg2$waves <- gsub("pw", "W", forPlotg2$waves)

# Group 3
pw0g3 <- as.data.frame(cbind(1:10, table(g3$ADHD_Decile, g3$phenoW0)))
colnames(pw0g3) <- c("Decil", "adhd0", "w0")
pw0g3$pw0 <- pw0g3$w0 / (pw0g3$adhd0 + pw0g3$w0)
pw0g3$Decil <- factor(pw0g3$Decil)

pw1g3 <- as.data.frame(cbind(1:10, table(g3$ADHD_Decile, g3$phenoW1)))
colnames(pw1g3) <- c("Decil", "adhd0", "w1")
pw1g3$pw1 <- pw1g3$w1 / (pw1g3$adhd0 + pw1g3$w1)
pw1g3$Decil <- factor(pw1g3$Decil)

pw2g3 <- as.data.frame(cbind(1:10, table(g3$ADHD_Decile, g3$phenoW2)))
colnames(pw2g3) <- c("Decil", "adhd0", "w2")
pw2g3$pw2 <- pw2g3$w2 / (pw2g3$adhd0 + pw2g3$w2)
pw2g3$Decil <- factor(pw2g3$Decil)

g3Prevalence <- plyr::join_all(
    list(pw0g3, pw1g3, pw2g3),
    type = "inner",
    by = "Decil"
) %>%
    select(Decil, pw0, pw1, pw2)
forPlotg3 <- pivot_longer(
    g3Prevalence,
    cols = pw0:pw2,
    names_to = "waves", values_to = "prevalence"
)
forPlotg3$waves <- gsub("pw", "W", forPlotg3$waves)

# Group 4
pw0g4 <- as.data.frame(cbind(1:10, table(g4$ADHD_Decile, g4$phenoW0)))
colnames(pw0g4) <- c("Decil", "adhd0", "w0")
pw0g4$pw0 <- pw0g4$w0 / (pw0g4$adhd0 + pw0g4$w0)
pw0g4$Decil <- factor(pw0g4$Decil)

pw1g4 <- as.data.frame(cbind(1:10, table(g4$ADHD_Decile, g4$phenoW1)))
colnames(pw1g4) <- c("Decil", "adhd0", "w1")
pw1g4$pw1 <- pw1g4$w1 / (pw1g4$adhd0 + pw1g4$w1)
pw1g4$Decil <- factor(pw1g4$Decil)

pw2g4 <- as.data.frame(cbind(1:10, table(g4$ADHD_Decile, g4$phenoW2)))
colnames(pw2g4) <- c("Decil", "adhd0", "w2")
pw2g4$pw2 <- pw2g4$w2 / (pw2g4$adhd0 + pw2g4$w2)
pw2g4$Decil <- factor(pw2g4$Decil)

g4Prevalence <- plyr::join_all(
    list(pw0g4, pw1g4, pw2g4),
    type = "inner",
    by = "Decil"
) %>%
    select(Decil, pw0, pw1, pw2)
forPlotg4 <- pivot_longer(
    g3Prevalence,
    cols = pw0:pw2,
    names_to = "waves", values_to = "prevalence"
)
forPlotg3$waves <- gsub("pw", "W", forPlotg3$waves)

forPlotNoAgeMod <- list(
    g1 = forPlotg1,
    g2 = forPlotg2,
    g3 = forPlotg3,
    g4 = forPlotg4
)

plotsPrev <- make_prev_plot(forPlotNoAgeMod)
plotOpt <- plotsPrev[[1]] / plotsPrev[[2]] / plotsPrev[[3]] / plotsPrev[[4]]

ggsave(
    "BHRC_ADHD_prevalenceByW0AgeImputation.png",
    plotOpt,
    device = "png", units = "cm",
    width = 30, height = 25
)
