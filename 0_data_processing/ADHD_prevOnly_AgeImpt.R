setwd("F:/objects_R")
pacman::p_load(ggplot2, dplyr, data.table, patchwork)

sex <- readRDS("cass_BHRC_sex.RDS")
state <- readRDS("cass_BHRC_STATE.RDS")
pc <- readRDS("cass_BHRC_PC20.RDS")
age <- readRDS("cass_BHRC_Age_Imputed_19-12-2023.RDS")
pheno <- readRDS("cass_BHRC_ADHD_modOnlyInCases_18-12-2023.RDS")
prs <- fread("../PRS_Database/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile") %>%
    select(IID, PRSCS_zscore) %>%
    rename(PRS = PRSCS_zscore)

forAjust <- plyr::join_all(
    list(prs, pc, sex, state),
    type = "inner",
    by = "IID"
)

newValues <- glm(PRS ~ PC1:PC20 + sex + popID, data = forAjust)
newPRS <- data.frame(cbind(forAjust$IID, newValues$residuals)) %>%
    rename(IID = X1, PRS = X2)

workData <- inner_join(pheno, newPRS, by = "IID") %>%
    mutate(ADHD_Decile = ntile(n = 10, PRS))

pw0 <- as.data.frame(cbind(1:10, table(workData$ADHD_Decile, workData$W0)))
colnames(pw0) <- c("Decil", "adhd0", "w0")
pw0$pw0 <- pw0$w0 / (pw0$adhd0 + pw0$w0)
pw0$Decil <- factor(pw0$Decil)

pw1 <- as.data.frame(cbind(1:10, table(workData$ADHD_Decile, workData$W1)))
colnames(pw1) <- c("Decil", "adhd1", "w1")
pw1$pw1 <- pw1$w1 / (pw1$adhd1 + pw1$w1)
pw1$Decil <- factor(pw1$Decil)

pw2 <- as.data.frame(cbind(1:10, table(workData$ADHD_Decile, workData$W2)))
colnames(pw2) <- c("Decil", "adhd2", "w2")
pw2$pw2 <- pw2$w2 / (pw2$adhd2 + pw2$w2)
pw2$Decil <- factor(pw2$Decil)

prevalence <- plyr::join_all(
    list(pw0, pw1, pw2),
    type = "inner",
    by = "Decil"
    ) %>%
    select(Decil, pw0, pw1, pw2)

forPlot <- tidyr::pivot_longer(
    prevalence,
    cols = pw0:pw2,
    names_to = "waves", values_to = "prevalence"
)
forPlot$waves <- gsub("pw", "W", forPlot$waves)

library(ggthemr)
ggthemr("fresh")
new_x_axis <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")

ggplot(forPlot, aes(x = Decil)) +
    geom_line(aes(
        x = Decil, y = prevalence * 100, color = waves, group = waves), linewidth = 4, linetype = "dashed") +
    geom_point(aes(
        x = Decil, y = prevalence * 100, color = waves), size = 8) +
    scale_y_continuous(n.breaks = 15) +
    scale_x_discrete(labels = new_x_axis) +
    labs(y = "Prevalence %", x = "PRS Decile") +
    ggtitle("ADHD prevalence in BHRC waves", "N = 1,598") +
    theme(
        legend.position = "right",
        axis.title = element_text(color = "#000000", face = "bold", size = 15),
        axis.text = element_text(color = "#000000", size = 15),
        panel.grid.major = element_line(color = "#9b9b9b", linetype = "dotted"),
        panel.grid.minor = element_line(color = "#9b9b9b", linetype = "dotted"),
        plot.title = element_text(color = "#000000", face = "bold.italic", size = 20),
        plot.subtitle = element_text(color = "#000000", face = "italic", size = 17),
        legend.text = element_text(color = "#000000", face = "italic", size = 15),
        legend.title = element_text(color = "#000000", face = "bold.italic", size = 15)
    )

ggsave(
    "../ADHD_BHRC_AgeImptCoRm_CaImpt_22122023.png",
    device = "png", width = 40, height = 15, units = "cm")
