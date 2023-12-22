setwd("F:/objects_R")

library(dplyr)
library(ggplot2)
library(ggthemr)
library(data.table)


valuesPrs <- fread("../PRS_database/Final_Scores_PRSCS/PRSCS_ANX_Score.profile") %>%
    select(IID, PRSCS_zscore) %>%
    rename(PRS = PRSCS_zscore)
phenotype <- readRDS("cass_BHRC_phenotype.RDS") %>%
    select(IID, wave, dcanyanx)
pc20 <- readRDS("cass_BHRC_PC20.RDS")
sex <- readRDS("cass_BHRC_sex.RDS")
state <- readRDS("cass_BHRC_STATE.RDS")


#> table(phenotype$wave, phenotype$dcanyanx)
#    
#        0    2
#  W0 2072  118
#  W1 1623  180
#  W2    0    0

# > table(anx_data$W0)

#    0    2
# 1698  118
# > table(anx_data$W1)

#    0    2
# 1547  269
# > table(anx_data$W2)

#    0    2
# 1547  269

forAjust <- plyr::join_all(list(
    valuesPrs, pc20, sex, state),
    type = "inner",
    by = "IID"
)
str(forAjust)

result <- glm(PRS ~ PC1:PC20 + sex + popID, data = forAjust)
ajusted <- select(forAjust, IID, PC1:PC20, sex, popID)

ajusted <- cbind(ajusted, result$residuals) %>%
    rename(PRS = V2)

type1 <- phenotype %>%
    select(IID, wave, dcanyanx) %>%
    filter(wave == "W0" & dcanyanx == 2)

phenotype <- readRDS("0_external_files/dawba_20200526.rds") %>%
    select(subjectid, redcap_event_name, dcanyanx) %>%
    rename(IID = subjectid, wave = redcap_event_name)
phenotype$wave <- gsub("wave0_arm_1", "W0", phenotype$wave)
phenotype$wave <- gsub("wave1_arm_1", "W1", phenotype$wave)
phenotype$wave <- gsub("wave2_arm_1", "W2", phenotype$wave)
phenotype$IID <- gsub("^", "C", phenotype$IID)

type1 <- phenotype %>%
    filter(wave == "W0" & dcanyanx == 2)

t1_IID <- data.frame(NULL)
t1_wave <- data.frame(NULL)
t1_pheno <- data.frame(NULL)

for (i in unique(type1$IID)) {
    t1_IID <- rbind(t1_IID, as.data.frame(rep(i, 3)))
    t1_wave <- rbind(t1_wave, as.data.frame(c("W0", "W1", "W2")))
    t1_pheno <- rbind(t1_pheno, as.data.frame(c(2, 2, 2)))
}

filter1 <- cbind(t1_IID, t1_wave, t1_pheno)

if (ncol(filter1) != 0) {
    colnames(filter1) <- c("IID", "wave", "dcanyanx")
    str(filter1)
} else {
    print("No data")
}

if (ncol(filter1 != 0)) { # Se o dado anterior não for vazio
    remove_t1 <- unique(filter1$IID) # Criar lista de IIDs já identificados
    type2 <- phenotype %>% # Carregar dado
        select(IID, wave, dcanyanx) %>% # Selecionar colunas
        filter(wave == "W1" & dcanyanx == 2) %>% # Pegar fenótipo de pessoas na wave 0, 1 e 2 que são caso
        filter(!IID %in% remove_t1) # Retirar os IIDs anteriormente identificados
} else {
    type2 <- phenotype %>%
        select(IID, wave, dcanyanx) %>%
        filter(wave == "W1" & dcanyanx == 2)
    print("No pior data of IIDs")
}

t2_IID <- data.frame(NULL) # criar data.frames vazios
t2_wave <- data.frame(NULL) # para preenchimento no loop
t2_pheno <- data.frame(NULL) # (temporário)

for (i in unique(type2$IID)) { # Para cada IID da BHRC
    t2_IID <- rbind(t2_IID, as.data.frame(rep(i, 3))) # Pegar o IID e repetir 3 vezes e juntar com os dados do data.frame t2_IID
    t2_wave <- rbind(t2_wave, as.data.frame(c("W0", "W1", "W2"))) # Colocar t2, W1 e W2 na linha e juntar aos dados do t2_wave
    t2_pheno <- rbind(t2_pheno, as.data.frame(c(0, 2, 2))) # Colocar 2, 2, 2 na linha e juntar aos dados do t2_pheno
}

filter2 <- cbind(t2_IID, t2_wave, t2_pheno) # Juntar todo o conteúdo armazenado nos data.frames do loop em um só objeto

if (ncol(filter2) != 0) {
    print("W1 = 2 data.frame sucessfully written")
    colnames(filter2) <- c("IID", "wave", "dcanyanx") # Dar nome as colunas do novo data.frame
    str(filter2) # Resumir estrutura e conteúdo do objeto
} else {
    print("No data")
}

if (ncol(filter2 != 0)) {
    remove_t2 <- unique(c(filter2$IID, filter1$IID))
    type3 <- phenotype %>%
        select(IID, wave, dcanyanx) %>%
        filter(wave == "W2" & dcanyanx == 2) %>%
        filter(!IID %in% remove_t2)
} else {
    type3 <- phenotype %>%
        select(IID, wave, dcanyanx) %>%
        filter(wave == "W2" & dcanyanx == 2)
    print("No pior data of IIDs")
}

t3_IID <- data.frame(NULL)
t3_wave <- data.frame(NULL)
t3_pheno <- data.frame(NULL)

for (i in unique(type3$IID)) {
    t3_IID <- rbind(t3_IID, as.data.frame(rep(i, 3)))
    t3_wave <- rbind(t3_wave, as.data.frame(c("W0", "W1", "W2")))
    t3_pheno <- rbind(t3_pheno, as.data.frame(c(0, 0, 2)))
}

filter3 <- cbind(t3_IID, t3_wave, t3_pheno)

if (ncol(filter3) != 0) {
    print("W2 = 2 data.frame sucessfully written")
    colnames(filter3) <- c("IID", "wave", "dcanyanx")
    str(filter3)
} else {
    print("No data")
}

cases <- do.call(rbind, list(filter1, filter2, filter3))
remove_cases <- unique(cases$IID)
str(remove_cases)
str(cases)
paste("N = ", length(cases$IID) / 3, sep = "")

ctrl <- filter(phenotype, !IID %in% remove_cases) %>% # Pegar fenótipo de pessoas que são controle (dcanyanx)
    select(IID, wave, dcanyanx) # selecionar colunas
ctrl$dcanyanx <- replace(ctrl$dcanyanx, is.na(ctrl$dcanyanx), "0") #
ctrl$wave <- replace(ctrl$wave, is.na(ctrl$wave), "W2")
str(ctrl) # Resumir estrutura e conteúdo do objeto

paste("N = ", length(ctrl$IID) / 3, sep = "")

new_pheno <- rbind(cases, ctrl) # Juntar novos fenótipos de casos e controles de anx
str(new_pheno) # Resumir estrutura e conteúdo do objeto
paste("N = ", length(new_pheno$IID) / 3, sep = "")

anx_w0 <- filter(new_pheno, wave == "W0") %>% # Pegar fenótipo de pessoas na wave 0 (dcanyanx)
    select(IID, dcanyanx) %>% # Selecionar colunas
    rename(W0 = dcanyanx) # Renomear coluna

anx_w1 <- filter(new_pheno, wave == "W1") %>% # Pegar fenótipo de pessoas na wave 1 (dcanyanx)
    select(IID, dcanyanx) %>% # Selecionar colunas
    rename(W1 = dcanyanx) # Renomear coluna

anx_w2 <- filter(new_pheno, wave == "W2") %>% # Pegar fenótipo de pessoas na wave 1 (dcanyanx)
    select(IID, dcanyanx) %>% # Selecionar colunas
    rename(W2 = dcanyanx) # Renomear coluna

str(list(anx_w0, anx_w1, anx_w2)) # Resumir estrutura e conteúdo do objeto

res_anx <- select(ajusted, IID, PRS) # Pegar as colunas de IID e resíduos de PRS de anx

anx_data <- plyr::join_all(list(res_anx, anx_w0, anx_w1, anx_w2), # Juntar objetos
    type = "inner", by = "IID"
) # por IID com tipo "inner"

anx_data$ANX_Decile <- ntile(anx_data$PRS, 10) # Criar quantis dos resíduos de PRS de anx
anx_data$W0 <- as.factor(anx_data$W0) # Transformar o tipo de dado na coluna para fator
anx_data$W1 <- as.factor(anx_data$W1) # Transformar o tipo de dado na coluna para fator
anx_data$W2 <- as.factor(anx_data$W2) # Transformar o tipo de dado na coluna para fator

str(anx_data) # Resumir estrutura e conteúdo do objeto

prevalence0 <- as.data.frame(cbind(1:10, table(anx_data$ANX_Decile, anx_data$W0))) # Fazer objeto com decil, N caso e N controle por decil
colnames(prevalence0) <- c("Decil", "anx0", "W0") # Nomear colunas
prevalence0$prevalence0 <- prevalence0$W0 / (prevalence0$anx0 + prevalence0$W0) # Calcular a prevalência
prevalence0$Decil <- factor(prevalence0$Decil) # Mudar tipo de dado da coluna
str(prevalence0) # Resumir a estrutura do objeto

prevalence1 <- as.data.frame(cbind(1:10, table(anx_data$ANX_Decile, anx_data$W1))) # Fazer objeto com decil, N caso e N controle por decil
colnames(prevalence1) <- c("Decil", "anx1", "W1") # Nomear colunas
prevalence1$prevalence1 <- prevalence1$W1 / (prevalence1$anx + prevalence1$W1) # Calcular a prevalência
prevalence1$Decil <- factor(prevalence1$Decil) # Mudar tipo de dado da coluna
str(prevalence1) # Resumir a estrutura do objeto

prevalence2 <- as.data.frame(cbind(1:10, table(anx_data$ANX_Decile, anx_data$W2))) # Fazer objeto com decil, N caso e N controle por decil
colnames(prevalence2) <- c("Decil", "anx1", "W2") # Nomear colunas
prevalence2$prevalence2 <- prevalence2$W2 / (prevalence2$anx + prevalence2$W2) # Calcular a prevalência
prevalence2$Decil <- factor(prevalence2$Decil) # Mudar tipo de dado da coluna
str(prevalence2) # Resumir a estrutura do objeto

prevalence_anx <- plyr::join_all(list(prevalence0, prevalence1, prevalence2), # Juntar data.frames na lista
    type = "inner", by = "Decil"
) # por Decil com o tipo de junção "inner"
str(prevalence_anx) # Resumir a estrutura do objeto

prev_for_plot <- prevalence_anx %>% # Carregar objeto e passar para os comandos adiante
    tidyr::pivot_longer(
        cols = starts_with("prevalence"), # diminuir a dimensão de colunas que começam com "prevalence"
        names_to = "prevalence", # renomear de prevalence
        values_to = "prev_wave"
    ) %>% # remoear para prev_wave
    select(Decil, prevalence, prev_wave) %>% # selecionar colunas
    rename(Wave = prevalence, prevalence = prev_wave) # renomear colunas

prev_for_plot$Wave <- stringr::str_replace_all(
    prev_for_plot$Wave, # Substitua tudo na coluna Wave
    c(
        "prevalence0" = "W0", # que tenha o valor de "prevalence0" por "W0"
        "prevalence1" = "W1", # valor de "prevalence1" por "W1"
        "prevalence2" = "W2"
    )
) # valor de "prevalence2" por "W2"

str(prev_for_plot) # Resumir a estrutura do objeto

library(ggthemr)
ggthemr("fresh")

new_x_axis <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")
new_x_axis <- c("1º", "2º", "3º", "4º", "5º", "6º", "7º", "8º", "9º", "10º")


prev <- ggplot(prev_for_plot, aes(x = Decil)) +
    geom_line(aes(x = Decil, y = prevalence * 100, color = Wave, group = Wave), linewidth = 2, linetype = "dashed") +
    geom_point(aes(x = Decil, y = prevalence * 100, color = Wave, group = Wave), size = 8) +
    scale_y_continuous(n.breaks = 10) +
    scale_x_discrete(labels = new_x_axis) +
    labs(y = "% Prevalência", x = "Decil de PRS") +
    ggtitle("Prevalencia de ANX na BHRC por wave", "N = 1.816") +
    theme(
        legend.position = "right",
        axis.title = element_text(color = "#000000", face = "bold", size = 15),
        axis.text = element_text(color = "#000000", face = "bold", size = 15),
        panel.grid.major = element_line(color = "#cecccc", linetype = "dotted"),
        panel.grid.minor = element_line(color = "#cecccc", linetype = "dotted"),
        plot.title = element_text(color = "#000000", face = "bold.italic", size = 20),
        plot.subtitle = element_text(color = "#000000", face = "italic", size = 17),
        legend.text = element_text(color = "#000000", face = "bold", size = 15),
        legend.title = element_text(color = "#000000", face = "bold", size = 15)
    )

prev

ggsave("ANX_BHRC_prevalence.png",
    device = "png",
    units = "cm",
    width = 30,
    height = 15)
