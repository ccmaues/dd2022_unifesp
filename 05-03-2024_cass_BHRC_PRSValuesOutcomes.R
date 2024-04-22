source("functions_to_source.R")

vADHD <-
prs_v2 %>%
select(IID, ADHD) %>%
rename(PRS = ADHD)

aADHD <-
ages %>%
tidyr::pivot_longer(
  cols = c(2, 3, 4),
  names_to = "wave",
  values_to = "age")
aADHD$wave <- gsub("age_", "", aADHD$wave)

pADHD <-
pheno %>%
select(IID, wave, dcanyhk) %>%
rename(diagnosis = 3)

data <-
plyr::join_all(list(sex, vADHD, state, aADHD), by = "IID", type = "inner") %>%
inner_join(., pADHD, by = c("IID", "wave")) %>%
filter(IID %in% ages$IID)

non_redudant_data <-
data %>%
select(IID, sex, popID, PRS) %>%
unique()

### Teste de normalidade
y <- rnorm(1363)
ks.test(unique(data$PRS), y)

### Teste de associação
## sexo
wilcox.test(PRS ~ sex, non_redudant_data)
## estado
wilcox.test(PRS ~ popID, non_redudant_data)
## diagnóstico (falta a força da associação)
# All waves (do i put :age to see only the PRS and diagnosis association?)
lm(PRS ~ diagnosis, data)
# W0
lm(PRS ~ diagnosis, filter(data, wave == "W0"))
# W1
lm(PRS ~ diagnosis, filter(data, wave == "W1"))
# W2
lm(PRS ~ diagnosis, filter(data, wave == "W2"))

## Parent diagnosis association
data_pt <- filter(parents, IID %in% data$IID)
lm(PRS ~ pdiagnosis, parents_data)

### Age association
# W0
lm(PRS ~ age, filter(data, wave == "W0"))
# W1
lm(PRS ~ age, filter(data, wave == "W1"))
# W2
lm(PRS ~ age, filter(data, wave == "W2"))
