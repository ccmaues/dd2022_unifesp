# começar pela descrição de dados
# quantas mães? Quantos pais?
# ver o dicionário

# fazer PRS
# calcular prevalência
# calcular R2, AUROC e AUCPR
# calular razão de prevalência
# calcular incidência
# calcular odds ratio
# ver correlação: PRS x D, PRS x tempo, PRS x pais.

# no caso de ADHD é bom verificar o separado por sexo também

source("functions_to_source.R")

pADHD <-
  pheno %>%
  select(IID, wave, dcanyhk) %>%
  pivot_wider(
    names_from = "wave",
    values_from = "dcanyhk"
  )
vADHD <-
  prs %>%
  select(IID, ADHD) %>%
  rename(PRS = ADHD)

# Não faz sentido calular razão de prevalência
# para esse estudo pq não temos um segundo grupo para usar
# (o que seria equivalente a exposto e não exposto)
# Eu posso ver se tem diferença estatística
# entre os quintis a cada wave (ex.: 5th na W0, W1 e W2)
# Odds ratio mesmo...

# Hipótese 1: os quintis de PRS apresentam diferença entre as waves.
# Teste: ANOVA (dado numérico x categórico) de independente/one-way ()
# não-paramétrica (N baixo, por decorrência não sei a distribuição)
# terei que fazer post-hoc viu???
# pergunta 2: onde fica a maior diferença?
# ESSA MERDA DE DADO DEU NORMAL AGORA EU VOU FAZER OS DOIS NESSA BUCETA

data <- plyr::join_all(
  list(calc_prev(data_adhd, 5, "PRS", "W0"),
  calc_prev(data_adhd, 5, "PRS", "W1"),
  calc_prev(data_adhd, 5, "PRS", "W2")),
  type = "inner", by = "ntile") %>%
rename(W0 = 2, W1 = 3, W2 = 4)

for_test <-
select(data, -ntile) %>%
tidyr::pivot_longer(
  ., cols = starts_with("W"),
  names_to = "wave",
  values_to = "prev"
)

# Teste de normalidade
shapiro.test(data$W0)
shapiro.test(data$W1)
shapiro.test(data$W2)

# Teste de esfericidade
library(car)
leveneTest(prev ~ wave, for_test)

# Sem outliers
ggplot(for_test, aes(prev, group = wave)) +
geom_boxplot()

# Shapiro-wilk aponta normalidade
# Levene aponta igualdade de variâncias
# Porém temos ouliers
# Conclusão: fazer teste não-paramétrico
# Kruskall-wallis
kruskal.test(prev ~ wave, for_test)

# Para saber em qual nível há a diferença
# i.e. em qual wave (post-hoc)
# posso fazer aquele gráfico de barra de erro!
# ou o dunn mesmo
library(FSA)
dunnTest(prev ~ wave, for_test)
