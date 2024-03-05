# https://stats.stackexchange.com/questions/484299/how-to-check-the-correlation-between-categorical-and-numeric-independent-variabl
source("functions_to_source.R")

# Check normality of the data
library(psych)
norm_test <- data.frame(NULL)

for (col in colnames(prs)[-1]) {
  opt <- describe(prs[[col]])
  print(opt)
  norm_test <-
    rbind(norm_test,
    c(opt$mean, opt$sd,
      opt$median, opt$skew,
      opt$kurtosis, opt$se))
}
rownames(norm_test) <- colnames(prs)[-1]
colnames(norm_test) <- c("Mean", "SD", "Median", "Skew", "Kurtosis", "SE")
str(norm_test)
#fwrite(norm_test, glue("{Path}/1_produced_files/cass_BHRC_PRSValuesNormTest.txt") ,row.names = TRUE, col.names = TRUE, sep = " ")

# Make Box-plots with the data
library(dplyr)
prs %>%
  select(ADHD, ANX, ASD, BD, CD, HEIGHT, MDD, OCD, PTSD, SCZ, TS) %>%
  pivot_longer(
    cols = c(ADHD:TS),
    names_to = "pheno",
    values_to = "prs"
  ) %>%
  ggplot(aes(prs, pheno, fill = pheno)) +
    geom_boxplot(
      outlier.color = "red",
      outlier.size = 3,
      linewidth = 1,
      color = "black"
    ) +
    labs(
      title = "BHRC PRS values",
      subtitle = "PRS ajusted to sex, state, age (all waves), and first 20 PCs",
      caption = "05-03-2024_cass_BHRC_PRSValuesOutcomes.R"
    ) +
    theme_publish() +
    theme(
      text = element_text(family = font),
      plot.title = element_text(size = 30, face = "bold"),
      plot.subtitle = element_text(size = 25),
      plot.caption = element_text(size = 20, hjust = 0.5),
      legend.position = "none",
      axis.text.y = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(size = 20),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )

# Prep de data for work
tADHD <-
  inner_join(
    prs[, c("IID", "ADHD")],
    pheno[, c("IID", "dcanyhk", "wave")],
    by = "IID") %>%
    rename(PRS = ADHD, pheno = dcanyhk)

dADHD <-
  list(
    W0 = filter(tADHD, wave == "W0"),
    W1 = filter(tADHD, wave == "W1"),
    W2 = filter(tADHD, wave == "W2")
  )

tANX <-
  inner_join(
    prs[, c("IID", "ANX")],
    pheno[, c("IID", "dcanyanx_pgc", "wave")],
    by = "IID") %>%
    rename(PRS = ANX, pheno = dcanyanx_pgc)

dANX <-
  list(
    W0 = filter(tANX, wave == "W0"),
    W1 = filter(tANX, wave == "W1"),
    W2 = filter(tANX, wave == "W2")
  )

tASD <-
  inner_join(
    prs[, c("IID", "ASD")],
    pheno[, c("IID", "dcpdd", "wave")],
    by = "IID") %>%
    rename(PRS = ASD, pheno = dcpdd)

dASD <-
  list(
    W0 = filter(tASD, wave == "W0"),
    W1 = filter(tASD, wave == "W1"),
    W2 = filter(tASD, wave == "W2")
  )

tBD <-
  inner_join(
    prs[, c("IID", "BD")],
    pheno[, c("IID", "dcmania", "wave")],
    by = "IID") %>%
    rename(PRS = BD, pheno = dcmania)

dBD <-
  list(
    W0 = filter(tBD, wave == "W0"),
    W1 = filter(tBD, wave == "W1"),
    W2 = filter(tBD, wave == "W2")
  )

tCD <-
  inner_join(
    prs[, c("IID", "CD")],
    pheno[, c("IID", "dcany", "wave")],
    by = "IID") %>%
    rename(PRS = CD, pheno = dcany)

dCD <-
  list(
    W0 = filter(tCD, wave == "W0"),
    W1 = filter(tCD, wave == "W1"),
    W2 = filter(tCD, wave == "W2")
  )

# falta o EA

tMDD <-
  inner_join(
    prs[, c("IID", "MDD")],
    pheno[, c("IID", "dcmadep", "wave")],
    by = "IID") %>%
    rename(PRS = MDD, pheno = dcmadep)

dMDD <-
  list(
    W0 = filter(tMDD, wave == "W0"),
    W1 = filter(tMDD, wave == "W1"),
    W2 = filter(tMDD, wave == "W2")
  )

tOCD <-
  inner_join(
    prs[, c("IID", "OCD")],
    pheno[, c("IID", "dcocd", "wave")],
    by = "IID") %>%
    rename(PRS = OCD, pheno = dcocd)


dOCD <-
  list(
    W0 = filter(tOCD, wave == "W0"),
    W1 = filter(tOCD, wave == "W1"),
    W2 = filter(tOCD, wave == "W2")
  )

tPTSD <-
  inner_join(
    prs[, c("IID", "PTSD")],
    pheno[, c("IID", "dcptsd", "wave")],
    by = "IID") %>%
    rename(PRS = PTSD, pheno = dcptsd)

dPTSD <-
  list(
    W0 = filter(tPTSD, wave == "W0"),
    W1 = filter(tPTSD, wave == "W1"),
    W2 = filter(tPTSD, wave == "W2")
  )

tSCZ <-
  inner_join(
    prs[, c("IID", "SCZ")],
    pheno[, c("IID", "dcpsych", "wave")],
    by = "IID") %>%
    rename(PRS = SCZ, pheno = dcpsych)

dSCZ <-
  list(
    W0 = filter(tSCZ, wave == "W0"),
    W1 = filter(tSCZ, wave == "W1"),
    W2 = filter(tSCZ, wave == "W2")
  )

tTS <-
  inner_join(
    prs[, c("IID", "TS")],
    pheno[, c("IID", "dctic", "wave")],
    by = "IID") %>%
    rename(PRS = TS, pheno = dctic)

dTS <-
  list(
    W0 = filter(tTS, wave == "W0"),
    W1 = filter(tTS, wave == "W1"),
    W2 = filter(tTS, wave == "W2")
  )

#######################################################
### Which PRS had more association with the diagnosis?
### In which wave? Put in beta bar plots with CI 95%
#######################################################

# Get association with pearson R
# (force association - no causation)
cor(w0ADHD$ADHD, w0ADHD$dcanyhk)
cor(w1ADHD$ADHD, w1ADHD$dcanyhk)
cor(w2ADHD$ADHD, w2ADHD$dcanyhk)

# Test the association
# the other variables say that they might
# no need to add more variables. The PRS has
# already been corrected prior to this
w0 <- lm(w0ADHD$ADHD ~ w0ADHD$dcanyhk)
w1 <- lm(w1ADHD$ADHD ~ w1ADHD$dcanyhk)
w2 <- lm(w2ADHD$ADHD ~ w2ADHD$dcanyhk)

# Get betas
# (sense o the association)
coef(w0)[2]
coef(w1)[2]
coef(w2)[2]

# Get p-values
# (if we can trust the result)
summary(w0)$coefficients[2, 4]
summary(w1)$coefficients[2, 4]
summary(w2)$coefficients[2, 4]

tests_ADHD <-
  data.frame(
    assoc = c(
      cor(w0ADHD$ADHD, w0ADHD$dcanyhk),
      cor(w1ADHD$ADHD, w1ADHD$dcanyhk),
      cor(w2ADHD$ADHD, w2ADHD$dcanyhk)),
    betas = c(
      coef(w0)[2],
      coef(w1)[2],
      coef(w2)[2]),
    p_values = c(
      summary(w0)$coefficients[2, 4],
      summary(w1)$coefficients[2, 4],
      summary(w2)$coefficients[2, 4]))

rownames(tests_ADHD) <- c("W0", "W1", "W2")
head(tests_ADHD)
str(tests_ADHD)
# Get betas and p_values

str(dADHD)
# DF
get_test <- function (data) {
  dftest <- lm(data$PRS ~ data$pheno)
  test <-
  data.frame(
    assoc = cor(data$PRS, data$pheno),
    betas = coef(dftest)[2],
    p_values = summary(dftest)$coefficients[2, 4])
  return(test)
}

# LIST
rADHD <- do.call(rbind, lapply(dADHD, get_test))
rANX <- do.call(rbind, lapply(dANX, get_test))
rASD <- do.call(rbind, lapply(dASD, get_test))
rBD <- do.call(rbind, lapply(dBD, get_test))
rCD <- do.call(rbind, lapply(dCD, get_test))
rMDD <- do.call(rbind, lapply(dMDD, get_test))
rOCD <- do.call(rbind, lapply(dOCD, get_test))
rPTSD <- do.call(rbind, lapply(dPTSD, get_test))
rSCZ <- do.call(rbind, lapply(dSCZ, get_test))
rTS <- do.call(rbind, lapply(dTS, get_test))

betas <- rbind(
  data.frame(rADHD, code = "ADHD"),
  data.frame(rANX, code = "ANX"),
  data.frame(rASD, code = "ASD"),
  data.frame(rBD, code = "BD"),
  data.frame(rCD, code = "CD"),
  data.frame(rMDD, code = "MDD"),
  data.frame(rOCD, code = "OCD"),
  data.frame(rPTSD, code = "PTSD"),
  data.frame(rSCZ, code = "SCZ"),
  data.frame(rTS, code = "TS")
  )

finaldf <-
  cbind(betas, wave = rep(c("W0", "W1", "W2"))) %>%
  mutate(
    code = factor(code),
    wave = factor(wave)
  )
rownames(finaldf) <- NULL
head(finaldf)
# make bar plot with betas
finaldf %>%
  filter(wave == "W0") %>%
  ggplot(aes(code, betas)) +
    geom_col() +
    coord_flip()

finaldf %>%
  filter(wave == "W1") %>%
  ggplot(aes(code, betas)) +
    geom_col() +
    coord_flip()

finaldf %>%
  filter(wave == "W2") %>%
  ggplot(aes(code, betas)) +
    geom_col() +
    coord_flip()
