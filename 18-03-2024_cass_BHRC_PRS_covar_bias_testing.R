source("functions_to_source.R")

## Does the PRS have association with...
test_bias <- function(prs_column) {
  opt <- NULL
  df <-
    select(prs, IID, {{prs_column}}) %>%
    rename(PRS = {{prs_column}})

  ## State?
  ## Mann-Whitney U test/Wilcox test
  for_test <- inner_join(df, state, by = "IID")
  state_test <- wilcox.test(PRS ~ popID, data = for_test)
  opt <-
  data.frame(
    "Statistics" = state_test$statistic,
    "P_value" = state_test$p.value
  )

  ## Sex?
  ## Mann-Whitney U test
  for_test <- inner_join(df, sex, by = "IID")
  sex_test <- wilcox.test(PRS ~ sex, data = for_test)
  opt <-
  rbind(opt,
  data.frame(
    "Statistics" = sex_test$statistic,
    "P_value" = sex_test$p.value
  ))

  ## Age?
  ## Kendall test
  for_test <- inner_join(df, ages, by = "IID")
  agW0_test <- cor.test(for_test$PRS, for_test$age_W0, method = "kendall")
  agW1_test <- cor.test(for_test$PRS, for_test$age_W1, method = "kendall")
  agW2_test <- cor.test(for_test$PRS, for_test$age_W2, method = "kendall")
  opt <-
  rbind(opt,
  data.frame(
    "Statistics" = agW0_test$statistic,
    "P_value" = agW0_test$p.value
  ))
  opt <-
  rbind(opt,
  data.frame(
    "Statistics" = agW1_test$statistic,
    "P_value" = agW1_test$p.value
  ))
  opt <-
  rbind(opt,
  data.frame(
    "Statistics" = agW2_test$statistic,
    "P_value" = agW2_test$p.value
  ))
  rownames(opt) <-
    c(
      "Wilcox test state",
      "Wilcox test sex",
      "Kendall test W0",
      "Kendall test W1",
      "Kendall test W2"
    )
  return(opt)
}

tests_output <-
  list(
    AD = test_bias(AD),
    ADHD = test_bias(ADHD),
    AN = test_bias(AN),
    ANX = test_bias(ANX),
    ASD = test_bias(ASD),
    BD = test_bias(BD),
    CD = test_bias(CD),
    EA = test_bias(EA),
    HEIGHT = test_bias(HEIGHT),
    MDD = test_bias(MDD),
    OCD = test_bias(OCD),
    PTSD = test_bias(PTSD),
    SA = test_bias(SA),
    SCZ = test_bias(SCZ),
    SD = test_bias(SD),
    SWB = test_bias(SWB),
    TS = test_bias(TS)
  )
str(tests_output)

lapply(tests_output, function(data) {
  if(any(data$P_value < 0.05)) {
    print("Deu merda")
  }
})
