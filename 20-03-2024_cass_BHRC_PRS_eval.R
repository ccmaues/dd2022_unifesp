source("functions_to_source.R")

## Load necessary libraries
# nsROC
library(nsROC)
# R2
library(DescTools)

evaluate_PRS <-
  function(prs_column, pheno_code) {
    ## Main data
    data <-
      prs %>%
      select(IID, {{prs_column}}) %>%
      inner_join(., select(pheno, IID, wave, {{pheno_code}}), by = "IID") %>%
      rename(PRS = 2, diagnosis = 4) %>%
      select(-IID)
    processing <- group_split(data, wave)

    ## Calculate ROC
    roc_results <- lapply(processing, function(data) {
      gROC(data$PRS, data$diagnosis, pvac.auc = TRUE, side = "auto")
    })
    names(roc_results) <- c("W0", "W1", "W2")
    ggthemr("fresh")
    p1 <- rbind(
      data.frame(
        FPR = roc_results$W0$points.coordinates[, "FPR"],
        TPR = roc_results$W0$points.coordinates[, "TPR"],
        wave = "W0"),
      data.frame(
        FPR = roc_results$W1$points.coordinates[, "FPR"],
        TPR = roc_results$W1$points.coordinates[, "TPR"],
        wave = "W1"),
      data.frame(
        FPR = roc_results$W2$points.coordinates[, "FPR"],
        TPR = roc_results$W2$points.coordinates[, "TPR"],
        wave = "W2")
    ) %>%
    ggplot(aes(x = FPR, y = TPR)) +
    geom_ribbon(
      aes(
        ymin = FPR,
        ymax = TPR,
        fill = wave)
    ) +
    geom_line(
      linewidth = 1
      ) +
    geom_abline(
      linewidth = 1,
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "#000000",
      alpha = 0.5
    ) +
    coord_equal() +
    labs(
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    labs(
      title = glue("ADHD - N = {nrow(data)/3}"),
      subtitle = glue("
      W0 = {format(roc_results$W0$auc*100, digits = 4)}% W1 = {format(roc_results$W1$auc*100, digits = 4)}% W2 = {format(roc_results$W2$auc*100, digits = 4)}%
      "),
      caption = "20-03-2024_cass_BHRC_PRS_eval.R"
    ) +
    theme_publish() +
    theme(
      text = element_text(family = font),
      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 20),
      plot.title = element_text(size = 30),
      plot.subtitle = element_text(size = 25),
      plot.caption = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_blank()
    )
    print(p1)
    ## Calculate R2
    R2 <- as.data.frame(do.call(bind_rows, lapply(processing, function(df){
      PseudoR2(glm(PRS ~ diagnosis, data = df), which = "Nagelkerke") * 100
    }))) %>%
    bind_cols(
      data.frame(
        c(roc_results$W0$auc*100,
        roc_results$W1$auc*100,
        roc_results$W2$auc*100)
      )
    )
    rownames(R2) <- c("W0", "W1", "W2")
    colnames(R2) <- c("Nagelkerke", "AUROC")
    print(R2)
    # poderia juntar os AUC no mesmo DF do R2
}
evaluate_PRS("ADHD", "dcanyhk")
