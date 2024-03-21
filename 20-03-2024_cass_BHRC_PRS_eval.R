source("functions_to_source.R")

## tentar colocar em paralelo
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

    # verificar se tá mesmo colocando os dados de W1 aqui
    # pq não parece
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

    # poderia juntar os AUC no mesmo DF do R2
    ## Calculate Precision-Recall Curves and AUC
    # tirando do loop aqui tá fudendo o meu resultado
    pr <- c()
    pr_curve_df <- lapply(processing, function(data) {
      t <- pr.curve(
        scores.class0 = data$PRS,
        weights.class0 = ifelse(data$diagnosis == 0, 0, 1),
        curve = TRUE,
        sorted = FALSE
        )
      data.frame(Precision = t$curve[, 2], Recall = t$curve[, 1], AUPRC = t$auc.integral)
    })
    names(pr_curve_df) <- c("W0", "W1", "W2")
    p2 <- rbind(
      data.frame(
      Recall = pr_curve_df$W0$Recall,
      Precision = pr_curve_df$W0$Precision,
      wave = "W0"),
      data.frame(
      Recall = pr_curve_df$W1$Recall,
      Precision = pr_curve_df$W1$Precision,
      wave = "W1"),
      data.frame(
      Recall = pr_curve_df$W2$Recall,
      Precision = pr_curve_df$W2$Precision,
      wave = "W2")
      ) %>%
      ggplot(aes(x = Precision, y = Recall, color = wave)) +
        geom_line() +
        geom_point() +
        labs(x = "Recall", y = "Precision", title = "Precision-Recall Curve") +
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

    ## Calculate R2
    R2 <- do.call(bind_rows, lapply(processing, function(df){
      PseudoR2(glm(PRS ~ diagnosis, data = df), which = "Nagelkerke") * 100
    })) %>%
    as.data.frame() %>%
    bind_cols(
      data.frame(
        c(
        roc_results$W0$auc*100,
        roc_results$W1$auc*100,
        roc_results$W2$auc*100
        ),
        c(
        unique(pr_curve_df$W0$AUPRC*100),
        unique(pr_curve_df$W1$AUPRC*100),
        unique(pr_curve_df$W2$AUPRC*100)
        ))
      )
    colnames(R2) <- c("Nagelkerke", "AUROC", "AUPRC")
    rownames(R2) <- c("W0", "W1", "W2")
  list(p1, p2, R2)
}

opt <- evaluate_PRS("ADHD", "dcanyhk")
