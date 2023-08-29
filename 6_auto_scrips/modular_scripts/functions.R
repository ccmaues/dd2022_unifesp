## Functions
make_describe_df <- function(data) {
    opt <- data.frame(
        Tool = name_tool,
        N = data$n,
        SD = data$sd,
        Median = data$median,
        Min = data$min,
        Max = data$max,
        Range = data$range,
        Skew = data$skew,
        Kurtosis = data$kurtosis,
        SE = data$se
    )
    return(opt)
}

make_test_df <- function(data) {
    opt <- data.frame(
        Tool = name_tool,
        Statistic = data$statistic,
        P_value = data$p.value,
        Method = "Anderson-Darling Test"
    )
    return(opt)
}

make_qq <- function(data) {
    opt <- list()
    for (i in 1:length(data)) {
        df <- data[[i]]
        ptitle <- subset_names[[i]]
        opt[[ptitle]] <-
            ggplot(df, aes(sample = value)) +
            geom_qq() +
            labs(
                title = ptitle,
                x = "Theoretical Quantiles",
                y = "Observed Quantiles"
            )
    }
    return(opt)
}

make_plot <- function(data) {
    if (class(for_plot) == "list") {
        tnames <- names(for_plot)
        plot_list <- list()
        for (i in 1:length(for_plot)) {
            data <- for_plot[[i]]
            p <- ggplot(
                data,
                aes(x = PRS, fill = factor(gp), color = factor(gp))
            ) +
                geom_density(alpha = 0.6) +
                labs(
                    x = "PRS scale",
                    title = glue("PRS values distribution by age at {tnames[i]}")
                )
            plot_list[[i]] <- p
        }
        library(patchwork)
        plot <- wrap_plots(plot_list, ncol = 1)
    } else {
        opt <-
            ggplot(data, aes(x = PRS, fill = gp, color = gp)) +
            geom_density(alpha = 0.4) +
            labs(
                x = "PRS scale",
                title = glue("PRS values distribution at {vars}")
            )
        return(opt)
    }
}

make_ttest <- function(data) {
    opt <- t.test(data[[1]]$PRS, data[[2]]$PRS)
    data <- data.frame(
        T = opt$statistic,
        P_value = opt$p.value,
        Lower_conf_interval = opt$conf.int[1],
        Upper_conf_interval = opt$conf.int[2],
        Mean_x = opt$estimate[1],
        Mean_y = opt$estimate[2],
        Method = opt$method,
        Alternative = opt$alternative,
        Std_error = opt$stderr
    )
    return(opt)
}

make_wilcoxtest <- function(data) {
    opt <- wilcox.test(data[[1]]$PRS, data[[2]]$PRS)
    data <- data.frame(
        W = opt$statistic,
        P_value = opt$p.value,
        Method = opt$method,
        Alternative = opt$alternative
    )
    return(opt)
}

make_kruskalwallis <- function(data) {
    opt <- kruskal.test(PRS ~ factor, data = data)
    data <- data.frame(
        Kruskal_Wallis_chi_squared = opt$statistic,
        P_value = opt$p.value,
        Method = opt$method,
    )
    return(opt)
}

make_moodmedian <- function(data) { # need to fix this
    opt <- coin::med_test(PRS ~ factor, data = data)
    data <- data.frame(
        Kruskal_Wallis_chi_squared = opt$statistic,
        P_value = opt$p.value,
        Method = opt$method,
    )
    return(opt)
}
