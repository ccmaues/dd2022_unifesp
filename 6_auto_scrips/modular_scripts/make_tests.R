if (!any(grepl("PRS", vars))) { # If given vars
# 1. Write N
    result_list <- lapply(vars, function(col_name) {
        as.data.frame(table(for_use[[col_name]])) %>%
            rename(!!col_name := Freq)
    })
    description <- list() # make pick up?
    test <- list()
    num_test <- select_if(for_use, is.numeric)
    if (ncol(num_test) != 0) { # If given vars are numeric
        subset_dfs <- list()
        for (i in 1:ncol(for_use)) {
            for_sep <- data.frame(for_use[[i]])
            col_name <- colnames(for_use)[i]
            colnames(for_sep) <- col_name
            subset_dfs[[col_name]] <- for_sep
            subset_names <- names(subset_dfs)
            for (name in subset_names) {
                data <- subset_dfs[[name]]
                # 2. Describe variables
                description[[name]] <- make_describe_df(
                    psych::describe(data[[glue("{name}")]])
                    )
                # 3. Test normality
                test[[name]] <- make_test_df(
                    nortest::ad.test(data[[glue("{name}")]])
                    )
            }
        }
    } else { # if NOT numeric variables
        norm_test <- select(readRDS(pheno), IID, all_of(vars)) %>%
            inner_join(., prs_values, by = "IID") %>%
            select(., all_of(vars), PRS)
        subset_dfs <- list()
        for (i in 1:ncol(norm_test)) { # MUST be 1:col or bugs
            if (names(norm_test)[i] != "PRS") {
                col_name <- names(norm_test)[i]
                fcolumn <- levels(norm_test[[i]])
                for_sep <- data.frame(norm_test[[i]], norm_test[["PRS"]])
                colnames(for_sep) <- c(col_name, "PRS")
                for (level in fcolumn) {
                    subset_data <- for_sep %>%
                        filter(!!sym(col_name) == level)
                    new_df_name <- paste(col_name, level, sep = "_")
                    subset_dfs[[new_df_name]] <- subset_data %>%
                        select(PRS)
                }
            }
        }
        subset_names <- names(subset_dfs)
        for (name in subset_names) {
            data <- subset_dfs[[name]]
            # 2. Describe variables
            description[[name]] <- make_describe_df(psych::describe(data$PRS))
            # 3. Test normality
            test[[name]] <- make_test_df(nortest::ad.test(data$PRS))
        }
    }
    subset_dfs <- lapply(subset_dfs, function(df) {
        colnames(df) <- "value"
        return(df)
    })
    description_df <- do.call(rbind, description)
    test_df <- do.call(rbind, test)
    for_ttest <- select(readRDS(pheno), all_of(vars), IID) %>%
        inner_join(prs_values, for_use, by = "IID") %>%
        select(-IID) %>%
        rename(factor = vars) %>%
        group_split(factor)
    if(length(vars) == 2) { # make T-test
        if(description_df$P_value > 0.05) { # normal
            diff_test <- make_ttest(for_ttest)
        } else { # not normal
            diff_test <- make_wilcoxtest(for_ttest)
        }
    } else { # make ANOVA
        for_ttest <- select(readRDS(pheno), all_of(vars), IID) %>%
            inner_join(prs_values, for_use, by = "IID") %>%
            select(-IID) %>%
            rename(factor = vars)
        if (description_df$P_value > 0.05) { # normal
            diff_test <- make_kruskalwallis(for_ttest) # see if the test is correct
        } else { # not normal
            diff_test <- make_moodmedian(for_ttest) # see if the test is correct
        }
    }
} else { # Variable IS PRS
    # 2. Describe variables
    description_df <- make_describe_df(psych::describe(prs_values$PRS))
    # 3. Test normality
    test_df <- make_test_df(nortest::ad.test(prs_values$PRS))
}