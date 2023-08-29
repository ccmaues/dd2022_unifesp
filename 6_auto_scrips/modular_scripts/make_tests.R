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
                d <- psych::describe(data[[glue("{name}")]])
                # 3. Test normality
                t <- nortest::ad.test(data[[glue("{name}")]])
                description[[name]] <- make_describe_df(d)
                test[[name]] <- make_test_df(t)
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
            d <- psych::describe(data$PRS)
            # 3. Test normality
            t <- nortest::ad.test(data$PRS)
            description[[name]] <- make_describe_df(d)
            test[[name]] <- make_test_df(t)
        }
    }
    subset_dfs <- lapply(subset_dfs, function(df) {
        colnames(df) <- "value"
        return(df)
    })
    description_df <- do.call(rbind, description)
    test_df <- do.call(rbind, test)
} else { # Variable IS PRS
    # 2. Describe variables
    description_df <- make_describe_df(psych::describe(prs_values$PRS))
    # 3. Test normality
    test_df <- make_test_df(nortest::ad.test(prs_values$PRS))
}

## 4. Save files:
if (!any(grepl("PRS", vars))) {
plyr::join_all(result_list, by = "Var1", type = "inner") %>%
    rename(Type = Var1) %>%
    data.table::fwrite(.,
        glue("{opt_path}/{opt_name}_{var_names}_N.tsv"),
        quote = FALSE, sep = ",",
        row.names = FALSE, col.names = TRUE
    )
}
data.table::fwrite(description_df,
    glue("{opt_path}/{opt_name}_{var_names}_{name_tool}_describe.tsv"),
    quote = FALSE, sep = ",",
    row.names = TRUE, col.names = TRUE
)
data.table::fwrite(test_df,
    glue("{opt_path}/{opt_name}_{var_names}_{name_tool}_norm_test.tsv"),
    quote = FALSE, sep = ",",
    row.names = TRUE, col.names = TRUE
)