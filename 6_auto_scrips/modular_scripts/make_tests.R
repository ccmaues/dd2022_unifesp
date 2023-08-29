if (vars != NULL) { # If given vars
    result_list <- lapply(vars, function(col_name) {
        opt <- as.data.frame(table(for_use[[col_name]])) %>%
            rename(!!col_name := Freq)
        opt
    })
    plyr::join_all(result_list, by = "Var1", type = "inner") %>%
        rename(Type = Var1) %>%
        data.table::fwrite(.,
            glue("{opt_path}/{opt_name}_{var_names}_N.tsv"),
            quote = FALSE, sep = ",",
            row.names = FALSE, col.names = TRUE
        )
    description <- list()
    test <- list()
    num_test <- select_if(for_use, is.numeric)
    if (ncol(num_test) != 0) {
        subset_dfs <- list()
        for (i in 1:ncol(for_use)) {
            for_sep <- data.frame(for_use[[i]])
            col_name <- colnames(for_use)[i]
            colnames(for_sep) <- col_name
            subset_dfs[[col_name]] <- for_sep
            subset_names <- names(subset_dfs)
            for (name in subset_names) {
                data <- subset_dfs[[name]]
                d <- psych::describe(data[[glue("{name}")]])
                t <- nortest::ad.test(data[[glue("{name}")]])
                description[[name]] <- make_describe_df(d)
                test[[name]] <- make_test_df(t)
            }
        }
    } else {
        norm_test <- select(variables, IID, all_of(vars)) %>%
            inner_join(., prs_values, by = "IID") %>%
            select(., all_of(vars), PRS)
        subset_dfs <- list()
        for (i in 1:ncol(norm_test)) {
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
            d <- psych::describe(data$PRS)
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
} else {
    # make tests for PRS alone
}

## Ideas: put the code blocks into another R sourceble script do it wont be as much long as it is
## Change the location of functions in the script
## Re-shape almost ALL of it
