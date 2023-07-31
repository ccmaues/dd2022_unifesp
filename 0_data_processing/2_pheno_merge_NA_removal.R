pacman::p_load(dplyr)

setwd("F:/")
pacman::p_load(dplyr, tidyr, glue)

ext_dir <- "F:/0_external_files"
obj_dir <- "F:/objects_R"

phenotype <- readRDS(glue("{obj_dir}/cass_BHRC_phenotype.RDS"))

separate_waves <- function(data, variable) {
    df <- select(data, IID, wave, {{ variable }}) %>%
        pivot_wider(names_from = wave, values_from = {{ variable }}) %>%
        select(IID, W0, W1, W2)
    df$W1[is.na(df$W1)] <- 0
    df$W2[is.na(df$W2)] <- 0
    df$sum <- rowSums(df[2:4])
    return(df)
}

adhd <- list(
    dcanyhk = separate_waves(phenotype, dcanyhk),
    dcadhdi = separate_waves(phenotype, dcadhdi),
    dcadhdh = separate_waves(phenotype, dcadhdh),
    dcadhdo = separate_waves(phenotype, dcadhdo),
    dcadhdc = separate_waves(phenotype, dcadhdc)
)

process_data <- function(df) {
    df_new <- data.frame(
        IID = character(),
        W0 = numeric(),
        W1 = numeric(),
        W2 = numeric(),
        stringsAsFactors = FALSE
    )

    ctrl <- filter(df, sum == 0)

    df_new <- bind_rows(
        df_new,
        data.frame(
            IID = ctrl$IID,
            W0 = 0,
            W1 = 0,
            W2 = 0
        )
    )

    if (nrow(filter(df, sum == 6)) > 0) {
        case1 <- filter(df, sum == 6) %>%
            filter(!IID %in% ctrl$IID)

        df_new <- bind_rows(
            df_new,
            data.frame(
                IID = case1$IID,
                W0 = 2,
                W1 = 2,
                W2 = 2
            )
        )
    }

    case2 <- filter(df, W0 == 2) %>%
        filter(!IID %in% df_new$IID)
    df_new <- bind_rows(
        df_new,
        data.frame(
            IID = case2$IID,
            W0 = 2,
            W1 = 2,
            W2 = 2
        )
    )

    case3 <- filter(df, W1 == 2) %>%
        filter(!IID %in% df_new$IID)
    df_new <- bind_rows(
        df_new,
        data.frame(
            IID = case3$IID,
            W0 = 0,
            W1 = 2,
            W2 = 2
        )
    )

    case4 <- filter(df, W2 == 2) %>%
        filter(!IID %in% df_new$IID)
    df_new <- bind_rows(
        df_new,
        data.frame(
            IID = case4$IID,
            W0 = 0,
            W1 = 0,
            W2 = 2
        )
    )

    return(df_new)
}

new_adhd <- list(
    dcanyhk = process_data(adhd$dcanyhk),
    dcadhdi = process_data(adhd$dcadhdi),
    dcadhdh = process_data(adhd$dcadhdh),
    dcadhdo = process_data(adhd$dcadhdo),
    dcadhdc = process_data(adhd$dcadhdc)
)

new_adhd <- lapply(new_adhd, function(df_column) {
  mutate(df_column,
    W0 = factor(case_when(W0 == "2" ~ "Case", W0 == "0" ~ "Control")),
    W1 = factor(case_when(W1 == "2" ~ "Case", W1 == "0" ~ "Control")),
    W2 = factor(case_when(W2 == "2" ~ "Case", W2 == "0" ~ "Control")))
})

saveRDS(new_adhd, glue("{obj_dir}/cass_BHRC_ADHD_mod_pheno.RDS"))
