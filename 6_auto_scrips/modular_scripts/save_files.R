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
data.table::fwrite(diff_test,
    glue("{opt_path}/{opt_name}_{var_names}_{name_tool}_diff_test.tsv"),
    quote = FALSE, sep = ",",
    row.names = TRUE, col.names = TRUE
)