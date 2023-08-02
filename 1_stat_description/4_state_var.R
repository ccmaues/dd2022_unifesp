## State
as.data.frame(table(vars$popID)) %>%
    rename(., "State" = Var1, "N" = Freq) %>%
    knitr::kable()
