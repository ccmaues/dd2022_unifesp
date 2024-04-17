source("functions_to_source.R")

# dados fenotípicos dos pais associados com os IIDs
parents <-
readRDS(glue("{Path}/0_external_files/MINIeFLI.rds")) %>%
select(FID, IID, starts_with("iMINI"), respondent)
str(parents)

table(parents$respondent)

# Entender as variáveis na tabela de fenótipos
library(stringr)
dic <-
readxl::read_xlsx(glue("{Path}/0_external_files/BHRC_Dict_of_Variables_W0W1W2.xlsx")) %>%
rename(var = 1, type = 4) %>%
filter(str_detect(var, "imini") == TRUE) %>%
select(var, type)

# Juntar colunas e fazer novo resultado de fenótipo
BP <- select(parents, IID, iMINI_cur_man, iMINI_lif_man, iMINI_cur_any_mood)
MDD <- select(parents, IID, iMINI_cur_dep, iMINI_cur_rdep)
ANX <-
select(parents, IID, iMINI_lif_panic,
iMINI_cur_panic, iMINI_cur_agor,
iMINI_cur_social, iMINI_cur_gad,
iMINI_cur_any_anx)
DEP <-
select(parents, IID, iMINI_cur_alcdep,
iMINI_cur_alcabus, iMINI_cur_drugdep,
iMINI_cur_drugabus, iMINI_cur_any_subs)
SCZ <- select(parents, IID, iMINI_lif_psych, iMINI_cur_psych)
ADHD <- select(parents, IID, iMINI_child_adhd, iMINI_cur_adhd)

make_new_pheno <-
function(data) {
  new_col <- rowSums(data[, -1])
  case_when(
  new_col != 0 & !is.na(new_col) ~ 1,
  new_col == 0 ~ 0,
  TRUE ~ NA)
}
new_pheno <-
data.frame(BP$IID, BP = make_new_pheno(BP),
MDD = make_new_pheno(MDD), ANX = make_new_pheno(ANX),
DEP = make_new_pheno(DEP), SCZ = make_new_pheno(SCZ),
ADHD = make_new_pheno(ADHD))

saveRDS(glue("{Path}/objects_R/cass_BHRC_parents_phenotype.RDS"))
