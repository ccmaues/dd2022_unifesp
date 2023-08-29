## Verify file creations

if (file.exists(glue("{opt_path}/{opt_name}_{var_names}_N.tsv"))) {
    print("Variable(s) N sample written!")
} else {
    print("Problem saving file with N sample.")
}
if (file.exists(
    glue("{opt_path}/{opt_name}_{var_names}_{name_tool}_describe.tsv")
)) {
    print("Description written!")
} else {
    print("Problem saving file with variable description.")
}
if (file.exists(
    glue("{opt_path}/{opt_name}_{var_names}_{name_tool}_norm_test.tsv")
)) {
    print("Normal distribution test DONE!")
} else {
    print("Problem saving file with distribution test.")
}
if (file.exists(
    glue("{opt_path}/{opt_name}_{name_tool}_norm_test.tsv")
)) {
    print("Normal distribution test DONE!")
} else {
    print("Problem saving file with distribution test for PRS.")
}
if (file.exists(
    glue("{opt_path}/{opt_name}_{var_names}_QQplot.png")
)) {
    print("QQ plot saved!")
} else {
    print("Problem saving file the QQ plot.")
}
if (file.exists(
    glue("{opt_path}/{opt_name}_{var_names}_Denplot.png")
)) {
    print("Density plot saved!")
} else {
    print("Problem saving file the density plot.")
}
