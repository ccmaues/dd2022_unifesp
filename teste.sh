#!/usr/bin/bash

set -e # Exit immediately if a command exits with a non-zero status
#set -x # printa linhas

function usage() { 
    echo "
    TomeiVergonhaNaCara v1.0 (13-11-2022)
    Maues, C.C. - ccmaues@gmail.com
    PGT - Psychiatric Genetics Team UNIFESP

-----------------------------------------------------------------------------------

    Uso: $0 <argumentos> [parâmetros]

        -i <nome_arquivo> : prefixos dos aquivos BED, BIM e FAM [OBRIGATORIO]
        -o <nome_arquivo> : nome dos arquivos output [OBRIGATORIO]
        -q [0 | choi | normal]: fazer controle de qualidade [OBRIGATORIO]
        -d : remover SNPs duplicados [OPCIONAL]
        
    Exemplo: $0 -q choi -d -i input -o output 

            - Fazer controle de qualidade do CHOI 
            - Retirar os SNPs duplicados
            - Input de nome "input"
            - Saída de "output"
        " 1>&2; exit 1; 
    }

[[ $# -eq 0  ]] && usage

while getopts dhi:o:q: flag
do
    case "${flag}" in
        i) input=${OPTARG}
        [[ ! -z $input ]] && echo "[$0] Arquivo de INPUT: $input"
        ;;
        o) opt=${OPTARG}
        [[ ! -z $opt ]] && echo "[$0] Arquivo de OUTPUT: $opt"
        ;;
        q) qc=${OPTARG}
        [[ $qc == 0 ]] && echo "[$0] Sem controle de qualidade"
        [[ $qc != 0 ]] && echo "[$0] Controle de qualidade: $qc"
        ;;
        d) dup=${OPTARG}
        [[ -z $dup ]] && echo "[$0] Remover SNPs duplicados" && remdp="yes"

        ;;
        h | *) usage ;;
    esac
done

function fazer_NORMAL() {
    plink --bfile "$input" --maf 0.01 --hwe 0.000001 --snps-only just-acgt \
    --write-snplist --out qc1_"$output"
    plink --bfile "$input" --geno 0.01 --mind 0.01 \
    --extract qc1_"$output".snplist --make-bed --out "$output"
}

function fazer_CHOI() {
    plink --bfile $input --maf 0.01 --hwe 0.000001 --snps-only just-acgt \
    --write-snplist --out qc1_"$output"
    plink --bfile $input --geno 0.01 --mind 0.01 --extract qc1_"$output".snplist \
    --write-snplist --make-just-fam --out qc2_"$output"
    plink --bfile $input --keep qc2_"$output".fam --extract qc2_"$output".snplist \
    --indep-pairwise 100 50 0.8 --out qc3_"$output"
    plink --bfile $input --keep qc2_"$output".fam --extract qc3_"$output".prune.in \
    --het --out qc4_"$output"
    if [[ $(ls | grep het.R) == "het.R" ]]
    then
        Rscript het.R
    else
        echo '
        install.packages("data.table") | !require(data.table)
        library(data.table)
        # Read in file
        dat <- fread("qc4_"$output".het")
        # Get samples with F coefficient within 3 SD of the population mean
        valid <- dat[F<=mean(F)+3*sd(F) & F>=mean(F)-3*sd(F)]
        # print FID and IID for valid samples
        fwrite(valid[,c("FID","IID")], "qc4_"$output".valid.sample", sep="\t")
        q()' | sed 's/\t//g' > het.R
        Rscript het.R
    fi
    plink --bfile $input --keep qc4_"$output".valid.sample --extract qc3_"$output".prune.in \
    --rel-cutoff 0.125 --out qc5_"$output"
    plink --bfile $input --keep qc5_"$output".rel.id --extract qc3_"$output".prune.in \
    --make-bed --out "$output"
}

if [[ $qc != 0 ]]
then
    if [[ "$qc" == "normal" ]]
    then
        echo "### Fazendo controle de qualidade normal..."
        #fazer_NORMAL
    elif [[ "$qc" == "choi" ]]
    then
        echo "### Fazendo controle de qualidade do Choi..."
        #fazer_CHOI
    fi
fi

# Retirar duplicados
function rem_DUP(){
    plink --bfile "$output" --list-duplicate-vars suppress-first --out dup_"$output"
    plink --bfile "$output" --exclude dup_"$output".dupvar --make-bed --out nodup_"$output"
}

if [[ $remdp == "yes" ]]
then
    echo "### Removendo SNPs duplicados"
    rem_DUP
fi

function universal_ID(){
    awk '{print $1,$1":"$4":"$5":"$6,$3,$4,$5,$6}' nodup_"$output".bim | sed 's/ /\t/g' > nodup_"$output".bim.mod
    rm nodup_"$output".bim && mv nodup_"$output".bim.mod nodup_"$output".bim
}

universal_ID $input