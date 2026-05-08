ImpulseDE2 Temporal Analysis
================
Zoe Dellaert
2026-05-08

- [ImpulseDE2 Temporal Analysis](#impulsede2-temporal-analysis)
  - [Background info](#background-info)
  - [0. Setup species-specific
    parameters](#0-setup-species-specific-parameters)
  - [1. Read in raw count data, vst-transformed counts, and
    metadata](#1-read-in-raw-count-data-vst-transformed-counts-and-metadata)
  - [2. Metadata formatting](#2-metadata-formatting)
  - [3. Then, run ImpulseDE2](#3-then-run-impulsede2)

# ImpulseDE2 Temporal Analysis

## Background info

Based on [this
paper](https://academic.oup.com/bib/article/20/1/288/4364840#130283262),
this is the best package to use other than comparing each time point
against each other individually.

Repo here: <https://github.com/YosefLab/ImpulseDE2>

To install the package

``` r
library(devtools)
install_github("YosefLab/ImpulseDE2")
```

Tutorial here:
<http://bioconductor.statistik.tu-dortmund.de/packages/3.11/bioc/vignettes/ImpulseDE2/inst/doc/ImpulseDE2_Tutorial.html>
, I followed closely with the section “Case-control differential
expression analysis”

Read the ImpulseDE2 paper
[here](https://academic.oup.com/nar/article/46/20/e119/5068248)

David S Fischer, Fabian J Theis, Nir Yosef, Impulse model-based
differential expression analysis of time course sequencing data, Nucleic
Acids Research, Volume 46, Issue 20, 16 November 2018, Page e119,
<https://doi.org/10.1093/nar/gky675>

``` r
# set up file paths so that Rmd outputs can be viewed using github markdown
knitr::opts_knit$set(base.dir = normalizePath(paste0("../../output_RNA/reports/", params$species, "/")), base.url = "./")

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE,fig.width = 10, fig.height = 8,
                      fig.path = "02_ImpulseDE_files/figure-gfm/")

#load packages
library(ImpulseDE2)
library(tidyverse)
library(ComplexHeatmap)
library(Mfuzz)

#load in parameters and functions
source("species_parameters.R")
source("utils.R")

sessionInfo() #provides list of loaded packages and version of R
```

    ## R version 4.5.1 (2025-06-13)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.1 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## time zone: Etc/UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] tcltk     grid      stats     graphics  grDevices utils     datasets 
    ## [8] methods   base     
    ## 
    ## other attached packages:
    ##  [1] Mfuzz_2.68.0          DynDoc_1.86.0         widgetTools_1.86.0   
    ##  [4] e1071_1.7-16          Biobase_2.70.0        BiocGenerics_0.56.0  
    ##  [7] generics_0.1.4        ComplexHeatmap_2.26.0 lubridate_1.9.4      
    ## [10] forcats_1.0.0         stringr_1.6.0         dplyr_1.1.4          
    ## [13] purrr_1.2.1           readr_2.1.6           tidyr_1.3.1          
    ## [16] tibble_3.3.0          ggplot2_4.0.1         tidyverse_2.0.0      
    ## [19] ImpulseDE2_0.99.10    rmarkdown_2.30       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1            farver_2.1.2               
    ##  [3] S7_0.2.1                    fastmap_1.2.0              
    ##  [5] digest_0.6.39               timechange_0.3.0           
    ##  [7] lifecycle_1.0.5             cluster_2.1.8.1            
    ##  [9] magrittr_2.0.4              compiler_4.5.1             
    ## [11] tkWidgets_1.86.0            rlang_1.1.7                
    ## [13] tools_4.5.1                 yaml_2.3.12                
    ## [15] knitr_1.50                  S4Arrays_1.10.0            
    ## [17] DelayedArray_0.36.0         RColorBrewer_1.1-3         
    ## [19] abind_1.4-8                 BiocParallel_1.44.0        
    ## [21] withr_3.0.2                 stats4_4.5.1               
    ## [23] colorspace_2.1-2            scales_1.4.0               
    ## [25] iterators_1.0.14            dichromat_2.0-0.1          
    ## [27] SummarizedExperiment_1.40.0 cli_3.6.5                  
    ## [29] crayon_1.5.3                rstudioapi_0.17.1          
    ## [31] tzdb_0.5.0                  rjson_0.2.23               
    ## [33] proxy_0.4-27                parallel_4.5.1             
    ## [35] XVector_0.50.0              matrixStats_1.5.0          
    ## [37] vctrs_0.7.0                 Matrix_1.6-4               
    ## [39] IRanges_2.44.0              GetoptLong_1.1.0           
    ## [41] hms_1.1.4                   S4Vectors_0.48.0           
    ## [43] clue_0.3-66                 locfit_1.5-9.12            
    ## [45] foreach_1.5.2               glue_1.8.0                 
    ## [47] codetools_0.2-20            cowplot_1.2.0              
    ## [49] stringi_1.8.7               shape_1.4.6.1              
    ## [51] gtable_0.3.6                GenomicRanges_1.62.0       
    ## [53] pillar_1.11.1               htmltools_0.5.9            
    ## [55] Seqinfo_1.0.0               circlize_0.4.17            
    ## [57] R6_2.6.1                    doParallel_1.0.17          
    ## [59] evaluate_1.0.5              lattice_0.22-7             
    ## [61] png_0.1-8                   class_7.3-23               
    ## [63] Rcpp_1.1.1                  SparseArray_1.10.2         
    ## [65] DESeq2_1.50.2               xfun_0.56                  
    ## [67] MatrixGenerics_1.22.0       pkgconfig_2.0.3            
    ## [69] GlobalOptions_0.1.3

## 0. Setup species-specific parameters

``` r
# get species
species <- params$species

# get parameters for this species
config <- get_params(species)
print_config(species)
```

    ## Species: Mcap
    ## Count matrix: MON_MCapV3_gene_count_matrix.csv
    ## Outliers: MON_R72_H1, MON_R72_H2
    ## WGCNA power: 10
    ## Mfuzz clusters: 6

``` r
# define preprocessing output directory (from 01_preprocessing.Rmd)
input_dir <- file.path("../../output_RNA/counts_filt_norm", species)

# set up necessary output directories if they don't exist
outdir <- file.path("../../output_RNA/ImpulseDE2", species)
outdir_mfuzz <- file.path(outdir,"Mfuzz")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
if (!dir.exists(outdir_mfuzz)) dir.create(outdir_mfuzz, recursive = TRUE)

reportdir <- file.path("../../output_RNA/reports", params$species, "02_ImpulseDE_files/figure-gfm/")
if (!dir.exists(reportdir)) dir.create(reportdir, recursive = TRUE)
```

## 1. Read in raw count data, vst-transformed counts, and metadata

``` r
# load in raw counts data
counts_raw <- read.csv(file.path("../../output_RNA/count_matrices", config$count_matrix), row.names = 1)

# load in vst-transformed counts
vst <- read.csv(file.path(input_dir, "vsd_expression_matrix.csv"))
vst <- vst %>% column_to_rownames(var = "X")

# load in metadata
meta <- read.csv(paste0("../../output_RNA/",species,"_RNA_seq_metadata.csv"))
meta <- meta %>% column_to_rownames(var = "X") #%>% select(-c(species, replicate))

# remove outliers that are still in metadata and raw_counts files but were removed prior to the vst transformation
outlier_samples <- config$outlier_samples

if(length(outlier_samples) > 0) {
    counts_raw <- counts_raw[, !colnames(counts_raw) %in% outlier_samples]
    meta <- meta[!rownames(meta) %in% outlier_samples, ]
}

all(rownames(meta) %in% colnames(vst))
```

    ## [1] TRUE

``` r
all(rownames(meta) == colnames(vst))
```

    ## [1] TRUE

## 2. Metadata formatting

First, reformat our metadata table to match the column names used in the
ImpulseDE2 vignette.

``` r
meta_impulse <- meta %>%
  dplyr::rename(Sample = sample, Time = time, Batch = replicate) %>% 
  mutate(Time = as.numeric(as.character(Time)),
         Condition = str_replace(treatment, "C", "control"),
         Condition = str_replace(Condition, "H", "case")
         ) %>%
  select(-c(species,treatment))
```

## 3. Then, run ImpulseDE2

This takes a ton of time and memory, so I run it once then save as an
RDS.

``` r
objectImpulseDE2 <- runImpulseDE2(
  matCountData    = as.matrix(counts_raw), #or use filtered_counts 
  dfAnnotation    = meta_impulse,
  boolCaseCtrl    = TRUE,
  vecConfounders  = c("Batch"), #only use if you want to try to control for batch effects
  boolIdentifyTransients = TRUE, #use if you want to ID transiently- vs permanently-regulated genes
  scaNProc        = 18 )
```

    ## Processing Details:
    ## ImpulseDE2 runs in case-ctrl mode.
    ## Found time points: 0,1,3,12,24,72,120
    ## Case: Found the samples at time point 0: MON_R0_H1,MON_R0_H2,MON_R0_H3
    ## Case: Found the samples at time point 1: MON_R1_H1,MON_R1_H2,MON_R1_H3
    ## Case: Found the samples at time point 3: MON_R3_H1,MON_R3_H2,MON_R3_H3
    ## Case: Found the samples at time point 12: MON_R12_H1,MON_R12_H2,MON_R12_H3
    ## Case: Found the samples at time point 24: MON_R24_H1,MON_R24_H2,MON_R24_H3
    ## Case: Found the samples at time point 72: MON_R72_H3
    ## Case: Found the samples at time point 120: MON_R120_H1,MON_R120_H2,MON_R120_H3
    ## Control: Found the following samples at time point 0:MON_R0_C1,MON_R0_C2,MON_R0_C3
    ## Control: Found the following samples at time point 1:MON_R1_C1,MON_R1_C2,MON_R1_C3
    ## Control: Found the following samples at time point 3:MON_R3_C1,MON_R3_C2,MON_R3_C3
    ## Control: Found the following samples at time point 12:MON_R12_C1,MON_R12_C2,MON_R12_C3
    ## Control: Found the following samples at time point 24:MON_R24_C1,MON_R24_C2,MON_R24_C3
    ## Control: Found the following samples at time point 72:MON_R72_C1,MON_R72_C2,MON_R72_C3
    ## Control: Found the following samples at time point 120:MON_R120_C1,MON_R120_C2,MON_R120_C3
    ## Found the following samples for confounder Batch and batch C1: MON_R0_C1,MON_R1_C1,MON_R3_C1,MON_R12_C1,MON_R24_C1,MON_R72_C1,MON_R120_C1
    ## Found the following samples for confounder Batch and batch C2: MON_R0_C2,MON_R1_C2,MON_R3_C2,MON_R12_C2,MON_R24_C2,MON_R72_C2,MON_R120_C2
    ## Found the following samples for confounder Batch and batch C3: MON_R0_C3,MON_R1_C3,MON_R3_C3,MON_R12_C3,MON_R24_C3,MON_R72_C3,MON_R120_C3
    ## Found the following samples for confounder Batch and batch H1: MON_R0_H1,MON_R1_H1,MON_R3_H1,MON_R12_H1,MON_R24_H1,MON_R120_H1
    ## Found the following samples for confounder Batch and batch H2: MON_R0_H2,MON_R1_H2,MON_R3_H2,MON_R12_H2,MON_R24_H2,MON_R120_H2
    ## Found the following samples for confounder Batch and batch H3: MON_R0_H3,MON_R1_H3,MON_R3_H3,MON_R12_H3,MON_R24_H3,MON_R72_H3,MON_R120_H3
    ## Input contained 54384 genes/regions.
    ## WARNING: 7046 out of 54384 genes do not have obserserved non-zero counts and are excluded.
    ## Selected 47338 genes/regions for analysis.

    ## [1] "Corrected 281 DESEq2 dispersion estimates which to avoid variance overestimation and loss of discriminatory power for model selection."

``` r
saveRDS(objectImpulseDE2, file.path(outdir, "objectImpulseDE2.rds"))
```

<!-- ## 4. View and save results -->

<!-- ```{r} -->

<!-- objectImpulseDE2 <- readRDS(file.path(outdir, "objectImpulseDE2.rds")) -->

<!-- impulse_results <- objectImpulseDE2$dfImpulseDE2Results -->

<!-- impulse_results_annot <- impulse_results %>% left_join(SwissProt, by = join_by("Gene"=="query")) %>% filter(!is.na(Gene)) -->

<!-- write.table(impulse_results,file.path(outdir, "ImpulseDE2_Results.txt"),row.names=F,quote=F,sep="\t") -->

<!-- # Genes with significant treatment effect on temporal trajectory -->

<!-- impulse_sig_genes <- impulse_results %>% filter(padj < 0.05)  -->

<!-- #preview top DE genes  -->

<!-- impulse_sig_genes %>% arrange(padj) %>% head(5) %>% dplyr::select(!contains("converge")) -->

<!-- cat("\nTotal significant genes:", nrow(impulse_sig_genes), "\n") -->

<!-- cat("\nResponse patterns:\n") -->

<!-- cat("Transient:", sum(impulse_sig_genes$isTransient), "\n") -->

<!-- cat("Monotonous:", sum(impulse_sig_genes$isMonotonous), "\n") -->

<!-- cat("Complex:", sum(!impulse_sig_genes$isTransient & !impulse_sig_genes$isMonotonous), "\n") -->

<!-- ``` -->

<!-- ## 5. Heatmap of transient and non-transiently heat-affected genes -->

<!-- ```{r} -->

<!-- lsHeatmaps <- plotHeatmap( -->

<!--   objectImpulseDE2       = objectImpulseDE2, -->

<!--   strCondition           = "control", -->

<!--   boolIdentifyTransients = TRUE, #set to true if true above -->

<!--   scaQThres              = 0.05) -->

<!-- draw(lsHeatmaps$complexHeatmapRaw)  -->

<!-- draw(lsHeatmaps$complexHeatmapFit)  -->

<!-- lsHeatmaps <- plotHeatmap( -->

<!--   objectImpulseDE2       = objectImpulseDE2, -->

<!--   strCondition           = "combined", -->

<!--   boolIdentifyTransients = TRUE, #set to true if true above -->

<!--   scaQThres              = 0.05) -->

<!-- draw(lsHeatmaps$complexHeatmapRaw)  -->

<!-- draw(lsHeatmaps$complexHeatmapFit)  -->

<!-- lsHeatmaps <- plotHeatmap( -->

<!--   objectImpulseDE2       = objectImpulseDE2, -->

<!--   strCondition           = "case", -->

<!--   boolIdentifyTransients = TRUE, #set to true if true above -->

<!--   scaQThres              = 0.05) -->

<!-- draw(lsHeatmaps$complexHeatmapRaw)  -->

<!-- draw(lsHeatmaps$complexHeatmapFit)  -->

<!-- png(paste0(outdir,"/ImpulseDE/ImpulseDE2_heatmap.png"), width = 2000, height = 2400, res = 300) -->

<!-- draw(lsHeatmaps$complexHeatmapRaw) -->

<!-- dev.off() -->

<!-- png(paste0(outdir,"/ImpulseDE/ImpulseDE2_heatmap_fit.png"), width = 2000, height = 2400, res = 300) -->

<!-- draw(lsHeatmaps$complexHeatmapFit) -->

<!-- dev.off() -->

<!-- str(lsHeatmaps$lsvecGeneGroups) -->

<!-- ``` -->

<!-- ## 6. Plot trajectories of top impulseDE genes and specific genes of interest -->

<!-- ```{r} -->

<!-- # Plot top 10 differentially expressed (by q-value) genes -->

<!-- lsgplotsGenes <- plotGenes( -->

<!--   vecGeneIDs       = NULL, -->

<!--   scaNTopIDs       = 10, -->

<!--   objectImpulseDE2 = objectImpulseDE2, -->

<!--   boolSimplePlot = TRUE,   boolCaseCtrl     = TRUE, -->

<!--   dirOut           = paste0(outdir,"/ImpulseDE/"), -->

<!--   boolMultiplePlotsPerPage = FALSE, -->

<!--   strNameRefMethod = NULL) -->

<!-- lsgplotsGenes -->

<!-- ``` -->

<!-- ```{r} -->

<!-- # HIF_genes -->

<!-- hif_genes <- SwissProt %>% filter(grepl("HIF-1-",ProteinNames))%>% pull(query) -->

<!-- impulse_results %>% filter(Gene %in% hif_genes) %>% arrange(padj) %>% left_join(SwissProt, by = join_by(Gene==query))  -->

<!-- HIFalphabeta <- plotGenes( -->

<!--   vecGeneIDs       = hif_genes, -->

<!--   objectImpulseDE2 = objectImpulseDE2, -->

<!--   boolSimplePlot = TRUE,   boolCaseCtrl     = TRUE, -->

<!--   dirOut           = paste0(outdir,"/ImpulseDE/"), -->

<!--   strFileName = "HIF-alphabeta.pdf", -->

<!--   boolMultiplePlotsPerPage = FALSE, -->

<!--   strNameRefMethod = NULL) -->

<!-- HIFalphabeta -->

<!-- # Majerova 2021 key qPCR genes -->

<!-- majerova_genes <- HeatStressGenes %>% filter(ref_first_author =="Majerova") -->

<!-- stress_genes_ids <- unique(majerova_genes$query) -->

<!-- plot_stress_genes <- stress_genes_ids[stress_genes_ids %in% rownames(objectImpulseDE2@matCountDataProc)]  -->

<!-- impulse_results %>% filter(Gene %in% stress_genes_ids) %>% arrange(padj) %>% left_join(HeatStressGenes_unique, by = join_by(Gene==query)) -->

<!-- heatgenes <- plotGenes( -->

<!--   vecGeneIDs       = plot_stress_genes, -->

<!--   objectImpulseDE2 = objectImpulseDE2, -->

<!--   boolSimplePlot = TRUE,   boolCaseCtrl     = TRUE, -->

<!--   dirOut           = paste0(outdir,"/ImpulseDE/"), -->

<!--   strFileName = "stress_genes_Majerova.pdf", -->

<!--   boolMultiplePlotsPerPage = FALSE, -->

<!--   strNameRefMethod = NULL) -->

<!-- heatgenes -->

<!-- # HSP genes -->

<!-- HSPS <- impulse_results %>% filter(Gene %in% stress_genes_ids) %>% arrange(padj) %>% left_join(HeatStressGenes_unique, by = join_by(Gene==query)) %>% filter(grepl("HSP",gene_id)) %>% pull(Gene) -->

<!-- HSPs <- plotGenes( -->

<!--   vecGeneIDs       = HSPS, -->

<!--   objectImpulseDE2 = objectImpulseDE2, -->

<!--   boolSimplePlot = TRUE, -->

<!--   boolCaseCtrl     = TRUE, -->

<!--   dirOut           = paste0(outdir,"/ImpulseDE/"), -->

<!--   strFileName = "HSPs.pdf", -->

<!--   boolMultiplePlotsPerPage = FALSE, -->

<!--   strNameRefMethod = NULL) -->

<!-- HSPs -->

<!-- ``` -->

<!-- ## 7. Heatmap of top 500 impulseDE genes -->

<!-- ```{r} -->

<!-- top_500_DE_genes <- impulse_results %>% arrange(padj) %>% head(500) %>% rownames() -->

<!-- pheatmap(assay(vsd)[top_500_DE_genes, ], cluster_rows=TRUE, show_rownames=FALSE, -->

<!--          cluster_cols=FALSE, -->

<!--          annotation_col= meta[,c("treatment","time")], -->

<!--          annotation_colors = list("treatment" = treat_colors, -->

<!--                                   "time" = time_colors)) -->

<!-- pheatmap(assay(vsd)[top_500_DE_genes, ], cluster_rows=TRUE, show_rownames=FALSE, -->

<!--          cluster_cols=TRUE, cutree_cols = 2, -->

<!--          annotation_col= meta[,c("treatment","time")], -->

<!--          annotation_colors = list("treatment" = treat_colors, -->

<!--                                   "time" = time_colors)) -->

<!-- ``` -->

<!-- ## 7.5. Heatmap of membrane channel genes -->

<!-- ```{r} -->

<!-- membrane_DE_genes <- impulse_results %>% -->

<!--   right_join(membrane_channels, join_by("Gene"=="query")) %>%  -->

<!--   filter(padj < 0.05) %>%  -->

<!--   column_to_rownames("Gene") -->

<!-- plot_df <- as.data.frame(t(vsd_mat)) %>% -->

<!--   rownames_to_column(var="sample") %>% -->

<!--   left_join(meta, by=c("sample"="sample")) %>%  -->

<!--   pivot_longer(cols = all_of(rownames(vsd_mat)), names_to="query", values_to="expression") %>% -->

<!--   mutate(is_DE = query %in% rownames(DE_05)) %>% right_join(membrane_DE_genes %>% rownames_to_column("query")) -->

<!-- membrane_list <- c("aquaporin","TRP",  "Mechanosensory" ,"calcium",  "ER_calcium" ,"Golgi_calcium" , "SLC24", "SLC25", "PMCA",  "Sodium_calcium_exchanger","Bhattacharya2016") -->

<!-- pdf(paste0(outdir,"/ImpulseDE/membrane_sig_plots.pdf"), width = 8, height = 10) -->

<!-- pheatmap(assay(vsd)[rownames(membrane_DE_genes), ], cluster_rows=TRUE, show_rownames=TRUE, -->

<!--          cluster_cols=FALSE, -->

<!--          fontsize = 5, -->

<!--          labels_row=membrane_DE_genes$short_name, -->

<!--          annotation_row = membrane_DE_genes %>% dplyr::select(gene_set), -->

<!--          annotation_col= meta[,c("treatment","time")], -->

<!--          annotation_colors = list("treatment" = treat_colors, -->

<!--                                   "time" = time_colors)) -->

<!-- pheatmap(assay(vsd)[rownames(membrane_DE_genes), ], cluster_rows=TRUE, show_rownames=TRUE, -->

<!--          cluster_cols=TRUE,cutree_cols = 3, -->

<!--          fontsize = 5, -->

<!--          labels_row=membrane_DE_genes$short_name, -->

<!--          annotation_row = membrane_DE_genes %>% dplyr::select(gene_set), -->

<!--          annotation_col= meta[,c("treatment","time")], -->

<!--          annotation_colors = list("treatment" = treat_colors, -->

<!--                                   "time" = time_colors)) -->

<!-- for (genelist in unique(plot_df$gene_set)){ -->

<!--   plot_df_filtered <- plot_df %>% filter(grepl(genelist,gene_set))# %>% filter(is_DE == TRUE) -->

<!--   if (nrow(plot_df_filtered)>1){ -->

<!--     plot <- plot_df_filtered  %>% ggplot(aes(x=time, y=expression, color=treatment, group=treatment)) + -->

<!--     stat_summary(fun="mean", geom="line") + -->

<!--     scale_color_manual(values = treat_colors) + -->

<!--     stat_summary(fun.data=mean_se, geom="errorbar", width=0.2) + -->

<!--     facet_wrap(short_name~query,scales="free_y") + -->

<!--     theme_bw() + -->

<!--     theme(strip.text = element_text(size = 6)) + -->

<!--     labs(y="VST expression", x="Timepoint",title=paste(genelist)) -->

<!--     print(plot) -->

<!--   } -->

<!-- } -->

<!-- dev.off() -->

<!-- ``` -->

<!-- ## 8. Cluster ImpulseDE2-significant genes by trajectory -->

<!-- For this we will use the package [Mfuzz](https://bioconductor.org/packages/release/bioc/html/Mfuzz.html), [vignette here](https://bioconductor.org/packages/release/bioc/vignettes/Mfuzz/inst/doc/Mfuzz.pdf). -->

<!-- ```{r} -->

<!-- #BiocManager::install("Mfuzz") -->

<!-- library(Mfuzz) -->

<!-- ``` -->

<!-- ```{r} -->

<!-- # analyze which of our ImpulseDE2 significant genes are in our vsd matrix -->

<!-- sum(impulse_sig_genes$Gene %in% rownames(vsd_mat)) -->

<!-- length(impulse_sig_genes$Gene) -->

<!-- # which ones are missing?  -->

<!-- missing_genes <- impulse_sig_genes$Gene[!(impulse_sig_genes$Gene %in% rownames(vsd_mat))] -->

<!-- # 8 are missing and it is because they were filtered out during pOverA filtering -- as seen with rowSums below, each has fewer than 3 samples with a count >10 -->

<!-- counts_raw[missing_genes,] -->

<!-- impulse_sig_genes[missing_genes,] -->

<!-- rowSums(counts_raw[missing_genes,] > 10) -->

<!-- ``` -->

<!-- ```{r} -->

<!-- impulse_sig_genes_transient <- impulse_sig_genes %>% filter(isTransient==TRUE) -->

<!-- impulse_sig_genes_mat <- vsd_mat[rownames(vsd_mat) %in% impulse_sig_genes_transient$Gene,] -->

<!-- heat <- impulse_sig_genes_mat %>% as.data.frame %>% select(contains("_H")) -->

<!-- # average values together across replicates -->

<!-- heat_avg <- heat %>% -->

<!--   rowwise() %>% -->

<!--   mutate( -->

<!--     R0 = mean(c_across(starts_with("POC_R0"))), -->

<!--     R1 = mean(c_across(starts_with("POC_R1"))), -->

<!--     R3 = mean(c_across(starts_with("POC_R3"))), -->

<!--     R12 = mean(c_across(starts_with("POC_R12"))), -->

<!--     R24 = mean(c_across(starts_with("POC_R24"))), -->

<!--     R72 = mean(c_across(starts_with("POC_R72"))), -->

<!--     R120 = mean(c_across(starts_with("POC_R120"))) -->

<!--   ) %>% -->

<!--   select(R0, R1, R3, R12, R24, R72, R120) -->

<!-- rownames(heat_avg) <- rownames(heat) -->

<!-- heat_eset <- ExpressionSet(assayData = as.matrix(heat_avg)) -->

<!-- heat_eset <- standardise(heat_eset)  -->

<!-- ``` -->

<!-- For fuzzy c-means clustering, the fuzzifier m and the number of clusters c has to be chosen in advance -->

<!-- ```{r} -->

<!-- # Determine fuzzifier -->

<!-- m <- mestimate(heat_eset) -->

<!-- # Choose optimal cluster number -->

<!-- #Dmin(heat_eset, m = m, repeats = 3) -->

<!-- optimal_c <- 6 -->

<!-- # Run Mfuzz clustering -->

<!-- mfuzz_clusters <- mfuzz(heat_eset, c = optimal_c, m = m) -->

<!-- mfuzz.plot(heat_eset, cl = mfuzz_clusters, new.window =FALSE, mfrow = c(3,3), time.labels =  c(0,1,3,12,24,72,120)) -->

<!-- # Visualize clusters -->

<!-- pdf(paste0(outdir,"/temporal_clusters.pdf"), width = 12, height = 10) -->

<!-- mfuzz.plot2(heat_eset, cl = mfuzz_clusters, mfrow = c(3, 4),  -->

<!--             time.labels = c("0", "1", "3", "12", "24", "72", "120"), -->

<!--             xlab = "Time (hours)",x11=FALSE) -->

<!-- dev.off() -->

<!-- ``` -->

<!-- ```{r} -->

<!-- cluster2 <- plotGenes( -->

<!--   vecGeneIDs       = head(names(mfuzz_clusters$cluster)[mfuzz_clusters$cluster==2],5), -->

<!--   objectImpulseDE2 = objectImpulseDE2, -->

<!--   boolSimplePlot = TRUE, -->

<!--   boolCaseCtrl     = TRUE, -->

<!--   dirOut           = paste0(outdir,"/ImpulseDE/"), -->

<!--   strFileName = "cluster2.pdf", -->

<!--   boolMultiplePlotsPerPage = FALSE, -->

<!--   strNameRefMethod = NULL) -->

<!-- cluster2 -->

<!-- ``` -->

<!-- ```{r} -->

<!-- cluster3 <- plotGenes( -->

<!--   vecGeneIDs       = head(names(mfuzz_clusters$cluster)[mfuzz_clusters$cluster==3],5), -->

<!--   objectImpulseDE2 = objectImpulseDE2, -->

<!--   boolSimplePlot = TRUE, -->

<!--   boolCaseCtrl     = TRUE, -->

<!--   dirOut           = paste0(outdir,"/ImpulseDE/"), -->

<!--   strFileName = "cluster3.pdf", -->

<!--   boolMultiplePlotsPerPage = FALSE, -->

<!--   strNameRefMethod = NULL) -->

<!-- cluster3 -->

<!-- ``` -->

<!-- ```{r} -->

<!-- # Extract cluster assignments -->

<!-- cluster_assignments <- data.frame( -->

<!--   gene = names(mfuzz_clusters$cluster), -->

<!--   cluster = mfuzz_clusters$cluster, -->

<!--   membership = apply(mfuzz_clusters$membership, 1, max) -->

<!-- ) -->

<!-- # Get cluster centers (average trajectory) -->

<!-- cluster_centers <- mfuzz_clusters$centers -->

<!-- # Identify peak timepoint for each cluster -->

<!-- peak_times <- apply(cluster_centers, 1, which.max) -->

<!-- timepoints <- c(0, 1, 3, 12, 24, 72, 120) -->

<!-- cluster_peaks <- data.frame( -->

<!--   cluster = 1:optimal_c, -->

<!--   peak_time = timepoints[peak_times]) -->

<!-- print(cluster_peaks) -->

<!-- ``` -->

<!-- ### View clustered genes of interest -->

<!-- ```{r} -->

<!-- HSPS <- impulse_results %>% filter(Gene %in% stress_genes_ids) %>% arrange(padj) %>% left_join(HeatStressGenes_unique, by = join_by(Gene==query)) %>% filter(grepl("HSP",gene_id)) %>% pull(Gene) -->

<!-- cluster_assignments %>% filter(gene %in% HSPS) -->

<!-- ``` -->

<!-- ```{r} -->

<!-- heat_clustered <- HeatStressGenes_unique %>% left_join(cluster_assignments, by = join_by(query==gene)) %>% -->

<!--   arrange(cluster, desc(cluster)) -->

<!-- # plot this to show which genes are in which cluster -->

<!-- heat_clustered %>% filter(!is.na(cluster)) %>% ggplot(aes(y=reorder(gene_id, cluster), x=factor(cluster), fill=cluster)) + -->

<!--   geom_tile() + -->

<!--   theme_bw() + -->

<!--   labs(x="Mfuzz Cluster", y="Gene ID", title="Heat stress genes clustered by temporal expression pattern") -->

<!-- ``` -->

<!-- ```{r} -->

<!-- channel_clustered <- membrane_channels  %>% unique() %>%  -->

<!--   left_join(cluster_assignments, by = join_by(query==gene)) %>% -->

<!--   arrange(cluster)  %>% -->

<!--   filter(!is.na(cluster))  -->

<!-- cat("Total membrane channel genes:", nrow(membrane_channels), "\n") -->

<!-- cat("Found in ImpulseDE/Mfuzz data:", nrow(channel_clustered), "\n") -->

<!-- channel_clustered %>% -->

<!--     ggplot(aes(x = factor(cluster), y = reorder(short_name, cluster), fill = gene_set)) + -->

<!--   geom_tile(color = "white") + -->

<!--   geom_text(aes(label = cluster), size = 2) + -->

<!--   theme_bw() + -->

<!--   theme(axis.text.y = element_text(size = 6)) + -->

<!--   labs(x = "Mfuzz Cluster",  -->

<!--        y = "Gene ID",  -->

<!--        fill = "Channel Type", -->

<!--        title = "Membrane Channel Genes - Cluster Assignment", -->

<!--        subtitle = paste0(sum(!is.na(channel_clustered$cluster)),  -->

<!--                         " genes significantly DE")) -->

<!-- heat_eset_membrane <- heat_eset[channel_clustered$query, ] -->

<!-- mfuzz_clusters_membrane <- mfuzz_clusters -->

<!-- mfuzz_clusters_membrane$membership <- mfuzz_clusters$membership[channel_clustered$query, ] -->

<!-- mfuzz_clusters_membrane$cluster <- mfuzz_clusters$cluster[channel_clustered$query] -->

<!-- mfuzz.plot( -->

<!--   heat_eset_membrane, -->

<!--   cl = mfuzz_clusters_membrane, -->

<!--   new.window = FALSE, -->

<!--   mfrow = c(3,3), -->

<!--   time.labels = c(0,1,3,12,24,72,120) -->

<!-- ) -->

<!-- # Visualize clusters -->

<!-- pdf(paste0(outdir,"/membrane_temporal_clusters.pdf"), width = 12, height = 10) -->

<!-- mfuzz.plot2(heat_eset_membrane, cl = mfuzz_clusters_membrane, mfrow = c(3, 4),  -->

<!--             time.labels = c("0", "1", "3", "12", "24", "72", "120"), -->

<!--             xlab = "Time (hours)",x11=FALSE) -->

<!-- dev.off() -->

<!-- plot_df <- as.data.frame(t(vsd_mat)) %>% -->

<!--   rownames_to_column(var="sample") %>% -->

<!--   left_join(meta, by=c("sample"="sample")) %>%  -->

<!--   pivot_longer(cols = all_of(rownames(vsd_mat)), names_to="query", values_to="expression") %>% -->

<!--   mutate(is_DE = query %in% rownames(DE_05)) %>% right_join(membrane_DE_genes %>% rownames_to_column("query")) -->

<!-- pdf(paste0(outdir,"/ImpulseDE/membrane_sig_plots_clustered.pdf"), width = 8, height = 10) -->

<!-- for (cl in c(0,unique(channel_clustered$cluster))){ -->

<!--   plot_df_filtered <- plot_df %>% left_join(channel_clustered) %>% filter(!is.na(cluster)) -->

<!--   if (cl ==0){ -->

<!--     plot_df_low_membership <- plot_df_filtered %>% filter(membership<0.5) -->

<!--     plot <- plot_df_low_membership  %>% ggplot(aes(x=time, y=expression, color=treatment, group=treatment)) + -->

<!--     stat_summary(fun="mean", geom="line") + -->

<!--     scale_color_manual(values = treat_colors) + -->

<!--     stat_summary(fun.data=mean_se, geom="errorbar", width=0.2) + -->

<!--     facet_wrap(short_name~query,scales="free_y") + -->

<!--     theme_bw() + -->

<!--     theme(strip.text = element_text(size = 6)) + -->

<!--     labs(y="VST expression", x="Timepoint",title=paste("Membership < 0.5")) -->

<!--     print(plot) -->

<!--   } else{ -->

<!--     plot_df_cluster <- plot_df_filtered %>% filter(cluster==cl) -->

<!--       if (nrow(plot_df_cluster)>1){ -->

<!--     plot <- plot_df_cluster  %>% ggplot(aes(x=time, y=expression, color=treatment, group=treatment)) + -->

<!--     stat_summary(fun="mean", geom="line") + -->

<!--     scale_color_manual(values = treat_colors) + -->

<!--     stat_summary(fun.data=mean_se, geom="errorbar", width=0.2) + -->

<!--     facet_wrap(short_name~query,scales="free_y") + -->

<!--     theme_bw() + -->

<!--     theme(strip.text = element_text(size = 6)) + -->

<!--     labs(y="VST expression", x="Timepoint",title=paste("Cluster ",cl)) -->

<!--     print(plot) -->

<!--   } -->

<!--   } -->

<!-- } -->

<!-- dev.off() -->

<!-- ``` -->

<!-- ```{r} -->

<!-- sessionInfo() -->

<!-- ``` -->
