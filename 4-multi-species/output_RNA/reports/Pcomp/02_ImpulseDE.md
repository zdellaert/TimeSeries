ImpulseDE2 Temporal Analysis
================
Zoe Dellaert
2026-05-11

- [Bulk RNA-seq Time Course Trajectory Analysis and
  Clustering](#bulk-rna-seq-time-course-trajectory-analysis-and-clustering)
  - [Introduction to packages](#introduction-to-packages)
    - [ImpulseDE2](#impulsede2)
    - [Mfuzz: clustering of temporal
      trajectories](#mfuzz-clustering-of-temporal-trajectories)
- [Setup](#setup)
  - [1. Load packages and functions](#1-load-packages-and-functions)
  - [2. Setup species-specific
    parameters](#2-setup-species-specific-parameters)
  - [3. Load in raw counts, transformed counts, metadata, and
    annotations](#3-load-in-raw-counts-transformed-counts-metadata-and-annotations)
- [ImpulseDE2 Analysis](#impulsede2-analysis)
  - [1. Metadata formatting](#1-metadata-formatting)
  - [1. Run ImpulseDE2](#1-run-impulsede2)
  - [3. Extract ImpulseDE2 results](#3-extract-impulsede2-results)
    - [All genes](#all-genes)
    - [Significant genes](#significant-genes)
    - [Quick summary](#quick-summary)
  - [4. Visualize ImpulseDE2 Results](#4-visualize-impulsede2-results)
    - [Heatmap of significant genes](#heatmap-of-significant-genes)
    - [Top gene trajectories](#top-gene-trajectories)
- [Mfuzz: Cluster ImpulseDE2-significant genes by
  trajectory](#mfuzz-cluster-impulsede2-significant-genes-by-trajectory)
  - [1. Preparing expression data](#1-preparing-expression-data)
  - [2. Determine Mfuzz parameters](#2-determine-mfuzz-parameters)
  - [3. Run MFuzz](#3-run-mfuzz)
  - [4. Characterize Clusters](#4-characterize-clusters)
    - [Visualize all significant genes in their
      clusters](#visualize-all-significant-genes-in-their-clusters)
- [Exploring genes of interest](#exploring-genes-of-interest)
  - [Manually-curated heat stress genes by
    cluster](#manually-curated-heat-stress-genes-by-cluster)

# Bulk RNA-seq Time Course Trajectory Analysis and Clustering

## Introduction to packages

### ImpulseDE2

- Based on [this
  paper](https://academic.oup.com/bib/article/20/1/288/4364840#130283262),
  this is the best package to use other than comparing each time point
  against each other individually.
- Repo here: <https://github.com/YosefLab/ImpulseDE2>
- Tutorial here:
  <http://bioconductor.statistik.tu-dortmund.de/packages/3.11/bioc/vignettes/ImpulseDE2/inst/doc/ImpulseDE2_Tutorial.html>
  , I followed closely with the section “Case-control differential
  expression analysis”
- Read the ImpulseDE2 paper
  [here](https://academic.oup.com/nar/article/46/20/e119/5068248)

*David S Fischer, Fabian J Theis, Nir Yosef, Impulse model-based
differential expression analysis of time course sequencing data, Nucleic
Acids Research, Volume 46, Issue 20, 16 November 2018, Page e119,
<https://doi.org/10.1093/nar/gky675>*

To install the package

``` r
library(devtools)
install_github("YosefLab/ImpulseDE2")
```

### Mfuzz: clustering of temporal trajectories

For this we will use the package
[Mfuzz](https://bioconductor.org/packages/release/bioc/html/Mfuzz.html),
[vignette
here](https://bioconductor.org/packages/release/bioc/vignettes/Mfuzz/inst/doc/Mfuzz.pdf)
and more documentation [here](http://mfuzz.sysbiolab.eu/).

To install the package

``` r
BiocManager::install("Mfuzz")
```

------------------------------------------------------------------------

# Setup

## 1. Load packages and functions

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
    ##  [1] tcltk     grid      stats4    stats     graphics  grDevices utils    
    ##  [8] datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] WGCNA_1.73                  fastcluster_1.3.0          
    ##  [3] dynamicTreeCut_1.63-1       Mfuzz_2.68.0               
    ##  [5] DynDoc_1.86.0               widgetTools_1.86.0         
    ##  [7] e1071_1.7-16                ComplexHeatmap_2.26.0      
    ##  [9] ImpulseDE2_0.99.10          BiocParallel_1.44.0        
    ## [11] ggnewscale_0.5.2            genefilter_1.90.0          
    ## [13] RColorBrewer_1.1-3          pheatmap_1.0.13            
    ## [15] DESeq2_1.50.2               SummarizedExperiment_1.40.0
    ## [17] Biobase_2.70.0              MatrixGenerics_1.22.0      
    ## [19] matrixStats_1.5.0           GenomicRanges_1.62.0       
    ## [21] Seqinfo_1.0.0               IRanges_2.44.0             
    ## [23] S4Vectors_0.48.0            BiocGenerics_0.56.0        
    ## [25] generics_0.1.4              lubridate_1.9.4            
    ## [27] forcats_1.0.0               stringr_1.6.0              
    ## [29] dplyr_1.1.4                 purrr_1.2.1                
    ## [31] readr_2.1.6                 tidyr_1.3.1                
    ## [33] tibble_3.3.0                ggplot2_4.0.1              
    ## [35] tidyverse_2.0.0             rmarkdown_2.30             
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] rstudioapi_0.17.1       jsonlite_2.0.0          shape_1.4.6.1          
    ##   [4] magrittr_2.0.4          magick_2.9.0            farver_2.1.2           
    ##   [7] GlobalOptions_0.1.3     ragg_1.5.0              vctrs_0.7.0            
    ##  [10] memoise_2.0.1           Cairo_1.7-0             base64enc_0.1-3        
    ##  [13] htmltools_0.5.9         S4Arrays_1.10.0         SparseArray_1.10.2     
    ##  [16] Formula_1.2-5           htmlwidgets_1.6.4       impute_1.84.0          
    ##  [19] cachem_1.1.0            lifecycle_1.0.5         iterators_1.0.14       
    ##  [22] pkgconfig_2.0.3         Matrix_1.6-4            R6_2.6.1               
    ##  [25] fastmap_1.2.0           GenomeInfoDbData_1.2.14 clue_0.3-66            
    ##  [28] digest_0.6.39           colorspace_2.1-2        AnnotationDbi_1.72.0   
    ##  [31] textshaping_1.0.4       Hmisc_5.2-5             RSQLite_2.4.5          
    ##  [34] labeling_0.4.3          timechange_0.3.0        httr_1.4.7             
    ##  [37] abind_1.4-8             compiler_4.5.1          proxy_0.4-27           
    ##  [40] bit64_4.6.0-1           withr_3.0.2             doParallel_1.0.17      
    ##  [43] backports_1.5.0         htmlTable_2.4.3         S7_0.2.1               
    ##  [46] DBI_1.2.3               tkWidgets_1.86.0        DelayedArray_0.36.0    
    ##  [49] rjson_0.2.23            tools_4.5.1             foreign_0.8-90         
    ##  [52] nnet_7.3-20             glue_1.8.0              checkmate_2.3.3        
    ##  [55] cluster_2.1.8.1         gtable_0.3.6            tzdb_0.5.0             
    ##  [58] preprocessCore_1.72.0   class_7.3-23            data.table_1.18.0      
    ##  [61] hms_1.1.4               XVector_0.50.0          foreach_1.5.2          
    ##  [64] pillar_1.11.1           limma_3.64.3            vroom_1.6.7            
    ##  [67] circlize_0.4.17         splines_4.5.1           lattice_0.22-7         
    ##  [70] survival_3.8-3          bit_4.6.0               annotate_1.86.1        
    ##  [73] tidyselect_1.2.1        GO.db_3.22.0            locfit_1.5-9.12        
    ##  [76] Biostrings_2.78.0       knitr_1.50              gridExtra_2.3          
    ##  [79] xfun_0.56               statmod_1.5.1           stringi_1.8.7          
    ##  [82] UCSC.utils_1.4.0        yaml_2.3.12             evaluate_1.0.5         
    ##  [85] codetools_0.2-20        cli_3.6.5               rpart_4.1.24           
    ##  [88] xtable_1.8-4            systemfonts_1.3.1       dichromat_2.0-0.1      
    ##  [91] Rcpp_1.1.1              GenomeInfoDb_1.44.3     png_0.1-8              
    ##  [94] XML_3.99-0.18           parallel_4.5.1          blob_1.2.4             
    ##  [97] scales_1.4.0            crayon_1.5.3            GetoptLong_1.1.0       
    ## [100] rlang_1.2.0             cowplot_1.2.0           KEGGREST_1.50.0

## 2. Setup species-specific parameters

``` r
# get species
species <- params$species

# get parameters for this species
config <- get_params(species)
print_config(species)
```

    ## Species: Pcomp
    ## Count matrix: POR_Pcomp_gene_count_matrix.csv
    ## Outliers: POR_R72_H1, POR_R72_H2, POR_R24_H1
    ## WGCNA power: 28
    ## Mfuzz clusters: 6

``` r
# define preprocessing output directory (from 01_preprocessing.Rmd)
input_dir <- file.path("../../output_RNA/counts_filt_norm", species)

# set up necessary output directories if they don't exist
outdir <- file.path("../../output_RNA/ImpulseDE2", species)
outdir_mfuzz <- file.path(outdir,"Mfuzz")
outdir_plots <- file.path(outdir,"plots")
outdir_plots_pdf <- file.path(outdir_plots,"pdf_figs")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
if (!dir.exists(outdir_mfuzz)) dir.create(outdir_mfuzz, recursive = TRUE)
if (!dir.exists(outdir_plots)) dir.create(outdir_plots, recursive = TRUE)
if (!dir.exists(outdir_plots_pdf)) dir.create(outdir_plots_pdf, recursive = TRUE)

reportdir <- file.path("../../output_RNA/reports", params$species, "02_ImpulseDE_files/figure-gfm/")
if (!dir.exists(reportdir)) dir.create(reportdir, recursive = TRUE)
```

## 3. Load in raw counts, transformed counts, metadata, and annotations

``` r
# load in raw counts data
counts_raw <- read.csv(file.path("../../output_RNA/count_matrices", config$count_matrix), row.names = 1)

# load in filtered counts data
filtered_counts <- read.csv(file.path(input_dir, "filtered_counts.csv"), row.names = 1)

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

``` r
# read in SwissProt annotation
SwissProt <- read.delim(file.path(annot_dir,config$SwissProt))
cat("Annotations:", nrow(SwissProt), "Swissprot-annotated genes")
```

    ## Annotations: 22929 Swissprot-annotated genes

------------------------------------------------------------------------

# ImpulseDE2 Analysis

## 1. Metadata formatting

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

## 1. Run ImpulseDE2

This takes a ton of time and memory, so I run it once then save as an
RDS.

``` r
if(params$run_ImpulseDE2 == TRUE) {
  objectImpulseDE2 <- runImpulseDE2(
    matCountData    = as.matrix(filtered_counts), #or use filtered_counts  
    dfAnnotation    = meta_impulse,
    boolCaseCtrl    = TRUE,
    vecConfounders  = c("Batch"), #only use if you want to try to control for batch effects
    boolIdentifyTransients = TRUE, #use if you want to ID transiently- vs permanently-regulated genes
    scaNProc        = 18 )
  
  saveRDS(objectImpulseDE2, file.path(outdir, "objectImpulseDE2.rds"))
} else {
  objectImpulseDE2 <- readRDS(file.path(outdir, "objectImpulseDE2.rds"))
}
```

    ## Processing Details:
    ## ImpulseDE2 runs in case-ctrl mode.
    ## Found time points: 0,1,3,12,24,72,120
    ## Case: Found the samples at time point 0: POR_R0_H1,POR_R0_H2,POR_R0_H3
    ## Case: Found the samples at time point 1: POR_R1_H1,POR_R1_H2,POR_R1_H3
    ## Case: Found the samples at time point 3: POR_R3_H1,POR_R3_H2,POR_R3_H3
    ## Case: Found the samples at time point 12: POR_R12_H1,POR_R12_H2,POR_R12_H3
    ## Case: Found the samples at time point 24: POR_R24_H2,POR_R24_H3
    ## Case: Found the samples at time point 72: POR_R72_H3
    ## Case: Found the samples at time point 120: POR_R120_H1,POR_R120_H2,POR_R120_H3
    ## Control: Found the following samples at time point 0:POR_R0_C1,POR_R0_C2,POR_R0_C3
    ## Control: Found the following samples at time point 1:POR_R1_C1,POR_R1_C2,POR_R1_C3
    ## Control: Found the following samples at time point 3:POR_R3_C1,POR_R3_C2,POR_R3_C3
    ## Control: Found the following samples at time point 12:POR_R12_C1,POR_R12_C2,POR_R12_C3
    ## Control: Found the following samples at time point 24:POR_R24_C1,POR_R24_C2,POR_R24_C3
    ## Control: Found the following samples at time point 72:POR_R72_C1,POR_R72_C2,POR_R72_C3
    ## Control: Found the following samples at time point 120:POR_R120_C1,POR_R120_C2,POR_R120_C3
    ## Found the following samples for confounder Batch and batch C1: POR_R0_C1,POR_R1_C1,POR_R3_C1,POR_R12_C1,POR_R24_C1,POR_R72_C1,POR_R120_C1
    ## Found the following samples for confounder Batch and batch C2: POR_R0_C2,POR_R1_C2,POR_R3_C2,POR_R12_C2,POR_R24_C2,POR_R72_C2,POR_R120_C2
    ## Found the following samples for confounder Batch and batch C3: POR_R0_C3,POR_R1_C3,POR_R3_C3,POR_R12_C3,POR_R24_C3,POR_R72_C3,POR_R120_C3
    ## Found the following samples for confounder Batch and batch H1: POR_R0_H1,POR_R1_H1,POR_R3_H1,POR_R12_H1,POR_R120_H1
    ## Found the following samples for confounder Batch and batch H2: POR_R0_H2,POR_R1_H2,POR_R3_H2,POR_R12_H2,POR_R24_H2,POR_R120_H2
    ## Found the following samples for confounder Batch and batch H3: POR_R0_H3,POR_R1_H3,POR_R3_H3,POR_R12_H3,POR_R24_H3,POR_R72_H3,POR_R120_H3
    ## Input contained 27492 genes/regions.
    ## Selected 27492 genes/regions for analysis.

    ## [1] "Corrected 200 DESEq2 dispersion estimates which to avoid variance overestimation and loss of discriminatory power for model selection."

``` r
# Print processing report
cat(objectImpulseDE2@strReport)
```

    ## ImpulseDE2 for count data, v0.99.10
    ## # Process inputProcessing Details:
    ## ImpulseDE2 runs in case-ctrl mode.
    ## Found time points: 0,1,3,12,24,72,120
    ## Case: Found the samples at time point 0: POR_R0_H1,POR_R0_H2,POR_R0_H3
    ## Case: Found the samples at time point 1: POR_R1_H1,POR_R1_H2,POR_R1_H3
    ## Case: Found the samples at time point 3: POR_R3_H1,POR_R3_H2,POR_R3_H3
    ## Case: Found the samples at time point 12: POR_R12_H1,POR_R12_H2,POR_R12_H3
    ## Case: Found the samples at time point 24: POR_R24_H2,POR_R24_H3
    ## Case: Found the samples at time point 72: POR_R72_H3
    ## Case: Found the samples at time point 120: POR_R120_H1,POR_R120_H2,POR_R120_H3
    ## Control: Found the following samples at time point 0:POR_R0_C1,POR_R0_C2,POR_R0_C3
    ## Control: Found the following samples at time point 1:POR_R1_C1,POR_R1_C2,POR_R1_C3
    ## Control: Found the following samples at time point 3:POR_R3_C1,POR_R3_C2,POR_R3_C3
    ## Control: Found the following samples at time point 12:POR_R12_C1,POR_R12_C2,POR_R12_C3
    ## Control: Found the following samples at time point 24:POR_R24_C1,POR_R24_C2,POR_R24_C3
    ## Control: Found the following samples at time point 72:POR_R72_C1,POR_R72_C2,POR_R72_C3
    ## Control: Found the following samples at time point 120:POR_R120_C1,POR_R120_C2,POR_R120_C3
    ## Found the following samples for confounder Batch and batch C1: POR_R0_C1,POR_R1_C1,POR_R3_C1,POR_R12_C1,POR_R24_C1,POR_R72_C1,POR_R120_C1
    ## Found the following samples for confounder Batch and batch C2: POR_R0_C2,POR_R1_C2,POR_R3_C2,POR_R12_C2,POR_R24_C2,POR_R72_C2,POR_R120_C2
    ## Found the following samples for confounder Batch and batch C3: POR_R0_C3,POR_R1_C3,POR_R3_C3,POR_R12_C3,POR_R24_C3,POR_R72_C3,POR_R120_C3
    ## Found the following samples for confounder Batch and batch H1: POR_R0_H1,POR_R1_H1,POR_R3_H1,POR_R12_H1,POR_R120_H1
    ## Found the following samples for confounder Batch and batch H2: POR_R0_H2,POR_R1_H2,POR_R3_H2,POR_R12_H2,POR_R24_H2,POR_R120_H2
    ## Found the following samples for confounder Batch and batch H3: POR_R0_H3,POR_R1_H3,POR_R3_H3,POR_R12_H3,POR_R24_H3,POR_R72_H3,POR_R120_H3
    ## Input contained 27492 genes/regions.
    ## Selected 27492 genes/regions for analysis.
    ## # Run DESeq2: Using dispersion factorscomputed by DESeq2.
    ## Consumed time: 1.3 min.
    ## # Compute size factors
    ## # Fitting null and alternative model to the genes
    ## Consumed time: 12.2 min.
    ## # Fitting sigmoid model to case condition
    ## Consumed time: 1.14 min.
    ## # Differentially expression analysis based on model fits
    ## Finished running ImpulseDE2.
    ## TOTAL consumed time: 14.8 min.

## 3. Extract ImpulseDE2 results

### All genes

Extract and save results for all non-zero genes

``` r
impulse_results <- objectImpulseDE2$dfImpulseDE2Results
impulse_results <- impulse_results %>% filter(allZero==FALSE) #remove genes with zero counts

impulse_results_annot <- impulse_results %>%
  left_join(SwissProt %>% select(query,ProteinNames,BiologicalProcess), by = join_by("Gene"=="query"))

write.csv(impulse_results, file.path(outdir, "ImpulseDE2_results.csv"), row.names = FALSE)
```

### Significant genes

Extract genes with significant treatment effect on temporal trajectory,
classify them as transiently or monotonously regulated, and save results

``` r
impulse_sig <- impulse_results %>%
  filter(padj < global_params$padj_threshold) %>%
  mutate(response_type = case_when(
    isTransient & !is.na(isTransient) ~ "Transient",
    isMonotonous & !is.na(isMonotonous) ~ "Monotonous",
    .default = "Other"
  ))

write.csv(impulse_sig, file.path(outdir, "ImpulseDE2_significant.csv"), row.names = FALSE)

#preview top DE genes and annotations
impulse_sig %>% arrange(padj) %>% head(20) %>% dplyr::select(Gene,padj,loglik_red,response_type) %>%
  left_join(SwissProt %>% select(query,ProteinNames,BiologicalProcess), by = join_by("Gene"=="query"))
```

    ##                                         Gene         padj loglik_red
    ## 1  Porites_compressa_HIv1___RNAseq.g40862.t1 1.196780e-51  -465.1336
    ## 2  Porites_compressa_HIv1___RNAseq.g11463.t1 3.629366e-46  -377.0231
    ## 3  Porites_compressa_HIv1___RNAseq.g19794.t1 1.285642e-39  -268.0140
    ## 4  Porites_compressa_HIv1___RNAseq.g23528.t1 4.352686e-35  -338.3881
    ## 5   Porites_compressa_HIv1___RNAseq.g5937.t1 3.202255e-32  -284.5357
    ## 6      Porites_compressa_HIv1___TS.g22192.t1 2.758836e-31  -295.1459
    ## 7  Porites_compressa_HIv1___RNAseq.g36355.t1 1.615223e-30  -356.1101
    ## 8  Porites_compressa_HIv1___RNAseq.g40324.t1 7.805573e-30  -366.0583
    ## 9  Porites_compressa_HIv1___RNAseq.g41296.t1 5.539682e-28  -264.8514
    ## 10   Porites_compressa_HIv1___RNAseq.g915.t1 2.461376e-27  -318.3641
    ## 11 Porites_compressa_HIv1___RNAseq.g40327.t1 4.192663e-27  -385.8910
    ## 12   Porites_compressa_HIv1___RNAseq.12682_t 2.864996e-26  -279.6759
    ## 13     Porites_compressa_HIv1___TS.g27105.t1 1.094858e-25  -274.2507
    ## 14 Porites_compressa_HIv1___RNAseq.g30626.t1 1.472416e-25  -312.8647
    ## 15 Porites_compressa_HIv1___RNAseq.g34309.t1 1.643385e-23  -267.5069
    ## 16 Porites_compressa_HIv1___RNAseq.g10466.t1 1.835991e-23  -365.6882
    ## 17 Porites_compressa_HIv1___RNAseq.g34311.t1 2.339750e-23  -264.1778
    ## 18  Porites_compressa_HIv1___RNAseq.g7115.t2 3.753693e-23  -288.0491
    ## 19 Porites_compressa_HIv1___RNAseq.g22671.t1 8.085354e-23  -300.1998
    ## 20 Porites_compressa_HIv1___RNAseq.g41254.t1 9.808238e-23  -292.6734
    ##    response_type
    ## 1     Monotonous
    ## 2      Transient
    ## 3     Monotonous
    ## 4     Monotonous
    ## 5     Monotonous
    ## 6     Monotonous
    ## 7      Transient
    ## 8      Transient
    ## 9     Monotonous
    ## 10    Monotonous
    ## 11     Transient
    ## 12    Monotonous
    ## 13    Monotonous
    ## 14    Monotonous
    ## 15    Monotonous
    ## 16    Monotonous
    ## 17    Monotonous
    ## 18     Transient
    ## 19    Monotonous
    ## 20     Transient
    ##                                                                                                                                                                                                                                                  ProteinNames
    ## 1                                                                                                                                                                                                 Dermatopontin (Tyrosine-rich acidic matrix protein) (TRAMP)
    ## 2                                                                                                                                                                            Serine/arginine-rich splicing factor 4 (Splicing factor, arginine/serine-rich 4)
    ## 3                                                                                                                                                                                                                                                        <NA>
    ## 4                                                                                                                                                                                                                          SID1 transmembrane family member 1
    ## 5                                                                                                                                                                                  Uncharacterized skeletal organic matrix protein 5 (Uncharacterized SOMP-5)
    ## 6                                                                                                                                                                                                                                                        <NA>
    ## 7                                                                                                                                                                         LON peptidase N-terminal domain and RING finger protein 3 (RING finger protein 127)
    ## 8                                                         Glycine-rich RNA-binding protein 4, mitochondrial (AtGR-RBP4) (AtRBG4) (Glycine-rich protein 4) (AtGRP4) (Mitochondrial RNA-binding protein 1b) (At-mRBP1b) (Small RNA binding protein 4) (AtSRBP4)
    ## 9                                                                                                                                      DELTA-actitoxin-Afr1a (DELTA-AITX-Afr1a) (Alpha-helical pore-forming toxin) (PFT) (Cytolysin) (Fragaceatoxin C) (fraC)
    ## 10                                                                                                                                                                                                                                                Calumenin-A
    ## 11 Glycine-rich RNA-binding protein 2, mitochondrial (AtGR-RBP2) (AtRBG2) (Glycine-rich protein 2) (AtGRP2) (Mitochondrial RNA-binding protein 1a) (At-mRBP1a) (Organelle RNA recognition motif-containing protein 5) (Small RNA binding protein 3) (AtSRBP3)
    ## 12                                                                                                                                                         Neuronal pentraxin-2 (NP2) (Neuronal activity-regulated pentraxin) (Neuronal pentraxin II) (NP-II)
    ## 13                                                                                                                                                                     Mu-theraphotoxin-Cg2a 3 (Mu-TRTX-Cg2a) (Jingzhaotoxin-IV) (JZTX-IV) (Peptide F1-23.73)
    ## 14                                                                                                                                           A disintegrin and metalloproteinase with thrombospondin motifs 6 (ADAM-TS 6) (ADAM-TS6) (ADAMTS-6) (EC 3.4.24.-)
    ## 15                                                                                                                                                                                                          Collagen triple helix repeat-containing protein 1
    ## 16                                                                                                                                                                                                                                        Myc protein (c-myc)
    ## 17                                                                                                                                                                                                          Collagen triple helix repeat-containing protein 1
    ## 18                                                                                                                                                                                                          Ribonucleoprotein PTB-binding 1 (Protein raver-1)
    ## 19                                                                                                                                                                                                                               ZP domain-containing protein
    ## 20                                                                                                               Serine/arginine-rich splicing factor 2 (Splicing component, 35 kDa) (Splicing factor SC35) (SC-35) (Splicing factor, arginine/serine-rich 2)
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  BiologicalProcess
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            cell adhesion [GO:0007155]; collagen fibril organization [GO:0030199]
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                          hematopoietic progenitor cell differentiation [GO:0002244]; mRNA processing [GO:0006397]; negative regulation of mRNA splicing, via spliceosome [GO:0048025]; RNA splicing [GO:0008380]
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       RNA transport [GO:0050658]
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
    ## 8                                                                                                                                                                                                                                                                                                      extracellular transport [GO:0006858]; miRNA transport [GO:1990428]; mitochondrial RNA modification [GO:1900864]; regulation of defense response to virus [GO:0050688]; response to cold [GO:0009409]; response to osmotic stress [GO:0006970]; response to salt stress [GO:0009651]; response to water deprivation [GO:0009414]; RNA transport [GO:0050658]
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         cytolysis in another organism [GO:0051715]; monoatomic cation transport [GO:0006812]; pore complex assembly [GO:0046931]
    ## 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
    ## 11                                                                                                                                                        cold acclimation [GO:0009631]; extracellular transport [GO:0006858]; miRNA transport [GO:1990428]; mitochondrial mRNA processing [GO:0090615]; negative regulation of termination of DNA-templated transcription [GO:0060567]; regulation of defense response to virus [GO:0050688]; response to cold [GO:0009409]; response to osmotic stress [GO:0006970]; response to salt stress [GO:0009651]; response to water deprivation [GO:0009414]; RNA transport [GO:0050658]; seed germination [GO:0009845]
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  associative learning [GO:0008306]; neurotransmitter receptor localization to postsynaptic specialization membrane [GO:0099645]
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                               aorta development [GO:0035904]; cardiac septum development [GO:0003279]; coronary vasculature development [GO:0060976]; extracellular matrix organization [GO:0030198]; kidney development [GO:0001822]; proteolysis [GO:0006508]
    ## 15 cell migration [GO:0016477]; cochlea morphogenesis [GO:0090103]; cyclooxygenase pathway [GO:0019371]; establishment of planar polarity involved in neural tube closure [GO:0090177]; inner ear receptor cell stereocilium organization [GO:0060122]; negative regulation of canonical Wnt signaling pathway [GO:0090090]; ossification involved in bone remodeling [GO:0043932]; osteoblast differentiation [GO:0001649]; osteoblast proliferation [GO:0033687]; positive regulation of osteoblast differentiation [GO:0045669]; positive regulation of osteoblast proliferation [GO:0033690]; Wnt signaling pathway, planar cell polarity pathway [GO:0060071]
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
    ## 17 cell migration [GO:0016477]; cochlea morphogenesis [GO:0090103]; cyclooxygenase pathway [GO:0019371]; establishment of planar polarity involved in neural tube closure [GO:0090177]; inner ear receptor cell stereocilium organization [GO:0060122]; negative regulation of canonical Wnt signaling pathway [GO:0090090]; ossification involved in bone remodeling [GO:0043932]; osteoblast differentiation [GO:0001649]; osteoblast proliferation [GO:0033687]; positive regulation of osteoblast differentiation [GO:0045669]; positive regulation of osteoblast proliferation [GO:0033690]; Wnt signaling pathway, planar cell polarity pathway [GO:0060071]
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
    ## 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         mRNA processing [GO:0006397]; RNA splicing [GO:0008380]

### Quick summary

    ## Total significant genes: 6233

    ## Response patterns:

    ## Transient: 1534

    ## Monotonous: 2243

    ## Other: 2456

## 4. Visualize ImpulseDE2 Results

### Heatmap of significant genes

``` r
lsHeatmaps <- plotHeatmap(
  objectImpulseDE2       = objectImpulseDE2,
  strCondition           = "case",
  boolIdentifyTransients = TRUE, #set to true if true above
  scaQThres              = global_params$padj_threshold)

# complexHeatmapRaw = Heatmap of raw data by time point: Average of the size factor (and batch factor) normalised counts per time point and gene.
draw(lsHeatmaps$complexHeatmapRaw)
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# complexHeatmapFit = Heatmap of impulse-fitted data by time point.
draw(lsHeatmaps$complexHeatmapFit)
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
png(file.path(outdir_plots,"ImpulseDE2_heatmap_case_fit.png"), width = 2000, height = 2400, res = 300)
draw(lsHeatmaps$complexHeatmapFit)
dev.off()
```

    ## png 
    ##   2

``` r
pdf(file.path(outdir_plots,"pdf_figs/ImpulseDE2_heatmap_case_fit.pdf"), width = 10, height = 12)
draw(lsHeatmaps$complexHeatmapFit)
dev.off()
```

    ## png 
    ##   2

``` r
png(file.path(outdir_plots,"ImpulseDE2_heatmap_case.png"), width = 2000, height = 2400, res = 300)
draw(lsHeatmaps$complexHeatmapRaw)
dev.off()
```

    ## png 
    ##   2

``` r
pdf(file.path(outdir_plots, "pdf_figs/ImpulseDE2_heatmap_case.pdf"), width = 10, height = 12)
draw(lsHeatmaps$complexHeatmapRaw)
dev.off()
```

    ## png 
    ##   2

### Top gene trajectories

``` r
# Plot top 10 differentially expressed (by q-value) genes
top_genes <- impulse_sig %>% arrange(padj) %>% head(10) %>% pull(Gene)

plotGenes(
  vecGeneIDs = top_genes,
  objectImpulseDE2 = objectImpulseDE2,
  boolSimplePlot = TRUE,
  boolCaseCtrl     = TRUE,
  dirOut = outdir_plots,
  strFileName = "/top10_DE_genes.pdf",
  boolMultiplePlotsPerPage = FALSE)
```

    ## [1] "Creating ../../output_RNA/ImpulseDE2/Pcomp/plots/top10_DE_genes.pdf"

    ## [[1]]

    ## 
    ## [[2]]

    ## 
    ## [[3]]

    ## 
    ## [[4]]

    ## 
    ## [[5]]

    ## 
    ## [[6]]

    ## 
    ## [[7]]

    ## 
    ## [[8]]

    ## 
    ## [[9]]

    ## 
    ## [[10]]

------------------------------------------------------------------------

# Mfuzz: Cluster ImpulseDE2-significant genes by trajectory

## 1. Preparing expression data

Filter vst transformed counts to keep significant genes and only
heat-samples

``` r
sig_genes <- impulse_sig %>% filter(Gene %in% rownames(vst)) %>% pull(Gene)

# Get VST expression for case (heat) samples only for sig genes
heat_samples <- meta %>% filter(treatment == "H") %>% pull(sample)
heat_vsd <- vst[sig_genes, heat_samples]
```

Average across replicates per timepoint, heat samples only

``` r
heat_avg <- vst[sig_genes, heat_samples] %>%
  as.data.frame() %>%
  rownames_to_column("Gene") %>%
  pivot_longer(-Gene, names_to = "sample", values_to = "expr") %>%
  left_join(meta %>% select(sample, time), by = "sample") %>%
  group_by(Gene, time) %>%
  summarize(mean_expr = mean(expr), .groups = "drop") %>%
  pivot_wider(names_from = time, values_from = mean_expr) %>%
  column_to_rownames("Gene")

# Reorder columns by time
heat_avg <- heat_avg[, order(as.numeric(colnames(heat_avg)))]
colnames(heat_avg) <- paste0("R", colnames(heat_avg))
```

Prepare data in eset format

``` r
# Create ExpressionSet
heat_eset <- ExpressionSet(assayData = as.matrix(heat_avg))
heat_eset <- standardise(heat_eset)
```

## 2. Determine Mfuzz parameters

For fuzzy c-means clustering, the fuzzifier m and the number of clusters
c has to be chosen in advance

``` r
# Estimate fuzzifier
m <- mestimate(heat_eset)
cat("Estimated fuzzifier m:", round(m, 2), "\n")
```

    ## Estimated fuzzifier m: 1.55

``` r
# Choose optimal cluster number based on elbow plot (do this once and add to species_parameters.R script)
Dmin(heat_eset, m = m, crange = seq(2, 12, 1), repeats = 3)

# adjust below to test what different numbers of clusters reveal in the data -- don't over-cluster
for (k in c(4,6,8)) {
  set.seed(global_params$seed)
  result <- mfuzz(heat_eset, c = k, m = m)
  mfuzz.plot(heat_eset, cl = result, mfrow = c(2, ceiling(k/2)), 
             new.window =FALSE,, time.labels =  c(0,1,3,12,24,72,120))
}
```

## 3. Run MFuzz

Run clustering with species-specific cluster number

``` r
k <-  config$n_clusters
set.seed(global_params$seed)
mfuzz_clusters <- mfuzz(heat_eset, c = k, m = m)
```

Extract cluster assignments and save results

``` r
cluster_assignments <- data.frame(
  Gene = names(mfuzz_clusters$cluster),
  cluster = mfuzz_clusters$cluster,
  membership = apply(mfuzz_clusters$membership, 1, max)) %>%
  left_join(impulse_sig %>% select(Gene, response_type), by = "Gene") # join to impulseDE results

table(cluster_assignments$cluster, cluster_assignments$response_type)
```

    ##    
    ##     Monotonous Other Transient
    ##   1        385   324        75
    ##   2       1071   325        20
    ##   3        246   495       283
    ##   4         37   287       351
    ##   5        127   423       710
    ##   6        377   602        95

``` r
# Save mfuzz objects and cluster assignments
saveRDS(mfuzz_clusters, file.path(outdir_mfuzz, "mfuzz_result.rds"))
saveRDS(heat_eset, file.path(outdir_mfuzz, "mfuzz_input_eset.rds"))
write.csv(cluster_assignments, file.path(outdir_mfuzz, "cluster_assignments.csv"), row.names = FALSE)
write.csv(as.data.frame(mfuzz_clusters$centers), file.path(outdir_mfuzz, "cluster_centers.csv"))
```

## 4. Characterize Clusters

``` r
# Get cluster centers (average trajectory)
cluster_centers <- mfuzz_clusters$centers

# Identify peak and trough timepoint for each cluster
timepoints <- c(0, 1, 3, 12, 24, 72, 120)
peak_times <- timepoints[apply(cluster_centers, 1, which.max)]
trough_times <- timepoints[apply(cluster_centers, 1, which.min)]

cluster_info <- data.frame(
  cluster = 1:k,
  peak_time = peak_times,
  trough_time = trough_times,
  n_genes = as.numeric(table(mfuzz_clusters$cluster)))

print(cluster_info)
```

    ##   cluster peak_time trough_time n_genes
    ## 1       1         3         120     784
    ## 2       2         1         120    1416
    ## 3       3         1          12    1024
    ## 4       4         0           3     675
    ## 5       5         3           1    1260
    ## 6       6        12           1    1074

``` r
write.csv(cluster_info, file.path(outdir_mfuzz, "cluster_info.csv"), row.names = FALSE)
```

### Visualize all significant genes in their clusters

``` r
mfuzz.plot(heat_eset, cl = mfuzz_clusters, new.window =FALSE, mfrow = c(2,k/2), time.labels =  c(0,1,3,12,24,72,120))
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
mfuzz.plot(heat_eset, cl = mfuzz_clusters, new.window =FALSE, mfrow = c(2,k/2), time.labels =  c(0,1,3,12,24,72,120), min.mem = 0.5)
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
# Visualize clusters
png(paste0(outdir_plots,"/temporal_clusters.png"), width = 12, height = 10, units = "in", res = 300)
mfuzz.plot2(heat_eset, cl = mfuzz_clusters, mfrow = c(2,k/2),
            time.labels = c("0", "1", "3", "12", "24", "72", "120"),
            xlab = "Time (hours)",x11=FALSE)
dev.off()
```

    ## png 
    ##   2

``` r
pdf(paste0(outdir_plots,"/pdf_figs/temporal_clusters.pdf"), width = 12, height = 10)
mfuzz.plot2(heat_eset, cl = mfuzz_clusters, mfrow = c(2,k/2),
            time.labels = c("0", "1", "3", "12", "24", "72", "120"),
            xlab = "Time (hours)",x11=FALSE)
dev.off()
```

    ## png 
    ##   2

``` r
png(paste0(outdir_plots,"/temporal_clusters_membership50.png"), width = 12, height = 10, units = "in", res = 300)
mfuzz.plot2(heat_eset, cl = mfuzz_clusters, mfrow = c(2,k/2),
            min.mem = 0.5,
            time.labels = c("0", "1", "3", "12", "24", "72", "120"),
            xlab = "Time (hours)",x11=FALSE)
dev.off()
```

    ## png 
    ##   2

``` r
pdf(paste0(outdir_plots,"/pdf_figs/temporal_clusters_membership50.pdf"), width = 12, height = 10)
mfuzz.plot2(heat_eset, cl = mfuzz_clusters, mfrow = c(2,k/2),
            min.mem = 0.5,
            time.labels = c("0", "1", "3", "12", "24", "72", "120"),
            xlab = "Time (hours)",x11=FALSE)
dev.off()
```

    ## png 
    ##   2

------------------------------------------------------------------------

# Exploring genes of interest

## Manually-curated heat stress genes by cluster

``` r
# read in Manual heat stress genes annotation
HeatStressGenes <- read_csv(paste0(annot_dir,"/heatstress/HeatStressGenes_", species ,".csv")) %>%
  dplyr::select(-1) %>% dplyr::rename(query = paste0(species,"_gene")) %>% dplyr::select(query,everything())

HeatStressGenes_unique <- HeatStressGenes %>% group_by(query) %>%
  summarize(gene_id = paste(unique(gene_id), collapse = ","),
            response_type = paste(unique(response_type), collapse = ","),
            category = paste(unique(category), collapse = ",")) 

HeatStressGenes_unique <- HeatStressGenes_unique %>% filter(query %in% rownames(vst))
 
stress_genes_ids <- unique(HeatStressGenes_unique$query) 
```

``` r
HSPS <- impulse_results %>% filter(Gene %in% stress_genes_ids) %>% arrange(padj) %>% left_join(HeatStressGenes_unique, by = join_by(Gene==query)) %>% filter(grepl("HSP",gene_id)) %>% pull(Gene)

cluster_assignments %>% filter(Gene %in% HSPS)
```

    ##                                        Gene cluster membership response_type
    ## 1 Porites_compressa_HIv1___RNAseq.g10172.t1       5  0.8621075     Transient
    ## 2  Porites_compressa_HIv1___RNAseq.g1475.t1       5  0.9220535     Transient

``` r
heat_clustered <- HeatStressGenes_unique %>% left_join(cluster_assignments, by = join_by(query==Gene)) %>%
  arrange(cluster, desc(cluster))

# plot this to show which genes are in which cluster
heat_clustered %>% filter(!is.na(cluster)) %>% ggplot(aes(y=reorder(gene_id, cluster), x=factor(cluster), fill=cluster)) +
  geom_tile() +
  theme_bw() +
  labs(x="Mfuzz Cluster", y="Gene ID", title="Heat stress genes clustered by temporal expression pattern")
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
sessionInfo()
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
    ##  [1] tcltk     grid      stats4    stats     graphics  grDevices utils    
    ##  [8] datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] WGCNA_1.73                  fastcluster_1.3.0          
    ##  [3] dynamicTreeCut_1.63-1       Mfuzz_2.68.0               
    ##  [5] DynDoc_1.86.0               widgetTools_1.86.0         
    ##  [7] e1071_1.7-16                ComplexHeatmap_2.26.0      
    ##  [9] ImpulseDE2_0.99.10          BiocParallel_1.44.0        
    ## [11] ggnewscale_0.5.2            genefilter_1.90.0          
    ## [13] RColorBrewer_1.1-3          pheatmap_1.0.13            
    ## [15] DESeq2_1.50.2               SummarizedExperiment_1.40.0
    ## [17] Biobase_2.70.0              MatrixGenerics_1.22.0      
    ## [19] matrixStats_1.5.0           GenomicRanges_1.62.0       
    ## [21] Seqinfo_1.0.0               IRanges_2.44.0             
    ## [23] S4Vectors_0.48.0            BiocGenerics_0.56.0        
    ## [25] generics_0.1.4              lubridate_1.9.4            
    ## [27] forcats_1.0.0               stringr_1.6.0              
    ## [29] dplyr_1.1.4                 purrr_1.2.1                
    ## [31] readr_2.1.6                 tidyr_1.3.1                
    ## [33] tibble_3.3.0                ggplot2_4.0.1              
    ## [35] tidyverse_2.0.0             rmarkdown_2.30             
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] rstudioapi_0.17.1       jsonlite_2.0.0          shape_1.4.6.1          
    ##   [4] magrittr_2.0.4          magick_2.9.0            farver_2.1.2           
    ##   [7] GlobalOptions_0.1.3     ragg_1.5.0              vctrs_0.7.0            
    ##  [10] memoise_2.0.1           Cairo_1.7-0             base64enc_0.1-3        
    ##  [13] htmltools_0.5.9         S4Arrays_1.10.0         SparseArray_1.10.2     
    ##  [16] Formula_1.2-5           htmlwidgets_1.6.4       impute_1.84.0          
    ##  [19] cachem_1.1.0            lifecycle_1.0.5         iterators_1.0.14       
    ##  [22] pkgconfig_2.0.3         Matrix_1.6-4            R6_2.6.1               
    ##  [25] fastmap_1.2.0           GenomeInfoDbData_1.2.14 clue_0.3-66            
    ##  [28] digest_0.6.39           colorspace_2.1-2        AnnotationDbi_1.72.0   
    ##  [31] textshaping_1.0.4       Hmisc_5.2-5             RSQLite_2.4.5          
    ##  [34] labeling_0.4.3          timechange_0.3.0        httr_1.4.7             
    ##  [37] abind_1.4-8             compiler_4.5.1          proxy_0.4-27           
    ##  [40] bit64_4.6.0-1           withr_3.0.2             doParallel_1.0.17      
    ##  [43] backports_1.5.0         htmlTable_2.4.3         S7_0.2.1               
    ##  [46] DBI_1.2.3               tkWidgets_1.86.0        DelayedArray_0.36.0    
    ##  [49] rjson_0.2.23            tools_4.5.1             foreign_0.8-90         
    ##  [52] nnet_7.3-20             glue_1.8.0              checkmate_2.3.3        
    ##  [55] cluster_2.1.8.1         gtable_0.3.6            tzdb_0.5.0             
    ##  [58] preprocessCore_1.72.0   class_7.3-23            data.table_1.18.0      
    ##  [61] hms_1.1.4               XVector_0.50.0          foreach_1.5.2          
    ##  [64] pillar_1.11.1           limma_3.64.3            vroom_1.6.7            
    ##  [67] circlize_0.4.17         splines_4.5.1           lattice_0.22-7         
    ##  [70] survival_3.8-3          bit_4.6.0               annotate_1.86.1        
    ##  [73] tidyselect_1.2.1        GO.db_3.22.0            locfit_1.5-9.12        
    ##  [76] Biostrings_2.78.0       knitr_1.50              gridExtra_2.3          
    ##  [79] xfun_0.56               statmod_1.5.1           stringi_1.8.7          
    ##  [82] UCSC.utils_1.4.0        yaml_2.3.12             evaluate_1.0.5         
    ##  [85] codetools_0.2-20        cli_3.6.5               rpart_4.1.24           
    ##  [88] xtable_1.8-4            systemfonts_1.3.1       dichromat_2.0-0.1      
    ##  [91] Rcpp_1.1.1              GenomeInfoDb_1.44.3     png_0.1-8              
    ##  [94] XML_3.99-0.18           parallel_4.5.1          blob_1.2.4             
    ##  [97] scales_1.4.0            crayon_1.5.3            GetoptLong_1.1.0       
    ## [100] rlang_1.2.0             cowplot_1.2.0           KEGGREST_1.50.0
