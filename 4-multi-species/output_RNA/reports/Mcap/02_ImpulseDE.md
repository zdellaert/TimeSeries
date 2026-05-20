ImpulseDE2 Temporal Analysis
================
Zoe Dellaert
2026-05-20

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
  - [2. Run ImpulseDE2](#2-run-impulsede2)
  - [3. Extract ImpulseDE2 results](#3-extract-impulsede2-results)
    - [All genes](#all-genes)
    - [Significant genes](#significant-genes)
    - [Quick summary](#quick-summary)
  - [4. Visualize ImpulseDE2 Results](#4-visualize-impulsede2-results)
    - [Classify significant genes by up or
      downregulation](#classify-significant-genes-by-up-or-downregulation)
    - [Heatmap of significant genes by
      trajectory](#heatmap-of-significant-genes-by-trajectory)
    - [Heatmap of top 100 significant transient/transition UP genes
      normalized to
      T0](#heatmap-of-top-100-significant-transienttransition-up-genes-normalized-to-t0)
    - [Heatmap of top 100 significant transient/transition downregulated
      genes normalized to
      T0](#heatmap-of-top-100-significant-transienttransition-downregulated-genes-normalized-to-t0)
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
library(pheatmap)
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
    ##  [7] generics_0.1.4        pheatmap_1.0.13       ComplexHeatmap_2.26.0
    ## [10] lubridate_1.9.4       forcats_1.0.0         stringr_1.6.0        
    ## [13] dplyr_1.1.4           purrr_1.2.1           readr_2.1.6          
    ## [16] tidyr_1.3.1           tibble_3.3.0          ggplot2_4.0.1        
    ## [19] tidyverse_2.0.0       ImpulseDE2_0.99.10    rmarkdown_2.30       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1            farver_2.1.2               
    ##  [3] S7_0.2.1                    fastmap_1.2.0              
    ##  [5] digest_0.6.39               timechange_0.3.0           
    ##  [7] lifecycle_1.0.5             cluster_2.1.8.1            
    ##  [9] Cairo_1.7-0                 magrittr_2.0.4             
    ## [11] compiler_4.5.1              tkWidgets_1.86.0           
    ## [13] rlang_1.2.0                 tools_4.5.1                
    ## [15] yaml_2.3.12                 knitr_1.50                 
    ## [17] labeling_0.4.3              S4Arrays_1.10.0            
    ## [19] bit_4.6.0                   DelayedArray_0.36.0        
    ## [21] RColorBrewer_1.1-3          abind_1.4-8                
    ## [23] BiocParallel_1.44.0         withr_3.0.2                
    ## [25] stats4_4.5.1                colorspace_2.1-2           
    ## [27] scales_1.4.0                iterators_1.0.14           
    ## [29] dichromat_2.0-0.1           SummarizedExperiment_1.40.0
    ## [31] cli_3.6.5                   crayon_1.5.3               
    ## [33] ragg_1.5.0                  rstudioapi_0.17.1          
    ## [35] tzdb_0.5.0                  rjson_0.2.23               
    ## [37] proxy_0.4-27                parallel_4.5.1             
    ## [39] XVector_0.50.0              matrixStats_1.5.0          
    ## [41] vctrs_0.7.0                 Matrix_1.6-4               
    ## [43] IRanges_2.44.0              GetoptLong_1.1.0           
    ## [45] hms_1.1.4                   S4Vectors_0.48.0           
    ## [47] bit64_4.6.0-1               clue_0.3-66                
    ## [49] systemfonts_1.3.1           magick_2.9.0               
    ## [51] locfit_1.5-9.12             foreach_1.5.2              
    ## [53] glue_1.8.0                  codetools_0.2-20           
    ## [55] cowplot_1.2.0               stringi_1.8.7              
    ## [57] shape_1.4.6.1               gtable_0.3.6               
    ## [59] GenomicRanges_1.62.0        pillar_1.11.1              
    ## [61] htmltools_0.5.9             Seqinfo_1.0.0              
    ## [63] circlize_0.4.17             R6_2.6.1                   
    ## [65] textshaping_1.0.4           doParallel_1.0.17          
    ## [67] vroom_1.6.7                 evaluate_1.0.5             
    ## [69] lattice_0.22-7              png_0.1-8                  
    ## [71] class_7.3-23                Rcpp_1.1.1                 
    ## [73] SparseArray_1.10.2          DESeq2_1.50.2              
    ## [75] xfun_0.56                   MatrixGenerics_1.22.0      
    ## [77] pkgconfig_2.0.3             GlobalOptions_0.1.3

## 2. Setup species-specific parameters

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
    ## WGCNA power: 12
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
# once outliers are removed, define extra sample lists
heat_samples <- meta %>% filter(treatment == "H") %>% pull(sample)
```

``` r
# read in SwissProt annotation
SwissProt <- read.delim(file.path(annot_dir,config$SwissProt))
cat("Annotations:", nrow(SwissProt), "Swissprot-annotated genes")
```

    ## Annotations: 22471 Swissprot-annotated genes

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

## 2. Run ImpulseDE2

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

``` r
# Print processing report
cat(objectImpulseDE2@strReport)
```

    ## ImpulseDE2 for count data, v0.99.10
    ## # Process inputProcessing Details:
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
    ## Input contained 30089 genes/regions.
    ## Selected 30089 genes/regions for analysis.
    ## # Run DESeq2: Using dispersion factorscomputed by DESeq2.
    ## Consumed time: 1.05 min.
    ## # Compute size factors
    ## # Fitting null and alternative model to the genes
    ## Consumed time: 11.84 min.
    ## # Fitting sigmoid model to case condition
    ## Consumed time: 1.18 min.
    ## # Differentially expression analysis based on model fits
    ## Finished running ImpulseDE2.
    ## TOTAL consumed time: 14.21 min.

## 3. Extract ImpulseDE2 results

### All genes

Extract and save results for all non-zero genes

``` r
impulse_results <- objectImpulseDE2$dfImpulseDE2Results
impulse_results <- impulse_results %>% filter(allZero==FALSE) #remove genes with zero counts

# classify genes as transiently or monotonously regulated if impulseDE gave them one of those labels
impulse_results <- impulse_results %>%
  mutate(response_type = case_when(
    isTransient & !is.na(isTransient) ~ "Transient",
    isMonotonous & !is.na(isMonotonous) ~ "Monotonous",
    .default = "Other"
  ))

impulse_results_annot <- impulse_results %>%
  left_join(SwissProt %>% select(query,ProteinNames,BiologicalProcess), by = join_by("Gene"=="query"))

write.csv(impulse_results, file.path(outdir, "ImpulseDE2_results.csv"), row.names = FALSE)
```

### Significant genes

Extract genes with significant treatment effect on temporal trajectory
and save results

``` r
impulse_sig <- impulse_results %>%
  filter(padj < global_params$padj_threshold)

#preview top DE genes and annotations
impulse_sig %>% arrange(padj) %>% head(20) %>% dplyr::select(Gene,padj,loglik_red,response_type) %>%
  left_join(SwissProt %>% select(query,ProteinNames,BiologicalProcess), by = join_by("Gene"=="query"))
```

    ##                                          Gene          padj loglik_red
    ## 1  Montipora_capitata_HIv3___RNAseq.g49833.t1 1.260194e-130  -639.8092
    ## 2  Montipora_capitata_HIv3___RNAseq.g49832.t1  2.895595e-57  -498.7410
    ## 3   Montipora_capitata_HIv3___RNAseq.g7282.t1  1.380504e-39  -409.3506
    ## 4        Montipora_capitata_HIv3___TS.g637.t1  4.147187e-36  -400.5779
    ## 5  Montipora_capitata_HIv3___RNAseq.g40931.t1  8.557425e-34  -322.4355
    ## 6    Montipora_capitata_HIv3___RNAseq.g984.t1  9.781209e-33  -346.8036
    ## 7  Montipora_capitata_HIv3___RNAseq.g26806.t1  1.011798e-29  -406.8376
    ## 8      Montipora_capitata_HIv3___TS.g49643.t1  3.967573e-27  -256.2814
    ## 9  Montipora_capitata_HIv3___RNAseq.g37139.t1  1.869750e-26  -309.2054
    ## 10 Montipora_capitata_HIv3___RNAseq.g26807.t1  5.080430e-26  -369.5454
    ## 11 Montipora_capitata_HIv3___RNAseq.g41225.t1  6.791640e-26  -386.6196
    ## 12  Montipora_capitata_HIv3___RNAseq.g6371.t1  2.491826e-24  -211.9523
    ## 13  Montipora_capitata_HIv3___RNAseq.g7502.t1  2.491826e-24  -325.6822
    ## 14 Montipora_capitata_HIv3___RNAseq.g20397.t1  2.491826e-24  -341.2884
    ## 15      Montipora_capitata_HIv3___TS.g6919.t1  4.279707e-24  -336.4517
    ## 16 Montipora_capitata_HIv3___RNAseq.g22408.t1  4.689950e-24  -335.2139
    ## 17 Montipora_capitata_HIv3___RNAseq.g15259.t1  7.687494e-24  -329.2330
    ## 18 Montipora_capitata_HIv3___RNAseq.g11828.t1  1.401850e-23  -319.6805
    ## 19 Montipora_capitata_HIv3___RNAseq.g18448.t1  3.725846e-23  -330.9515
    ## 20     Montipora_capitata_HIv3___TS.g48780.t1  1.032917e-22  -370.2430
    ##    response_type
    ## 1      Transient
    ## 2      Transient
    ## 3      Transient
    ## 4      Transient
    ## 5     Monotonous
    ## 6     Monotonous
    ## 7      Transient
    ## 8      Transient
    ## 9      Transient
    ## 10     Transient
    ## 11     Transient
    ## 12    Monotonous
    ## 13    Monotonous
    ## 14     Transient
    ## 15    Monotonous
    ## 16     Transient
    ## 17    Monotonous
    ## 18    Monotonous
    ## 19    Monotonous
    ## 20    Monotonous
    ##                                                                                                                                                                                                                                                     ProteinNames
    ## 1                                                                                                                                                                                                                             Glycine-rich RNA-binding protein 1
    ## 2                                                            Glycine-rich RNA-binding protein 4, mitochondrial (AtGR-RBP4) (AtRBG4) (Glycine-rich protein 4) (AtGRP4) (Mitochondrial RNA-binding protein 1b) (At-mRBP1b) (Small RNA binding protein 4) (AtSRBP4)
    ## 3                                                                                                                                                                               Serine/arginine-rich splicing factor 4 (Splicing factor, arginine/serine-rich 4)
    ## 4                                                                                                                                                                                                                        Cytochrome P450 10 (EC 1.14.-.-) (CYPX)
    ## 5                                                                                                                                                                                Prominin-1 (Antigen AC133 homolog) (Prominin-like protein 1) (CD antigen CD133)
    ## 6                                                                                                                                                                                                                                                      Galaxin-2
    ## 7                                                                                                                                                                                                                  DNA-binding protein MNB1B (HMG1-like protein)
    ## 8                                                                                                                                                                                                                                                           <NA>
    ## 9  Methylcrotonoyl-CoA carboxylase subunit alpha, mitochondrial (MCCase subunit alpha) (EC 6.4.1.4) (3-methylcrotonyl-CoA carboxylase 1) (3-methylcrotonyl-CoA carboxylase biotin-containing subunit) (3-methylcrotonyl-CoA:carbon dioxide ligase subunit alpha)
    ## 10                                                                                                                                                                                                                 Zinc finger MYND domain-containing protein 10
    ## 11                                                                                                          Heterogeneous nuclear ribonucleoproteins A1 homolog (hnRNP A1) (Helix-destabilizing protein) (Single-strand-binding protein) (hnRNP core protein A1)
    ## 12                                                                                                                                  ATP-binding cassette subfamily C member 4 (EC 7.6.2.-) (EC 7.6.2.2) (EC 7.6.2.3) (Multidrug resistance-associated protein 4)
    ## 13                                                                                                                                                                                                                                         Meteorin-like protein
    ## 14                                                                                                                      Cleavage stimulation factor subunit 3 (CF-1 77 kDa subunit) (Cleavage stimulation factor 77 kDa subunit) (CSTF 77 kDa subunit) (CstF-77)
    ## 15                                                                                                                                                                                                                                  ZP domain-containing protein
    ## 16                                                                                                                                  Endoplasmic reticulum-Golgi intermediate compartment protein 1 (ER-Golgi intermediate compartment 32 kDa protein) (ERGIC-32)
    ## 17                                                                                                                                                            Protein-glucosylgalactosylhydroxylysine glucosidase (EC 3.2.1.107) (Acid trehalase-like protein 1)
    ## 18                                                                                                                                                                                                                                    Transcription factor SOX-4
    ## 19                                                                                                                                                                                                                                                          <NA>
    ## 20                                                                                                                                                                                                                                                          <NA>
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  BiologicalProcess
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      mRNA transport [GO:0051028]
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      extracellular transport [GO:0006858]; miRNA transport [GO:1990428]; mitochondrial RNA modification [GO:1900864]; regulation of defense response to virus [GO:0050688]; response to cold [GO:0009409]; response to osmotic stress [GO:0006970]; response to salt stress [GO:0009651]; response to water deprivation [GO:0009414]; RNA transport [GO:0050658]
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          hematopoietic progenitor cell differentiation [GO:0002244]; mRNA processing [GO:0006397]; negative regulation of mRNA splicing, via spliceosome [GO:0048025]; RNA splicing [GO:0008380]
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             camera-type eye photoreceptor cell differentiation [GO:0060219]; retina layer formation [GO:0010842]
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
    ## 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           regulation of DNA-templated transcription [GO:0006355]
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         L-leucine catabolic process [GO:0006552]
    ## 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 inner dynein arm assembly [GO:0036159]; motile cilium assembly [GO:0044458]; outer dynein arm assembly [GO:0036158]; positive regulation of motile cilium assembly [GO:1905505]
    ## 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     mRNA splicing, via spliceosome [GO:0000398]
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    bile acid and bile salt transport [GO:0015721]; cAMP transport [GO:0070730]; cilium assembly [GO:0060271]; export across plasma membrane [GO:0140115]; leukotriene transport [GO:0071716]; positive regulation of smooth muscle cell proliferation [GO:0048661]; prostaglandin secretion [GO:0032310]; prostaglandin transport [GO:0015732]; response to tetrachloromethane [GO:1904772]; transmembrane transport [GO:0055085]; urate transport [GO:0015747]
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            brown fat cell differentiation [GO:0050873]; energy homeostasis [GO:0097009]; negative regulation of inflammatory response [GO:0050728]; positive regulation of brown fat cell differentiation [GO:0090336]; response to cold [GO:0009409]; response to muscle activity [GO:0014850]
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    co-transcriptional mRNA 3'-end processing, cleavage and polyadenylation pathway [GO:0180010]
    ## 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      endoplasmic reticulum to Golgi vesicle-mediated transport [GO:0006888]; retrograde vesicle-mediated transport, Golgi to endoplasmic reticulum [GO:0006890]
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     carbohydrate metabolic process [GO:0005975]
    ## 18 ascending aorta morphogenesis [GO:0035910]; atrial septum primum morphogenesis [GO:0003289]; cardiac right ventricle morphogenesis [GO:0003215]; cellular response to glucose stimulus [GO:0071333]; endocrine pancreas development [GO:0031018]; gene expression [GO:0010467]; glial cell development [GO:0021782]; glial cell proliferation [GO:0014009]; glucose homeostasis [GO:0042593]; heart development [GO:0007507]; hematopoietic stem cell homeostasis [GO:0061484]; kidney morphogenesis [GO:0060993]; mesenchyme development [GO:0060485]; mitral valve morphogenesis [GO:0003183]; negative regulation of myoblast differentiation [GO:0045662]; negative regulation of transcription by RNA polymerase II [GO:0000122]; nervous system development [GO:0007399]; neuroepithelial cell differentiation [GO:0060563]; noradrenergic neuron differentiation [GO:0003357]; positive regulation of apoptotic process [GO:0043065]; positive regulation of canonical Wnt signaling pathway [GO:0090263]; positive regulation of cell population proliferation [GO:0008284]; positive regulation of DNA-templated transcription [GO:0045893]; positive regulation of gamma-delta T cell differentiation [GO:0045588]; positive regulation of insulin secretion [GO:0032024]; positive regulation of myoblast differentiation [GO:0045663]; positive regulation of transcription by RNA polymerase II [GO:0045944]; pro-B cell differentiation [GO:0002328]; protein stabilization [GO:0050821]; regulation of DNA damage response, signal transduction by p53 class mediator [GO:0043516]; regulation of DNA-templated transcription [GO:0006355]; somatic stem cell population maintenance [GO:0035019]; spinal cord development [GO:0021510]; sympathetic nervous system development [GO:0048485]; T cell differentiation [GO:0030217]; ventricular septum morphogenesis [GO:0060412]
    ## 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>

### Quick summary

    ## Total significant genes: 6631

    ## Response patterns:

    ## Transient: 1989

    ## Monotonous: 2486

    ## Other: 2156

## 4. Visualize ImpulseDE2 Results

### Classify significant genes by up or downregulation

``` r
lsHeatmaps <- plotHeatmap(
  objectImpulseDE2       = objectImpulseDE2,
  strCondition           = "case",
  boolIdentifyTransients = TRUE, #set to true if true above
  scaQThres              = global_params$padj_threshold)

# this code is also handy because the object it creates sorts signficiant genes into the following four categories:
transition_up <- lsHeatmaps$lsvecGeneGroups$transition_up
transition_down <- lsHeatmaps$lsvecGeneGroups$transition_down
transient_up <- lsHeatmaps$lsvecGeneGroups$transient_up
transient_down <- lsHeatmaps$lsvecGeneGroups$transient_down
classified <- c(transition_up,transition_down,transient_up,transient_down)
# the unclassified ones below are the same as response_type=="Other"
unclassified <- impulse_sig %>% filter(!(Gene %in% classified)) %>% pull(Gene)

gene_classification <- data.frame(
  Gene = c(transition_up, transition_down, transient_up, transient_down, unclassified),
  classification = c(
    rep("transition_up", length(transition_up)),
    rep("transition_down", length(transition_down)),
    rep("transient_up", length(transient_up)),
    rep("transient_down", length(transient_down)),
    rep("unclassified", length(unclassified))
  )
)

impulse_sig_classifications <- impulse_sig %>% left_join(gene_classification)
write.csv(impulse_sig_classifications, file.path(outdir, "ImpulseDE2_significant.csv"), row.names = FALSE)
```

### Heatmap of significant genes by trajectory

``` r
# complexHeatmapRaw = Heatmap of raw data by time point: Average of the size factor (and batch factor) normalised counts per time point and gene.
draw(lsHeatmaps$complexHeatmapRaw)
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# complexHeatmapFit = Heatmap of impulse-fitted data by time point.
draw(lsHeatmaps$complexHeatmapFit)
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

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

### Heatmap of top 100 significant transient/transition UP genes normalized to T0

``` r
impulse_sig_up <- impulse_sig %>% filter(Gene %in% transient_up | Gene %in% transition_up)
top_100_DE_genes <- impulse_sig_up %>% arrange(padj) %>% filter(Gene %in% rownames(vst)) %>% head(100) %>% arrange(response_type,padj)
row_annot <- top_100_DE_genes %>% select(response_type)
#col_annot <- meta[heat_samples,] %>% select(treatment,time)
col_annot <- meta[heat_samples,] %>% select(time)

T0_samples <- meta[heat_samples,] %>% filter(time == 0 ) %>% pull(sample)

vsd_heat_top <- vst[top_100_DE_genes$Gene,heat_samples]
vsd_heat_top$T0mean <- rowMeans(vsd_heat_top[,T0_samples])
vsd_heat_norm <- vsd_heat_top - vsd_heat_top$T0mean
vsd_heat_norm <- vsd_heat_norm %>% select(-T0mean)

vsd_max <- max(abs(vsd_heat_norm), na.rm = TRUE)
scale_colors <- colorRampPalette(c("#3c58a7","#3c58a7", "white", "#ee2e25","#ee2e25"))(100)
legend_breaks <- seq(-vsd_max, vsd_max, length.out = 101)

heatmap_up <- pheatmap(vsd_heat_norm, 
         cluster_rows = FALSE, 
         show_rownames = FALSE,
         show_colnames = FALSE,
         cluster_cols = FALSE, 
         annotation_names_row = FALSE,
         annotation_names_col = FALSE,
         breaks = legend_breaks,
         col = scale_colors,
         border_color = NA,
         gaps_col = cumsum(table(col_annot$time))[-length(table(col_annot$time))],
         annotation_row = row_annot,
         annotation_col = col_annot,
         annotation_colors = list("treatment" = treat_colors,
                                  "time" = time_colors,
                                  "response_type" = c("Monotonous" = "#D8AF39FF", "Transient" = "#278B9AFF")))
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
save_ggplot(heatmap_up, "ImpulseDE2_heatmap_UP", width = 7, height = 10, units = "in", dpi = 300)

all_DE_genes <- impulse_sig_up %>% arrange(padj) %>% filter(Gene %in% rownames(vst)) %>% arrange(response_type,padj)
row_annot <- all_DE_genes %>% select(response_type)
#col_annot <- meta[heat_samples,] %>% select(treatment,time)
col_annot <- meta[heat_samples,] %>% select(time)

T0_samples <- meta[heat_samples,] %>% filter(time == 0 ) %>% pull(sample)

vsd_heat_top <- vst[all_DE_genes$Gene,heat_samples]
vsd_heat_top$T0mean <- rowMeans(vsd_heat_top[,T0_samples])
vsd_heat_norm <- vsd_heat_top - vsd_heat_top$T0mean
vsd_heat_norm <- vsd_heat_norm %>% select(-T0mean)

vsd_max <- max(abs(vsd_heat_norm), na.rm = TRUE)
scale_colors <- colorRampPalette(c("#3c58a7","#3c58a7", "white", "#ee2e25","#ee2e25"))(100)
legend_breaks <- seq(-vsd_max*.9, vsd_max*.9, length.out = 101)

heatmap_up_all <- pheatmap(vsd_heat_norm, 
         cluster_rows = FALSE, 
         show_rownames = FALSE,
         show_colnames = FALSE,
         cluster_cols = FALSE, 
         annotation_names_row = FALSE,
         annotation_names_col = FALSE,
         breaks = legend_breaks,
         col = scale_colors,
         border_color = NA,
         gaps_col = cumsum(table(col_annot$time))[-length(table(col_annot$time))],
         annotation_row = row_annot,
         annotation_col = col_annot,
         annotation_colors = list("treatment" = treat_colors,
                                  "time" = time_colors,
                                  "response_type" = c("Monotonous" = "#D8AF39FF", "Transient" = "#278B9AFF")))
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
save_ggplot(heatmap_up_all, "ImpulseDE2_heatmap_UP_ALL", width = 7, height = 10, units = "in", dpi = 300)
```

### Heatmap of top 100 significant transient/transition downregulated genes normalized to T0

``` r
impulse_sig_down <- impulse_sig %>% filter(Gene %in% transient_down | Gene %in% transition_down)
top_100_DE_genes <- impulse_sig_down %>% arrange(padj) %>% filter(Gene %in% rownames(vst)) %>% head(100) %>% arrange(response_type,padj)
row_annot <- top_100_DE_genes %>% select(response_type)
#col_annot <- meta[heat_samples,] %>% select(treatment,time)
col_annot <- meta[heat_samples,] %>% select(time)

vsd_heat_top <- vst[top_100_DE_genes$Gene,heat_samples]
vsd_heat_top$T0mean <- rowMeans(vsd_heat_top[,T0_samples])
vsd_heat_norm <- vsd_heat_top - vsd_heat_top$T0mean
vsd_heat_norm <- vsd_heat_norm %>% select(-T0mean)

vsd_max <- max(abs(vsd_heat_norm), na.rm = TRUE)
scale_colors <- colorRampPalette(c("#3c58a7","#3c58a7", "white", "#ee2e25","#ee2e25"))(100)
legend_breaks <- seq(-vsd_max, vsd_max, length.out = 101)

heatmap_down <- pheatmap(vsd_heat_norm, 
         cluster_rows = FALSE, 
         show_rownames = FALSE,
         show_colnames = FALSE,
         cluster_cols = FALSE, 
         annotation_names_row = FALSE,
         annotation_names_col = FALSE,
         breaks = legend_breaks,
         col = scale_colors,
         border_color = NA,
         gaps_col = cumsum(table(col_annot$time))[-length(table(col_annot$time))],
         annotation_row = row_annot,
         annotation_col = col_annot,
         annotation_colors = list("treatment" = treat_colors,
                                  "time" = time_colors,
                                  "response_type" = c("Monotonous" = "#D8AF39FF", "Transient" = "#278B9AFF")))
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
save_ggplot(heatmap_down, "ImpulseDE2_heatmap_DOWN", width = 7, height = 10, units = "in", dpi = 300)

all_DE_genes <- impulse_sig_down %>% arrange(padj) %>% filter(Gene %in% rownames(vst)) %>% arrange(response_type,padj)
row_annot <- all_DE_genes %>% select(response_type)
#col_annot <- meta[heat_samples,] %>% select(treatment,time)
col_annot <- meta[heat_samples,] %>% select(time)

T0_samples <- meta[heat_samples,] %>% filter(time == 0 ) %>% pull(sample)

vsd_heat_top <- vst[all_DE_genes$Gene,heat_samples]
vsd_heat_top$T0mean <- rowMeans(vsd_heat_top[,T0_samples])
vsd_heat_norm <- vsd_heat_top - vsd_heat_top$T0mean
vsd_heat_norm <- vsd_heat_norm %>% select(-T0mean)

vsd_max <- max(abs(vsd_heat_norm), na.rm = TRUE)
scale_colors <- colorRampPalette(c("#3c58a7","#3c58a7", "white", "#ee2e25","#ee2e25"))(100)
legend_breaks <- seq(-vsd_max*.9, vsd_max*.9, length.out = 101)

heatmap_down_all <- pheatmap(vsd_heat_norm, 
         cluster_rows = FALSE, 
         show_rownames = FALSE,
         show_colnames = FALSE,
         cluster_cols = FALSE, 
         annotation_names_row = FALSE,
         annotation_names_col = FALSE,
         breaks = legend_breaks,
         col = scale_colors,
         border_color = NA,
         gaps_col = cumsum(table(col_annot$time))[-length(table(col_annot$time))],
         annotation_row = row_annot,
         annotation_col = col_annot,
         annotation_colors = list("treatment" = treat_colors,
                                  "time" = time_colors,
                                  "response_type" = c("Monotonous" = "#D8AF39FF", "Transient" = "#278B9AFF")))
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
save_ggplot(heatmap_down_all, "ImpulseDE2_heatmap_DOWN_ALL", width = 7, height = 10, units = "in", dpi = 300)
```

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

    ## [1] "Creating ../../output_RNA/ImpulseDE2/Mcap/plots/top10_DE_genes.pdf"

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
    ##   1        847   371        52
    ##   2        635   328       133
    ##   3        152   211       600
    ##   4         90   384       491
    ##   5         65   386       592
    ##   6        697   476       121

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
    ## 1       1         0          12    1270
    ## 2       2        12           0    1096
    ## 3       3         3           0     963
    ## 4       4       120           3     965
    ## 5       5         0          12    1043
    ## 6       6         3         120    1294

``` r
write.csv(cluster_info, file.path(outdir_mfuzz, "cluster_info.csv"), row.names = FALSE)
```

### Visualize all significant genes in their clusters

``` r
mfuzz.plot(heat_eset, cl = mfuzz_clusters, new.window =FALSE, mfrow = c(2,k/2), time.labels =  c(0,1,3,12,24,72,120))
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
mfuzz.plot(heat_eset, cl = mfuzz_clusters, new.window =FALSE, mfrow = c(2,k/2), time.labels =  c(0,1,3,12,24,72,120), min.mem = 0.5)
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

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

``` r
png(paste0(outdir_plots,"/temporal_clusters_scaled.png"), width = 12, height = 10, units = "in", res = 300)
mfuzz.plot2(heat_eset, cl = mfuzz_clusters, mfrow = c(2,k/2),
            time.points = c(0, 1, 3, 12, 24, 72, 120),
            time.labels = c("0", "1", "3", "12", "24", "72", "120"),
            xlab = "Time (hours)",x11=FALSE)
dev.off()
```

    ## png 
    ##   2

------------------------------------------------------------------------

## Exploring genes of interest

### Manually-curated heat stress genes by cluster

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

    ##                                         Gene cluster membership response_type
    ## 1 Montipora_capitata_HIv3___RNAseq.g15043.t1       3  0.7169819     Transient
    ## 2 Montipora_capitata_HIv3___RNAseq.g18811.t1       3  0.7176118     Transient
    ## 3     Montipora_capitata_HIv3___TS.g35289.t2       3  0.8739001     Transient

``` r
heat_clustered <- HeatStressGenes_unique %>% left_join(cluster_assignments, by = join_by(query==Gene)) %>%
  arrange(cluster, desc(cluster))

# plot this to show which genes are in which cluster
heat_clustered %>% filter(!is.na(cluster)) %>% ggplot(aes(y=reorder(gene_id, cluster), x=factor(cluster), fill=cluster)) +
  geom_tile() +
  theme_bw() +
  labs(x="Mfuzz Cluster", y="Gene ID", title="Heat stress genes clustered by temporal expression pattern")
```

![](./02_ImpulseDE_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

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
    ## [1] tcltk     grid      stats     graphics  grDevices utils     datasets 
    ## [8] methods   base     
    ## 
    ## other attached packages:
    ##  [1] Mfuzz_2.68.0          DynDoc_1.86.0         widgetTools_1.86.0   
    ##  [4] e1071_1.7-16          Biobase_2.70.0        BiocGenerics_0.56.0  
    ##  [7] generics_0.1.4        pheatmap_1.0.13       ComplexHeatmap_2.26.0
    ## [10] lubridate_1.9.4       forcats_1.0.0         stringr_1.6.0        
    ## [13] dplyr_1.1.4           purrr_1.2.1           readr_2.1.6          
    ## [16] tidyr_1.3.1           tibble_3.3.0          ggplot2_4.0.1        
    ## [19] tidyverse_2.0.0       ImpulseDE2_0.99.10    rmarkdown_2.30       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1            farver_2.1.2               
    ##  [3] S7_0.2.1                    fastmap_1.2.0              
    ##  [5] digest_0.6.39               timechange_0.3.0           
    ##  [7] lifecycle_1.0.5             cluster_2.1.8.1            
    ##  [9] Cairo_1.7-0                 magrittr_2.0.4             
    ## [11] compiler_4.5.1              tkWidgets_1.86.0           
    ## [13] rlang_1.2.0                 tools_4.5.1                
    ## [15] yaml_2.3.12                 knitr_1.50                 
    ## [17] labeling_0.4.3              S4Arrays_1.10.0            
    ## [19] bit_4.6.0                   DelayedArray_0.36.0        
    ## [21] RColorBrewer_1.1-3          abind_1.4-8                
    ## [23] BiocParallel_1.44.0         withr_3.0.2                
    ## [25] stats4_4.5.1                colorspace_2.1-2           
    ## [27] scales_1.4.0                iterators_1.0.14           
    ## [29] dichromat_2.0-0.1           SummarizedExperiment_1.40.0
    ## [31] cli_3.6.5                   crayon_1.5.3               
    ## [33] ragg_1.5.0                  rstudioapi_0.17.1          
    ## [35] tzdb_0.5.0                  rjson_0.2.23               
    ## [37] proxy_0.4-27                parallel_4.5.1             
    ## [39] XVector_0.50.0              matrixStats_1.5.0          
    ## [41] vctrs_0.7.0                 Matrix_1.6-4               
    ## [43] IRanges_2.44.0              GetoptLong_1.1.0           
    ## [45] hms_1.1.4                   S4Vectors_0.48.0           
    ## [47] bit64_4.6.0-1               clue_0.3-66                
    ## [49] systemfonts_1.3.1           magick_2.9.0               
    ## [51] locfit_1.5-9.12             foreach_1.5.2              
    ## [53] glue_1.8.0                  codetools_0.2-20           
    ## [55] cowplot_1.2.0               stringi_1.8.7              
    ## [57] shape_1.4.6.1               gtable_0.3.6               
    ## [59] GenomicRanges_1.62.0        pillar_1.11.1              
    ## [61] htmltools_0.5.9             Seqinfo_1.0.0              
    ## [63] circlize_0.4.17             R6_2.6.1                   
    ## [65] textshaping_1.0.4           doParallel_1.0.17          
    ## [67] vroom_1.6.7                 evaluate_1.0.5             
    ## [69] lattice_0.22-7              png_0.1-8                  
    ## [71] class_7.3-23                Rcpp_1.1.1                 
    ## [73] SparseArray_1.10.2          DESeq2_1.50.2              
    ## [75] xfun_0.56                   MatrixGenerics_1.22.0      
    ## [77] pkgconfig_2.0.3             GlobalOptions_0.1.3
