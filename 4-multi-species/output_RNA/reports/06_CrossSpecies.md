Cross Species Analysis
================
Zoe Dellaert
2026-05-17

- [Analysis of Time Series bulk RNA-seq data: Cross-Species
  Analysis](#analysis-of-time-series-bulk-rna-seq-data-cross-species-analysis)
  - [Introduction](#introduction)
  - [Inputs](#inputs)
  - [1. Load packages and functions](#1-load-packages-and-functions)
  - [2. Define directories](#2-define-directories)
  - [3. Load in ortholog data](#3-load-in-ortholog-data)
  - [4. Load in integrated expression results for all three
    species](#4-load-in-integrated-expression-results-for-all-three-species)
  - [5. Join orthologs with expression
    data](#5-join-orthologs-with-expression-data)
    - [Species-level summaries of
      orthogroups:](#species-level-summaries-of-orthogroups)
  - [6. Filter to complete 1:1:1 ortholog
    groups](#6-filter-to-complete-111-ortholog-groups)
    - [Species-level summaries of 1:1:1
      orthogroups:](#species-level-summaries-of-111-orthogroups)

# Analysis of Time Series bulk RNA-seq data: Cross-Species Analysis

## Introduction

This analysis integrates expression data across three coral species
(*Pocillopora acuta*, *Montipora capitata*, and *Porites compressa*)
using orthologous relationships identified using the program
[Broccoli](https://github.com/rderelle/Broccoli.git). In my annotation
[repository](https://github.com/zdellaert/HI_genome_annotations.git)
(read about this is my
[README.md](https://github.com/zdellaert/TimeSeries/blob/main/4-multi-species/code/RNA/README.md)),
I identified orthologous genes between these genomes, which I will use
to compare expression patterns across species.

## Inputs

| Source | File |
|----|----|
| Cross-Species Orthologs | `HI_genome_annotations/annotation/orthologs_3sp.csv` |
| Per-species | `output_RNA/analysis_integration/{species}/master_gene_table.csv` |

------------------------------------------------------------------------

## 1. Load packages and functions

``` r
# set up file paths so that Rmd outputs can be viewed using github markdown
knitr::opts_knit$set(base.dir = normalizePath("../../output_RNA/reports/"), base.url = "./")

knitr::opts_chunk$set(echo = TRUE, message = FALSE, fig.width = 10, fig.height = 8,
                      fig.path = "06_CrossSpecies_files/figure-gfm/")

#load packages
library(tidyverse)
library(knitr)

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
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] knitr_1.50      lubridate_1.9.4 forcats_1.0.0   stringr_1.6.0  
    ##  [5] dplyr_1.1.4     purrr_1.2.1     readr_2.1.6     tidyr_1.3.1    
    ##  [9] tibble_3.3.0    ggplot2_4.0.1   tidyverse_2.0.0 rmarkdown_2.30 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.6       compiler_4.5.1     tidyselect_1.2.1   dichromat_2.0-0.1 
    ##  [5] scales_1.4.0       yaml_2.3.12        fastmap_1.2.0      R6_2.6.1          
    ##  [9] generics_0.1.4     pillar_1.11.1      RColorBrewer_1.1-3 tzdb_0.5.0        
    ## [13] rlang_1.2.0        stringi_1.8.7      xfun_0.56          S7_0.2.1          
    ## [17] timechange_0.3.0   cli_3.6.5          withr_3.0.2        magrittr_2.0.4    
    ## [21] digest_0.6.39      grid_4.5.1         rstudioapi_0.17.1  hms_1.1.4         
    ## [25] lifecycle_1.0.5    vctrs_0.7.0        evaluate_1.0.5     glue_1.8.0        
    ## [29] farver_2.1.2       tools_4.5.1        pkgconfig_2.0.3    htmltools_0.5.9

## 2. Define directories

``` r
# set up necessary output directories if they don't exist
outdir <- file.path("../../output_RNA/cross_species")
outdir_plots <- file.path(outdir,"plots")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
if (!dir.exists(outdir_plots)) dir.create(outdir_plots, recursive = TRUE)

reportdir <- "../../output_RNA/reports/06_CrossSpecies_files/figure-gfm/"
if (!dir.exists(reportdir)) dir.create(reportdir, recursive = TRUE)
```

## 3. Load in ortholog data

``` r
orthologs <- read_csv(file.path(annot_dir,"orthologs_3sp.csv"))

orthologs %>%
  group_by(species) %>%
  summarise(
    n_genes = n(),
    n_in_all_3 = sum(OG_in_all_3),
    pct_in_all_3 = 100*mean(OG_in_all_3),
    n_1to1to1 = sum(OG_1to1to1),
    pct_1to1to1 = 100*mean(OG_1to1to1),
    .groups = 'drop'
  ) %>% kable(format = "markdown")
```

| species | n_genes | n_in_all_3 | pct_in_all_3 | n_1to1to1 | pct_1to1to1 |
|:--------|--------:|-----------:|-------------:|----------:|------------:|
| Mcap    |   43030 |      32767 |     76.14920 |      8877 |    20.62979 |
| Pacuta  |   27546 |      23018 |     83.56204 |      8877 |    32.22609 |
| Pcomp   |   35131 |      26683 |     75.95286 |      8877 |    25.26828 |

## 4. Load in integrated expression results for all three species

``` r
for (species in species_list){
  integration_dir <- file.path("../../output_RNA/analysis_integration", species)
  master_table <- read.csv(file.path(integration_dir, "master_gene_table.csv")) %>% mutate(species=species)
  
  assign(paste0(species,"_master"),master_table)
  rm(master_table)
}

tables <- paste0(species_list,"_master")

# Combine
all_master <- bind_rows(Pacuta_master,Mcap_master,Pcomp_master)

cat("Genes per species:\n")
```

    ## Genes per species:

``` r
print(all_master %>% count(species))
```

    ##   species     n
    ## 1    Mcap 30089
    ## 2  Pacuta 24941
    ## 3   Pcomp 27492

## 5. Join orthologs with expression data

``` r
cross_species <- all_master %>%
  left_join(orthologs, by = c("gene_id", "species"))

write_csv(cross_species, file.path(outdir, "cross_species_integrated_results.csv"))
```

### Species-level summaries of orthogroups:

- n_genes = how many genes for this species are represented in the
  cross_species dataset (should be the same number as in
  master_gene_table.csv for each species since we are only adding
  orthogroup data for genes we have expression data for)
- n_in_OG = of these genes, how many are in an orthogroup
- n_in_all_3 = of these genes, how many are in an orthogroup that has
  genes from all three species in it
- n_1to1to1 = of these genes, how many are in an orthogroup that has
  exactly one gene from all three species in it (though, all of these
  might not be in this table if one of the species had this gene
  filtered out during expression filtering)

``` r
# quick summary
cross_species %>%
  group_by(species) %>%
  summarise(
    n_genes = n(),
    n_in_OG = sum(!is.na(OG)),
    pct_in_OG = 100*mean(!is.na(OG)),
    n_in_all_3 = sum(OG_in_all_3, na.rm = TRUE),
    pct_in_all_3 = 100*mean(OG_in_all_3, na.rm = TRUE),
    n_1to1to1 = sum(OG_1to1to1, na.rm = TRUE),
    pct_1to1to1 = 100*mean(OG_1to1to1, na.rm = TRUE)
  ) %>% kable(format = "markdown")
```

| species | n_genes | n_in_OG | pct_in_OG | n_in_all_3 | pct_in_all_3 | n_1to1to1 | pct_1to1to1 |
|:--------|--------:|--------:|----------:|-----------:|-------------:|----------:|------------:|
| Mcap    |   30089 |   26102 |  86.74931 |      21825 |     83.61428 |      8694 |    33.30779 |
| Pacuta  |   24941 |   21389 |  85.75839 |      18268 |     85.40839 |      8683 |    40.59563 |
| Pcomp   |   27492 |   23869 |  86.82162 |      19397 |     81.26440 |      8551 |    35.82471 |

## 6. Filter to complete 1:1:1 ortholog groups

Get list of one-to-one-to-one orthogroups for which all three species
are represented in the *filtered* data

``` r
complete_1to1_ogs <- cross_species %>%
  filter(OG_1to1to1) %>%
  group_by(OG) %>%
  filter(n_distinct(species) == 3) %>%
  ungroup()

write_csv(complete_1to1_ogs, file.path(outdir, "filtered_orthogroups_1to1to1.csv"))
```

### Species-level summaries of 1:1:1 orthogroups:

- n_genes = how many genes for this species are represented in the 1:1:1
  ortholog dataset (post-expression filtering)
- n_DE = of these genes, how many are DE by ImpulseDE2
- n_in_clusters = of these genes, how many showed membership \> 0.5 in
  their assigned Mfuzz cluster
- n_hubs = of these genes, how many are WGCNA hub genes

``` r
# quick summary
complete_1to1_ogs %>%
  group_by(species) %>%
  summarise(
    n_genes = n(),
    n_DE = sum(is_DE, na.rm = TRUE),
    pct_DE = 100*mean(is_DE, na.rm = TRUE),
    n_in_clusters = sum(Mfuzz_highconf, na.rm = TRUE),
    n_hubs = sum(is_hub, na.rm = TRUE),
    .groups = 'drop'
  ) %>% kable(format = "markdown")
```

| species | n_genes | n_DE |   pct_DE | n_in_clusters | n_hubs |
|:--------|--------:|-----:|---------:|--------------:|-------:|
| Mcap    |    8460 | 2849 | 33.67612 |          2496 |    903 |
| Pacuta  |    8460 | 4022 | 47.54137 |          3301 |    955 |
| Pcomp   |    8460 | 2447 | 28.92435 |          2124 |    779 |
