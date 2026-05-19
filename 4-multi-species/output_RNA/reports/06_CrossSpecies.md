Cross Species Analysis
================
Zoe Dellaert
2026-05-19

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
  - [7. Conserved responses?](#7-conserved-responses)
  - [8. Heat stress genes](#8-heat-stress-genes)

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
    ## [1] grid      stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] ComplexHeatmap_2.26.0 knitr_1.50            lubridate_1.9.4      
    ##  [4] forcats_1.0.0         stringr_1.6.0         dplyr_1.1.4          
    ##  [7] purrr_1.2.1           readr_2.1.6           tidyr_1.3.1          
    ## [10] tibble_3.3.0          ggplot2_4.0.1         tidyverse_2.0.0      
    ## [13] rmarkdown_2.30       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.6        circlize_0.4.17     shape_1.4.6.1      
    ##  [4] rjson_0.2.23        xfun_0.56           GlobalOptions_0.1.3
    ##  [7] tzdb_0.5.0          Cairo_1.7-0         vctrs_0.7.0        
    ## [10] tools_4.5.1         generics_0.1.4      stats4_4.5.1       
    ## [13] parallel_4.5.1      cluster_2.1.8.1     pkgconfig_2.0.3    
    ## [16] RColorBrewer_1.1-3  S7_0.2.1            S4Vectors_0.48.0   
    ## [19] lifecycle_1.0.5     compiler_4.5.1      farver_2.1.2       
    ## [22] textshaping_1.0.4   codetools_0.2-20    clue_0.3-66        
    ## [25] htmltools_0.5.9     yaml_2.3.12         pillar_1.11.1      
    ## [28] crayon_1.5.3        magick_2.9.0        iterators_1.0.14   
    ## [31] foreach_1.5.2       tidyselect_1.2.1    digest_0.6.39      
    ## [34] stringi_1.8.7       labeling_0.4.3      fastmap_1.2.0      
    ## [37] colorspace_2.1-2    cli_3.6.5           magrittr_2.0.4     
    ## [40] dichromat_2.0-0.1   withr_3.0.2         scales_1.4.0       
    ## [43] bit64_4.6.0-1       timechange_0.3.0    matrixStats_1.5.0  
    ## [46] bit_4.6.0           ragg_1.5.0          png_0.1-8          
    ## [49] GetoptLong_1.1.0    hms_1.1.4           evaluate_1.0.5     
    ## [52] IRanges_2.44.0      doParallel_1.0.17   rlang_1.2.0        
    ## [55] Rcpp_1.1.1          glue_1.8.0          BiocGenerics_0.56.0
    ## [58] rstudioapi_0.17.1   vroom_1.6.7         R6_2.6.1           
    ## [61] systemfonts_1.3.1

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
| Mcap    |   42992 |      32767 |     76.21651 |      8889 |    20.67594 |
| Pacuta  |   27568 |      23038 |     83.56790 |      8889 |    32.24391 |
| Pcomp   |   35140 |      26674 |     75.90780 |      8889 |    25.29596 |

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
| Mcap    |   30089 |   26066 |  86.62967 |      21813 |     83.68373 |      8703 |    33.38832 |
| Pacuta  |   24941 |   21380 |  85.72230 |      18235 |     85.28999 |      8693 |    40.65949 |
| Pcomp   |   27492 |   23885 |  86.87982 |      19387 |     81.16810 |      8562 |    35.84677 |

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
complete_1to1_ogs_summary <- complete_1to1_ogs %>%
  group_by(species) %>%
  summarise(
    n_genes = n(),
    n_DE = sum(is_DE, na.rm = TRUE),
    pct_DE = 100*mean(is_DE, na.rm = TRUE),
    n_in_clusters = sum(Mfuzz_highconf, na.rm = TRUE),
    n_hubs = sum(is_hub, na.rm = TRUE),
    .groups = 'drop'
  )

kable(complete_1to1_ogs_summary,format = "markdown")
```

| species | n_genes | n_DE |   pct_DE | n_in_clusters | n_hubs |
|:--------|--------:|-----:|---------:|--------------:|-------:|
| Mcap    |    8471 | 2851 | 33.65600 |          2498 |    904 |
| Pacuta  |    8471 | 4028 | 47.55047 |          3304 |    954 |
| Pcomp   |    8471 | 2452 | 28.94582 |          2129 |    782 |

## 7. Conserved responses?

``` r
sus_up_3hr <- cross_species %>% filter(Mfuzz_pattern == "Sustained Up (3h)")

# ortholog summary
sus_up_3hr %>% group_by(species) %>%
    summarise(
    n_genes = n(),
    n_in_OG = sum(!is.na(OG)),
    pct_in_OG = 100*mean(!is.na(OG)),
    n_in_all_3 = sum(OG_in_all_3, na.rm = TRUE),
    pct_in_all_3 = 100*mean(OG_in_all_3, na.rm = TRUE),
    n_1to1to1 = sum(OG_1to1to1, na.rm = TRUE),
    pct_1to1to1 = 100*mean(OG_1to1to1, na.rm = TRUE)
  )# %>% kable(format = "markdown")
```

    ## # A tibble: 3 × 8
    ##   species n_genes n_in_OG pct_in_OG n_in_all_3 pct_in_all_3 n_1to1to1
    ##   <chr>     <int>   <int>     <dbl>      <int>        <dbl>     <int>
    ## 1 Mcap        963     889      92.3        810         91.1       464
    ## 2 Pacuta     1610    1442      89.6       1279         88.7       636
    ## 3 Pcomp      1260    1181      93.7       1051         89.0       638
    ## # ℹ 1 more variable: pct_1to1to1 <dbl>

``` r
sus_up_3hr %>% group_by(species) %>%
    summarise(
    n_genes = n(),
    n_DE = sum(is_DE, na.rm = TRUE),
    pct_DE = 100*mean(is_DE, na.rm = TRUE),
    Mfuzz_cluster = paste0(unique(Mfuzz_cluster, na.rm = TRUE)),
    n_high_conf = sum(Mfuzz_highconf, na.rm = TRUE),
    pct_high_conf = 100*mean(Mfuzz_highconf, na.rm = TRUE),
    n_Modules = length(unique(na.omit(WGCNA_module))),
    largest_module = names(table(WGCNA_module))[which.max(table(WGCNA_module))],
    n_hubs = sum(is_hub, na.rm = TRUE),
    has_HSF1 = sum(has_HSF1, na.rm = TRUE),
    has_FOXO3 = sum(has_FOXO3, na.rm = TRUE),
    has_NFE2L2 = sum(has_NFE2L2, na.rm = TRUE),
    .groups = 'drop'
  )# %>% kable(format = "markdown")
```

    ## # A tibble: 3 × 13
    ##   species n_genes  n_DE pct_DE Mfuzz_cluster n_high_conf pct_high_conf n_Modules
    ##   <chr>     <int> <int>  <dbl> <chr>               <int>         <dbl>     <int>
    ## 1 Mcap        963   963    100 3                     838          87.0        18
    ## 2 Pacuta     1610  1610    100 4                    1255          78.0        19
    ## 3 Pcomp      1260  1260    100 5                    1140          90.5        13
    ## # ℹ 5 more variables: largest_module <chr>, n_hubs <int>, has_HSF1 <int>,
    ## #   has_FOXO3 <int>, has_NFE2L2 <int>

``` r
sus_up_12hr <- cross_species %>% filter(Mfuzz_pattern == "Sustained Up (12h)")

# ortholog summary
sus_up_12hr %>% group_by(species) %>%
    summarise(
    n_genes = n(),
    n_in_OG = sum(!is.na(OG)),
    pct_in_OG = 100*mean(!is.na(OG)),
    n_in_all_3 = sum(OG_in_all_3, na.rm = TRUE),
    pct_in_all_3 = 100*mean(OG_in_all_3, na.rm = TRUE),
    n_1to1to1 = sum(OG_1to1to1, na.rm = TRUE),
    pct_1to1to1 = 100*mean(OG_1to1to1, na.rm = TRUE)
  )# %>% kable(format = "markdown")
```

    ## # A tibble: 3 × 8
    ##   species n_genes n_in_OG pct_in_OG n_in_all_3 pct_in_all_3 n_1to1to1
    ##   <chr>     <int>   <int>     <dbl>      <int>        <dbl>     <int>
    ## 1 Mcap       1096    1011      92.2        916         90.6       510
    ## 2 Pacuta     1563    1443      92.3       1319         91.4       845
    ## 3 Pcomp      1074     958      89.2        801         83.6       400
    ## # ℹ 1 more variable: pct_1to1to1 <dbl>

``` r
sus_up_12hr %>% group_by(species) %>%
    summarise(
    n_genes = n(),
    n_DE = sum(is_DE, na.rm = TRUE),
    pct_DE = 100*mean(is_DE, na.rm = TRUE),
    Mfuzz_cluster = paste0(unique(Mfuzz_cluster, na.rm = TRUE)),
    n_high_conf = sum(Mfuzz_highconf, na.rm = TRUE),
    pct_high_conf = 100*mean(Mfuzz_highconf, na.rm = TRUE),
    n_Modules = length(unique(na.omit(WGCNA_module))),
    largest_module = names(table(WGCNA_module))[which.max(table(WGCNA_module))],
    n_hubs = sum(is_hub, na.rm = TRUE),
    has_HSF1 = sum(has_HSF1, na.rm = TRUE),
    has_FOXO3 = sum(has_FOXO3, na.rm = TRUE),
    has_NFE2L2 = sum(has_NFE2L2, na.rm = TRUE),
    .groups = 'drop'
  )# %>% kable(format = "markdown")
```

    ## # A tibble: 3 × 13
    ##   species n_genes  n_DE pct_DE Mfuzz_cluster n_high_conf pct_high_conf n_Modules
    ##   <chr>     <int> <int>  <dbl> <chr>               <int>         <dbl>     <int>
    ## 1 Mcap       1096  1096    100 2                     946          86.3        23
    ## 2 Pacuta     1563  1563    100 6                    1208          77.3        18
    ## 3 Pcomp      1074  1074    100 6                     953          88.7        17
    ## # ℹ 5 more variables: largest_module <chr>, n_hubs <int>, has_HSF1 <int>,
    ## #   has_FOXO3 <int>, has_NFE2L2 <int>

``` r
grad_down <- cross_species %>% filter(Mfuzz_pattern == "Gradual Down")

# ortholog summary
grad_down %>% group_by(species) %>%
    summarise(
    n_genes = n(),
    n_in_OG = sum(!is.na(OG)),
    pct_in_OG = 100*mean(!is.na(OG)),
    n_in_all_3 = sum(OG_in_all_3, na.rm = TRUE),
    pct_in_all_3 = 100*mean(OG_in_all_3, na.rm = TRUE),
    n_1to1to1 = sum(OG_1to1to1, na.rm = TRUE),
    pct_1to1to1 = 100*mean(OG_1to1to1, na.rm = TRUE)
  )# %>% kable(format = "markdown")
```

    ## # A tibble: 3 × 8
    ##   species n_genes n_in_OG pct_in_OG n_in_all_3 pct_in_all_3 n_1to1to1
    ##   <chr>     <int>   <int>     <dbl>      <int>        <dbl>     <int>
    ## 1 Mcap       1294    1127      87.1        949         84.2       542
    ## 2 Pacuta     1733    1422      82.1       1152         81.0       561
    ## 3 Pcomp       784     656      83.7        521         79.4       270
    ## # ℹ 1 more variable: pct_1to1to1 <dbl>

``` r
grad_down %>% group_by(species) %>%
    summarise(
    n_genes = n(),
    n_DE = sum(is_DE, na.rm = TRUE),
    pct_DE = 100*mean(is_DE, na.rm = TRUE),
    Mfuzz_cluster = paste0(unique(Mfuzz_cluster, na.rm = TRUE)),
    n_high_conf = sum(Mfuzz_highconf, na.rm = TRUE),
    pct_high_conf = 100*mean(Mfuzz_highconf, na.rm = TRUE),
    n_Modules = length(unique(na.omit(WGCNA_module))),
    largest_module = names(table(WGCNA_module))[which.max(table(WGCNA_module))],
    n_hubs = sum(is_hub, na.rm = TRUE),
    has_HSF1 = sum(has_HSF1, na.rm = TRUE),
    has_FOXO3 = sum(has_FOXO3, na.rm = TRUE),
    has_NFE2L2 = sum(has_NFE2L2, na.rm = TRUE),
    .groups = 'drop'
  )# %>% kable(format = "markdown")
```

    ## # A tibble: 3 × 13
    ##   species n_genes  n_DE pct_DE Mfuzz_cluster n_high_conf pct_high_conf n_Modules
    ##   <chr>     <int> <int>  <dbl> <chr>               <int>         <dbl>     <int>
    ## 1 Mcap       1294  1294    100 6                    1130          87.3        24
    ## 2 Pacuta     1733  1733    100 5                    1392          80.3        15
    ## 3 Pcomp       784   784    100 1                     641          81.8        19
    ## # ℹ 5 more variables: largest_module <chr>, n_hubs <int>, has_HSF1 <int>,
    ## #   has_FOXO3 <int>, has_NFE2L2 <int>

## 8. Heat stress genes

``` r
for (species in species_list){
  HeatStressGenes <- read_csv(paste0(annot_dir,"/heatstress/HeatStressGenes_", species ,".csv")) %>%
    dplyr::select(-1) %>%
    dplyr::rename(query = paste0(species,"_gene")) %>%
    dplyr::select(query,everything())
  
  # rename columns
  HeatStressGenes <- HeatStressGenes %>%
    dplyr::rename(ref_species=species) %>% 
    dplyr::rename(gene_sym=gene_id) %>% 
    dplyr::rename(gene_id=query)
  
  # keep only expressed genes
  HeatStressGenes <- HeatStressGenes %>% filter(gene_id %in% all_master$gene_id)

  assign(paste0("heatstress_",species),HeatStressGenes)
  
  HeatStressGenes_unique <- HeatStressGenes %>% group_by(gene_id) %>%
  summarize(gene_sym = paste(unique(gene_sym), collapse = ","),
            response_type = paste(unique(response_type), collapse = ","),
            category = paste(unique(category), collapse = ",")
            ) 
  
  assign(paste0("heatstress_unique_",species),HeatStressGenes_unique)
  rm(HeatStressGenes)
  rm(HeatStressGenes_unique)
}

Pacuta_expanded_sp <- heatstress_unique_Pacuta %>% select(gene_id, gene_sym) %>%
  left_join(orthologs %>% select(gene_id,OG), by="gene_id") %>% 
  filter(!is.na(OG))  %>%  select(-gene_id) %>% left_join(cross_species, by="OG")  %>% distinct()
```

    ## Warning in left_join(., cross_species, by = "OG"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 1817 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
Mcap_expanded_sp <- heatstress_unique_Mcap %>% select(gene_id, gene_sym) %>%
  left_join(orthologs %>% select(gene_id,OG), by="gene_id") %>% 
  filter(!is.na(OG))  %>%  select(-gene_id) %>% left_join(cross_species, by="OG")  %>% distinct()
```

    ## Warning in left_join(., cross_species, by = "OG"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 4449 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
Pcomp_expanded_sp <- heatstress_unique_Pcomp %>% select(gene_id, gene_sym) %>%
  left_join(orthologs %>% select(gene_id,OG), by="gene_id") %>% 
  filter(!is.na(OG))  %>%  select(-gene_id) %>% left_join(cross_species, by="OG")  %>% distinct()
```

    ## Warning in left_join(., cross_species, by = "OG"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 79352 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
unique(Pacuta_expanded_sp$gene_id.x)[!(unique(Pacuta_expanded_sp$gene_id.x) %in% unique(Pcomp_expanded_sp$gene_id.y))]
```

    ## Warning: Unknown or uninitialised column: `gene_id.x`.

    ## Warning: Unknown or uninitialised column: `gene_id.x`.

    ## Warning: Unknown or uninitialised column: `gene_id.y`.

    ## NULL

``` r
all_heat <- bind_rows(Pacuta_expanded_sp,Mcap_expanded_sp,Pcomp_expanded_sp) %>% distinct()

all_heat %>% group_by(species,gene_sym) %>%
  summarize(n=n())  %>% pivot_wider(names_from = species, values_from = n) %>% kable(format = "markdown")
```

| gene_sym        | Mcap | Pacuta | Pcomp |
|:----------------|-----:|-------:|------:|
| ACAN            |   70 |     26 |    11 |
| ALDH3B1         |    1 |      1 |     1 |
| ALOX12B         |    6 |      5 |    12 |
| AMPK            |    1 |      1 |     1 |
| AMT1            |    2 |      1 |     2 |
| APAF-1          |    1 |      1 |     1 |
| Alox5           |    1 |      1 |     1 |
| BAK             |    1 |      1 |     1 |
| BAX             |    1 |      1 |     1 |
| BI-1            |    1 |      1 |     1 |
| Bcl-2           |    3 |      3 |     3 |
| CAT             |    1 |      1 |     1 |
| Cas3,Casp       |    2 |      2 |     1 |
| DNaj            |    1 |      2 |     1 |
| Foxo3           |    1 |      1 |     1 |
| GDH             |    1 |      1 |     1 |
| GOGAT           |    1 |      1 |     1 |
| GR              |    1 |      1 |     1 |
| GS              |    2 |      1 |     1 |
| Glut8,SLC2A8    |    1 |      1 |     1 |
| HSF1            |    1 |      1 |     1 |
| HSP70,Hsc71     |    1 |      1 |     2 |
| HSP90           |    2 |      1 |     1 |
| NF-KB           |    3 |      1 |     2 |
| NID-2           |   45 |     27 |    23 |
| NPC2            |    3 |      3 |     3 |
| Nrf2,Nrf1       |    1 |      1 |     1 |
| OGG1            |    1 |      1 |     1 |
| PMP34           |    1 |      1 |     1 |
| RhBG            |    1 |      1 |     1 |
| SLC2A8,Glut8    |    1 |      1 |     1 |
| ST1C2           |    9 |     18 |    21 |
| Survivin        |    1 |      1 |     1 |
| TGFa            |    2 |      1 |    NA |
| TNFRSF27,TNFR27 |    4 |      1 |     2 |
| VIT             |    1 |      1 |     6 |
| mTOR            |    1 |      3 |     1 |
| Casp7           |   NA |     NA |     2 |
| NF-KB1          |   NA |     NA |     2 |

``` r
summary <- all_heat %>% group_by(species,gene_sym) %>%
  summarize(n=n(),
            n_DE = sum(is_DE, na.rm = TRUE),
            pct_DE = 100*mean(is_DE, na.rm = TRUE),
    n_in_clusters = sum(Mfuzz_highconf, na.rm = TRUE),
    n_hubs = sum(is_hub, na.rm = TRUE)) 
```
