Integrate across analyses
================
Zoe Dellaert
2026-05-17

- [Analysis of Time Series bulk RNA-seq data: Combine results from all
  analyses](#analysis-of-time-series-bulk-rna-seq-data-combine-results-from-all-analyses)
  - [1. Load packages and functions](#1-load-packages-and-functions)
  - [2. Setup species-specific parameters and define
    directories](#2-setup-species-specific-parameters-and-define-directories)
  - [3. Load in filtered counts and expression results (ImpuseDE, Mfuzz,
    WGCNA, TFBS) and SwissProt
    annotations](#3-load-in-filtered-counts-and-expression-results-impusede-mfuzz-wgcna-tfbs-and-swissprot-annotations)
  - [4. Combine results across
    analyses](#4-combine-results-across-analyses)
  - [5. Visualize comparisons and overlaps across
    analyses](#5-visualize-comparisons-and-overlaps-across-analyses)
  - [6. Save results](#6-save-results)

# Analysis of Time Series bulk RNA-seq data: Combine results from all analyses

------------------------------------------------------------------------

## 1. Load packages and functions

``` r
# set up file paths so that Rmd outputs can be viewed using github markdown
knitr::opts_knit$set(base.dir = normalizePath(paste0("../../output_RNA/reports/", params$species, "/")), base.url = "./")

knitr::opts_chunk$set(echo = TRUE, message = FALSE, fig.width = 10, fig.height = 8,
                      fig.path = "05_Integration_files/figure-gfm/")

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

## 2. Setup species-specific parameters and define directories

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
# define preprocessing and analysis output directories (from 01_preprocessing.Rmd, 02_ImpulseDE2.Rmd, 03_WGCNA.Rmd, & 04_TFBS.Rmd)
input_dir <- file.path("../../output_RNA/counts_filt_norm", species)
impulse_dir <- file.path("../../output_RNA/ImpulseDE2", species)
mfuzz_dir <- file.path(impulse_dir, "Mfuzz")
wgcna_dir <- file.path("../../output_RNA/WGCNA", species)
tfbs_dir <- file.path("../../output_RNA/TFBS", species)

# set up necessary output directories if they don't exist
outdir <- file.path("../../output_RNA/analysis_integration", species)
outdir_plots <- file.path(outdir,"plots")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
if (!dir.exists(outdir_plots)) dir.create(outdir_plots, recursive = TRUE)

reportdir <- file.path("../../output_RNA/reports", params$species, "05_Integration_files/figure-gfm/")
if (!dir.exists(reportdir)) dir.create(reportdir, recursive = TRUE)
```

## 3. Load in filtered counts and expression results (ImpuseDE, Mfuzz, WGCNA, TFBS) and SwissProt annotations

``` r
# load in filtered counts data
filtered_counts <- read.csv(file.path(input_dir, "filtered_counts.csv"), row.names = 1)
all_genes <- rownames(filtered_counts)

# ImpulseDE2 results
impulse_results <- read.csv(file.path(impulse_dir, "ImpulseDE2_results.csv"))
impulse_sig <- impulse_results %>%
  filter(padj < global_params$padj_threshold)

# Mfuzz cluster assignments
mfuzz_clusters <- read.csv(file.path(mfuzz_dir, "cluster_assignments.csv"))
mfuzz_clusters_mem50 <- mfuzz_clusters %>% filter(membership > 0.5)

# WGCNA module assignments
wgcna_modules <- read.csv(file.path(wgcna_dir, "gene_modules.csv"))
hub_genes <- read.csv(file.path(wgcna_dir, "hub_genes.csv"))

# Module interaction stats (to ID significant modules)
module_stats <- read.csv(file.path(wgcna_dir, "module_interaction_stats.csv"))
sig_modules <- module_stats %>% filter(adj.P.Val < 0.05) %>% pull(module)

# TFBS Count data
tfbs <- read.csv(file.path(tfbs_dir, "TFBS_counts.csv"))

cat("Number of genes after filtering: ", nrow(filtered_counts), "\n")
```

    ## Number of genes after filtering:  30089

``` r
cat("ImpulseDE2 significant genes:", nrow(impulse_sig), "\n")
```

    ## ImpulseDE2 significant genes: 6631

``` r
cat("Mfuzz clustered genes:", nrow(mfuzz_clusters), "\n")
```

    ## Mfuzz clustered genes: 6631

``` r
cat("Mfuzz clustered genes with membership > 0.5:", nrow(mfuzz_clusters_mem50), "\n")
```

    ## Mfuzz clustered genes with membership > 0.5: 5724

``` r
cat("WGCNA modules:", n_distinct(wgcna_modules$module), "\n")
```

    ## WGCNA modules: 32

``` r
cat("Significant modules (treatment*time):", length(sig_modules), "\n")
```

    ## Significant modules (treatment*time): 12

``` r
cat("Genes with putative HSF1, FOXO3, or NRF2 transcription factor binding sites:", nrow(tfbs), "\n")
```

    ## Genes with putative HSF1, FOXO3, or NRF2 transcription factor binding sites: 9385

``` r
# SwissProt annotations
SwissProt <- read.delim(file.path(annot_dir,config$SwissProt))
cat("Annotations:", nrow(SwissProt), "Swissprot-annotated genes")
```

    ## Annotations: 22471 Swissprot-annotated genes

``` r
#loads the pattern mapping assessed by me after running ImpulseDE2 and comparing across species (see ../../output_RNA/ImpulseDE2/cluster_patterns.md)

Mfuzz_pattern_mapping <- NULL
source("../../output_RNA/ImpulseDE2/cluster_patterns.R")

Mfuzz_pattern_mapping <- pattern_mapping %>% filter(species ==  params$species) %>% dplyr::select(-species)
```

## 4. Combine results across analyses

``` r
# combine impulseDE results, Mfuzz clusters, WGCNA modules, and TFBS quantification into one master dataframe for downstream analysis and visualization

master_table <- data.frame(gene_id=all_genes) %>%
  left_join(impulse_results %>% select(Gene, padj, response_type), by = join_by(gene_id == Gene)) %>%
  mutate(is_DE = padj < 0.05) %>%
  left_join(mfuzz_clusters %>% select(Gene, cluster, membership), by = join_by(gene_id == Gene)) %>%
  mutate(Mfuzz_highconf = membership > 0.5) %>%
  left_join(Mfuzz_pattern_mapping, by = "cluster") %>%
  left_join(wgcna_modules %>% select(gene_id, module,kME_own), by = "gene_id") %>%
  mutate(is_hub = gene_id %in% hub_genes$gene_id) %>% 
  left_join(tfbs, by = join_by(gene_id == sequence_name)) %>%
  mutate(across(starts_with("count_"), ~ replace_na(.x, 0))) %>%
  mutate(has_HSF1 = count_HSF1 > 0, has_FOXO3 = count_FOXO3 > 0, has_NFE2L2 = count_NFE2L2 > 0) %>% 
  left_join(SwissProt, by = join_by(gene_id == query)) %>%
  dplyr::rename(ImpulseDE2_padj = padj,
         ImpulseDE2_response_type = response_type,
         Mfuzz_cluster = cluster,
         Mfuzz_membership = membership,
         Mfuzz_pattern = pattern,
         WGCNA_module = module,
         SwissProt_BlastHit = blast_hit,
         SwissProt_BlastEval = evalue,
         SwissProt_ProteinName = ProteinNames)
```

## 5. Visualize comparisons and overlaps across analyses

``` r
summary_stats <- tibble(
  metric = c(
    "Total genes",
    "DE genes (padj < 0.05)",
    "Mfuzz clustered",
    "Mfuzz high confidence (membership > 0.5)",
    "WGCNA assigned",
    "Hub genes",
    "Genes with HSF1 TFBS",
    "Genes with FOXO3 TFBS",
    "Genes with NFE2L2 TFBS",
    "DE + Mfuzz clustered",
    "DE + Hub gene"
  ),
  count = c(
    nrow(master_table),
    sum(master_table$is_DE, na.rm = TRUE),
    sum(!is.na(master_table$Mfuzz_cluster)),
    sum(master_table$Mfuzz_highconf, na.rm = TRUE),
    sum(!is.na(master_table$WGCNA_module)),
    sum(master_table$is_hub),
    sum(master_table$has_HSF1),
    sum(master_table$has_FOXO3),
    sum(master_table$has_NFE2L2),
    sum(master_table$is_DE & !is.na(master_table$Mfuzz_cluster), na.rm = TRUE),
    sum(master_table$is_DE & master_table$is_hub, na.rm = TRUE)
  )
)

summary_stats %>% kable(format = "markdown")
```

| metric                                    | count |
|:------------------------------------------|------:|
| Total genes                               | 30089 |
| DE genes (padj \< 0.05)                   |  6631 |
| Mfuzz clustered                           |  6631 |
| Mfuzz high confidence (membership \> 0.5) |  5724 |
| WGCNA assigned                            | 30089 |
| Hub genes                                 |  2995 |
| Genes with HSF1 TFBS                      |  3114 |
| Genes with FOXO3 TFBS                     |  4712 |
| Genes with NFE2L2 TFBS                    |  2660 |
| DE + Mfuzz clustered                      |  6631 |
| DE + Hub gene                             |  1226 |

## 6. Save results

``` r
write.csv(master_table, file.path(outdir,"master_gene_table.csv"),row.names = FALSE)
```
