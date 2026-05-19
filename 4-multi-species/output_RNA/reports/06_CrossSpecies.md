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
  - [7. Core response in 1:1:1
    orthologs](#7-core-response-in-111-orthologs)
    - [Define response categories for 1:1:1
      orthologs](#define-response-categories-for-111-orthologs)
    - [Are TFBS conserved across DE 1:1:1
      orthologs?](#are-tfbs-conserved-across-de-111-orthologs)
  - [8. Non 1:1:1 conserved responses?](#8-non-111-conserved-responses)
  - [9. Manually-curated heat stress
    genes](#9-manually-curated-heat-stress-genes)

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
library(ComplexHeatmap)

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
    ##  [1] knitr_1.50            Mfuzz_2.68.0          DynDoc_1.86.0        
    ##  [4] widgetTools_1.86.0    e1071_1.7-16          Biobase_2.70.0       
    ##  [7] BiocGenerics_0.56.0   generics_0.1.4        pheatmap_1.0.13      
    ## [10] ComplexHeatmap_2.26.0 lubridate_1.9.4       forcats_1.0.0        
    ## [13] stringr_1.6.0         dplyr_1.1.4           purrr_1.2.1          
    ## [16] readr_2.1.6           tidyr_1.3.1           tibble_3.3.0         
    ## [19] ggplot2_4.0.1         tidyverse_2.0.0       ImpulseDE2_0.99.10   
    ## [22] rmarkdown_2.30       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1            farver_2.1.2               
    ##  [3] S7_0.2.1                    fastmap_1.2.0              
    ##  [5] digest_0.6.39               timechange_0.3.0           
    ##  [7] lifecycle_1.0.5             cluster_2.1.8.1            
    ##  [9] Cairo_1.7-0                 magrittr_2.0.4             
    ## [11] compiler_4.5.1              tkWidgets_1.86.0           
    ## [13] rlang_1.2.0                 tools_4.5.1                
    ## [15] yaml_2.3.12                 labeling_0.4.3             
    ## [17] S4Arrays_1.10.0             bit_4.6.0                  
    ## [19] DelayedArray_0.36.0         RColorBrewer_1.1-3         
    ## [21] abind_1.4-8                 BiocParallel_1.44.0        
    ## [23] withr_3.0.2                 stats4_4.5.1               
    ## [25] colorspace_2.1-2            scales_1.4.0               
    ## [27] iterators_1.0.14            dichromat_2.0-0.1          
    ## [29] SummarizedExperiment_1.40.0 cli_3.6.5                  
    ## [31] crayon_1.5.3                ragg_1.5.0                 
    ## [33] rstudioapi_0.17.1           tzdb_0.5.0                 
    ## [35] rjson_0.2.23                proxy_0.4-27               
    ## [37] parallel_4.5.1              XVector_0.50.0             
    ## [39] matrixStats_1.5.0           vctrs_0.7.0                
    ## [41] Matrix_1.6-4                IRanges_2.44.0             
    ## [43] GetoptLong_1.1.0            hms_1.1.4                  
    ## [45] S4Vectors_0.48.0            bit64_4.6.0-1              
    ## [47] clue_0.3-66                 systemfonts_1.3.1          
    ## [49] magick_2.9.0                locfit_1.5-9.12            
    ## [51] foreach_1.5.2               glue_1.8.0                 
    ## [53] codetools_0.2-20            cowplot_1.2.0              
    ## [55] stringi_1.8.7               shape_1.4.6.1              
    ## [57] gtable_0.3.6                GenomicRanges_1.62.0       
    ## [59] pillar_1.11.1               htmltools_0.5.9            
    ## [61] Seqinfo_1.0.0               circlize_0.4.17            
    ## [63] R6_2.6.1                    textshaping_1.0.4          
    ## [65] doParallel_1.0.17           vroom_1.6.7                
    ## [67] evaluate_1.0.5              lattice_0.22-7             
    ## [69] png_0.1-8                   class_7.3-23               
    ## [71] Rcpp_1.1.1                  SparseArray_1.10.2         
    ## [73] DESeq2_1.50.2               xfun_0.56                  
    ## [75] MatrixGenerics_1.22.0       pkgconfig_2.0.3            
    ## [77] GlobalOptions_0.1.3

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
sp_summary_1to1_ogs <- complete_1to1_ogs %>%
  group_by(species) %>%
  summarise(
    n_genes = n(),
    n_DE = sum(is_DE, na.rm = TRUE),
    pct_DE = 100*mean(is_DE, na.rm = TRUE),
    n_in_clusters = sum(Mfuzz_highconf, na.rm = TRUE),
    n_hubs = sum(is_hub, na.rm = TRUE),
    .groups = 'drop'
  )

kable(sp_summary_1to1_ogs,format = "markdown")
```

| species | n_genes | n_DE |   pct_DE | n_in_clusters | n_hubs |
|:--------|--------:|-----:|---------:|--------------:|-------:|
| Mcap    |    8471 | 2851 | 33.65600 |          2498 |    904 |
| Pacuta  |    8471 | 4028 | 47.55047 |          3304 |    954 |
| Pcomp   |    8471 | 2452 | 28.94582 |          2129 |    782 |

#### Upset plot: 1:1:1 orthologs DE across species

Upset plot from ComplexHeatmap package, documentation
[here](https://github.com/jokergoo/ComplexHeatmap-reference/blob/master/book/08-upset.md)

``` r
de_matrix <- complete_1to1_ogs %>%
  select(OG, species, is_DE) %>%
  mutate(is_DE = replace_na(is_DE, FALSE)) %>%
  pivot_wider(names_from = species, values_from = is_DE, values_fill = FALSE) %>%
  column_to_rownames("OG") %>%
  as.matrix()

m <- make_comb_mat(de_matrix)
plot <- UpSet(m, pt_size = unit(5, "mm"), lwd = 3,
      top_annotation = upset_top_annotation(m, add_numbers = TRUE),
      right_annotation = upset_right_annotation(m, add_numbers = TRUE),
      comb_col = c("gray","#00A087","#4DBBD5","#E64B35")[comb_degree(m)+1],
      column_title = "Differential Expression of 1:1:1 Orthologs")

print(plot)
```

![](./06_CrossSpecies_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
pdf(file.path(outdir_plots, "DE_overlap_upset.pdf"), width=10, height=6)
  print(plot)
dev.off()
```

    ## png 
    ##   2

``` r
png(file.path(outdir_plots, "DE_overlap_upset.png"), width=10, height=6, units="in", res=300)
  print(plot)
dev.off()
```

    ## png 
    ##   2

## 7. Core response in 1:1:1 orthologs

This does not include any genes that have more than one paralog in a
species.

``` r
complete_1to1_ogs_summary <- complete_1to1_ogs %>%
  group_by(OG) %>%
  arrange(SwissProt_BlastEval, .by_group = TRUE) %>% 
  summarise(
    # how many of the which 1:1:1 orthologs are differentially expressed in all three species
    n_DE_species = sum(is_DE, na.rm = TRUE),
    all_species_DE = n_DE_species == 3,
    
    # what mfuzz patterns are represented
    Mfuzz_patterns = paste(sort(unique(na.omit(Mfuzz_pattern))), collapse = " | "),
    Mfuzz_patterns_unique = length(unique(na.omit(Mfuzz_pattern))),
    
    # get annotation info (we pre-sorted by evalue above, so the best annotation will be chosen)
    annotation = first(na.omit(SwissProt_ProteinName)),
    annotation_evalue = first(na.omit(SwissProt_BlastEval)),
    has_annotation = !all(is.na(SwissProt_ProteinName)),
    
    # list the gene for each species (again, these are only OGs where there is exactly one gene per species)
    gene_Pacuta = gene_id[species == "Pacuta"],
    gene_Mcap = gene_id[species == "Mcap"],
    gene_Pcomp = gene_id[species == "Pcomp"],
    
    # and also gather details for each species to go along with the summary info above
    # actual impulseDE2 p-values
    padj_Pacuta = ImpulseDE2_padj[species == "Pacuta"],
    padj_Mcap = ImpulseDE2_padj[species == "Mcap"],
    padj_Pcomp = ImpulseDE2_padj[species == "Pcomp"],
    
    # Mfuzz membership
    MfuzzM_Pacuta = Mfuzz_membership[species == "Pacuta"],
    MfuzzM_Mcap = Mfuzz_membership[species == "Mcap"],
    MfuzzM_Pcomp = Mfuzz_membership[species == "Pcomp"],
    
    # are any members of the 1:1:1 OG hub genes of their WGCNA module
    n_species_hub = sum(is_hub, na.rm = TRUE),
    hub_in_any = n_species_hub > 0,
    hub_in_all = n_species_hub == 3,
    
    # do any of the 1:1:1 OG genes have TF binding sites and are they conserved across species?
    n_species_HSF1 = sum(has_HSF1, na.rm = TRUE),
    n_species_FOXO3 = sum(has_FOXO3, na.rm = TRUE),
    n_species_NFE2L2 = sum(has_NFE2L2, na.rm = TRUE),
    HSF1_conserved = n_species_HSF1 == 3,
    FOXO3_conserved = n_species_FOXO3 == 3,
    NFE2L2_conserved = n_species_NFE2L2 == 3,
    any_TF_conserved = HSF1_conserved | FOXO3_conserved | NFE2L2_conserved,
    
    .groups = 'drop'
  ) %>%
  #finally, sort by some columns of interest so that the top rows will show 1:1:1 orthologs which are all DE, share an Mfuzz pattern, and show potential transcription factor binding site conservation
  arrange(desc(all_species_DE), Mfuzz_patterns_unique, desc(any_TF_conserved))

kable(head(complete_1to1_ogs_summary),format = "markdown")
```

| OG | n_DE_species | all_species_DE | Mfuzz_patterns | Mfuzz_patterns_unique | annotation | annotation_evalue | has_annotation | gene_Pacuta | gene_Mcap | gene_Pcomp | padj_Pacuta | padj_Mcap | padj_Pcomp | MfuzzM_Pacuta | MfuzzM_Mcap | MfuzzM_Pcomp | n_species_hub | hub_in_any | hub_in_all | n_species_HSF1 | n_species_FOXO3 | n_species_NFE2L2 | HSF1_conserved | FOXO3_conserved | NFE2L2_conserved | any_TF_conserved |
|:---|---:|:---|:---|---:|:---|---:|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|:---|:---|---:|---:|---:|:---|:---|:---|:---|
| OG_12811 | 3 | TRUE | Gradual Down | 1 | NA | NA | FALSE | Pocillopora_acuta_HIv2\_\_\_RNAseq.g11037.t1 | Montipora_capitata_HIv3\_\_\_RNAseq.g20746.t1 | Porites_compressa_HIv1\_\_\_RNAseq.g3627.t1 | 0.0000001 | 0.0000206 | 0.0000000 | 0.4061767 | 0.9833159 | 0.9527887 | 2 | TRUE | FALSE | 0 | 3 | 0 | FALSE | TRUE | FALSE | TRUE |
| OG_3443 | 3 | TRUE | Sustained Up (3h) | 1 | Protein DDI1 homolog 2 (EC 3.4.23.-) | 0 | TRUE | Pocillopora_acuta_HIv2\_\_\_RNAseq.g30800.t1 | Montipora_capitata_HIv3\_\_\_RNAseq.g30166.t1 | Porites_compressa_HIv1\_\_\_RNAseq.g624.t1 | 0.0000000 | 0.0060282 | 0.0033115 | 0.6787366 | 0.8643735 | 0.9568157 | 3 | TRUE | TRUE | 1 | 0 | 3 | FALSE | FALSE | TRUE | TRUE |
| OG_7584 | 3 | TRUE | Sustained Up (3h) | 1 | Eukaryotic translation initiation factor 2-alpha kinase 3 (EC 2.7.11.1) (PRKR-like endoplasmic reticulum kinase) (Pancreatic eIF2-alpha kinase) (HsPEK) (Protein tyrosine kinase EIF2AK3) (EC 2.7.10.2) | 0 | TRUE | Pocillopora_acuta_HIv2\_\_\_RNAseq.g24155.t1 | Montipora_capitata_HIv3\_\_\_RNAseq.g23342.t1 | Porites_compressa_HIv1\_\_\_RNAseq.g14248.t1 | 0.0000000 | 0.0000000 | 0.0005182 | 0.7288276 | 0.9882804 | 0.5299947 | 2 | TRUE | FALSE | 3 | 0 | 0 | TRUE | FALSE | FALSE | TRUE |
| OG_1019 | 3 | TRUE | Gradual Down | 1 | Beta-glucuronidase (EC 3.2.1.31) | 0 | TRUE | Pocillopora_acuta_HIv2\_\_\_RNAseq.g17111.t1 | Montipora_capitata_HIv3\_\_\_TS.g42393.t1 | Porites_compressa_HIv1\_\_\_RNAseq.g10924.t1 | 0.0013432 | 0.0004659 | 0.0261287 | 0.5301712 | 0.9190285 | 0.5863140 | 0 | FALSE | FALSE | 1 | 0 | 1 | FALSE | FALSE | FALSE | FALSE |
| OG_11009 | 3 | TRUE | Sustained Up (3h) | 1 | Netrin receptor UNC5C (Protein unc-5 homolog 3) (cUNC-5H3) (Protein unc-5 homolog C) | 0 | TRUE | Pocillopora_acuta_HIv2\_\_\_RNAseq.7998_t | Montipora_capitata_HIv3\_\_\_RNAseq.g50148.t1 | Porites_compressa_HIv1\_\_\_RNAseq.g11603.t1 | 0.0093969 | 0.0000017 | 0.0098370 | 0.4859274 | 0.9673555 | 0.9721135 | 2 | TRUE | FALSE | 1 | 1 | 0 | FALSE | FALSE | FALSE | FALSE |
| OG_11214 | 3 | TRUE | Gradual Down | 1 | NA | NA | FALSE | Pocillopora_acuta_HIv2\_\_\_TS.g15881.t1 | Montipora_capitata_HIv3\_\_\_RNAseq.g41945.t1c | Porites_compressa_HIv1\_\_\_RNAseq.g37129.t1 | 0.0003044 | 0.0010584 | 0.0248931 | 0.8406773 | 0.9883217 | 0.7523045 | 0 | FALSE | FALSE | 0 | 0 | 0 | FALSE | FALSE | FALSE | FALSE |

### Define response categories for 1:1:1 orthologs

``` r
# Core conserved: DE in all 3, same Mfuzz pattern
core_conserved_response <- complete_1to1_ogs_summary %>%
  filter(all_species_DE, Mfuzz_patterns_unique == 1) 

# Core divergent: DE in all 3, different Mfuzz patterns  
core_divergent_response <- complete_1to1_ogs_summary %>%
  filter(all_species_DE, Mfuzz_patterns_unique > 1)

# Variable response: DE in 1-2 species only
variable_response <- complete_1to1_ogs_summary %>%
  filter(!all_species_DE, n_DE_species >= 1)

# Not responsive
not_responsive <- complete_1to1_ogs_summary %>%
  filter(n_DE_species == 0)

cat("1:1:1 Ortholog Response Categories:\n")
```

    ## 1:1:1 Ortholog Response Categories:

``` r
cat("  Core conserved (DE in 3 species, same Mfuzz pattern):", nrow(core_conserved_response), "\n")
```

    ##   Core conserved (DE in 3 species, same Mfuzz pattern): 81

``` r
cat("  Core divergent (DE in 3 species, different Mfuzz patterns):", nrow(core_divergent_response), "\n")
```

    ##   Core divergent (DE in 3 species, different Mfuzz patterns): 566

``` r
cat("  Variable response (DE in 1-2 species):", nrow(variable_response), "\n")
```

    ##   Variable response (DE in 1-2 species): 5372

``` r
cat("  Not responsive:", nrow(not_responsive), "\n\n")
```

    ##   Not responsive: 2452

### Are TFBS conserved across DE 1:1:1 orthologs?

``` r
core_conserved_response %>% filter(HSF1_conserved == TRUE) %>% kable(format = "markdown")
```

| OG | n_DE_species | all_species_DE | Mfuzz_patterns | Mfuzz_patterns_unique | annotation | annotation_evalue | has_annotation | gene_Pacuta | gene_Mcap | gene_Pcomp | padj_Pacuta | padj_Mcap | padj_Pcomp | MfuzzM_Pacuta | MfuzzM_Mcap | MfuzzM_Pcomp | n_species_hub | hub_in_any | hub_in_all | n_species_HSF1 | n_species_FOXO3 | n_species_NFE2L2 | HSF1_conserved | FOXO3_conserved | NFE2L2_conserved | any_TF_conserved |
|:---|---:|:---|:---|---:|:---|---:|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|:---|:---|---:|---:|---:|:---|:---|:---|:---|
| OG_7584 | 3 | TRUE | Sustained Up (3h) | 1 | Eukaryotic translation initiation factor 2-alpha kinase 3 (EC 2.7.11.1) (PRKR-like endoplasmic reticulum kinase) (Pancreatic eIF2-alpha kinase) (HsPEK) (Protein tyrosine kinase EIF2AK3) (EC 2.7.10.2) | 0 | TRUE | Pocillopora_acuta_HIv2\_\_\_RNAseq.g24155.t1 | Montipora_capitata_HIv3\_\_\_RNAseq.g23342.t1 | Porites_compressa_HIv1\_\_\_RNAseq.g14248.t1 | 0 | 0 | 0.0005182 | 0.7288276 | 0.9882804 | 0.5299947 | 2 | TRUE | FALSE | 3 | 0 | 0 | TRUE | FALSE | FALSE | TRUE |

``` r
core_conserved_response %>% filter(FOXO3_conserved == TRUE) %>% kable(format = "markdown")
```

| OG | n_DE_species | all_species_DE | Mfuzz_patterns | Mfuzz_patterns_unique | annotation | annotation_evalue | has_annotation | gene_Pacuta | gene_Mcap | gene_Pcomp | padj_Pacuta | padj_Mcap | padj_Pcomp | MfuzzM_Pacuta | MfuzzM_Mcap | MfuzzM_Pcomp | n_species_hub | hub_in_any | hub_in_all | n_species_HSF1 | n_species_FOXO3 | n_species_NFE2L2 | HSF1_conserved | FOXO3_conserved | NFE2L2_conserved | any_TF_conserved |
|:---|---:|:---|:---|---:|:---|---:|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|:---|:---|---:|---:|---:|:---|:---|:---|:---|
| OG_12811 | 3 | TRUE | Gradual Down | 1 | NA | NA | FALSE | Pocillopora_acuta_HIv2\_\_\_RNAseq.g11037.t1 | Montipora_capitata_HIv3\_\_\_RNAseq.g20746.t1 | Porites_compressa_HIv1\_\_\_RNAseq.g3627.t1 | 1e-07 | 2.06e-05 | 0 | 0.4061767 | 0.9833159 | 0.9527887 | 2 | TRUE | FALSE | 0 | 3 | 0 | FALSE | TRUE | FALSE | TRUE |

``` r
core_conserved_response %>% filter(NFE2L2_conserved == TRUE) %>% kable(format = "markdown")
```

| OG | n_DE_species | all_species_DE | Mfuzz_patterns | Mfuzz_patterns_unique | annotation | annotation_evalue | has_annotation | gene_Pacuta | gene_Mcap | gene_Pcomp | padj_Pacuta | padj_Mcap | padj_Pcomp | MfuzzM_Pacuta | MfuzzM_Mcap | MfuzzM_Pcomp | n_species_hub | hub_in_any | hub_in_all | n_species_HSF1 | n_species_FOXO3 | n_species_NFE2L2 | HSF1_conserved | FOXO3_conserved | NFE2L2_conserved | any_TF_conserved |
|:---|---:|:---|:---|---:|:---|---:|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|:---|:---|---:|---:|---:|:---|:---|:---|:---|
| OG_3443 | 3 | TRUE | Sustained Up (3h) | 1 | Protein DDI1 homolog 2 (EC 3.4.23.-) | 0 | TRUE | Pocillopora_acuta_HIv2\_\_\_RNAseq.g30800.t1 | Montipora_capitata_HIv3\_\_\_RNAseq.g30166.t1 | Porites_compressa_HIv1\_\_\_RNAseq.g624.t1 | 0 | 0.0060282 | 0.0033115 | 0.6787366 | 0.8643735 | 0.9568157 | 3 | TRUE | TRUE | 1 | 0 | 3 | FALSE | FALSE | TRUE | TRUE |

## 8. Non 1:1:1 conserved responses?

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
  ) %>% kable(format = "markdown")
```

| species | n_genes | n_in_OG | pct_in_OG | n_in_all_3 | pct_in_all_3 | n_1to1to1 | pct_1to1to1 |
|:--------|--------:|--------:|----------:|-----------:|-------------:|----------:|------------:|
| Mcap    |     963 |     889 |  92.31568 |        810 |     91.11361 |       464 |    52.19348 |
| Pacuta  |    1610 |    1442 |  89.56522 |       1279 |     88.69626 |       636 |    44.10541 |
| Pcomp   |    1260 |    1181 |  93.73016 |       1051 |     88.99238 |       638 |    54.02202 |

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
  ) %>% kable(format = "markdown")
```

| species | n_genes | n_DE | pct_DE | Mfuzz_cluster | n_high_conf | pct_high_conf | n_Modules | largest_module | n_hubs | has_HSF1 | has_FOXO3 | has_NFE2L2 |
|:---|---:|---:|---:|:---|---:|---:|---:|:---|---:|---:|---:|---:|
| Mcap | 963 | 963 | 100 | 3 | 838 | 87.01973 | 18 | ME1 | 253 | 140 | 168 | 109 |
| Pacuta | 1610 | 1610 | 100 | 4 | 1255 | 77.95031 | 19 | ME1 | 378 | 180 | 244 | 156 |
| Pcomp | 1260 | 1260 | 100 | 5 | 1140 | 90.47619 | 13 | ME1 | 336 | 171 | 208 | 136 |

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

## 9. Manually-curated heat stress genes

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
