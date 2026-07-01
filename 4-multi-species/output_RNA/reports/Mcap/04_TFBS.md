Transcription Factor Binding Sites Analysis
================
Zoe Dellaert
2026-07-01

- [Analysis of Time Series bulk RNA-seq data: Transcription Factor
  Binding Sites (TFBS)
  Analysis](#analysis-of-time-series-bulk-rna-seq-data-transcription-factor-binding-sites-tfbs-analysis)
  - [Introduction](#introduction)
    - [Fimo code used (for reference, not run in this Rmd since it was
      run in my annotation
      repository):](#fimo-code-used-for-reference-not-run-in-this-rmd-since-it-was-run-in-my-annotation-repository)
  - [1. Load packages and functions](#1-load-packages-and-functions)
  - [2. Setup species-specific parameters and define
    directories](#2-setup-species-specific-parameters-and-define-directories)
  - [3. Load in filtered counts and SwissProt
    annotations](#3-load-in-filtered-counts-and-swissprot-annotations)
  - [4. Load in Stress Transcription Factor Binding Site
    Data](#4-load-in-stress-transcription-factor-binding-site-data)

# Analysis of Time Series bulk RNA-seq data: Transcription Factor Binding Sites (TFBS) Analysis

## Introduction

In my annotation
[repository](https://github.com/zdellaert/HI_genome_annotations.git)
(read about this is my
[README.md](https://github.com/zdellaert/TimeSeries/blob/main/4-multi-species/code/RNA/README.md)),
I identified the putative promoter regions (500 bp upstream) of all
genes for the three species analyzed in this project. I used the program
[FIMO](https://meme-suite.org/meme/tools/fimo) to identify putative
binding sites for 3 stress-related transcription factors (FOXO3, HSF1,
and NRF2/NFE2L2) in these promoter regions. Here, I load in the FIMO
output and quantify the number of putative binding sites for each TF in
the promoter region of expressed genes in my RNA-seq dataset.

TFs analyzed:

- **HSF1** - Heat Shock Factor 1 (primary regulator of heat shock
  response)
  - Human binding site motif used:
    `wget "https://jaspar.elixir.no/api/v1/matrix/MA0486.1.meme"`
- **FOXO3** - Forkhead box O3 (potentially involved in coral
  apoptosis/autophagy response)
  - Human binding site motif used:
    `wget "https://jaspar.elixir.no/api/v1/matrix/MA0157.2.meme"`
- **NFE2L2** (Nrf2) - Nuclear factor erythroid 2-related factor 2
  (oxidative stress response)
  - Human binding site motif used:
    `wget "https://jaspar.elixir.no/api/v1/matrix/MA0150.1.meme"`

This analysis was originally inspired by the paper *Cleves PA, Krediet
CJ, Lehnert EM, Onishi M, Pringle JR. Insights into coral bleaching
under heat stress from analysis of gene expression in a sea anemone
model system. Proceedings of the National Academy of Sciences. 2020 Nov
17;117(46):28906–17.
(<https://www.pnas.org/doi/10.1073/pnas.2015737117>)* and the updated
analysis in the paper *Swinhoe N, Tinoco AI, Sarfati DN, Henderson CF,
Kowalewski GP, Meier EK, et al. CRISPR/Cas9-mutagenesis reveals that
varying dependence on HSF1 is associated with differences in coral heat
tolerance. bioRxiv; 2026. p. 2026.04.01.714264.
<doi:10.64898/2026.04.01.714264>* by the same lab.

### Fimo code used (for reference, not run in this Rmd since it was run in my annotation [repository](https://github.com/zdellaert/HI_genome_annotations.git)):

Fimo was run using the docker image of MEMEsuite version 5.5.9 in March
2026.

``` bash
SINGULARITY_IMAGE="docker://memesuite/memesuite:latest"

    # run FIMO with default settings (--thresh 0.0001, --max-stored-scores 100000)
    singularity exec --cleanenv $SINGULARITY_IMAGE fimo \
        -oc "fimo_output/${sp}_stress_TFs" \
        --thresh 0.0001 \
        --max-stored-scores 100000 \
        ../../references/motif_databases/stress_TFs.meme \
        "$fasta"
```

------------------------------------------------------------------------

## 1. Load packages and functions

``` r
# set up file paths so that Rmd outputs can be viewed using github markdown
knitr::opts_knit$set(base.dir = normalizePath(paste0("../../output_RNA/reports/", params$species, "/")), base.url = "./")

knitr::opts_chunk$set(echo = TRUE, message = FALSE, fig.width = 10, fig.height = 8,
                      fig.path = "04_TFBS_files/figure-gfm/")

#load packages
library(tidyverse)

#load in parameters and functions
source("species_parameters.R")
source("utils.R")

sessionInfo() #provides list of loaded packages and version of R
```

    ## R version 4.5.1 (2025-06-13)
    ## Platform: x86_64-apple-darwin20
    ## Running under: macOS Tahoe 26.4.1
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: America/New_York
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ##  [1] tcltk     grid      stats4    stats     graphics  grDevices utils    
    ##  [8] datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] knitr_1.51                  fastcluster_1.3.0          
    ##  [3] dynamicTreeCut_1.63-1       DynDoc_1.88.0              
    ##  [5] widgetTools_1.88.0          e1071_1.7-17               
    ##  [7] BiocParallel_1.44.0         ggnewscale_0.5.2           
    ##  [9] RColorBrewer_1.1-3          SummarizedExperiment_1.40.0
    ## [11] Biobase_2.70.0              MatrixGenerics_1.22.0      
    ## [13] matrixStats_1.5.0           GenomicRanges_1.62.1       
    ## [15] Seqinfo_1.0.0               IRanges_2.44.0             
    ## [17] S4Vectors_0.48.1            BiocGenerics_0.56.0        
    ## [19] generics_0.1.4              lubridate_1.9.5            
    ## [21] forcats_1.0.1               stringr_1.6.0              
    ## [23] dplyr_1.2.1                 purrr_1.2.2                
    ## [25] readr_2.2.0                 tidyr_1.3.2                
    ## [27] tibble_3.3.1                ggplot2_4.0.3              
    ## [29] tidyverse_2.0.0             rmarkdown_2.31             
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] rstudioapi_0.19.0     shape_1.4.6.1         magrittr_2.0.5       
    ##   [4] farver_2.1.2          GlobalOptions_0.1.4   ragg_1.5.2           
    ##   [7] vctrs_0.7.3           memoise_2.0.1         base64enc_0.1-6      
    ##  [10] htmltools_0.5.9       S4Arrays_1.10.1       SparseArray_1.10.10  
    ##  [13] Formula_1.2-5         htmlwidgets_1.6.4     impute_1.84.0        
    ##  [16] cachem_1.1.0          igraph_2.3.3          lifecycle_1.0.5      
    ##  [19] iterators_1.0.14      pkgconfig_2.0.3       Matrix_1.7-5         
    ##  [22] R6_2.6.1              fastmap_1.2.0         clue_0.3-68          
    ##  [25] digest_0.6.39         colorspace_2.1-2      AnnotationDbi_1.72.0 
    ##  [28] DESeq2_1.50.2         textshaping_1.0.5     Hmisc_5.2-6          
    ##  [31] RSQLite_3.53.2        labeling_0.4.3        timechange_0.4.0     
    ##  [34] mgcv_1.9-4            polyclip_1.10-7       httr_1.4.8           
    ##  [37] abind_1.4-8           compiler_4.5.1        proxy_0.4-29         
    ##  [40] bit64_4.8.2           withr_3.0.3           doParallel_1.0.17    
    ##  [43] htmlTable_2.5.0       S7_0.2.2              backports_1.5.1      
    ##  [46] viridis_0.6.5         DBI_1.3.0             ggforce_0.5.0        
    ##  [49] MASS_7.3-65           tkWidgets_1.88.0      DelayedArray_0.36.1  
    ##  [52] rjson_0.2.23          tools_4.5.1           foreign_0.8-91       
    ##  [55] otel_0.2.0            nnet_7.3-20           glue_1.8.1           
    ##  [58] nlme_3.1-169          checkmate_2.3.4       cluster_2.1.8.2      
    ##  [61] gtable_0.3.6          tzdb_0.5.0            preprocessCore_1.72.0
    ##  [64] class_7.3-23          data.table_1.18.4     hms_1.1.4            
    ##  [67] tidygraph_1.3.1       utf8_1.2.6            XVector_0.50.0       
    ##  [70] ggrepel_0.9.8         foreach_1.5.2         pillar_1.11.1        
    ##  [73] limma_3.66.0          vroom_1.7.1           circlize_0.4.18      
    ##  [76] splines_4.5.1         tweenr_2.0.3          lattice_0.22-9       
    ##  [79] survival_3.8-6        bit_4.6.0             annotate_1.88.0      
    ##  [82] tidyselect_1.2.1      ComplexHeatmap_2.26.1 locfit_1.5-9.12      
    ##  [85] Biostrings_2.78.0     gridExtra_2.3.1       xfun_0.59            
    ##  [88] graphlayouts_1.2.4    statmod_1.5.2         stringi_1.8.7        
    ##  [91] yaml_2.3.12           evaluate_1.0.5        codetools_0.2-20     
    ##  [94] cli_3.6.6             rpart_4.1.27          xtable_1.8-8         
    ##  [97] systemfonts_1.3.2     Rcpp_1.1.1-1.1        png_0.1-9            
    ## [100] XML_3.99-0.23         parallel_4.5.1        blob_1.3.0           
    ## [103] viridisLite_0.4.3     scales_1.4.0          crayon_1.5.3         
    ## [106] GetoptLong_1.1.1      rlang_1.2.0           cowplot_1.2.0        
    ## [109] KEGGREST_1.50.0

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
# define preprocessing output directory (from 01_preprocessing.Rmd)
input_dir <- file.path("../../output_RNA/counts_filt_norm", species)

# FIMO path (from annotation repo)
fimo_path <- file.path(annot_dir, "promoters/fimo_output", paste0(species, "_stress_TFs"))
cat("\nFIMO path:", fimo_path, "\n")
```

    ## 
    ## FIMO path: ../../../../HI_genome_annotations/annotation/promoters/fimo_output/Mcap_stress_TFs

``` r
# set up necessary output directories if they don't exist
outdir <- file.path("../../output_RNA/TFBS", species)
outdir_plots <- file.path(outdir,"plots")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
if (!dir.exists(outdir_plots)) dir.create(outdir_plots, recursive = TRUE)

reportdir <- file.path("../../output_RNA/reports", params$species, "04_TFBS_files/figure-gfm/")
if (!dir.exists(reportdir)) dir.create(reportdir, recursive = TRUE)
```

## 3. Load in filtered counts and SwissProt annotations

``` r
# load in filtered counts data
filtered_counts <- read.csv(file.path(input_dir, "filtered_counts.csv"), row.names = 1)

# SwissProt annotations
SwissProt <- read.delim(file.path(annot_dir,config$SwissProt))
cat("Annotations:", nrow(SwissProt), "Swissprot-annotated genes")
```

    ## Annotations: 22471 Swissprot-annotated genes

## 4. Load in Stress Transcription Factor Binding Site Data

``` r
Stress_TFs <- read.delim(file.path(fimo_path,"fimo.tsv"), comment.char = "#")

# only keep genes in the filtered dataset
Stress_TFs <- Stress_TFs %>% filter(sequence_name %in% rownames(filtered_counts))
  
#get list of motifs/TFs present in the data
all_tfs <- unique(Stress_TFs$motif_alt_id)
cat("Transcription factor binding sites found for:", paste(unique(Stress_TFs$motif_alt_id), collapse = ", "), "\n")
```

    ## Transcription factor binding sites found for: HSF1, NFE2L2, FOXO3

``` r
Stress_TFs_quantification <- Stress_TFs %>%
  group_by(motif_alt_id,sequence_name) %>%
  summarize(count=n(), .groups = "drop") 

Foxo3_quantification <- Stress_TFs_quantification %>% filter(motif_alt_id =="FOXO3") %>% left_join(SwissProt, by = join_by("sequence_name"=="query")) 
Hsf1_quantification <- Stress_TFs_quantification %>% filter(motif_alt_id =="HSF1") %>% left_join(SwissProt, by = join_by("sequence_name"=="query")) 
Nrf2_quantification <- Stress_TFs_quantification %>% filter(motif_alt_id =="NFE2L2") %>% left_join(SwissProt, by = join_by("sequence_name"=="query")) 

cat("Transcription Factor", "FOXO3:", nrow(Foxo3_quantification), "genes with putative binding sites.\nGene(s) with most binding sites:",
    Foxo3_quantification %>% filter(count == max(count)) %>% pull(sequence_name), "( Best Annotation:", Foxo3_quantification %>% filter(count == max(count)) %>% arrange(evalue) %>% head(1) %>% pull(ProteinNames)  %>% word(1:4), ") with", max(Foxo3_quantification$count), "binding sites.\n\n")
```

    ## Transcription Factor FOXO3: 4712 genes with putative binding sites.
    ## Gene(s) with most binding sites: Montipora_capitata_HIv3___RNAseq.39849_t Montipora_capitata_HIv3___RNAseq.g19683.t1 Montipora_capitata_HIv3___RNAseq.g30792.t1 Montipora_capitata_HIv3___RNAseq.g34180.t1 Montipora_capitata_HIv3___TS.g17705.t1 ( Best Annotation: Uromodulin (Tamm-Horsfall urinary glycoprotein) ) with 4 binding sites.

``` r
cat("Transcription Factor", "HSF1:", nrow(Hsf1_quantification), "genes with putative binding sites.\nGene(s) with most binding sites:",
    Hsf1_quantification %>% filter(count == max(count)) %>% pull(sequence_name), "( Best Annotation:", Hsf1_quantification %>% filter(count == max(count)) %>% arrange(evalue) %>% head(1) %>% pull(ProteinNames)  %>% word(1:4), ") with", max(Hsf1_quantification$count), "binding sites.\n\n")
```

    ## Transcription Factor HSF1: 3114 genes with putative binding sites.
    ## Gene(s) with most binding sites: Montipora_capitata_HIv3___RNAseq.g15043.t1 Montipora_capitata_HIv3___RNAseq.g18811.t1 ( Best Annotation: Heat shock protein HSP ) with 11 binding sites.

``` r
cat("Transcription Factor", "NFE2L2:", nrow(Nrf2_quantification), "genes with putative binding sites.\nGene(s) with most binding sites:",
    Nrf2_quantification %>% filter(count == max(count)) %>% pull(sequence_name), "( Best Annotation:", Nrf2_quantification %>% filter(count == max(count)) %>% arrange(evalue) %>% head(1) %>% pull(ProteinNames)  %>% word(1:4), ") with", max(Nrf2_quantification$count), "binding sites.")
```

    ## Transcription Factor NFE2L2: 2660 genes with putative binding sites.
    ## Gene(s) with most binding sites: Montipora_capitata_HIv3___RNAseq.g13267.t1 ( Best Annotation: Clustered mitochondria protein (Friendly ) with 6 binding sites.

Make dataframe that collapses genes with sites from more than one TF in
Stress_TFs_quantification so that there is only one row per gene.

``` r
TFBS_by_gene <- Stress_TFs_quantification %>%
  pivot_wider(names_from = motif_alt_id, values_from = count, values_fill = 0, names_prefix = "count_") 

head(TFBS_by_gene %>% mutate(total_motifs=(`count_FOXO3` + `count_HSF1` + `count_NFE2L2`)) %>% arrange(desc(total_motifs)) %>% left_join(SwissProt %>% dplyr::select(query,ProteinNames,blast_hit), by = join_by("sequence_name"=="query")) )
```

    ## # A tibble: 6 × 7
    ##   sequence_name    count_FOXO3 count_HSF1 count_NFE2L2 total_motifs ProteinNames
    ##   <chr>                  <int>      <int>        <int>        <int> <chr>       
    ## 1 Montipora_capit…           0         11            0           11 Heat shock …
    ## 2 Montipora_capit…           0         11            0           11 Heat shock …
    ## 3 Montipora_capit…           1          6            1            8 Uncharacter…
    ## 4 Montipora_capit…           0          8            0            8 RNA polymer…
    ## 5 Montipora_capit…           0          8            0            8 <NA>        
    ## 6 Montipora_capit…           1          6            0            7 Uncharacter…
    ## # ℹ 1 more variable: blast_hit <chr>

``` r
write.csv(TFBS_by_gene, file.path(outdir, "TFBS_counts.csv"), row.names = FALSE)
```

``` r
sessionInfo()
```

    ## R version 4.5.1 (2025-06-13)
    ## Platform: x86_64-apple-darwin20
    ## Running under: macOS Tahoe 26.4.1
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: America/New_York
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ##  [1] tcltk     grid      stats4    stats     graphics  grDevices utils    
    ##  [8] datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] knitr_1.51                  fastcluster_1.3.0          
    ##  [3] dynamicTreeCut_1.63-1       DynDoc_1.88.0              
    ##  [5] widgetTools_1.88.0          e1071_1.7-17               
    ##  [7] BiocParallel_1.44.0         ggnewscale_0.5.2           
    ##  [9] RColorBrewer_1.1-3          SummarizedExperiment_1.40.0
    ## [11] Biobase_2.70.0              MatrixGenerics_1.22.0      
    ## [13] matrixStats_1.5.0           GenomicRanges_1.62.1       
    ## [15] Seqinfo_1.0.0               IRanges_2.44.0             
    ## [17] S4Vectors_0.48.1            BiocGenerics_0.56.0        
    ## [19] generics_0.1.4              lubridate_1.9.5            
    ## [21] forcats_1.0.1               stringr_1.6.0              
    ## [23] dplyr_1.2.1                 purrr_1.2.2                
    ## [25] readr_2.2.0                 tidyr_1.3.2                
    ## [27] tibble_3.3.1                ggplot2_4.0.3              
    ## [29] tidyverse_2.0.0             rmarkdown_2.31             
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] rstudioapi_0.19.0     shape_1.4.6.1         magrittr_2.0.5       
    ##   [4] farver_2.1.2          GlobalOptions_0.1.4   ragg_1.5.2           
    ##   [7] vctrs_0.7.3           memoise_2.0.1         base64enc_0.1-6      
    ##  [10] htmltools_0.5.9       S4Arrays_1.10.1       SparseArray_1.10.10  
    ##  [13] Formula_1.2-5         htmlwidgets_1.6.4     impute_1.84.0        
    ##  [16] cachem_1.1.0          igraph_2.3.3          lifecycle_1.0.5      
    ##  [19] iterators_1.0.14      pkgconfig_2.0.3       Matrix_1.7-5         
    ##  [22] R6_2.6.1              fastmap_1.2.0         clue_0.3-68          
    ##  [25] digest_0.6.39         colorspace_2.1-2      AnnotationDbi_1.72.0 
    ##  [28] DESeq2_1.50.2         textshaping_1.0.5     Hmisc_5.2-6          
    ##  [31] RSQLite_3.53.2        labeling_0.4.3        timechange_0.4.0     
    ##  [34] mgcv_1.9-4            polyclip_1.10-7       httr_1.4.8           
    ##  [37] abind_1.4-8           compiler_4.5.1        proxy_0.4-29         
    ##  [40] bit64_4.8.2           withr_3.0.3           doParallel_1.0.17    
    ##  [43] htmlTable_2.5.0       S7_0.2.2              backports_1.5.1      
    ##  [46] viridis_0.6.5         DBI_1.3.0             ggforce_0.5.0        
    ##  [49] MASS_7.3-65           tkWidgets_1.88.0      DelayedArray_0.36.1  
    ##  [52] rjson_0.2.23          tools_4.5.1           foreign_0.8-91       
    ##  [55] otel_0.2.0            nnet_7.3-20           glue_1.8.1           
    ##  [58] nlme_3.1-169          checkmate_2.3.4       cluster_2.1.8.2      
    ##  [61] gtable_0.3.6          tzdb_0.5.0            preprocessCore_1.72.0
    ##  [64] class_7.3-23          data.table_1.18.4     hms_1.1.4            
    ##  [67] tidygraph_1.3.1       utf8_1.2.6            XVector_0.50.0       
    ##  [70] ggrepel_0.9.8         foreach_1.5.2         pillar_1.11.1        
    ##  [73] limma_3.66.0          vroom_1.7.1           circlize_0.4.18      
    ##  [76] splines_4.5.1         tweenr_2.0.3          lattice_0.22-9       
    ##  [79] survival_3.8-6        bit_4.6.0             annotate_1.88.0      
    ##  [82] tidyselect_1.2.1      ComplexHeatmap_2.26.1 locfit_1.5-9.12      
    ##  [85] Biostrings_2.78.0     gridExtra_2.3.1       xfun_0.59            
    ##  [88] graphlayouts_1.2.4    statmod_1.5.2         stringi_1.8.7        
    ##  [91] yaml_2.3.12           evaluate_1.0.5        codetools_0.2-20     
    ##  [94] cli_3.6.6             rpart_4.1.27          xtable_1.8-8         
    ##  [97] systemfonts_1.3.2     Rcpp_1.1.1-1.1        png_0.1-9            
    ## [100] XML_3.99-0.23         parallel_4.5.1        blob_1.3.0           
    ## [103] viridisLite_0.4.3     scales_1.4.0          crayon_1.5.3         
    ## [106] GetoptLong_1.1.1      rlang_1.2.0           cowplot_1.2.0        
    ## [109] KEGGREST_1.50.0
