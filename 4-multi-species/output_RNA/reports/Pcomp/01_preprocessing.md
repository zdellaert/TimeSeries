RNA-seq Preprocessing and Normalization
================
Zoe Dellaert
2026-07-01

- [Preproccessing of bulk RNA-seq
  data](#preproccessing-of-bulk-rna-seq-data)
  - [0. Setup species-specific
    parameters](#0-setup-species-specific-parameters)
  - [1. Read in raw count data](#1-read-in-raw-count-data)
  - [2. Extract metadata from sample
    names](#2-extract-metadata-from-sample-names)
  - [3. Remove outliers, if
    identified](#3-remove-outliers-if-identified)
  - [4. pOverA filtering to reduce
    dataset](#4-povera-filtering-to-reduce-dataset)
    - [Note to self: maybe replace this with treatment-specific
      filtering. To get genes expressed only at one timepoint in one
      treatment](#note-to-self-maybe-replace-this-with-treatment-specific-filtering-to-get-genes-expressed-only-at-one-timepoint-in-one-treatment)
  - [5. Create DESeq object and run
    DESeq2](#5-create-deseq-object-and-run-deseq2)
  - [6. VST-Transforming count data for
    visualization](#6-vst-transforming-count-data-for-visualization)
  - [7. Two tools to identiy potential
    outliers:](#7-two-tools-to-identiy-potential-outliers)
    - [PCA](#pca)
    - [Hierarchical Clustering](#hierarchical-clustering)
    - [Note: If outliers are identified, add them to
      species_parameters.R for this
      species.](#note-if-outliers-are-identified-add-them-to-species_parametersr-for-this-species)
  - [Final summary](#final-summary)
    - [Heatmap of variable genes](#heatmap-of-variable-genes)
    - [Text summary](#text-summary)

# Preproccessing of bulk RNA-seq data

``` r
# set up file paths so that Rmd outputs can be viewed using github markdown
knitr::opts_knit$set(base.dir = normalizePath(paste0("../../output_RNA/reports/", params$species, "/")), base.url = "./")

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE,fig.width = 10, fig.height = 8,
                      fig.path = "01_preprocessing_files/figure-gfm/")

#load packages
library(tidyverse)
library(DESeq2)
```

    ## Warning: package 'DESeq2' was built under R version 4.5.2

``` r
library(pheatmap)
library(RColorBrewer)
library(genefilter)
```

    ## Warning: package 'genefilter' was built under R version 4.5.2

    ## 
    ## Attaching package: 'genefilter'

    ## The following objects are masked from 'package:MatrixGenerics':
    ## 
    ##     rowSds, rowVars

    ## The following objects are masked from 'package:matrixStats':
    ## 
    ##     rowSds, rowVars

    ## The following object is masked from 'package:readr':
    ## 
    ##     spec

``` r
library(ggnewscale)
library(BiocParallel)

#load in parameters and functions
source("species_parameters.R")
source("utils.R")

# set number of cores to use for parallel DESeq2 processing
register(MulticoreParam(workers = global_params$n_cores))

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
    ##  [1] genefilter_1.92.0           pheatmap_1.0.13            
    ##  [3] DESeq2_1.50.2               knitr_1.51                 
    ##  [5] fastcluster_1.3.0           dynamicTreeCut_1.63-1      
    ##  [7] DynDoc_1.88.0               widgetTools_1.88.0         
    ##  [9] e1071_1.7-17                BiocParallel_1.44.0        
    ## [11] ggnewscale_0.5.2            RColorBrewer_1.1-3         
    ## [13] SummarizedExperiment_1.40.0 Biobase_2.70.0             
    ## [15] MatrixGenerics_1.22.0       matrixStats_1.5.0          
    ## [17] GenomicRanges_1.62.1        Seqinfo_1.0.0              
    ## [19] IRanges_2.44.0              S4Vectors_0.48.1           
    ## [21] BiocGenerics_0.56.0         generics_0.1.4             
    ## [23] lubridate_1.9.5             forcats_1.0.1              
    ## [25] stringr_1.6.0               dplyr_1.2.1                
    ## [27] purrr_1.2.2                 readr_2.2.0                
    ## [29] tidyr_1.3.2                 tibble_3.3.1               
    ## [31] ggplot2_4.0.3               tidyverse_2.0.0            
    ## [33] rmarkdown_2.31             
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
    ##  [28] textshaping_1.0.5     Hmisc_5.2-6           RSQLite_3.53.2       
    ##  [31] labeling_0.4.3        timechange_0.4.0      mgcv_1.9-4           
    ##  [34] polyclip_1.10-7       httr_1.4.8            abind_1.4-8          
    ##  [37] compiler_4.5.1        proxy_0.4-29          bit64_4.8.2          
    ##  [40] withr_3.0.3           doParallel_1.0.17     htmlTable_2.5.0      
    ##  [43] S7_0.2.2              backports_1.5.1       viridis_0.6.5        
    ##  [46] DBI_1.3.0             ggforce_0.5.0         MASS_7.3-65          
    ##  [49] tkWidgets_1.88.0      DelayedArray_0.36.1   rjson_0.2.23         
    ##  [52] tools_4.5.1           foreign_0.8-91        otel_0.2.0           
    ##  [55] nnet_7.3-20           glue_1.8.1            nlme_3.1-169         
    ##  [58] checkmate_2.3.4       cluster_2.1.8.2       gtable_0.3.6         
    ##  [61] tzdb_0.5.0            preprocessCore_1.72.0 class_7.3-23         
    ##  [64] data.table_1.18.4     hms_1.1.4             tidygraph_1.3.1      
    ##  [67] utf8_1.2.6            XVector_0.50.0        ggrepel_0.9.8        
    ##  [70] foreach_1.5.2         pillar_1.11.1         limma_3.66.0         
    ##  [73] vroom_1.7.1           circlize_0.4.18       splines_4.5.1        
    ##  [76] tweenr_2.0.3          lattice_0.22-9        survival_3.8-6       
    ##  [79] bit_4.6.0             annotate_1.88.0       tidyselect_1.2.1     
    ##  [82] locfit_1.5-9.12       Biostrings_2.78.0     gridExtra_2.3.1      
    ##  [85] xfun_0.59             graphlayouts_1.2.4    statmod_1.5.2        
    ##  [88] stringi_1.8.7         yaml_2.3.12           evaluate_1.0.5       
    ##  [91] codetools_0.2-20      cli_3.6.6             rpart_4.1.27         
    ##  [94] xtable_1.8-8          systemfonts_1.3.2     Rcpp_1.1.1-1.1       
    ##  [97] png_0.1-9             XML_3.99-0.23         parallel_4.5.1       
    ## [100] blob_1.3.0            viridisLite_0.4.3     scales_1.4.0         
    ## [103] crayon_1.5.3          GetoptLong_1.1.1      rlang_1.2.0          
    ## [106] cowplot_1.2.0         KEGGREST_1.50.0

## 0. Setup species-specific parameters

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
# set up necessary output directories if they don't exist
outdir <- file.path("../../output_RNA/counts_filt_norm", species)
outdir_plots <- file.path(outdir,"plots")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
if (!dir.exists(outdir_plots)) dir.create(outdir_plots, recursive = TRUE)

reportdir <- file.path("../../output_RNA/reports", params$species, "01_preprocessing_files/figure-gfm/")
if (!dir.exists(reportdir)) dir.create(reportdir, recursive = TRUE)
```

## 1. Read in raw count data

``` r
# load in data
counts_raw <- read.csv(file.path("../../output_RNA/count_matrices", config$count_matrix), row.names = 1)

# make list of samples 
samples <- colnames(counts_raw)
cat("Raw counts:", nrow(counts_raw), "genes x", ncol(counts_raw), "samples")
```

    ## Raw counts: 44130 genes x 42 samples

``` r
# read in SwissProt annotation
SwissProt <- read.delim(file.path(annot_dir,config$SwissProt))
cat("Annotations:", nrow(SwissProt), "Swissprot-annotated genes")
```

    ## Annotations: 22929 Swissprot-annotated genes

## 2. Extract metadata from sample names

``` r
# create metadata dataframe from sample names
meta <- data.frame(
  sample = samples, 
  species = str_split(samples, "_", simplify = TRUE)[,1], #extract first part of sample name to get species
  time = str_replace(str_split(samples, "_", simplify = TRUE)[,2],"R", ""), #extract "R##" part to get timepoint then remove R
  replicate = str_split(samples, "_", simplify = TRUE)[,3], #extract "R##" part to get timepoint then remove R
  treatment = str_replace(str_split(samples, "_", simplify = TRUE)[,3],"\\d", "")
)

# add rownames
rownames(meta) <- meta$sample

# make time and treatment factors
meta$time <- factor(meta$time, levels = as.character(sort(unique(as.numeric(meta$time)))))
meta$treatment <- factor(meta$treatment)

# save metadata
meta <- meta %>% arrange(time, treatment)
write.csv(meta, paste0("../../output_RNA/",species,"_RNA_seq_metadata.csv"))
cat("Metadata file saved to:", paste0("../../output_RNA/",species,"_RNA_seq_metadata.csv"))
```

    ## Metadata file saved to: ../../output_RNA/Pcomp_RNA_seq_metadata.csv

``` r
# reorder count matrix to be in order of metadata table (should be already but just in case)
counts_raw <- counts_raw[, meta$sample]
```

## 3. Remove outliers, if identified

``` r
outlier_samples <- config$outlier_samples

if(length(outlier_samples) > 0) {
    counts_raw <- counts_raw[, !colnames(counts_raw) %in% outlier_samples]
    meta <- meta[!rownames(meta) %in% outlier_samples, ]
}

#Confirm that sample names in metadata and count matrix match and are in the same order
stopifnot(all(meta$sample %in% colnames(counts_raw))) #are all of the sample names in the metadata column names in the gene count matrix?
stopifnot(all(meta$sample == colnames(counts_raw))) #are they the same in the same order?
```

## 4. pOverA filtering to reduce dataset

### Note to self: maybe replace this with treatment-specific filtering. To get genes expressed only at one timepoint in one treatment

``` r
# Keep genes expressed at 10+ counts in at least 7% of samples - expressed in all 3 samples at one timepoint from one treatment, can change parameters in species_parameters.R script

ffun<-filterfun(pOverA(global_params$pOverA_proportion,global_params$pOverA_counts))
counts_filt_poa <- genefilter((counts_raw), ffun) #apply filter

filtered_counts <- counts_raw[counts_filt_poa,] #keep only rows that passed filter

paste0("Number of genes after filtering: ", sum(counts_filt_poa))
```

    ## [1] "Number of genes after filtering: 27492"

``` r
paste0("% of genes kept: ", round(100*(sum(counts_filt_poa)/nrow(counts_raw)),digits=2),"%")
```

    ## [1] "% of genes kept: 62.3%"

``` r
write.csv(filtered_counts, file = file.path(outdir, "filtered_counts.csv"))
cat("Filtered counts saved to:", file.path(outdir, "filtered_counts.csv"))
```

    ## Filtered counts saved to: ../../output_RNA/counts_filt_norm/Pcomp/filtered_counts.csv

## 5. Create DESeq object and run DESeq2

``` r
dds <- DESeqDataSetFromMatrix(countData = filtered_counts,
                              colData = meta,
                              design= ~ treatment + time + treatment:time)

dds <- DESeq(dds, parallel = TRUE)

# Estimate size factors to determine if we can use VST
SF.dds <- estimateSizeFactors(dds) 
print(sort(sizeFactors(SF.dds))) #View size factors
```

    ##  POR_R12_C1 POR_R120_C3   POR_R1_H2   POR_R1_C1   POR_R1_H1  POR_R12_C3 
    ##   0.3261723   0.3505368   0.3787834   0.4125180   0.4390233   0.5440971 
    ##  POR_R24_C3   POR_R0_H1   POR_R3_C3 POR_R120_H3   POR_R0_C2  POR_R72_C3 
    ##   0.6317648   0.6349231   0.6367453   0.6428541   0.7312940   0.7463064 
    ##  POR_R24_C2  POR_R72_C1   POR_R0_H2   POR_R1_C3 POR_R120_C1   POR_R3_H3 
    ##   0.7507718   0.7557128   0.7910232   0.8204148   0.8242397   0.8863441 
    ##   POR_R0_C1   POR_R0_H3   POR_R3_C2   POR_R1_C2   POR_R1_H3  POR_R12_H3 
    ##   0.9436057   1.0619299   1.0964813   1.1494741   1.2062284   1.3180353 
    ##  POR_R72_C2   POR_R0_C3   POR_R3_C1  POR_R24_H2   POR_R3_H1  POR_R72_H3 
    ##   1.4015234   1.4077285   1.4626289   1.6148504   1.6569470   1.6651926 
    ##   POR_R3_H2  POR_R12_C2 POR_R120_C2  POR_R12_H1  POR_R24_H3  POR_R24_C1 
    ##   1.6931237   1.7537687   1.8108391   1.8299722   1.9486318   2.0596268 
    ##  POR_R12_H2 POR_R120_H2 POR_R120_H1 
    ##   2.0768756   2.9911963   3.5047915

``` r
# if all are less than 4 we can use the VST transformation
all(sizeFactors(SF.dds)) < 4
```

    ## [1] TRUE

## 6. VST-Transforming count data for visualization

``` r
vst <- vst(dds, blind=FALSE)

#save the vst transformation
vst_mat <- assay(vst)
write.csv(vst_mat, file = file.path(outdir, "vst_expression_matrix.csv"))
cat("VST matrix saved to:", file.path(outdir, "vst_expression_matrix.csv"))
```

    ## VST matrix saved to: ../../output_RNA/counts_filt_norm/Pcomp/vst_expression_matrix.csv

## 7. Two tools to identiy potential outliers:

### PCA

``` r
pcaData <- plotPCA(vst, intgroup=c("time", "treatment"), returnData=TRUE)
percentVar <- round(100 * attr(pcaData, "percentVar"))

PCA <- ggplot() +
  geom_point(data = subset(pcaData, treatment == "C"),
             aes(x=PC1, y=PC2, color=time),
                 size=3) +
             scale_color_manual(values=brewer.pal(7, "Blues"), name = "Time (hrs) - Control") +
  
  #start new scale
  ggnewscale::new_scale_color() +
  geom_point(data = subset(pcaData, treatment == "H"),
             aes(x=PC1, y=PC2, color=time),
                 size=3) +
             scale_color_manual(values=brewer.pal(7, "Oranges"), name = "Time (hrs) - Heat") +

  xlab(paste0("PC1: ",percentVar[1],"% variance")) +
  ylab(paste0("PC2: ",percentVar[2],"% variance")) + 
  coord_fixed() + theme_bw() + ggtitle(paste(species, "- PCA of VST-transformed counts"))

print(PCA)
```

![](./01_preprocessing_files/figure-gfm/pca-1.png)<!-- -->

``` r
save_ggplot(PCA, "PCA")

PCA_simple <- ggplot(data = pcaData, aes(x=PC1, y=PC2, color=treatment, shape=time)) +
  geom_point(size=4) +
  scale_color_manual(values= c("C"= "#4292C6", "H" = "#D94801"), labels = c("Control", "Heat")) +
  scale_shape_manual(values = c(16, 17, 15, 18, 0, 1, 2)) +
  xlab(paste0("PC1: ",percentVar[1],"% variance")) +
  ylab(paste0("PC2: ",percentVar[2],"% variance")) + 
  labs(color = "Treatment", shape = "Time (h)") +
  coord_fixed() + theme_bw() + ggtitle(paste(species, "- PCA of VST-transformed counts"))

print(PCA_simple)
```

![](./01_preprocessing_files/figure-gfm/pca-2.png)<!-- -->

``` r
save_ggplot(PCA_simple, "PCA_simple", width = 8, height = 6)
```

### Hierarchical Clustering

``` r
sampleTree <- hclust(dist(t(vst_mat)), method = "average")

par(mar = c(8, 4, 2, 2))
plot(sampleTree, 
     xlab = "", sub = "", cex = 0.7)
abline(h = 100, col = "red", lty = 2)
```

![](./01_preprocessing_files/figure-gfm/cluster-1.png)<!-- -->

### Note: If outliers are identified, add them to species_parameters.R for this species.

## Final summary

### Heatmap of variable genes

``` r
topVarGenes <- head(order(rowVars(vst_mat), decreasing=TRUE), 500)

pheatmap(vst_mat[topVarGenes, ], cluster_rows=TRUE, show_rownames=FALSE,
         cluster_cols=FALSE, 
         annotation_col= meta[,c("treatment","time")],
         annotation_colors = list("treatment" = treat_colors,
                                  "time" = time_colors))
```

![](./01_preprocessing_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
pheatmap(vst_mat[topVarGenes, ], cluster_rows=TRUE, show_rownames=FALSE,
         cluster_cols=TRUE, cutree_cols = 2,
         annotation_col= meta[,c("treatment","time")],
         annotation_colors = list("treatment" = treat_colors,
                                  "time" = time_colors))
```

![](./01_preprocessing_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

### Text summary

    ## Preprocessing Summary: Pcomp

    ## Input

    ## ----------------------------------------

    ##   Count matrix: POR_Pcomp_gene_count_matrix.csv

    ##   Initial genes: 44130

    ##   Initial samples: 39

    ## Filtering

    ## ----------------------------------------

    ##   Outliers removed: 3

    ##      POR_R72_H1, POR_R72_H2, POR_R24_H1

    ##   Low-expression genes removed: 16638

    ##   pOverA filter: >= 10 counts in >= 7 % of samples

    ## Output

    ## ----------------------------------------

    ##   Final genes: 27492

    ##   Final samples: 39

    ##   Output directory: ../../output_RNA/counts_filt_norm/Pcomp

    ## QC Notes

    ## ----------------------------------------

    ##   Size factors range: 0.33 - 3.5

    ##   VST appropriate: Yes

    ##   PC1 variance: 44 %

    ##   PC2 variance: 16 %

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
    ##  [1] genefilter_1.92.0           pheatmap_1.0.13            
    ##  [3] DESeq2_1.50.2               knitr_1.51                 
    ##  [5] fastcluster_1.3.0           dynamicTreeCut_1.63-1      
    ##  [7] DynDoc_1.88.0               widgetTools_1.88.0         
    ##  [9] e1071_1.7-17                BiocParallel_1.44.0        
    ## [11] ggnewscale_0.5.2            RColorBrewer_1.1-3         
    ## [13] SummarizedExperiment_1.40.0 Biobase_2.70.0             
    ## [15] MatrixGenerics_1.22.0       matrixStats_1.5.0          
    ## [17] GenomicRanges_1.62.1        Seqinfo_1.0.0              
    ## [19] IRanges_2.44.0              S4Vectors_0.48.1           
    ## [21] BiocGenerics_0.56.0         generics_0.1.4             
    ## [23] lubridate_1.9.5             forcats_1.0.1              
    ## [25] stringr_1.6.0               dplyr_1.2.1                
    ## [27] purrr_1.2.2                 readr_2.2.0                
    ## [29] tidyr_1.3.2                 tibble_3.3.1               
    ## [31] ggplot2_4.0.3               tidyverse_2.0.0            
    ## [33] rmarkdown_2.31             
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
    ##  [28] textshaping_1.0.5     Hmisc_5.2-6           RSQLite_3.53.2       
    ##  [31] labeling_0.4.3        timechange_0.4.0      mgcv_1.9-4           
    ##  [34] polyclip_1.10-7       httr_1.4.8            abind_1.4-8          
    ##  [37] compiler_4.5.1        proxy_0.4-29          bit64_4.8.2          
    ##  [40] withr_3.0.3           doParallel_1.0.17     htmlTable_2.5.0      
    ##  [43] S7_0.2.2              backports_1.5.1       viridis_0.6.5        
    ##  [46] DBI_1.3.0             ggforce_0.5.0         MASS_7.3-65          
    ##  [49] tkWidgets_1.88.0      DelayedArray_0.36.1   rjson_0.2.23         
    ##  [52] tools_4.5.1           foreign_0.8-91        otel_0.2.0           
    ##  [55] nnet_7.3-20           glue_1.8.1            nlme_3.1-169         
    ##  [58] checkmate_2.3.4       cluster_2.1.8.2       gtable_0.3.6         
    ##  [61] tzdb_0.5.0            preprocessCore_1.72.0 class_7.3-23         
    ##  [64] data.table_1.18.4     hms_1.1.4             tidygraph_1.3.1      
    ##  [67] utf8_1.2.6            XVector_0.50.0        ggrepel_0.9.8        
    ##  [70] foreach_1.5.2         pillar_1.11.1         limma_3.66.0         
    ##  [73] vroom_1.7.1           circlize_0.4.18       splines_4.5.1        
    ##  [76] tweenr_2.0.3          lattice_0.22-9        survival_3.8-6       
    ##  [79] bit_4.6.0             annotate_1.88.0       tidyselect_1.2.1     
    ##  [82] locfit_1.5-9.12       Biostrings_2.78.0     gridExtra_2.3.1      
    ##  [85] xfun_0.59             graphlayouts_1.2.4    statmod_1.5.2        
    ##  [88] stringi_1.8.7         yaml_2.3.12           evaluate_1.0.5       
    ##  [91] codetools_0.2-20      cli_3.6.6             rpart_4.1.27         
    ##  [94] xtable_1.8-8          systemfonts_1.3.2     Rcpp_1.1.1-1.1       
    ##  [97] png_0.1-9             XML_3.99-0.23         parallel_4.5.1       
    ## [100] blob_1.3.0            viridisLite_0.4.3     scales_1.4.0         
    ## [103] crayon_1.5.3          GetoptLong_1.1.1      rlang_1.2.0          
    ## [106] cowplot_1.2.0         KEGGREST_1.50.0

``` r
detach(package:genefilter, unload=TRUE)
detach(package:DESeq2, unload=TRUE)
detach(package:pheatmap, unload=TRUE)
```
