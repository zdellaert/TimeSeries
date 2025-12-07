multiWGCNA
================
Zoe Dellaert
2025-12-03

### Network analysis of Time Series Bulk RNA Data – multiWGCNA

## Introduction

The goal of this script is to identify co-expressed gene modules from
our time-course RNA-seq data. I hope to identify genes which respond to
the heat stress similarly over time.

I will be following [this
vignette](https://bioc.r-universe.dev/articles/multiWGCNA/autism_full_workflow.html)
for the package [multiWGCNA](https://github.com/fogellab/multiWGCNA),
which is described in this
[paper](https://link.springer.com/article/10.1186/s12859-023-05233-z#Abs1).
An example paper with a use of this package I really like is [here,
figure 4](https://www.pnas.org/doi/epub/10.1073/pnas.2420811122).
Helpful *general* WGCNA tutorial to help with parameter decisions can be
found
[here](https://alexslemonade.github.io/refinebio-examples/04-advanced-topics/network-analysis_rnaseq_01_wgcna.html#46_Determine_parameters_for_WGCNA).

## Install necessary packages

First, download the necessary packages.

``` r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("impute", type = "source")
BiocManager::install("WGCNA",force = TRUE)
BiocManager::install("multiWGCNA")
```

### Load packages + general set-up

``` r
library(tidyverse)
library(WGCNA)
library(multiWGCNA)

#set standard output directory for figures
save_ggplot <- function(plot, filename, width = 10, height = 7, units = "in", dpi = 300,bg=NULL) {
  png_path <- file.path(outdir, paste0(filename, ".png"))
  pdf_dir <- file.path(outdir, "pdf_figs")
  pdf_path <- file.path(pdf_dir, paste0(filename, ".pdf"))
  
  # Ensure the pdf_figs directory exists
  if (!dir.exists(pdf_dir)) dir.create(pdf_dir, recursive = TRUE)
  
  # Save plots
  ggsave(filename = png_path, plot = plot, width = width, height = height, units = units, dpi = dpi,bg = bg)
  ggsave(filename = pdf_path, plot = plot, width = width, height = height, units = units, dpi = dpi,bg = bg)
}

treat_colors <- c("C" = "lightblue4", "H" = "#D55E00")
time_colors <- colorRampPalette(c("#ffffcc","#0c2c84"))(7)
names(time_colors) <- c("0", "1", "3", "12", "24", "72", "120")
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
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] multiWGCNA_1.6.0      ggalluvial_0.12.5     WGCNA_1.73           
    ##  [4] fastcluster_1.3.0     dynamicTreeCut_1.63-1 lubridate_1.9.4      
    ##  [7] forcats_1.0.0         stringr_1.6.0         dplyr_1.1.4          
    ## [10] purrr_1.2.0           readr_2.1.5           tidyr_1.3.1          
    ## [13] tibble_3.3.0          ggplot2_4.0.1         tidyverse_2.0.0      
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] DBI_1.2.3                   gridExtra_2.3              
    ##   [3] rlang_1.1.6                 magrittr_2.0.4             
    ##   [5] matrixStats_1.5.0           compiler_4.5.1             
    ##   [7] RSQLite_2.4.1               png_0.1-8                  
    ##   [9] vctrs_0.6.5                 pkgconfig_2.0.3            
    ##  [11] crayon_1.5.3                fastmap_1.2.0              
    ##  [13] backports_1.5.0             XVector_0.50.0             
    ##  [15] rmarkdown_2.30              tzdb_0.5.0                 
    ##  [17] UCSC.utils_1.4.0            preprocessCore_1.70.0      
    ##  [19] bit_4.6.0                   xfun_0.54                  
    ##  [21] cachem_1.1.0                dcanr_1.24.0               
    ##  [23] flashClust_1.01-2           GenomeInfoDb_1.44.3        
    ##  [25] jsonlite_2.0.0              blob_1.2.4                 
    ##  [27] DelayedArray_0.36.0         parallel_4.5.1             
    ##  [29] cluster_2.1.8.1             R6_2.6.1                   
    ##  [31] stringi_1.8.7               RColorBrewer_1.1-3         
    ##  [33] rpart_4.1.24                GenomicRanges_1.62.0       
    ##  [35] Rcpp_1.1.0                  Seqinfo_1.0.0              
    ##  [37] SummarizedExperiment_1.40.0 iterators_1.0.14           
    ##  [39] knitr_1.50                  base64enc_0.1-3            
    ##  [41] IRanges_2.44.0              igraph_2.1.4               
    ##  [43] Matrix_1.7-3                splines_4.5.1              
    ##  [45] nnet_7.3-20                 timechange_0.3.0           
    ##  [47] tidyselect_1.2.1            abind_1.4-8                
    ##  [49] rstudioapi_0.17.1           dichromat_2.0-0.1          
    ##  [51] yaml_2.3.11                 doParallel_1.0.17          
    ##  [53] codetools_0.2-20            doRNG_1.8.6.2              
    ##  [55] lattice_0.22-7              Biobase_2.70.0             
    ##  [57] withr_3.0.2                 KEGGREST_1.48.1            
    ##  [59] S7_0.2.1                    evaluate_1.0.5             
    ##  [61] foreign_0.8-90              survival_3.8-3             
    ##  [63] Biostrings_2.76.0           pillar_1.11.1              
    ##  [65] rngtools_1.5.2              MatrixGenerics_1.22.0      
    ##  [67] checkmate_2.3.2             foreach_1.5.2              
    ##  [69] stats4_4.5.1                generics_0.1.4             
    ##  [71] S4Vectors_0.48.0            hms_1.1.3                  
    ##  [73] scales_1.4.0                glue_1.8.0                 
    ##  [75] Hmisc_5.2-3                 tools_4.5.1                
    ##  [77] data.table_1.17.8           cowplot_1.2.0              
    ##  [79] grid_4.5.1                  impute_1.82.0              
    ##  [81] AnnotationDbi_1.70.0        colorspace_2.1-2           
    ##  [83] patchwork_1.3.2             GenomeInfoDbData_1.2.14    
    ##  [85] htmlTable_2.4.3             Formula_1.2-5              
    ##  [87] cli_3.6.5                   S4Arrays_1.10.0            
    ##  [89] gtable_0.3.6                digest_0.6.39              
    ##  [91] BiocGenerics_0.56.0         ggrepel_0.9.6              
    ##  [93] SparseArray_1.10.2          htmlwidgets_1.6.4          
    ##  [95] farver_2.1.2                memoise_2.0.1              
    ##  [97] htmltools_0.5.8.1           lifecycle_1.0.4            
    ##  [99] httr_1.4.7                  GO.db_3.21.0               
    ## [101] bit64_4.6.0-1

## POC

### Pre-processing

Read in variance-stabilized count info and metadata

``` r
outdir <- "../output_RNA/multiWGCNA/POC_PacutaV2/"
getwd()
```

    ## [1] "/project/pi_hputnam_uri_edu/zdellaert/TimeSeries/4-multi-species/code"

``` r
vst <- read.csv("../output_RNA/differential_expression/POC_PacutaV2/vsd_expression_matrix.csv")

vst <- vst %>% column_to_rownames(var = "X")
normalized_counts <- t(vst)

meta <- read.csv("../output_RNA/differential_expression/POC_PacutaV2/RNA_seq_metadata.csv")
meta <- meta %>% column_to_rownames(var = "X") %>% select(-c(species, replicate))
```

### First, identify and remove any outliers

Cluster the samples and view as a tree.

``` r
sampleTree = hclust(dist(normalized_counts), method = "average")
sizeGrWindow(12,9) 
par(cex = 0.6);
par(mar = c(0,4,2,0))
plot(sampleTree)
```

I did not identify outliers based on this plot.

### Determine parameters for WGCNA

<https://alexslemonade.github.io/refinebio-examples/04-advanced-topics/network-analysis_rnaseq_01_wgcna.html#46_Determine_parameters_for_WGCNA>

<https://bioinformaticsworkbook.org/tutorials/wgcna.html#gsc.tab=0>

``` r
options(stringsAsFactors = FALSE)
enableWGCNAThreads()
```

    ## Allowing parallel execution with up to 63 working processes.

``` r
#set powers to test
powers = c(c(1:10), seq(from = 12, to=40, by=2))

#the below takes a long time to run, so is commented out and the pre-run results are loaded in below
#sft <- pickSoftThreshold(normalized_counts, power=powers, networkType = "signed")
#save(sft, file = paste0(outdir, "sft.RData"))
load(paste0(outdir, "sft.RData"))

sft_df <- data.frame(sft$fitIndices) %>% dplyr::mutate(model_fit = -sign(slope) * SFT.R.sq)

ggplot(sft_df, aes(x = Power, y = model_fit, label = Power)) +
  geom_point() +
  geom_text(nudge_y = 0.1) +
  # We will plot what WGCNA recommends as an R^2 cutoff
  geom_hline(yintercept = 0.80, col = "red") +
  ylim(c(min(sft_df$model_fit), 1.05)) +
  xlab("Soft Threshold (power)") +
  ylab("Scale Free Topology Model Fit, signed R^2") +
  ggtitle("Scale independence") +
  theme_classic()
```

![](multiWGCNA_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(sft_df, aes(x = Power, y = mean.k., label = Power)) +
  geom_point() +
  geom_text(nudge_y = 500) +
  xlab("Soft Threshold (power)") +
  ylab("Mean Connectivity") +
  ggtitle("Mean Connectivity") +
  theme_classic()
```

![](multiWGCNA_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

We will move forward with Power = 12.

### One-step module detection

``` r
picked_power = 12

temp_cor <- cor
cor <- WGCNA::cor # Force it to use WGCNA cor function (fix a namespace conflict issue)
netwk <- blockwiseModules(normalized_counts,
                          nThreads = 16,

                          # Adjacency Function
                          power = picked_power,
                          corType = "bicor",
                          networkType = "signed",
                          TOMType = "signed",

                          # Tree and Block Options
                          deepSplit = 1,
                          pamRespectsDendro = F,
                          minModuleSize = 30,
                          maxBlockSize = 50000,

                          # topological overlap matrix, (TOM)
                          saveTOMs = TRUE,
                          saveTOMFileBase = paste0(outdir, "blockwiseTOM"),
                          #loadTOM = FALSE, #uncomment this if you are re-running with a previously saved TOM

                          # Output Options
                          mergeCutHeight = 0.25,
                          numericLabels = TRUE,
                          verbose = 3)

cor <- temp_cor     # Return cor function to original namespace
readr::write_rds(netwk, file = file.path(outdir, "wgcna_results.RDS"))
```

``` r
# load WGCNAresults
netwk <- readr::read_rds(file = file.path(outdir, "wgcna_results.RDS"))

# what is stored in this object?
names(netwk)
```

    ##  [1] "colors"         "unmergedColors" "MEs"            "goodSamples"   
    ##  [5] "goodGenes"      "dendrograms"    "TOMFiles"       "blockGenes"    
    ##  [9] "blocks"         "MEsOK"

``` r
# save the module labels
moduleLabels = netwk$colors

# how many modules are there?
paste("There are", length(unique(moduleLabels)), "modules in our current analysis.")
```

    ## [1] "There are 25 modules in our current analysis."

``` r
# see the distribution of genes across these labelled modules
table(netwk$colors)
```

    ## 
    ##    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ## 3800 3787 3256 2796 1772 1624 1586  860  768  694  548  523  390  386  308  272 
    ##   16   17   18   19   20   21   22   23   24 
    ##  227  202  188  173  151  145  130  104   98

``` r
# Convert labels to colors for plotting
moduleColors = labels2colors(moduleLabels)

# Plot the dendrogram and the module colors underneath
plotDendroAndColors(
  netwk$dendrograms[[1]],
  moduleColors,
  "Module colors",
  dendroLabels = FALSE,
  hang = 0.03,
  addGuide = TRUE,
  guideHang = 0.05, main = "Consensus gene dendrogram and module colors")
```

![](multiWGCNA_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Treatment and Time Module Correlation

``` r
# save the module info as a dataframe and txt file
module_df <- data.frame(
  gene_id = names(netwk$colors),
  module = paste0("ME", netwk$colors),
  color = labels2colors(netwk$colors)
)

head(module_df)
```

    ##                                     gene_id module     color
    ## 1     Pocillopora_acuta_HIv2___TS.g10153.t1    ME1 turquoise
    ## 2    Pocillopora_acuta_HIv2___TS.g28292.t1a    ME1 turquoise
    ## 3     Pocillopora_acuta_HIv2___TS.g28295.t1    ME2      blue
    ## 4     Pocillopora_acuta_HIv2___TS.g28301.t1    ME8      pink
    ## 5 Pocillopora_acuta_HIv2___RNAseq.g10157.t1    ME2      blue
    ## 6     Pocillopora_acuta_HIv2___TS.g10172.t1   ME16 lightcyan

``` r
write_delim(module_df, file = paste0(outdir,"gene_modules.txt"), delim = "\t")

# get the module eigengenes
module_eigengenes <- netwk$MEs
head(module_eigengenes)
```

    ##                    ME5         ME12         ME8         ME7        ME23
    ## POC_R0_C1 -0.055427058  0.071865825 -0.13012560 -0.11397561 -0.07076291
    ## POC_R0_C2 -0.030216898 -0.122451461 -0.07813953 -0.14020447 -0.24960645
    ## POC_R0_C3 -0.033165655 -0.047864436 -0.04958979 -0.11817335 -0.04200445
    ## POC_R0_H1 -0.028049139 -0.004143458 -0.14838840 -0.08653988  0.07730481
    ## POC_R0_H2  0.008369229 -0.076107560 -0.03473621 -0.10799470 -0.26820251
    ## POC_R0_H3  0.066125427  0.095891696 -0.08526258  0.10116217 -0.23300316
    ##                  ME2         ME6        ME15         ME13        ME21
    ## POC_R0_C1 -0.1285302 -0.05477251  0.06230250 -0.038733684 -0.13026062
    ## POC_R0_C2 -0.1336087 -0.16424113  0.14456414  0.121388722  0.09540330
    ## POC_R0_C3 -0.1427822 -0.13710310  0.10775194  0.002475003 -0.04976814
    ## POC_R0_H1 -0.1309182 -0.06975706 -0.05013472 -0.015507185 -0.08885978
    ## POC_R0_H2 -0.1508540 -0.18549632  0.19263682  0.086180101 -0.02044825
    ## POC_R0_H3 -0.1289640 -0.12755846  0.13675463  0.233571593  0.16103308
    ##                   ME17          ME9        ME11       ME20       ME24
    ## POC_R0_C1 -0.016981077  0.004737978  0.02528915 0.06570487 0.05117312
    ## POC_R0_C2  0.104838744 -0.163978576 -0.11085756 0.09898933 0.04872028
    ## POC_R0_C3 -0.022265787  0.053708750 -0.03907366 0.05361465 0.08905346
    ## POC_R0_H1 -0.009398839  0.184461816  0.01122931 0.06245619 0.14479023
    ## POC_R0_H2 -0.076118169 -0.172102618 -0.11923361 0.09355343 0.03281290
    ## POC_R0_H3 -0.031396391 -0.147953059 -0.07350157 0.10444632 0.13840435
    ##                  ME3         ME4        ME14        ME16        ME19
    ## POC_R0_C1 0.16368724  0.22646363 -0.08752314 -0.14084844 -0.04236932
    ## POC_R0_C2 0.18245300  0.11927213 -0.03540100 -0.11231991  0.02838421
    ## POC_R0_C3 0.13512723  0.06662749 -0.09462258 -0.11058993 -0.02151330
    ## POC_R0_H1 0.13749257  0.13672163 -0.09472251  0.06717273 -0.05118971
    ## POC_R0_H2 0.16273171  0.07679049 -0.08116125 -0.13386479 -0.00252362
    ## POC_R0_H3 0.06296345 -0.18051058 -0.04437940 -0.06482205  0.09348777
    ##                  ME10       ME22        ME1        ME18         ME0
    ## POC_R0_C1  0.16954322 0.07304600 0.07338757 0.159631125 -0.09548937
    ## POC_R0_C2  0.14862143 0.03748504 0.13526552 0.009937077 -0.04777610
    ## POC_R0_C3  0.08903449 0.06407156 0.13932645 0.142677641 -0.07075658
    ## POC_R0_H1  0.15086889 0.08196196 0.07560302 0.127519923 -0.10269888
    ## POC_R0_H2  0.09266639 0.07988181 0.17423785 0.137387282 -0.06167330
    ## POC_R0_H3 -0.05492273 0.06617152 0.16219846 0.079728927 -0.03998675

``` r
# get a list of all the genes in a module
gene_module_key <- tibble::enframe(netwk$colors, name = "gene", value = "module") %>%
  # Let's add the `ME` part so its more clear what these numbers are and it matches elsewhere
  dplyr::mutate(module = paste0("ME", module))

# confirm that the sample metadata and sample labels for the module eigengenes are matching
all.equal(meta$sample, rownames(module_eigengenes))
```

    ## [1] TRUE

#### Time+Treatment-Module Correlation Heatmaps

``` r
nSamples = nrow(normalized_counts)

time_treat_factorial <- meta %>%
  mutate(group = paste0(time, "hr-", ifelse(treatment == "C", "Control", "Heat"))) %>%
  select(sample, group) %>% 
  mutate(value = 1) %>% 
  tidyr::pivot_wider(names_from = group, values_from = value, values_fill = 0) %>%
  column_to_rownames(var = "sample") %>%
  relocate(contains("Control"), contains("Heat")) %>%
   mutate(across(everything(), as.factor))

# Reorder modules so similar modules are next to each other
module_eigengenes_ordered <- orderMEs(module_eigengenes)
module_order = names(module_eigengenes_ordered) 

moduleTraitCor =  WGCNA::cor(module_eigengenes_ordered, time_treat_factorial, use = "p");
moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples);

textMatrix <- ifelse(moduleTraitPvalue < 0.05,
                     paste0(signif(moduleTraitCor, 2), "\n(",
                            signif(moduleTraitPvalue, 2), ")"),"")

pdf(paste0(outdir,"/all_heatmap.pdf"),width=8, height=8)
# Will display correlations and their p-values

par(mar = c(4, 3, 2, 2))
labeledHeatmap(Matrix = moduleTraitCor,
               textMatrix = textMatrix,
               xLabels = names(time_treat_factorial),
               yLabels = names(module_eigengenes_ordered),
               ySymbols = names(module_eigengenes_ordered),
               colorLabels = TRUE,
               colors = blueWhiteRed(100),
               setStdMargins = FALSE,
               cex.text = 0.5,
               cex.lab = 0.7,
               cex.colorLabels = 0.7,
               zlim = c(-1,1),
               main = paste("Module-trait relationships - all"))

dev.off()
```

    ## png 
    ##   2

##### ggplot version

``` r
# Add treatment names
module_eigengenes_ordered$treatment_time = paste0(meta$time,"hr","-",ifelse(meta$treatment == "C", "Control", "Heat"))
module_eigengenes_ordered$treatment = meta$treatment
module_eigengenes_ordered <- module_eigengenes_ordered %>% arrange(treatment)

mmPval = moduleTraitPvalue %>% as.data.frame() %>% rownames_to_column("module") %>%
  pivot_longer(-module, names_to = "treatment_time", values_to = "pvalue")

mmCor = moduleTraitCor %>% as.data.frame() %>% rownames_to_column("module") %>%
  pivot_longer(-module, names_to = "treatment_time", values_to = "correlation") %>%
  left_join(mmPval, by = c("module", "treatment_time")) %>%
  mutate(
    label = ifelse(pvalue < 0.05,
                   paste0(signif(correlation, 2)), ""),
    # use this if you want the p-value also plotted
    #label = ifelse(pvalue < 0.05,
    #               paste0(signif(correlation, 2), "\n(", signif(pvalue, 2), ")"), ""),
    module = factor(module, levels = rev(module_order)),
    treatment_time = factor(treatment_time, levels = unique(module_eigengenes_ordered$treatment_time)))

ggplot(mmCor, aes(x=treatment_time, y=module, fill=correlation)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = label), size = 3, color = "black") +
  theme_minimal(base_size = 12) +
  scale_fill_gradient2(
    low = "#4575B4",
    high = "#D73027",
    mid = "white",
    midpoint = 0,
    limits = c(-1, 1)) +
  labs(title = "Module-trait Relationships", y = "Modules", fill="Correlation")+ 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  ) +coord_fixed(ratio = 0.7) 
```

![](multiWGCNA_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
save_ggplot(plot = last_plot(), filename = "all_heatmap_ggplot", width = 10, height = 8)
```

#### Run linear model on each module vs. treatment

``` r
# Create the design matrix for full (with interaction) models, use factor for time since non-evenly spaced intervals

meta$time_factor <- factor(meta$time)
des_mat_full <- model.matrix(~ treatment*time_factor, data = meta)
head(des_mat_full)
```

    ##           (Intercept) treatmentH time_factor1 time_factor3 time_factor12
    ## POC_R0_C1           1          0            0            0             0
    ## POC_R0_C2           1          0            0            0             0
    ## POC_R0_C3           1          0            0            0             0
    ## POC_R0_H1           1          1            0            0             0
    ## POC_R0_H2           1          1            0            0             0
    ## POC_R0_H3           1          1            0            0             0
    ##           time_factor24 time_factor72 time_factor120 treatmentH:time_factor1
    ## POC_R0_C1             0             0              0                       0
    ## POC_R0_C2             0             0              0                       0
    ## POC_R0_C3             0             0              0                       0
    ## POC_R0_H1             0             0              0                       0
    ## POC_R0_H2             0             0              0                       0
    ## POC_R0_H3             0             0              0                       0
    ##           treatmentH:time_factor3 treatmentH:time_factor12
    ## POC_R0_C1                       0                        0
    ## POC_R0_C2                       0                        0
    ## POC_R0_C3                       0                        0
    ## POC_R0_H1                       0                        0
    ## POC_R0_H2                       0                        0
    ## POC_R0_H3                       0                        0
    ##           treatmentH:time_factor24 treatmentH:time_factor72
    ## POC_R0_C1                        0                        0
    ## POC_R0_C2                        0                        0
    ## POC_R0_C3                        0                        0
    ## POC_R0_H1                        0                        0
    ## POC_R0_H2                        0                        0
    ## POC_R0_H3                        0                        0
    ##           treatmentH:time_factor120
    ## POC_R0_C1                         0
    ## POC_R0_C2                         0
    ## POC_R0_C3                         0
    ## POC_R0_H1                         0
    ## POC_R0_H2                         0
    ## POC_R0_H3                         0

``` r
# lmFit() needs a transposed version of the matrix
fit_full <- limma::lmFit(t(module_eigengenes), design = des_mat_full)

# Apply empirical Bayes to smooth standard errors
fit_full <- limma::eBayes(fit_full)

# Apply multiple testing correction and obtain stats

## interaction <- treatment effect differs by time
interaction_coefs <- grep("treatment.*:time", colnames(des_mat_full), value = TRUE)

stats_interaction <- limma::topTable(fit_full, coef = interaction_coefs, number = ncol(module_eigengenes)) %>%
  tibble::rownames_to_column("module")

stats_df_full <- limma::topTable(fit_full, number = ncol(module_eigengenes)) %>%
  tibble::rownames_to_column("module")

# we care most about the interaction, the full model will pull out modules that vary in both treatments by time also (like a circadian rhythm 0hr vs 12hr difference)

# almost all of our modules significant by this model:
stats_interaction %>% filter(adj.P.Val < 0.05)  %>% nrow()
```

    ## [1] 17

``` r
#save these as a vector
top_mod_sig_interaction <- stats_interaction %>% filter(adj.P.Val < 0.05)  %>% pull(module)

# print the top 5:
stats_interaction %>% filter(adj.P.Val < 0.05)  %>% head(5)
```

    ##   module treatmentH.time_factor1 treatmentH.time_factor3
    ## 1    ME2              0.06897635              0.36735227
    ## 2    ME0              0.07933062              0.54731190
    ## 3   ME20             -0.02843876              0.02564334
    ## 4    ME1             -0.09660943             -0.41331973
    ## 5    ME5             -0.17238093             -0.33087139
    ##   treatmentH.time_factor12 treatmentH.time_factor24 treatmentH.time_factor72
    ## 1               0.28447991              0.314179266                0.3052496
    ## 2               0.01324718             -0.002409258                0.0221069
    ## 3              -0.23242705             -0.260384023               -0.3449217
    ## 4              -0.35215507             -0.310156332               -0.1950631
    ## 5               0.01926794              0.119967118                0.1860590
    ##   treatmentH.time_factor120       AveExpr        F      P.Value    adj.P.Val
    ## 1                0.31225201 -1.486906e-18 46.25216 4.514939e-14 1.128735e-12
    ## 2                0.05195676 -1.928847e-17 22.72287 4.713186e-10 5.891482e-09
    ## 3               -0.30852149  1.218437e-18 20.92963 1.276613e-09 1.063845e-08
    ## 4               -0.23183818  1.078007e-17 12.72497 3.471887e-07 2.169930e-06
    ## 5                0.13050790  2.911857e-18 11.38533 1.086417e-06 5.432084e-06

Module 2 is the most differentially expressed across treatments + in the
full model.

#### Plot example module over time

``` r
eigengenes_treatment_df <- module_eigengenes %>%
  tibble::rownames_to_column("sample") %>%
  dplyr::inner_join(meta %>%
    dplyr::select(sample, treatment,time),
  by = c("sample" = "sample"))

ggplot(eigengenes_treatment_df, aes(x = factor(time), y = ME2,color = treatment)) +
  geom_boxplot(outlier.shape = NA) +
  ggforce::geom_sina(size=1, alpha = 0.5) +
  scale_color_manual(values = treat_colors) +
  theme_classic()
```

![](multiWGCNA_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

#### Trajectory plots for all modules

``` r
eigengenes_treatment_df_long <- eigengenes_treatment_df %>%
  pivot_longer(cols = starts_with("ME"),
               names_to = "module",
               values_to = "eigengene_value") %>%
  mutate(module = factor(module, levels = module_order)) %>%
  mutate(module_label = ifelse(module %in% top_mod_sig_interaction, 
                                paste0("*",module), 
                                as.character(module)))

ggplot(eigengenes_treatment_df_long, aes(x = factor(time), y = eigengene_value,color = treatment)) +
  geom_boxplot(outlier.shape = NA) +
  ggforce::geom_sina(size=1, alpha = 0.5) +
  scale_color_manual(values = treat_colors) +
  facet_wrap(~module_label, ncol = 5) +
  theme_classic() + theme(
    strip.text = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 7),
    legend.position = "bottom") +
  labs(x = "Time (hours)", y = "Module Eigengene")
```

![](multiWGCNA_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
save_ggplot(plot = last_plot(), filename = "all_modules", width = 14, height = 12)

eigengenes_summary <- eigengenes_treatment_df_long %>%
  group_by(module, module_label, time, treatment) %>%
  summarize(mean_value = mean(eigengene_value),
            se = sd(eigengene_value) / sqrt(n()),
            .groups = "drop")

ggplot(eigengenes_summary, aes(x = factor(time), y = mean_value, color = treatment, group = treatment)) +
  geom_line(linewidth = 0.5) +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2) +
  scale_color_manual(values = treat_colors) +
  facet_wrap(~module_label, ncol = 5, scales = "free_y") +
  theme_classic() +
  theme(
    strip.text = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 7),
    legend.position = "bottom") +
  labs(x = "Time (hours)", y = "Module Eigengene")
```

![](multiWGCNA_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
save_ggplot(plot = last_plot(), filename = "all_modules_lines", width = 14, height = 12)
```

#### Heatmap function based heavily off of the following tutorial:

<https://alexslemonade.github.io/refinebio-examples/04-advanced-topics/network-analysis_rnaseq_01_wgcna.html#46_Determine_parameters_for_WGCNA>

``` r
make_module_heatmap <- function(module_name,
                                expression_mat = normalized_counts,
                                metadata_df = meta,
                                gene_module_key_df = module_df,
                                module_eigengenes_df = module_eigengenes) {
  # Create a summary heatmap of a given module.
  # based on https://alexslemonade.github.io/refinebio-examples/04-advanced-topics/network-analysis_rnaseq_01_wgcna.html#46_Determine_parameters_for_WGCNA

  # Set up the module eigengene with its sample
  module_eigengene <- module_eigengenes_df %>%
    dplyr::select(all_of(module_name)) %>%
    tibble::rownames_to_column("sample")

  # Set up column annotation from metadata
  col_annot_df <- metadata_df %>%
    # Only select the treatment, time, and sample ID columns
    dplyr::select(sample, treatment, time) %>%
    # Add on the eigengene expression by joining with sample IDs
    dplyr::inner_join(module_eigengene, by = "sample") %>%
    # Arrange by treatment and time point
    dplyr::arrange(treatment, time, sample) %>%
    # Store sample
    tibble::column_to_rownames("sample")

  # Create the ComplexHeatmap column annotation object
  col_annot <- ComplexHeatmap::HeatmapAnnotation(
    # Supply treatment and time labels
    treatment = col_annot_df$treatment,
    time = col_annot_df$time,
    # Add annotation barplot
    module_eigengene = ComplexHeatmap::anno_barplot(dplyr::select(col_annot_df, module_name)),
    # Pick colors for each experimental group in treatment
    col = list(treatment = c("C" = "lightblue4", "H" = "#D55E00"),
               time = time_colors)
  )

  # Get a vector of the gene IDs that correspond to this module
  module_genes <- gene_module_key_df %>%
    dplyr::filter(module == module_name) %>%
    dplyr::pull(gene_id)

  # Set up the gene expression data frame
  mod_mat <- expression_mat %>%
    t() %>%
    as.data.frame() %>%
    # Only keep genes from this module
    dplyr::filter(rownames(.) %in% module_genes) %>%
    # Order the samples to match col_annot_df
    dplyr::select(rownames(col_annot_df)) %>%
    # Data needs to be a matrix
    as.matrix()

  # Normalize the gene expression values
  mod_mat <- mod_mat %>%
    # Scale can work on matrices, but it does it by column so we will need to
    # transpose first
    t() %>%
    scale() %>%
    # And now we need to transpose back
    t()

  # Create a color function based on standardized scale
  color_func <- circlize::colorRamp2(
    c(-1.5, 0, 1.5),
    c("#67a9cf", "#f7f7f7", "#ef8a62")
  )

  # Plot on a heatmap
  heatmap <- ComplexHeatmap::Heatmap(mod_mat,
    name = module_name,
    # Supply color function
    col = color_func,
    # Supply column annotation
    bottom_annotation = col_annot,
    # We don't want to cluster samples
    cluster_columns = FALSE,
    # We don't need to show sample or gene labels
    show_row_names = FALSE,
    show_column_names = FALSE
  )

  # Return heatmap
  return(heatmap)
}
```

``` r
make_module_heatmap(module_name = "ME2")
```

![](multiWGCNA_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
make_module_heatmap(module_name = "ME0")
```

![](multiWGCNA_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
make_module_heatmap(module_name = "ME21")
```

![](multiWGCNA_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

### multiWGCNA

``` r
# Construct the combined networks
autism_networks = constructNetworks(autism_se, meta, unique(meta$time), unique(meta$treatment),
                                  networkType = "signed", power = 12,
                                  minModuleSize = 50, maxBlockSize = 25000,
                                  reassignThreshold = 0, minKMEtoStay = 0.7,
                                  mergeCutHeight = 0.10, numericLabels = TRUE,
                                  pamRespectsDendro = FALSE, verbose=3,
                                  saveTOMs = TRUE)
```
