ColorScore
================
Zoe Dellaert
2026-02-18

## Color Score Analysis of Time Series Image Data

Basing this on [Emma Strand’s
script](https://github.com/emmastrand/EmmaStrand_Notebook/blob/master/Dani_colorscore/Colorscore.md)

Look at the Hackerott et al repo
[here](https://github.com/eelabfiu/CoralColorScore) for further analysis
ideas

### Load libraries

``` r
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(vegan)
library(Rmisc)
library(ggpubr)
library(factoextra)
library(MASS)
```

``` r
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

treat_colors <- c("Control" = "lightblue4", "Heat" = "#D55E00")
outdir <- "../output/ColorScore"
```

## *Pocillopora acuta*

### Read in datafiles

``` r
raw <- read.csv("../../3-Pacu/data/Images/ColorScore.csv", colClasses=c("Date"="character", 
                                                                               "Timepoint"="character",
                                                                               "Plug"="character",
                                                                               "PhotoDate"="character"))  %>%
  dplyr::rename(Sample=FileName)

raw <- raw %>% mutate(Treatment=str_replace(Treatment,"heat","Heat"),
                      Treatment=str_replace(Treatment,"control","Control"))
```

Calculate normalized coral color values based on standard color values
for that image.

``` r
proc <- raw %>% mutate(
    Red.Norm.Coral = Red.Coral/Red.Standard,
    Green.Norm.Coral = Green.Coral/Green.Standard,
    Blue.Norm.Coral = Blue.Coral/Blue.Standard
)

rownames(proc) <- proc$Sample
proc_out_rm <- proc %>% filter(Sample!="POC_PAM_1059")
```

### Color Score Hackerott

#### Run PCA

``` r
Color.PCA<-prcomp(proc_out_rm[,c("Red.Norm.Coral","Green.Norm.Coral","Blue.Norm.Coral")], scale.=TRUE) 

#Initial plot
fviz_pca_ind(Color.PCA)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#Check Variance Explained by Components
summary(Color.PCA)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3
    ## Standard deviation     1.5693 0.6321 0.37123
    ## Proportion of Variance 0.8209 0.1332 0.04594
    ## Cumulative Proportion  0.8209 0.9541 1.00000

``` r
#Visualize the importance of each principal component
fviz_eig(Color.PCA, addlabels = TRUE) 
```

![](ColorScore_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

PC1 Explains 83.1% of the variance in the color data.

``` r
##Extract % Variance PC 1
PCA.PC1<-sprintf("%1.2f",get_eigenvalue(Color.PCA)$variance.percent[1])
PCA.PC1
```

    ## [1] "82.09"

``` r
##Extract % Variance PC 2
PCA.PC2<-sprintf("%1.2f",get_eigenvalue(Color.PCA)$variance.percent[2])
PCA.PC2
```

    ## [1] "13.32"

``` r
##Extract Individual Sample Scores
Color.PCA.scores <- as.data.frame(Color.PCA$x)
Color.PCA.scores$Sample<-rownames(Color.PCA.scores)

#Prepare for Plotting
Color.PCA.scores<-left_join(proc_out_rm, Color.PCA.scores, by = "Sample") 
```

#### Plot PCA

``` r
#Plot PCA
ggplot(data = Color.PCA.scores, aes(x = PC1, y = PC2)) + 
  geom_point(aes(colour = Treatment),  alpha = 0.8) + 
  scale_colour_manual(values =treat_colors)+
  theme_classic()+
  scale_x_continuous(limits = c(-3, 5.5))+
  scale_y_continuous(limits = c(-4.25, 4.25))+
  labs(x=paste0('PC 1 (',PCA.PC1,"%)"), y=paste0('PC 2 (',PCA.PC2,"%)"))+
  ggtitle("Color Score PCA")
```

![](ColorScore_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

#### LDA on PCA Scores

``` r
##LDA on PCA Scores
Color.LDA_PCA<-lda(Treatment~PC1+PC2+PC3, data=Color.PCA.scores)

##Predict
Color.pLDA_PCA<-predict(object=Color.LDA_PCA, newdata=Color.PCA.scores)

##Save LD1 Scores
Color.LDA_PCA.scores<-data.frame(Sample=Color.PCA.scores$Sample, LD1=Color.pLDA_PCA$x)

#Prepare for Plotting
ColorData<-left_join(proc_out_rm, Color.LDA_PCA.scores, by = "Sample") 

# make time and treatment factors
ColorData$Timepoint <- factor(ColorData$Timepoint, levels = as.character(sort(unique(as.numeric(ColorData$Timepoint)))))
ColorData$Treatment <- factor(ColorData$Treatment)
```

#### LD1 Density Plot

``` r
ggplot(data = ColorData, aes(x = LD1)) + 
  geom_density(aes(fill = Treatment), alpha = 0.6) + 
  scale_fill_manual(values =treat_colors)+
  theme_classic()+
  scale_x_continuous(limits = c(-3, 5.5))+
  labs(x="LD1", y= "Density")+
  ggtitle("Full Dataset")
```

![](ColorScore_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

#### Extract Color Score

``` r
##Invert signs for Control > Heated
ColorData$LD1<-ColorData$LD1*(-1)

#Adding 10 to make all score values positive 
#ColorData$LD1<- ColorData$LD1 +10

ColorData %>%
  ggplot(., aes(x=Timepoint, y=LD1, color=Treatment)) +
  geom_jitter(alpha=0.25, size=1.5, width = 0.22) + 
  theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
  scale_color_manual(values = treat_colors) +
  geom_point(stat = "summary", fun = mean, aes(group = Treatment), size=3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, aes(group = Treatment)) +
  geom_line(stat = "summary", fun = mean, size = 1.2, aes(group = Treatment)) +
  stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
save_ggplot(last_plot(), "Pacu_Hackerott")

ColorData_Full <- ColorData
```

#### by timepoint

``` r
for (timepoint in unique(raw$Timepoint)){
  proc_timepoint <- proc %>% filter(Timepoint == timepoint)
  
  proc_out_rm_tp <- proc_timepoint #%>% filter(Sample!="POC_PAM_1059")
  Color.PCA<-prcomp(proc_out_rm_tp[,c("Red.Norm.Coral","Green.Norm.Coral","Blue.Norm.Coral")], scale.=TRUE) 

  ##Extract % Variance PC 1 & 2
  PCA.PC1<-sprintf("%1.2f",get_eigenvalue(Color.PCA)$variance.percent[1])
  PCA.PC2<-sprintf("%1.2f",get_eigenvalue(Color.PCA)$variance.percent[2])

  ##Extract Individual Sample Scores
  Color.PCA.scores <- as.data.frame(Color.PCA$x)
  Color.PCA.scores$Sample<-rownames(Color.PCA.scores)
  
  #Prepare for Plotting
  Color.PCA.scores<-left_join(proc_out_rm_tp, Color.PCA.scores, by = "Sample") 
  
  timepoint_pca <- ggplot(data = Color.PCA.scores, aes(x = PC1, y = PC2)) + 
    geom_point(aes(colour = Treatment),  alpha = 0.8) + 
    scale_colour_manual(values =treat_colors)+
    theme_classic()+
    scale_x_continuous(limits = c(-3, 5.5))+
    scale_y_continuous(limits = c(-4.25, 4.25))+
    labs(x=paste0('PC 1 (',PCA.PC1,"%)"), y=paste0('PC 2 (',PCA.PC2,"%)"))+
    ggtitle(paste0("Color Score PCA: ",timepoint,"hrs"))
  
  print(timepoint_pca)
  
  ##LDA on PCA Scores
  Color.LDA_PCA<-lda(Treatment~PC1+PC2+PC3, data=Color.PCA.scores)
  
  ##Predict
  Color.pLDA_PCA<-predict(object=Color.LDA_PCA, newdata=Color.PCA.scores)
  
  ##Save LD1 Scores
  Color.LDA_PCA.scores<-data.frame(Sample=Color.PCA.scores$Sample, LD1=Color.pLDA_PCA$x)
  
  #Prepare for Plotting
  ColorData<-left_join(proc_out_rm_tp, Color.LDA_PCA.scores, by = "Sample") 
  
  # make time and treatment factors
  ColorData$Timepoint <- factor(ColorData$Timepoint, levels = as.character(sort(unique(as.numeric(ColorData$Timepoint)))))
  ColorData$Treatment <- factor(ColorData$Treatment)
  
  timepoint_density <- ggplot(data = ColorData, aes(x = LD1)) + 
  geom_density(aes(fill = Treatment), alpha = 0.6) + 
  scale_fill_manual(values =treat_colors)+
  theme_classic()+
  scale_x_continuous(limits = c(-3, 5.5))+
  labs(x="LD1", y= "Density") +
    ggtitle(paste0("Density plot: ",timepoint,"hrs"))
  
  print(timepoint_density)

  ##Invert signs for Control > Heated
  ColorData$LD1<-ColorData$LD1*(-1)
  
  #Adding 10 to make all score values positive 
  #ColorData$LD1<- ColorData$LD1 +10

  plot <- ColorData %>%
    ggplot(., aes(x=Timepoint, y=LD1, color=Treatment)) +
    geom_jitter(alpha=0.25, size=1.5, width = 0.22) + 
    theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
    scale_color_manual(values = treat_colors) +
    geom_point(stat = "summary", fun = mean, aes(group = Treatment), size=3) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, aes(group = Treatment)) +
    geom_line(stat = "summary", fun = mean, size = 1.2, aes(group = Treatment)) +
    stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
  
  print(plot)
  
  assign(paste0("ColorData_TP_",timepoint),ColorData)
}
```

![](ColorScore_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-5.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-6.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-7.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-8.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-9.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-10.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-11.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-12.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-13.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-14.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-15.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-16.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-17.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-18.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-19.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-20.png)<!-- -->![](ColorScore_files/figure-gfm/unnamed-chunk-11-21.png)<!-- -->
\#### combine full and by timepoint data

``` r
paste0("ColorData_TP_",unique(raw$Timepoint))
```

    ## [1] "ColorData_TP_0"   "ColorData_TP_1"   "ColorData_TP_3"   "ColorData_TP_12" 
    ## [5] "ColorData_TP_24"  "ColorData_TP_72"  "ColorData_TP_120"

``` r
##Combine results from individual timepoints
ColorData.TP<-dplyr::bind_rows(mget(paste0("ColorData_TP_", unique(raw$Timepoint))))

##Retain PC1 as Color Score
ColorData.TP$Score_TP<-ColorData.TP$LD1

##Initial Visual Check
ggplot(ColorData.TP, aes(x=Timepoint, y=Score_TP, color=Treatment)) + 
  geom_boxplot(alpha=0.5, shape=2, outlier.shape = NA)+
  geom_jitter(shape=16, position=position_jitter(0.1))+
  theme(axis.text.x = element_text(angle = 90))
```

![](ColorScore_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
##Plot by Treatment
ggplot(ColorData.TP, aes(x=Treatment, y=Score_TP)) + 
  geom_boxplot(alpha=0.5, shape=2, outlier.shape = NA)+
  geom_jitter(shape=16, position=position_jitter(0.1))+
  theme(axis.text.x = element_text(angle = 90))
```

![](ColorScore_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
##Merge with Color Data
names(ColorData.TP)
```

    ##  [1] "FileName_Orig"    "Sample"           "Date"             "Timepoint"       
    ##  [5] "Treatment"        "Tank_ID"          "Plug"             "Red.Standard"    
    ##  [9] "Green.Standard"   "Blue.Standard"    "Red.Coral"        "Green.Coral"     
    ## [13] "Blue.Coral"       "Notes"            "Red.Norm.Coral"   "Green.Norm.Coral"
    ## [17] "Blue.Norm.Coral"  "LD1"              "Score_TP"

``` r
ColorData<-merge(ColorData_Full, ColorData.TP[,c("Sample", "Score_TP")])
ColorData <- ColorData %>% dplyr::rename(Score_Full=LD1)
```

#### Correlation

``` r
cor.test(ColorData$Score_Full, ColorData$Score_TP, method="spearman")
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  ColorData$Score_Full and ColorData$Score_TP
    ## S = 388046, p-value = 0.01478
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.2037534

``` r
plot(ColorData$Score_Full, ColorData$Score_TP)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### color score default

``` r
proc_matrix <- as.matrix(cbind(proc_out_rm$Red.Norm.Coral,
                                   proc_out_rm$Green.Norm.Coral,
                                   proc_out_rm$Blue.Norm.Coral)) #create matrix

rownames(proc_matrix) <- proc_out_rm$Sample #name columns in dataframe

dist <- vegdist(proc_matrix, method="euclidean") 

PCA <- princomp(dist)
```

``` r
#extract PC1 as color score
colorscore <- as.data.frame(-PCA$scores[,1]) #extract PC1
colorscore$Sample <- rownames(proc_matrix)

colorscore <- colorscore %>% dplyr::rename(., ColorScore = `-PCA$scores[, 1]`)

final <- left_join(proc, colorscore, by = "Sample") 

# make time and treatment factors
final$Timepoint <- factor(final$Timepoint, levels = as.character(sort(unique(as.numeric(final$Timepoint)))))
final$Treatment <- factor(final$Treatment)
```

### Analyze

``` r
range(final$ColorScore)
```

    ## [1] NA NA

``` r
hist(final$ColorScore)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

### Plot

``` r
final %>%
  ggplot(., aes(x=Timepoint, y=ColorScore, color=Treatment)) +
  geom_jitter(alpha=0.25, size=1.5, width = 0.22) + 
  theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
  scale_color_manual(values = treat_colors) +
  geom_point(stat = "summary", fun = mean, aes(group = Treatment), size=3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, aes(group = Treatment)) +
  geom_line(stat = "summary", fun = mean, size = 1.2, aes(group = Treatment)) +
  stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
save_ggplot(last_plot(), "Pacu")

final %>%
  ggplot(., aes(x=Timepoint, y=ColorScore, color=Treatment, shape=Tank_ID)) +
  geom_jitter(alpha=0.25, size=1.5, width = 0.22) + 
  theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
  scale_color_manual(values = treat_colors) +
  geom_point(stat = "summary", fun = mean, aes(group = Tank_ID), size=3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, size = 0.4, aes(group = Tank_ID)) +
  geom_line(stat = "summary", fun = mean, size = 0.4, aes(group = Tank_ID)) +
  stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
save_ggplot(last_plot(), "Pacu_by_tank")

final %>%
  ggplot(., aes(x=Timepoint, y=ColorScore, color=Treatment, shape=Tank_ID)) +
  theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
  scale_color_manual(values = treat_colors) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.5), width = 0.2, alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(width = 0.5), alpha = 0.5,size=2.5) +
  stat_summary(fun = mean, geom = "point", aes(group = Treatment), size = 2.5) +
  stat_summary(fun = mean, geom = "line", aes(group = Treatment), size = 1.2) +
  stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->

``` r
save_ggplot(last_plot(), "Pacu_tank_means")
```

## *Porites compressa*

### Read in datafiles

``` r
raw <- read.csv("../../1-Pcom/data/Images/ColorScore.csv", colClasses=c("Date"="character", 
                                                                               "Timepoint"="character",
                                                                               "Plug"="character",
                                                                               "PhotoDate"="character"))  %>%
  dplyr::rename(Sample=FileName)
```

Calculate normalized coral color values based on standard color values
for that image.

``` r
proc <- raw %>% mutate(
    Red.Norm.Coral = Red.Coral/Red.Standard,
    Green.Norm.Coral = Green.Coral/Green.Standard,
    Blue.Norm.Coral = Blue.Coral/Blue.Standard
)

rownames(proc) <- proc$Sample
proc_out_rm <- proc #%>% filter(Sample!="POC_PAM_1059")
```

### Color Score Hackerott

#### Run PCA

``` r
Color.PCA<-prcomp(proc_out_rm[,c("Red.Norm.Coral","Green.Norm.Coral","Blue.Norm.Coral")], scale.=TRUE) 

#Initial plot
fviz_pca_ind(Color.PCA)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
#Check Variance Explained by Components
summary(Color.PCA)
```

    ## Importance of components:
    ##                           PC1     PC2     PC3
    ## Standard deviation     1.6307 0.48237 0.32861
    ## Proportion of Variance 0.8864 0.07756 0.03599
    ## Cumulative Proportion  0.8864 0.96401 1.00000

``` r
#Visualize the importance of each principal component
fviz_eig(Color.PCA, addlabels = TRUE) 
```

![](ColorScore_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

PC1 Explains 83.1% of the variance in the color data.

``` r
##Extract % Variance PC 1
PCA.PC1<-sprintf("%1.2f",get_eigenvalue(Color.PCA)$variance.percent[1])
PCA.PC1
```

    ## [1] "88.64"

``` r
##Extract % Variance PC 2
PCA.PC2<-sprintf("%1.2f",get_eigenvalue(Color.PCA)$variance.percent[2])
PCA.PC2
```

    ## [1] "7.76"

``` r
##Extract Individual Sample Scores
Color.PCA.scores <- as.data.frame(Color.PCA$x)
Color.PCA.scores$Sample<-rownames(Color.PCA.scores)

#Prepare for Plotting
Color.PCA.scores<-left_join(proc_out_rm, Color.PCA.scores, by = "Sample") 
```

#### Plot PCA

``` r
#Plot PCA
ggplot(data = Color.PCA.scores, aes(x = PC1, y = PC2)) + 
  geom_point(aes(colour = Treatment),  alpha = 0.8) + 
  scale_colour_manual(values =treat_colors)+
  theme_classic()+
  scale_x_continuous(limits = c(-3, 5.5))+
  scale_y_continuous(limits = c(-4.25, 4.25))+
  labs(x=paste0('PC 1 (',PCA.PC1,"%)"), y=paste0('PC 2 (',PCA.PC2,"%)"))+
  ggtitle("Color Score PCA")
```

![](ColorScore_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

#### LDA on PCA Scores

``` r
##LDA on PCA Scores
Color.LDA_PCA<-lda(Treatment~PC1+PC2+PC3, data=Color.PCA.scores)

##Predict
Color.pLDA_PCA<-predict(object=Color.LDA_PCA, newdata=Color.PCA.scores)

##Save LD1 Scores
Color.LDA_PCA.scores<-data.frame(Sample=Color.PCA.scores$Sample, LD1=Color.pLDA_PCA$x)

#Prepare for Plotting
ColorData<-left_join(proc_out_rm, Color.LDA_PCA.scores, by = "Sample") 

# make time and treatment factors
ColorData$Timepoint <- factor(ColorData$Timepoint, levels = as.character(sort(unique(as.numeric(ColorData$Timepoint)))))
ColorData$Treatment <- factor(ColorData$Treatment)
```

#### LD1 Density Plot

``` r
ggplot(data = ColorData, aes(x = LD1)) + 
  geom_density(aes(fill = Treatment), alpha = 0.6) + 
  scale_fill_manual(values =treat_colors)+
  theme_classic()+
  scale_x_continuous(limits = c(-3, 5.5))+
  labs(x="LD1", y= "Density")+
  ggtitle("Full Dataset")
```

![](ColorScore_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

#### Extract Color Score

``` r
##Invert signs for Control > Heated
ColorData$LD1<-ColorData$LD1*(-1)

#Adding 10 to make all score values positive 
#ColorData$LD1<- ColorData$LD1 +10

ColorData %>%
  ggplot(., aes(x=Timepoint, y=LD1, color=Treatment)) +
  geom_jitter(alpha=0.25, size=1.5, width = 0.22) + 
  theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
  scale_color_manual(values = treat_colors) +
  geom_point(stat = "summary", fun = mean, aes(group = Treatment), size=3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, aes(group = Treatment)) +
  geom_line(stat = "summary", fun = mean, size = 1.2, aes(group = Treatment)) +
  stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
save_ggplot(last_plot(), "Pcom_Hackerott")

ColorData_Full <- ColorData
```

### color score default

``` r
proc_matrix <- as.matrix(cbind(proc_out_rm$Red.Norm.Coral,
                                   proc_out_rm$Green.Norm.Coral,
                                   proc_out_rm$Blue.Norm.Coral)) #create matrix

rownames(proc_matrix) <- proc_out_rm$Sample #name columns in dataframe

dist <- vegdist(proc_matrix, method="euclidean") 

PCA <- princomp(dist)
```

``` r
#extract PC1 as color score
colorscore <- as.data.frame(-PCA$scores[,1]) #extract PC1
colorscore$Sample <- rownames(proc_matrix)

colorscore <- colorscore %>% dplyr::rename(., ColorScore = `-PCA$scores[, 1]`)

final <- left_join(proc, colorscore, by = "Sample") 

# make time and treatment factors
final$Timepoint <- factor(final$Timepoint, levels = as.character(sort(unique(as.numeric(final$Timepoint)))))
final$Treatment <- factor(final$Treatment)
```

### Analyze

``` r
range(final$ColorScore)
```

    ## [1] -4.768232  1.150231

``` r
hist(final$ColorScore)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

### Plot

``` r
final %>%
  ggplot(., aes(x=Timepoint, y=ColorScore, color=Treatment)) +
  geom_jitter(alpha=0.25, size=1.5, width = 0.22) + 
  theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
  scale_color_manual(values = treat_colors) +
  geom_point(stat = "summary", fun = mean, aes(group = Treatment), size=3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, aes(group = Treatment)) +
  geom_line(stat = "summary", fun = mean, size = 1.2, aes(group = Treatment)) +
  stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
save_ggplot(last_plot(), "Pcom")

final %>%
  ggplot(., aes(x=Timepoint, y=ColorScore, color=Treatment, shape=Tank_ID)) +
  geom_jitter(alpha=0.25, size=1.5, width = 0.22) + 
  theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
  scale_color_manual(values = treat_colors) +
  geom_point(stat = "summary", fun = mean, aes(group = Tank_ID), size=3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, size = 0.4, aes(group = Tank_ID)) +
  geom_line(stat = "summary", fun = mean, size = 0.4, aes(group = Tank_ID)) +
  stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-29-2.png)<!-- -->

``` r
save_ggplot(last_plot(), "Pcom_by_tank")

final %>%
  ggplot(., aes(x=Timepoint, y=ColorScore, color=Treatment, shape=Tank_ID)) +
  theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
  scale_color_manual(values = treat_colors) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.5), width = 0.2, alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(width = 0.5), alpha = 0.5,size=2.5) +
  stat_summary(fun = mean, geom = "point", aes(group = Treatment), size = 2.5) +
  stat_summary(fun = mean, geom = "line", aes(group = Treatment), size = 1.2) +
  stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-29-3.png)<!-- -->

``` r
save_ggplot(last_plot(), "Pcom_tank_means")
```

## *Montipora capitata*

### Read in datafiles

``` r
raw <- read.csv("../../2-Mcap/data/Images/ColorScore.csv", colClasses=c("Date"="character", 
                                                                               "Timepoint"="character",
                                                                               "Plug"="character",
                                                                               "PhotoDate"="character"))  %>%
  dplyr::rename(Sample=FileName)

raw <- raw %>% mutate(Treatment=str_replace(Treatment,"heat","Heat"),
                      Treatment=str_replace(Treatment,"control","Control"))
```

Calculate normalized coral color values based on standard color values
for that image.

``` r
proc <- raw %>% mutate(
    Red.Norm.Coral = Red.Coral/Red.Standard,
    Green.Norm.Coral = Green.Coral/Green.Standard,
    Blue.Norm.Coral = Blue.Coral/Blue.Standard
)

rownames(proc) <- proc$Sample
proc_out_rm <- proc %>% filter(Sample!="MON_P12_C2")
```

### Color Score Hackerott

#### Run PCA

``` r
Color.PCA<-prcomp(proc_out_rm[,c("Red.Norm.Coral","Green.Norm.Coral","Blue.Norm.Coral")], scale.=TRUE) 

#Initial plot
fviz_pca_ind(Color.PCA)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
#Check Variance Explained by Components
summary(Color.PCA)
```

    ## Importance of components:
    ##                           PC1     PC2     PC3
    ## Standard deviation     1.6690 0.42728 0.17869
    ## Proportion of Variance 0.9285 0.06086 0.01064
    ## Cumulative Proportion  0.9285 0.98936 1.00000

``` r
#Visualize the importance of each principal component
fviz_eig(Color.PCA, addlabels = TRUE) 
```

![](ColorScore_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->

PC1 Explains 92.9% of the variance in the color data with outlier
removed

``` r
##Extract % Variance PC 1
PCA.PC1<-sprintf("%1.2f",get_eigenvalue(Color.PCA)$variance.percent[1])
PCA.PC1
```

    ## [1] "92.85"

``` r
##Extract % Variance PC 2
PCA.PC2<-sprintf("%1.2f",get_eigenvalue(Color.PCA)$variance.percent[2])
PCA.PC2
```

    ## [1] "6.09"

``` r
##Extract Individual Sample Scores
Color.PCA.scores <- as.data.frame(Color.PCA$x)
Color.PCA.scores$Sample<-rownames(Color.PCA.scores)

#Prepare for Plotting
Color.PCA.scores<-left_join(proc_out_rm, Color.PCA.scores, by = "Sample") 
```

#### Plot PCA

``` r
#Plot PCA
ggplot(data = Color.PCA.scores, aes(x = PC1, y = PC2)) + 
  geom_point(aes(colour = Treatment),  alpha = 0.8) + 
  scale_colour_manual(values =treat_colors)+
  theme_classic()+
  scale_x_continuous(limits = c(-3, 5.5))+
  scale_y_continuous(limits = c(-4.25, 4.25))+
  labs(x=paste0('PC 1 (',PCA.PC1,"%)"), y=paste0('PC 2 (',PCA.PC2,"%)"))+
  ggtitle("Color Score PCA")
```

![](ColorScore_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

#### LDA on PCA Scores

``` r
##LDA on PCA Scores
Color.LDA_PCA<-lda(Treatment~PC1+PC2+PC3, data=Color.PCA.scores)

##Predict
Color.pLDA_PCA<-predict(object=Color.LDA_PCA, newdata=Color.PCA.scores)

##Save LD1 Scores
Color.LDA_PCA.scores<-data.frame(Sample=Color.PCA.scores$Sample, LD1=Color.pLDA_PCA$x)

#Prepare for Plotting
ColorData<-left_join(proc_out_rm, Color.LDA_PCA.scores, by = "Sample") 

# make time and treatment factors
ColorData$Timepoint <- factor(ColorData$Timepoint, levels = as.character(sort(unique(as.numeric(ColorData$Timepoint)))))
ColorData$Treatment <- factor(ColorData$Treatment)
```

#### LD1 Density Plot

``` r
ggplot(data = ColorData, aes(x = LD1)) + 
  geom_density(aes(fill = Treatment), alpha = 0.6) + 
  scale_fill_manual(values =treat_colors)+
  theme_classic()+
  scale_x_continuous(limits = c(-3, 5.5))+
  labs(x="LD1", y= "Density")+
  ggtitle("Full Dataset")
```

![](ColorScore_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

#### Extract Color Score

``` r
##Invert signs for Control > Heated
ColorData$LD1<-ColorData$LD1*(-1)

#Adding 10 to make all score values positive 
#ColorData$LD1<- ColorData$LD1 +10

ColorData %>%
  ggplot(., aes(x=Timepoint, y=LD1, color=Treatment)) +
  geom_jitter(alpha=0.25, size=1.5, width = 0.22) + 
  theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
  scale_color_manual(values = treat_colors) +
  geom_point(stat = "summary", fun = mean, aes(group = Treatment), size=3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, aes(group = Treatment)) +
  geom_line(stat = "summary", fun = mean, size = 1.2, aes(group = Treatment)) +
  stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
save_ggplot(last_plot(), "Mcap_Hackerott")

ColorData_Full <- ColorData
```

### color score default

``` r
proc_matrix <- as.matrix(cbind(proc_out_rm$Red.Norm.Coral,
                                   proc_out_rm$Green.Norm.Coral,
                                   proc_out_rm$Blue.Norm.Coral)) #create matrix

rownames(proc_matrix) <- proc_out_rm$Sample #name columns in dataframe

dist <- vegdist(proc_matrix, method="euclidean") 

PCA <- princomp(dist)
```

``` r
#extract PC1 as color score
colorscore <- as.data.frame(-PCA$scores[,1]) #extract PC1
colorscore$Sample <- rownames(proc_matrix)

colorscore <- colorscore %>% dplyr::rename(., ColorScore = `-PCA$scores[, 1]`)

final <- left_join(proc, colorscore, by = "Sample") 

# make time and treatment factors
final$Timepoint <- factor(final$Timepoint, levels = as.character(sort(unique(as.numeric(final$Timepoint)))))
final$Treatment <- factor(final$Treatment)
```

### Analyze

``` r
range(final$ColorScore)
```

    ## [1] NA NA

``` r
hist(final$ColorScore)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

### Plot

``` r
final %>%
  ggplot(., aes(x=Timepoint, y=ColorScore, color=Treatment)) +
  geom_jitter(alpha=0.25, size=1.5, width = 0.22) + 
  theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
  scale_color_manual(values = treat_colors) +
  geom_point(stat = "summary", fun = mean, aes(group = Treatment), size=3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, aes(group = Treatment)) +
  geom_line(stat = "summary", fun = mean, size = 1.2, aes(group = Treatment)) +
  stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
save_ggplot(last_plot(), "Mcap")

final %>%
  ggplot(., aes(x=Timepoint, y=ColorScore, color=Treatment, shape=Tank_ID)) +
  geom_jitter(alpha=0.25, size=1.5, width = 0.22) + 
  theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
  scale_color_manual(values = treat_colors) +
  geom_point(stat = "summary", fun = mean, aes(group = Tank_ID), size=3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, size = 0.4, aes(group = Tank_ID)) +
  geom_line(stat = "summary", fun = mean, size = 0.4, aes(group = Tank_ID)) +
  stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-41-2.png)<!-- -->

``` r
save_ggplot(last_plot(), "Mcap_by_tank")

final %>%
  ggplot(., aes(x=Timepoint, y=ColorScore, color=Treatment, shape=Tank_ID)) +
  theme_minimal() + labs(x = "Timepoint",y = "Color Score") +
  scale_color_manual(values = treat_colors) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.5), width = 0.2, alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(width = 0.5), alpha = 0.5,size=2.5) +
  stat_summary(fun = mean, geom = "point", aes(group = Treatment), size = 2.5) +
  stat_summary(fun = mean, geom = "line", aes(group = Treatment), size = 1.2) +
  stat_compare_means(aes(group = Treatment),method = "anova",label = "p.format",size = 3)
```

![](ColorScore_files/figure-gfm/unnamed-chunk-41-3.png)<!-- -->

``` r
save_ggplot(last_plot(), "Mcap_tank_means")
```
