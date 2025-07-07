PAM Mcap
================
Zoe Dellaert
2025-06-24

- [0.1 Statistical Mixed Model by treatment, timepoint, and
  tank_id](#01-statistical-mixed-model-by-treatment-timepoint-and-tank_id)

``` r
library(tidyverse)
library(janitor)
library(ggpubr)
library(lme4)
library(lmerTest)
library(emmeans)


custom_colors <- c("Control" = "lightblue4", "Heat" = "#D55E00")

PAM <- read.csv("../data/PAM.csv") %>% clean_names()
PAM <- PAM %>% mutate(date = as.factor(date)) %>% 
                mutate(timepoint = factor(timepoint)) %>%#,levels = c("0","1","3","6","12",
                                                    #           "24","36","48","72","120","170"), ordered = TRUE)) %>% 
                mutate(plug = as.factor(plug)) %>%
                mutate(treatment = factor(treatment,levels = c("Acclimation","Recovery","Control","Heat", ordered=TRUE)))

ggplot(PAM,aes(x = date, y = fv_fm_y_1000)) + 
    geom_boxplot(aes(fill = tank_id)) + labs(x = "Date", y = "Fv/Fm", title = "Fv/Fm by Date and Tank") +
  theme_minimal()
```

<img src="PAM_files/figure-gfm/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

``` r
outlier_dates <- c("20250620")
  
PAM <- PAM %>% filter(!(date %in% outlier_dates))

ggplot(PAM,aes(x = date, y = fv_fm_y_1000)) + 
    geom_boxplot(aes(fill = treatment)) + labs(x = "Date", y = "Fv/Fm", title = "Fv/Fm by Date and Tank") +
  theme_minimal()
```

<img src="PAM_files/figure-gfm/unnamed-chunk-1-2.png" style="display: block; margin: auto;" />

``` r
ggsave("../output/FvFm_recovery_acclimation.png", plot = last_plot(), width = 8, height = 4, bg = "white")
```

``` r
PAM_exp <- PAM %>% filter(treatment!="Acclimation" & treatment!="Recovery")
table(PAM_exp$plug)
```

    ## 
    ## 1041 1056 1086 1108 1113 1145 1211 1248 1250 1252 1339 1441 1458 1461 1472 1494 
    ##    9    9    9    8    9    0    9    9    9    9    9    9    9    9    8    9 
    ## 1538 1548 1549 1560 1563 1597 1631 1739 2084 2360 2852 
    ##    0    0    9    9    9    9    9    9    9    9    9

``` r
ggplot(PAM_exp,aes(x = timepoint, y = fv_fm_y_1000)) + 
    geom_boxplot(aes(fill = treatment)) +theme_minimal() +scale_fill_manual(values = custom_colors) 
```

<img src="PAM_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

``` r
ggplot(PAM_exp,aes(x = timepoint, y = fv_fm_y_1000, group = plug)) + 
    geom_path(aes(color=plug)) + theme_minimal() #+facet_wrap(tank_id~treatment)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-2-2.png" style="display: block; margin: auto;" />

``` r
ggplot(PAM_exp,aes(x = timepoint, y = fv_fm_y_1000, group = plug)) + 
    geom_path(aes(color=tank_id)) + theme_minimal() #+facet_wrap(tank_id~treatment)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-2-3.png" style="display: block; margin: auto;" />

``` r
# mixed model
model <- lm(fv_fm_y_1000 ~ timepoint, data = PAM_exp)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = fv_fm_y_1000 ~ timepoint, data = PAM_exp)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.160667 -0.038466  0.008609  0.038986  0.109333 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.6178333  0.0109945  56.195  < 2e-16 ***
    ## timepoint1    0.0008623  0.0157166   0.055 0.956300    
    ## timepoint3    0.0135000  0.0155485   0.868 0.386308    
    ## timepoint12   0.0025580  0.0157166   0.163 0.870877    
    ## timepoint24  -0.0082917  0.0155485  -0.533 0.594440    
    ## timepoint36  -0.0115833  0.0155485  -0.745 0.457167    
    ## timepoint72  -0.0234697  0.0158980  -1.476 0.141460    
    ## timepoint96  -0.0174242  0.0158980  -1.096 0.274408    
    ## timepoint120 -0.0591667  0.0160942  -3.676 0.000305 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05386 on 198 degrees of freedom
    ##   (9 observations deleted due to missingness)
    ## Multiple R-squared:  0.1195, Adjusted R-squared:  0.08397 
    ## F-statistic:  3.36 on 8 and 198 DF,  p-value: 0.001237

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ timepoint)
pairs(emm)
```

    ##  contrast                    estimate     SE  df t.ratio p.value
    ##  timepoint0 - timepoint1    -0.000862 0.0157 198  -0.055  1.0000
    ##  timepoint0 - timepoint3    -0.013500 0.0155 198  -0.868  0.9943
    ##  timepoint0 - timepoint12   -0.002558 0.0157 198  -0.163  1.0000
    ##  timepoint0 - timepoint24    0.008292 0.0155 198   0.533  0.9998
    ##  timepoint0 - timepoint36    0.011583 0.0155 198   0.745  0.9980
    ##  timepoint0 - timepoint72    0.023470 0.0159 198   1.476  0.8653
    ##  timepoint0 - timepoint96    0.017424 0.0159 198   1.096  0.9743
    ##  timepoint0 - timepoint120   0.059167 0.0161 198   3.676  0.0091
    ##  timepoint1 - timepoint3    -0.012638 0.0157 198  -0.804  0.9967
    ##  timepoint1 - timepoint12   -0.001696 0.0159 198  -0.107  1.0000
    ##  timepoint1 - timepoint24    0.009154 0.0157 198   0.582  0.9997
    ##  timepoint1 - timepoint36    0.012446 0.0157 198   0.792  0.9970
    ##  timepoint1 - timepoint72    0.024332 0.0161 198   1.515  0.8475
    ##  timepoint1 - timepoint96    0.018287 0.0161 198   1.138  0.9676
    ##  timepoint1 - timepoint120   0.060029 0.0163 198   3.693  0.0086
    ##  timepoint3 - timepoint12    0.010942 0.0157 198   0.696  0.9988
    ##  timepoint3 - timepoint24    0.021792 0.0155 198   1.402  0.8962
    ##  timepoint3 - timepoint36    0.025083 0.0155 198   1.613  0.7966
    ##  timepoint3 - timepoint72    0.036970 0.0159 198   2.325  0.3321
    ##  timepoint3 - timepoint96    0.030924 0.0159 198   1.945  0.5833
    ##  timepoint3 - timepoint120   0.072667 0.0161 198   4.515  0.0004
    ##  timepoint12 - timepoint24   0.010850 0.0157 198   0.690  0.9989
    ##  timepoint12 - timepoint36   0.014141 0.0157 198   0.900  0.9928
    ##  timepoint12 - timepoint72   0.026028 0.0161 198   1.620  0.7926
    ##  timepoint12 - timepoint96   0.019982 0.0161 198   1.244  0.9455
    ##  timepoint12 - timepoint120  0.061725 0.0163 198   3.797  0.0060
    ##  timepoint24 - timepoint36   0.003292 0.0155 198   0.212  1.0000
    ##  timepoint24 - timepoint72   0.015178 0.0159 198   0.955  0.9893
    ##  timepoint24 - timepoint96   0.009133 0.0159 198   0.574  0.9997
    ##  timepoint24 - timepoint120  0.050875 0.0161 198   3.161  0.0466
    ##  timepoint36 - timepoint72   0.011886 0.0159 198   0.748  0.9980
    ##  timepoint36 - timepoint96   0.005841 0.0159 198   0.367  1.0000
    ##  timepoint36 - timepoint120  0.047583 0.0161 198   2.957  0.0820
    ##  timepoint72 - timepoint96  -0.006045 0.0162 198  -0.372  1.0000
    ##  timepoint72 - timepoint120  0.035697 0.0164 198   2.172  0.4278
    ##  timepoint96 - timepoint120  0.041742 0.0164 198   2.540  0.2194
    ## 
    ## P value adjustment: tukey method for comparing a family of 9 estimates

## 0.1 Statistical Mixed Model by treatment, timepoint, and tank_id

``` r
# mixed model
model <- lmer(fv_fm_y_1000 ~ treatment * timepoint + (1 | treatment:tank_id), data = PAM_exp)
summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: fv_fm_y_1000 ~ treatment * timepoint + (1 | treatment:tank_id)
    ##    Data: PAM_exp
    ## 
    ## REML criterion at convergence: -693.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4232 -0.5391  0.1860  0.7318  1.8665 
    ## 
    ## Random effects:
    ##  Groups            Name        Variance  Std.Dev.
    ##  treatment:tank_id (Intercept) 3.472e-05 0.005893
    ##  Residual                      1.163e-03 0.034109
    ## Number of obs: 207, groups:  treatment:tank_id, 6
    ## 
    ## Fixed effects:
    ##                              Estimate Std. Error         df t value Pr(>|t|)
    ## (Intercept)                  0.622250   0.010418  70.036914  59.731  < 2e-16
    ## treatmentHeat               -0.008833   0.014733  70.036914  -0.600 0.550725
    ## timepoint1                   0.018045   0.014241 185.238204   1.267 0.206717
    ## timepoint3                   0.030417   0.013925 185.078438   2.184 0.030195
    ## timepoint12                  0.025134   0.014241 185.238204   1.765 0.079226
    ## timepoint24                  0.015583   0.013925 185.078438   1.119 0.264549
    ## timepoint36                  0.012833   0.013925 185.078438   0.922 0.357932
    ## timepoint72                  0.012333   0.013925 185.078438   0.886 0.376927
    ## timepoint96                  0.035500   0.013925 185.078438   2.549 0.011602
    ## timepoint120                 0.008333   0.013925 185.078438   0.598 0.550273
    ## treatmentHeat:timepoint1    -0.032211   0.019918 185.160504  -1.617 0.107532
    ## treatmentHeat:timepoint3    -0.033833   0.019693 185.078438  -1.718 0.087459
    ## treatmentHeat:timepoint12   -0.042884   0.019918 185.160504  -2.153 0.032606
    ## treatmentHeat:timepoint24   -0.047750   0.019693 185.078438  -2.425 0.016280
    ## treatmentHeat:timepoint36   -0.048833   0.019693 185.078438  -2.480 0.014041
    ## treatmentHeat:timepoint72   -0.079190   0.020182 185.176653  -3.924 0.000123
    ## treatmentHeat:timepoint96   -0.116857   0.020182 185.176653  -5.790 2.96e-08
    ## treatmentHeat:timepoint120  -0.158268   0.020507 185.434248  -7.718 7.10e-13
    ##                               
    ## (Intercept)                ***
    ## treatmentHeat                 
    ## timepoint1                    
    ## timepoint3                 *  
    ## timepoint12                .  
    ## timepoint24                   
    ## timepoint36                   
    ## timepoint72                   
    ## timepoint96                *  
    ## timepoint120                  
    ## treatmentHeat:timepoint1      
    ## treatmentHeat:timepoint3   .  
    ## treatmentHeat:timepoint12  *  
    ## treatmentHeat:timepoint24  *  
    ## treatmentHeat:timepoint36  *  
    ## treatmentHeat:timepoint72  ***
    ## treatmentHeat:timepoint96  ***
    ## treatmentHeat:timepoint120 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ treatment | timepoint)
pairs(emm)
```

    ## timepoint = 0:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.00883 0.0147 69.2   0.600  0.5507
    ## 
    ## timepoint = 1:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.04104 0.0150 72.9   2.730  0.0079
    ## 
    ## timepoint = 3:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.04267 0.0147 69.2   2.896  0.0051
    ## 
    ## timepoint = 12:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.05172 0.0150 72.9   3.440  0.0010
    ## 
    ## timepoint = 24:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.05658 0.0147 69.2   3.841  0.0003
    ## 
    ## timepoint = 36:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.05767 0.0147 69.2   3.914  0.0002
    ## 
    ## timepoint = 72:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.08802 0.0154 77.5   5.722  <.0001
    ## 
    ## timepoint = 96:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.12569 0.0154 77.5   8.170  <.0001
    ## 
    ## timepoint = 120:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.16710 0.0158 82.5  10.564  <.0001
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
summary_table <- as.data.frame(pairs(emm)) %>%
  transmute(
    Timepoint = as.numeric(as.character(timepoint)),
    `Estimate (Control-Heat)` = round(estimate, 4),
    `SE` = round(SE, 4),
    `t-ratio` = round(t.ratio, 2),
    `p-value` = signif(p.value, 3),
    `Significant?` = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
  )

print(summary_table)
```

    ##   Timepoint Estimate (Control-Heat)     SE t-ratio  p-value Significant?
    ## 1         0                  0.0088 0.0147    0.60 5.51e-01             
    ## 2         1                  0.0410 0.0150    2.73 7.94e-03           **
    ## 3         3                  0.0427 0.0147    2.90 5.05e-03           **
    ## 4        12                  0.0517 0.0150    3.44 9.66e-04          ***
    ## 5        24                  0.0566 0.0147    3.84 2.69e-04          ***
    ## 6        36                  0.0577 0.0147    3.91 2.10e-04          ***
    ## 7        72                  0.0880 0.0154    5.72 1.89e-07          ***
    ## 8        96                  0.1257 0.0154    8.17 4.57e-12          ***
    ## 9       120                  0.1671 0.0158   10.56 5.40e-17          ***

``` r
write.csv(summary_table, "../output/FvFm_treatment_effect_summary.csv", row.names = FALSE)
```

``` r
contrast_table <- as.data.frame(pairs(emm)) %>%
  mutate(
    timepoint = as.numeric(as.character(timepoint)),
    signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
  )

ggplot(contrast_table, aes(x = timepoint, y = estimate)) +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE),width = 3, alpha = 0.6) +
  geom_line(size = 1, aes(group = 1), color = "black", alpha = 0.7) +
  geom_point(size = 2.5, aes(color = p.value < 0.05)) +
  geom_text(aes(label = signif), vjust = -2.5, size = 5, fontface = "bold", color = "black") +
  scale_color_manual(values = c("TRUE" = "#D55E00", "FALSE" = "grey60"), name = "p < 0.05") +
  labs(
    title = "Estimated Treatment Effect (Control-Heat) on Fv/Fm",
    x = "Timepoint (h)",
    y = "Estimated Difference in Fv/Fm") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),panel.grid.major.x = element_blank())
```

<img src="PAM_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r
ggsave("../output/FvFm_line_treatment_tank_modelestimates.png", plot = last_plot(), width = 8, height = 4, bg = "white")
ggsave("../output/pdf_figs/FvFm_line_treatment_tank_modelestimates.pdf", plot = last_plot(), width = 8, height = 4)
```

``` r
PAM_means_treatment <- PAM_exp %>%
  group_by(date, timepoint, treatment) %>%
  summarise(
    FvFm_mean = mean(fv_fm_y_1000, na.rm = TRUE),
    FvFm_SE = sd(fv_fm_y_1000, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

ggplot(PAM_means_treatment, aes(x = timepoint, y = FvFm_mean, color = treatment,group = treatment)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = FvFm_mean - FvFm_SE, ymax = FvFm_mean + FvFm_SE),
                width = 0.2) +
  geom_line(stat = "summary", fun = mean, size = 1.2, aes(group = treatment)) +
  labs(x = "Timepoint",y = "Mean Fv/Fm") +
  theme_minimal() +scale_color_manual(values = custom_colors)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r
PAM_means <- PAM_exp %>%
  group_by(date, timepoint, treatment, tank_id) %>%
  summarise(
    FvFm_mean = mean(fv_fm_y_1000, na.rm = TRUE),
    FvFm_SE = sd(fv_fm_y_1000, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

ggplot(PAM_means, aes(x = timepoint, y = FvFm_mean, color = treatment, shape = tank_id)) +
  geom_point(stat = "summary", fun = mean, aes(group = treatment), size=2.5) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5, alpha=0.5) +
  geom_errorbar(aes(ymin = FvFm_mean - FvFm_SE, ymax = FvFm_mean + FvFm_SE),
                width = 0.2, position = position_dodge(width = 0.5), alpha=0.5) +
  geom_line(stat = "summary", fun = mean, size = 1.2, aes(group = treatment)) +
  labs(x = "Timepoint",y = "Mean Fv/Fm") +
  theme_minimal() +scale_color_manual(values = custom_colors) +
  stat_compare_means(aes(group = treatment),method = "anova",label = "p.format",size = 2.5)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-7-2.png" style="display: block; margin: auto;" />

``` r
ggsave("../output/FvFm_line_treatment_tank_means.png", plot = last_plot(), width = 8, height = 4, bg = "white")
ggsave("../output/pdf_figs/FvFm_line_treatment_tank_means.pdf", plot = last_plot(), width = 8, height = 4)

ggplot(PAM_exp, aes(x = timepoint, y = fv_fm_y_1000, color = treatment, shape = tank_id)) +
  geom_point(stat = "summary", fun = mean, aes(group = treatment), size=2.5) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5, alpha=0.5) +
  geom_line(stat = "summary", fun = mean, size = 1.2, aes(group = treatment)) +
  labs(x = "Timepoint",y = "Mean Fv/Fm") +
  theme_minimal() +scale_color_manual(values = custom_colors) +
  stat_compare_means(aes(group = treatment),method = "anova",label = "p.format",size = 3)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-7-3.png" style="display: block; margin: auto;" />

``` r
ggsave("../output/FvFm_line_treatment_all_points.png", plot = last_plot(), width = 8, height = 4, bg = "white")
ggsave("../output/pdf_figs/FvFm_line_treatment_all_points.pdf", plot = last_plot(), width = 8, height = 4)

ggplot(PAM_means, aes(x = timepoint, y = FvFm_mean, color = tank_id, group = tank_id)) +
  geom_line(size = 0.8, alpha = 0.7) +
  labs(x = "Timepoint",y = "Mean Fv/Fm") +
  theme_minimal() + facet_wrap(~treatment)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-7-4.png" style="display: block; margin: auto;" />

``` r
ggplot(PAM_exp, aes(x = timepoint, y = fv_fm_y_1000, color = treatment)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) 
```

<img src="PAM_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
PAM_exp %>% filter(dark_adapt_mins != "overnight") %>% ggplot(aes(x = dark_adapt_mins, y = fv_fm_y_1000)) + 
    geom_point(aes(color=treatment)) +
  geom_smooth(aes(group = treatment, color = treatment), method = "lm", se = FALSE) +  # or method = "loess"
  theme_minimal() + scale_color_manual(values = custom_colors)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />
