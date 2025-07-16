PAM Pcomp
================
Zoe Dellaert
2025-06-24

- [0.1 Statistical Mixed Model by treatment, timepoint, and
  tank_id](#01-statistical-mixed-model-by-treatment-timepoint-and-tank_id)
- [0.2 Outliers](#02-outliers)
  - [0.2.1 Remove outliers via IQR](#021-remove-outliers-via-iqr)
- [0.3 Statistical Mixed Model by treatment, timepoint, and
  tank_id](#03-statistical-mixed-model-by-treatment-timepoint-and-tank_id)

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
outlier_dates <- c("20250609","20250610","20250620")
  
PAM <- PAM %>% filter(!(date %in% outlier_dates))

# remove frags that were not designated PAM frags

PAM <- PAM %>% filter(!grepl("not designated PAM fragment",notes))

ggplot(PAM,aes(x = date, y = fv_fm_y_1000)) + 
    geom_boxplot(aes(fill = treatment)) + labs(x = "Date", y = "Fv/Fm", title = "Fv/Fm by Date and Tank") +
  theme_minimal()
```

<img src="PAM_files/figure-gfm/unnamed-chunk-1-2.png" style="display: block; margin: auto;" />

``` r
ggsave("../output/FvFm_recovery_acclimation.png", plot = last_plot(), width = 8, height = 4, bg = "white")
```

``` r
PAM_exp <- PAM %>% filter(treatment!="Acclimation" & treatment!="Recovery") %>% mutate(plug = fct_drop(plug))
table(PAM_exp$plug)
```

    ## 
    ## 1043 1065 1102 1122 1130 1133 1209 1244 1281 1337 1473 1555 1575 1615 1626 1761 
    ##    9    9    9    9    9    9    9    9    9    9    9    9    9    9    9    9 
    ## 1771 2730 
    ##    9    9

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
    ## REML criterion at convergence: -343.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9627 -0.4501  0.1138  0.6426  2.0192 
    ## 
    ## Random effects:
    ##  Groups            Name        Variance  Std.Dev.
    ##  treatment:tank_id (Intercept) 0.0007746 0.02783 
    ##  Residual                      0.0038888 0.06236 
    ## Number of obs: 162, groups:  treatment:tank_id, 6
    ## 
    ## Fixed effects:
    ##                              Estimate Std. Error         df t value Pr(>|t|)
    ## (Intercept)                  0.621000   0.026273  19.453457  23.636 8.53e-16
    ## treatmentHeat                0.008222   0.037156  19.453457   0.221 0.827173
    ## timepoint1                   0.029333   0.029397 140.000000   0.998 0.320079
    ## timepoint3                   0.031333   0.029397 140.000000   1.066 0.288316
    ## timepoint12                  0.017889   0.029397 140.000000   0.609 0.543822
    ## timepoint24                  0.002000   0.029397 140.000000   0.068 0.945855
    ## timepoint36                  0.029111   0.029397 140.000000   0.990 0.323746
    ## timepoint72                 -0.060222   0.029397 140.000000  -2.049 0.042370
    ## timepoint96                 -0.038889   0.029397 140.000000  -1.323 0.188028
    ## timepoint120                -0.067667   0.029397 140.000000  -2.302 0.022820
    ## treatmentHeat:timepoint1    -0.023222   0.041573 140.000000  -0.559 0.577339
    ## treatmentHeat:timepoint3    -0.048111   0.041573 140.000000  -1.157 0.249139
    ## treatmentHeat:timepoint12   -0.048667   0.041573 140.000000  -1.171 0.243741
    ## treatmentHeat:timepoint24   -0.044778   0.041573 140.000000  -1.077 0.283299
    ## treatmentHeat:timepoint36   -0.075333   0.041573 140.000000  -1.812 0.072121
    ## treatmentHeat:timepoint72   -0.114111   0.041573 140.000000  -2.745 0.006849
    ## treatmentHeat:timepoint96   -0.165222   0.041573 140.000000  -3.974 0.000113
    ## treatmentHeat:timepoint120  -0.194111   0.041573 140.000000  -4.669 7.01e-06
    ##                               
    ## (Intercept)                ***
    ## treatmentHeat                 
    ## timepoint1                    
    ## timepoint3                    
    ## timepoint12                   
    ## timepoint24                   
    ## timepoint36                   
    ## timepoint72                *  
    ## timepoint96                   
    ## timepoint120               *  
    ## treatmentHeat:timepoint1      
    ## treatmentHeat:timepoint3      
    ## treatmentHeat:timepoint12     
    ## treatmentHeat:timepoint24     
    ## treatmentHeat:timepoint36  .  
    ## treatmentHeat:timepoint72  ** 
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
    ##  Control - Heat -0.00822 0.0372 19.4  -0.221  0.8272
    ## 
    ## timepoint = 1:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.01500 0.0372 19.4   0.404  0.6908
    ## 
    ## timepoint = 3:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.03989 0.0372 19.4   1.074  0.2962
    ## 
    ## timepoint = 12:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.04044 0.0372 19.4   1.089  0.2897
    ## 
    ## timepoint = 24:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.03656 0.0372 19.4   0.984  0.3373
    ## 
    ## timepoint = 36:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.06711 0.0372 19.4   1.806  0.0864
    ## 
    ## timepoint = 72:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.10589 0.0372 19.4   2.850  0.0101
    ## 
    ## timepoint = 96:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.15700 0.0372 19.4   4.225  0.0004
    ## 
    ## timepoint = 120:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.18589 0.0372 19.4   5.003  0.0001
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
    ## 1         0                 -0.0082 0.0372   -0.22 8.27e-01             
    ## 2         1                  0.0150 0.0372    0.40 6.91e-01             
    ## 3         3                  0.0399 0.0372    1.07 2.96e-01             
    ## 4        12                  0.0404 0.0372    1.09 2.90e-01             
    ## 5        24                  0.0366 0.0372    0.98 3.37e-01             
    ## 6        36                  0.0671 0.0372    1.81 8.64e-02            .
    ## 7        72                  0.1059 0.0372    2.85 1.01e-02            *
    ## 8        96                  0.1570 0.0372    4.23 4.38e-04          ***
    ## 9       120                  0.1859 0.0372    5.00 7.38e-05          ***

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

<img src="PAM_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

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

<img src="PAM_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

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

<img src="PAM_files/figure-gfm/unnamed-chunk-6-2.png" style="display: block; margin: auto;" />

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

<img src="PAM_files/figure-gfm/unnamed-chunk-6-3.png" style="display: block; margin: auto;" />

``` r
ggsave("../output/FvFm_line_treatment_all_points.png", plot = last_plot(), width = 8, height = 4, bg = "white")
ggsave("../output/pdf_figs/FvFm_line_treatment_all_points.pdf", plot = last_plot(), width = 8, height = 4)

ggplot(PAM_means, aes(x = timepoint, y = FvFm_mean, color = tank_id, group = tank_id)) +
  geom_line(size = 0.8, alpha = 0.7) +
  labs(x = "Timepoint",y = "Mean Fv/Fm") +
  theme_minimal() + facet_wrap(~treatment)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-6-4.png" style="display: block; margin: auto;" />

``` r
ggplot(PAM_exp, aes(x = timepoint, y = fv_fm_y_1000, color = treatment)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) 
```

<img src="PAM_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r
PAM_exp %>% filter(dark_adapt_mins != "overnight") %>% ggplot(aes(x = dark_adapt_mins, y = fv_fm_y_1000)) + 
    geom_point(aes(color=treatment)) +
  geom_smooth(aes(group = treatment, color = treatment), method = "lm", se = FALSE) +  # or method = "loess"
  theme_minimal() + scale_color_manual(values = custom_colors) 
```

<img src="PAM_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
PAM_exp %>% filter(date=="20250626") %>% ggplot(aes(x = notes, y = fv_fm_y_1000)) + 
    geom_point(aes(color=treatment)) +
  theme_minimal(base_size = 6) + scale_color_manual(values = custom_colors) 
```

<img src="PAM_files/figure-gfm/unnamed-chunk-8-2.png" style="display: block; margin: auto;" />

## 0.2 Outliers

``` r
ggplot(PAM_exp, aes(x = timepoint, y = fv_fm_y_1000, color = treatment)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) 
```

<img src="PAM_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

``` r
ggplot(PAM_exp, aes(x = treatment, y = fv_fm_y_1000)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) 
```

<img src="PAM_files/figure-gfm/unnamed-chunk-9-2.png" style="display: block; margin: auto;" />

### 0.2.1 Remove outliers via IQR

``` r
# IQR method 
Q1 <- quantile(PAM_exp$fv_fm_y_1000, 0.25)
Q3 <- quantile(PAM_exp$fv_fm_y_1000, 0.75)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val
outliers <- PAM_exp[PAM_exp$fv_fm_y_1000 < lower_bound | PAM_exp$fv_fm_y_1000 > upper_bound, ]
print(outliers)
```

    ##         date treatment timepoint tank_id  time initials dark_adapt_mins plug
    ## 121 20250628      Heat        72       2 09:03       ZD       overnight 1122
    ## 130 20250629      Heat        96       2 10:30       ZD              45 1122
    ## 157 20250630      Heat       120       2             ZD       overnight 1122
    ##      mem fluorescence_yield_f max_fluorescence_yield_m_fm yield_y fv_fm_y_1000
    ## 121 2924                  346                         466     257        0.257
    ## 130 2939                  307                         392     216        0.216
    ## 157 2977                  221                         271     184        0.184
    ##     notes
    ## 121      
    ## 130      
    ## 157

``` r
# Filter dataset to remove outliers
PAM_exp_filtered_IQR <- PAM_exp[PAM_exp$fv_fm_y_1000 >= lower_bound & PAM_exp$fv_fm_y_1000 <= upper_bound, ]

# Check dimensions before and after
dim(PAM_exp)
```

    ## [1] 162  14

``` r
dim(PAM_exp_filtered_IQR)
```

    ## [1] 159  14

``` r
table(PAM_exp_filtered_IQR$plug)
```

    ## 
    ## 1043 1065 1102 1122 1130 1133 1209 1244 1281 1337 1473 1555 1575 1615 1626 1761 
    ##    9    9    9    6    9    9    9    9    9    9    9    9    9    9    9    9 
    ## 1771 2730 
    ##    9    9

``` r
ggplot(PAM_exp_filtered_IQR,aes(x = timepoint, y = fv_fm_y_1000)) + 
    geom_boxplot(aes(fill = treatment)) +theme_minimal() +scale_fill_manual(values = custom_colors) 
```

<img src="PAM_files/figure-gfm/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

``` r
ggplot(PAM_exp_filtered_IQR,aes(x = timepoint, y = fv_fm_y_1000, group = plug)) + 
    geom_path(aes(color=plug)) + theme_minimal() #+facet_wrap(tank_id~treatment)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-11-2.png" style="display: block; margin: auto;" />

``` r
ggplot(PAM_exp_filtered_IQR,aes(x = timepoint, y = fv_fm_y_1000, group = plug)) + 
    geom_path(aes(color=tank_id)) + theme_minimal() #+facet_wrap(tank_id~treatment)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-11-3.png" style="display: block; margin: auto;" />

``` r
# mixed model
model <- lm(fv_fm_y_1000 ~ timepoint, data = PAM_exp_filtered_IQR)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = fv_fm_y_1000 ~ timepoint, data = PAM_exp_filtered_IQR)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.217556 -0.043127  0.006444  0.044121  0.155353 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.625111   0.016860  37.076  < 2e-16 ***
    ## timepoint1    0.017722   0.023844   0.743    0.458    
    ## timepoint3    0.007278   0.023844   0.305    0.761    
    ## timepoint12  -0.006444   0.023844  -0.270    0.787    
    ## timepoint24  -0.020389   0.023844  -0.855    0.394    
    ## timepoint36  -0.008556   0.023844  -0.359    0.720    
    ## timepoint72  -0.102523   0.024192  -4.238 3.93e-05 ***
    ## timepoint96  -0.104582   0.024192  -4.323 2.79e-05 ***
    ## timepoint120 -0.148464   0.024192  -6.137 7.16e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07153 on 150 degrees of freedom
    ## Multiple R-squared:  0.3998, Adjusted R-squared:  0.3678 
    ## F-statistic: 12.49 on 8 and 150 DF,  p-value: 1.218e-13

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ timepoint)
pairs(emm)
```

    ##  contrast                   estimate     SE  df t.ratio p.value
    ##  timepoint0 - timepoint1    -0.01772 0.0238 150  -0.743  0.9980
    ##  timepoint0 - timepoint3    -0.00728 0.0238 150  -0.305  1.0000
    ##  timepoint0 - timepoint12    0.00644 0.0238 150   0.270  1.0000
    ##  timepoint0 - timepoint24    0.02039 0.0238 150   0.855  0.9948
    ##  timepoint0 - timepoint36    0.00856 0.0238 150   0.359  1.0000
    ##  timepoint0 - timepoint72    0.10252 0.0242 150   4.238  0.0013
    ##  timepoint0 - timepoint96    0.10458 0.0242 150   4.323  0.0009
    ##  timepoint0 - timepoint120   0.14846 0.0242 150   6.137  <.0001
    ##  timepoint1 - timepoint3     0.01044 0.0238 150   0.438  1.0000
    ##  timepoint1 - timepoint12    0.02417 0.0238 150   1.014  0.9841
    ##  timepoint1 - timepoint24    0.03811 0.0238 150   1.598  0.8044
    ##  timepoint1 - timepoint36    0.02628 0.0238 150   1.102  0.9731
    ##  timepoint1 - timepoint72    0.12025 0.0242 150   4.970  0.0001
    ##  timepoint1 - timepoint96    0.12230 0.0242 150   5.055  <.0001
    ##  timepoint1 - timepoint120   0.16619 0.0242 150   6.869  <.0001
    ##  timepoint3 - timepoint12    0.01372 0.0238 150   0.575  0.9997
    ##  timepoint3 - timepoint24    0.02767 0.0238 150   1.160  0.9634
    ##  timepoint3 - timepoint36    0.01583 0.0238 150   0.664  0.9991
    ##  timepoint3 - timepoint72    0.10980 0.0242 150   4.539  0.0004
    ##  timepoint3 - timepoint96    0.11186 0.0242 150   4.624  0.0003
    ##  timepoint3 - timepoint120   0.15574 0.0242 150   6.438  <.0001
    ##  timepoint12 - timepoint24   0.01394 0.0238 150   0.585  0.9997
    ##  timepoint12 - timepoint36   0.00211 0.0238 150   0.089  1.0000
    ##  timepoint12 - timepoint72   0.09608 0.0242 150   3.971  0.0035
    ##  timepoint12 - timepoint96   0.09814 0.0242 150   4.057  0.0025
    ##  timepoint12 - timepoint120  0.14202 0.0242 150   5.870  <.0001
    ##  timepoint24 - timepoint36  -0.01183 0.0238 150  -0.496  0.9999
    ##  timepoint24 - timepoint72   0.08213 0.0242 150   3.395  0.0241
    ##  timepoint24 - timepoint96   0.08419 0.0242 150   3.480  0.0184
    ##  timepoint24 - timepoint120  0.12808 0.0242 150   5.294  <.0001
    ##  timepoint36 - timepoint72   0.09397 0.0242 150   3.884  0.0047
    ##  timepoint36 - timepoint96   0.09603 0.0242 150   3.969  0.0035
    ##  timepoint36 - timepoint120  0.13991 0.0242 150   5.783  <.0001
    ##  timepoint72 - timepoint96   0.00206 0.0245 150   0.084  1.0000
    ##  timepoint72 - timepoint120  0.04594 0.0245 150   1.872  0.6336
    ##  timepoint96 - timepoint120  0.04388 0.0245 150   1.789  0.6897
    ## 
    ## P value adjustment: tukey method for comparing a family of 9 estimates

## 0.3 Statistical Mixed Model by treatment, timepoint, and tank_id

``` r
# mixed model
model <- lmer(fv_fm_y_1000 ~ treatment * timepoint + (1 | treatment:tank_id), data = PAM_exp_filtered_IQR)
summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: fv_fm_y_1000 ~ treatment * timepoint + (1 | treatment:tank_id)
    ##    Data: PAM_exp_filtered_IQR
    ## 
    ## REML criterion at convergence: -363.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0511 -0.4825  0.1071  0.6714  1.9865 
    ## 
    ## Random effects:
    ##  Groups            Name        Variance  Std.Dev.
    ##  treatment:tank_id (Intercept) 0.0005567 0.02359 
    ##  Residual                      0.0032132 0.05668 
    ## Number of obs: 159, groups:  treatment:tank_id, 6
    ## 
    ## Fixed effects:
    ##                              Estimate Std. Error         df t value Pr(>|t|)
    ## (Intercept)                  0.621000   0.023294  21.872562  26.660  < 2e-16
    ## treatmentHeat                0.008222   0.032942  21.872562   0.250  0.80523
    ## timepoint1                   0.029333   0.026721 137.018688   1.098  0.27424
    ## timepoint3                   0.031333   0.026721 137.018688   1.173  0.24299
    ## timepoint12                  0.017889   0.026721 137.018688   0.669  0.50433
    ## timepoint24                  0.002000   0.026721 137.018688   0.075  0.94045
    ## timepoint36                  0.029111   0.026721 137.018688   1.089  0.27788
    ## timepoint72                 -0.060222   0.026721 137.018688  -2.254  0.02580
    ## timepoint96                 -0.038889   0.026721 137.018688  -1.455  0.14786
    ## timepoint120                -0.067667   0.026721 137.018688  -2.532  0.01246
    ## treatmentHeat:timepoint1    -0.023222   0.037790 137.018688  -0.615  0.53990
    ## treatmentHeat:timepoint3    -0.048111   0.037790 137.018688  -1.273  0.20513
    ## treatmentHeat:timepoint12   -0.048667   0.037790 137.018688  -1.288  0.19998
    ## treatmentHeat:timepoint24   -0.044778   0.037790 137.018688  -1.185  0.23810
    ## treatmentHeat:timepoint36   -0.075333   0.037790 137.018688  -1.993  0.04819
    ## treatmentHeat:timepoint72   -0.090756   0.038390 137.065329  -2.364  0.01948
    ## treatmentHeat:timepoint96   -0.140464   0.038390 137.065329  -3.659  0.00036
    ## treatmentHeat:timepoint120  -0.172561   0.038390 137.065329  -4.495 1.47e-05
    ##                               
    ## (Intercept)                ***
    ## treatmentHeat                 
    ## timepoint1                    
    ## timepoint3                    
    ## timepoint12                   
    ## timepoint24                   
    ## timepoint36                   
    ## timepoint72                *  
    ## timepoint96                   
    ## timepoint120               *  
    ## treatmentHeat:timepoint1      
    ## treatmentHeat:timepoint3      
    ## treatmentHeat:timepoint12     
    ## treatmentHeat:timepoint24     
    ## treatmentHeat:timepoint36  *  
    ## treatmentHeat:timepoint72  *  
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
    ##  Control - Heat -0.00822 0.0329 21.8  -0.250  0.8052
    ## 
    ## timepoint = 1:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.01500 0.0329 21.8   0.455  0.6534
    ## 
    ## timepoint = 3:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.03989 0.0329 21.8   1.211  0.2389
    ## 
    ## timepoint = 12:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.04044 0.0329 21.8   1.228  0.2326
    ## 
    ## timepoint = 24:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.03656 0.0329 21.8   1.110  0.2792
    ## 
    ## timepoint = 36:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.06711 0.0329 21.8   2.037  0.0540
    ## 
    ## timepoint = 72:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.08253 0.0336 23.4   2.454  0.0220
    ## 
    ## timepoint = 96:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.13224 0.0336 23.4   3.932  0.0006
    ## 
    ## timepoint = 120:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.16434 0.0336 23.4   4.886  0.0001
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
    ## 1         0                 -0.0082 0.0329   -0.25 8.05e-01             
    ## 2         1                  0.0150 0.0329    0.46 6.53e-01             
    ## 3         3                  0.0399 0.0329    1.21 2.39e-01             
    ## 4        12                  0.0404 0.0329    1.23 2.33e-01             
    ## 5        24                  0.0366 0.0329    1.11 2.79e-01             
    ## 6        36                  0.0671 0.0329    2.04 5.40e-02            .
    ## 7        72                  0.0825 0.0336    2.45 2.20e-02            *
    ## 8        96                  0.1322 0.0336    3.93 6.47e-04          ***
    ## 9       120                  0.1643 0.0336    4.89 5.89e-05          ***

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

<img src="PAM_files/figure-gfm/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

``` r
ggsave("../output/FvFm_line_treatment_tank_modelestimates.png", plot = last_plot(), width = 8, height = 4, bg = "white")
ggsave("../output/pdf_figs/FvFm_line_treatment_tank_modelestimates.pdf", plot = last_plot(), width = 8, height = 4)
```

``` r
PAM_means_treatment <- PAM_exp_filtered_IQR %>%
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

<img src="PAM_files/figure-gfm/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

``` r
PAM_means <- PAM_exp_filtered_IQR %>%
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

<img src="PAM_files/figure-gfm/unnamed-chunk-16-2.png" style="display: block; margin: auto;" />

``` r
ggsave("../output/FvFm_line_treatment_tank_means.png", plot = last_plot(), width = 8, height = 4, bg = "white")
ggsave("../output/pdf_figs/FvFm_line_treatment_tank_means.pdf", plot = last_plot(), width = 8, height = 4)

ggplot(PAM_exp_filtered_IQR, aes(x = timepoint, y = fv_fm_y_1000, color = treatment, shape = tank_id)) +
  geom_point(stat = "summary", fun = mean, aes(group = treatment), size=2.5) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5, alpha=0.5) +
  geom_line(stat = "summary", fun = mean, size = 1.2, aes(group = treatment)) +
  labs(x = "Timepoint",y = "Mean Fv/Fm") +
  theme_minimal() +scale_color_manual(values = custom_colors) +
  stat_compare_means(aes(group = treatment),method = "anova",label = "p.format",size = 3)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-16-3.png" style="display: block; margin: auto;" />

``` r
ggsave("../output/FvFm_line_treatment_all_points.png", plot = last_plot(), width = 8, height = 4, bg = "white")
ggsave("../output/pdf_figs/FvFm_line_treatment_all_points.pdf", plot = last_plot(), width = 8, height = 4)

ggplot(PAM_means, aes(x = timepoint, y = FvFm_mean, color = tank_id, group = tank_id)) +
  geom_line(size = 0.8, alpha = 0.7) +
  labs(x = "Timepoint",y = "Mean Fv/Fm") +
  theme_minimal() + facet_wrap(~treatment)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-16-4.png" style="display: block; margin: auto;" />

``` r
ggplot(PAM_exp_filtered_IQR, aes(x = timepoint, y = fv_fm_y_1000, color = treatment, shape = tank_id)) +
  geom_point(stat = "summary", fun = mean, aes(group = treatment), size=2.5) +
  geom_line(position = position_dodge(width = 0.25), size = 0.5, alpha=0.25, aes(group = plug)) +
  geom_line(stat = "summary", fun = mean, size = 1.2, aes(group = treatment)) +
  labs(x = "Timepoint",y = "Mean Fv/Fm") +
  theme_minimal() +scale_color_manual(values = custom_colors) +
  stat_compare_means(aes(group = treatment),method = "anova",label = "p.format",size = 3)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-16-5.png" style="display: block; margin: auto;" />

``` r
ggsave("../output/FvFm_line_treatment_all_trajectories.png", plot = last_plot(), width = 8, height = 4, bg = "white")
ggsave("../output/pdf_figs/FvFm_line_treatment_all_trajectories.pdf", plot = last_plot(), width = 8, height = 4)
```
