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
    ##    8    8    8    7    8    0    8    8    8    8    8    8    8    8    7    8 
    ## 1538 1548 1549 1560 1563 1597 1631 1739 2084 2360 2852 
    ##    0    0    8    8    8    8    8    8    8    8    8

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
    ## -0.114409 -0.035197  0.008534  0.038420  0.087591 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.6178333  0.0096823  63.811   <2e-16 ***
    ## timepoint1   0.0008623  0.0138409   0.062   0.9504    
    ## timepoint3   0.0135000  0.0136928   0.986   0.3255    
    ## timepoint12  0.0025580  0.0138409   0.185   0.8536    
    ## timepoint24 -0.0082917  0.0136928  -0.606   0.5456    
    ## timepoint36 -0.0115833  0.0136928  -0.846   0.3987    
    ## timepoint72 -0.0234697  0.0140006  -1.676   0.0954 .  
    ## timepoint96 -0.0174242  0.0140006  -1.245   0.2149    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.04743 on 178 degrees of freedom
    ##   (6 observations deleted due to missingness)
    ## Multiple R-squared:  0.05455,    Adjusted R-squared:  0.01737 
    ## F-statistic: 1.467 on 7 and 178 DF,  p-value: 0.1816

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ timepoint)
pairs(emm)
```

    ##  contrast                   estimate     SE  df t.ratio p.value
    ##  timepoint0 - timepoint1   -0.000862 0.0138 178  -0.062  1.0000
    ##  timepoint0 - timepoint3   -0.013500 0.0137 178  -0.986  0.9759
    ##  timepoint0 - timepoint12  -0.002558 0.0138 178  -0.185  1.0000
    ##  timepoint0 - timepoint24   0.008292 0.0137 178   0.606  0.9988
    ##  timepoint0 - timepoint36   0.011583 0.0137 178   0.846  0.9901
    ##  timepoint0 - timepoint72   0.023470 0.0140 178   1.676  0.7025
    ##  timepoint0 - timepoint96   0.017424 0.0140 178   1.245  0.9173
    ##  timepoint1 - timepoint3   -0.012638 0.0138 178  -0.913  0.9845
    ##  timepoint1 - timepoint12  -0.001696 0.0140 178  -0.121  1.0000
    ##  timepoint1 - timepoint24   0.009154 0.0138 178   0.661  0.9978
    ##  timepoint1 - timepoint36   0.012446 0.0138 178   0.899  0.9858
    ##  timepoint1 - timepoint72   0.024332 0.0141 178   1.720  0.6742
    ##  timepoint1 - timepoint96   0.018287 0.0141 178   1.293  0.9005
    ##  timepoint3 - timepoint12   0.010942 0.0138 178   0.791  0.9934
    ##  timepoint3 - timepoint24   0.021792 0.0137 178   1.591  0.7549
    ##  timepoint3 - timepoint36   0.025083 0.0137 178   1.832  0.5993
    ##  timepoint3 - timepoint72   0.036970 0.0140 178   2.641  0.1486
    ##  timepoint3 - timepoint96   0.030924 0.0140 178   2.209  0.3516
    ##  timepoint12 - timepoint24  0.010850 0.0138 178   0.784  0.9938
    ##  timepoint12 - timepoint36  0.014141 0.0138 178   1.022  0.9706
    ##  timepoint12 - timepoint72  0.026028 0.0141 178   1.840  0.5938
    ##  timepoint12 - timepoint96  0.019982 0.0141 178   1.413  0.8504
    ##  timepoint24 - timepoint36  0.003292 0.0137 178   0.240  1.0000
    ##  timepoint24 - timepoint72  0.015178 0.0140 178   1.084  0.9594
    ##  timepoint24 - timepoint96  0.009133 0.0140 178   0.652  0.9980
    ##  timepoint36 - timepoint72  0.011886 0.0140 178   0.849  0.9899
    ##  timepoint36 - timepoint96  0.005841 0.0140 178   0.417  0.9999
    ##  timepoint72 - timepoint96 -0.006045 0.0143 178  -0.423  0.9999
    ## 
    ## P value adjustment: tukey method for comparing a family of 8 estimates

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
    ## REML criterion at convergence: -630.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4530 -0.5403  0.1529  0.7180  1.6928 
    ## 
    ## Random effects:
    ##  Groups            Name        Variance  Std.Dev.
    ##  treatment:tank_id (Intercept) 4.042e-05 0.006358
    ##  Residual                      1.121e-03 0.033480
    ## Number of obs: 186, groups:  treatment:tank_id, 6
    ## 
    ## Fixed effects:
    ##                             Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)                 0.622250   0.010339  56.766921  60.187  < 2e-16 ***
    ## treatmentHeat              -0.008833   0.014621  56.766921  -0.604   0.5481    
    ## timepoint1                  0.018052   0.013979 166.209119   1.291   0.1984    
    ## timepoint3                  0.030417   0.013668 166.053145   2.225   0.0274 *  
    ## timepoint12                 0.025188   0.013979 166.209119   1.802   0.0734 .  
    ## timepoint24                 0.015583   0.013668 166.053145   1.140   0.2559    
    ## timepoint36                 0.012833   0.013668 166.053145   0.939   0.3491    
    ## timepoint72                 0.012333   0.013668 166.053145   0.902   0.3682    
    ## timepoint96                 0.035500   0.013668 166.053145   2.597   0.0102 *  
    ## treatmentHeat:timepoint1   -0.032218   0.019551 166.133251  -1.648   0.1013    
    ## treatmentHeat:timepoint3   -0.033833   0.019330 166.053145  -1.750   0.0819 .  
    ## treatmentHeat:timepoint12  -0.042938   0.019551 166.133251  -2.196   0.0295 *  
    ## treatmentHeat:timepoint24  -0.047750   0.019330 166.053145  -2.470   0.0145 *  
    ## treatmentHeat:timepoint36  -0.048833   0.019330 166.053145  -2.526   0.0125 *  
    ## treatmentHeat:timepoint72  -0.079138   0.019810 166.147116  -3.995 9.71e-05 ***
    ## treatmentHeat:timepoint96  -0.116805   0.019810 166.147116  -5.896 2.02e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ treatment | timepoint)
pairs(emm)
```

    ## timepoint = 0:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.00883 0.0146 56.2   0.604  0.5482
    ## 
    ## timepoint = 1:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.04105 0.0149 59.4   2.752  0.0078
    ## 
    ## timepoint = 3:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.04267 0.0146 56.2   2.918  0.0051
    ## 
    ## timepoint = 12:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.05177 0.0149 59.4   3.471  0.0010
    ## 
    ## timepoint = 24:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.05658 0.0146 56.2   3.870  0.0003
    ## 
    ## timepoint = 36:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.05767 0.0146 56.2   3.944  0.0002
    ## 
    ## timepoint = 72:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.08797 0.0153 63.3   5.767  <.0001
    ## 
    ## timepoint = 96:
    ##  contrast       estimate     SE   df t.ratio p.value
    ##  Control - Heat  0.12564 0.0153 63.3   8.236  <.0001
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
    ## 1         0                  0.0088 0.0146    0.60 5.48e-01             
    ## 2         1                  0.0411 0.0149    2.75 7.83e-03           **
    ## 3         3                  0.0427 0.0146    2.92 5.05e-03           **
    ## 4        12                  0.0518 0.0149    3.47 9.72e-04          ***
    ## 5        24                  0.0566 0.0146    3.87 2.85e-04          ***
    ## 6        36                  0.0577 0.0146    3.94 2.24e-04          ***
    ## 7        72                  0.0880 0.0153    5.77 2.60e-07          ***
    ## 8        96                  0.1256 0.0153    8.24 1.33e-11          ***

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
