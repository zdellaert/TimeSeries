PAM Pacuta
================
Zoe Dellaert
2025-07-05

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
                mutate(treatment = factor(treatment,levels = c("Parent","Recovery","Acclimation","Control","Heat", ordered=TRUE)))

ggplot(PAM,aes(x = date, y = fv_fm_y_1000)) + 
    geom_boxplot(aes(fill = treatment)) + labs(x = "Date", y = "Fv/Fm", title = "Fv/Fm by Date and Tank") +
  theme_minimal()
```

<img src="PAM_files/figure-gfm/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

``` r
ggsave("../output/FvFm_recovery_acclimation.png", plot = last_plot(), width = 8, height = 4, bg = "white")
```

``` r
PAM_exp <- PAM %>% filter(treatment!="Acclimation" & treatment!="Recovery" & treatment!="Parent")
table(PAM_exp$plug)
```

    ## 
    ##   1038   1043   1059   1100   1122   1159   1175   1223   1281   1450   1471 
    ##     10     10     10     10     10     10     10     10     10     10     10 
    ##   1473   1474   1614   1626   1691   1753   1761   2195   2370   2565   2666 
    ##     10     10     10     10     10     10     10     10     10     10     10 
    ##   2730   2986 Parent 
    ##     10     10      0

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
    ##      Min       1Q   Median       3Q      Max 
    ## -0.09583 -0.01529  0.00075  0.01888  0.05175 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.661250   0.004894 135.117  < 2e-16 ***
    ## timepoint1   -0.023708   0.006921  -3.426 0.000727 ***
    ## timepoint6   -0.031000   0.006921  -4.479 1.18e-05 ***
    ## timepoint12  -0.028125   0.006921  -4.064 6.63e-05 ***
    ## timepoint24  -0.031958   0.006921  -4.618 6.46e-06 ***
    ## timepoint36  -0.013167   0.006921  -1.902 0.058366 .  
    ## timepoint48  -0.028625   0.006921  -4.136 4.96e-05 ***
    ## timepoint72  -0.012208   0.006921  -1.764 0.079068 .  
    ## timepoint100 -0.018917   0.006921  -2.733 0.006759 ** 
    ## timepoint120 -0.031417   0.006921  -4.539 9.10e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02398 on 230 degrees of freedom
    ## Multiple R-squared:  0.1563, Adjusted R-squared:  0.1233 
    ## F-statistic: 4.734 on 9 and 230 DF,  p-value: 8.701e-06

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ timepoint)
pairs(emm)
```

    ##  contrast                     estimate      SE  df t.ratio p.value
    ##  timepoint0 - timepoint1      0.023708 0.00692 230   3.426  0.0248
    ##  timepoint0 - timepoint6      0.031000 0.00692 230   4.479  0.0005
    ##  timepoint0 - timepoint12     0.028125 0.00692 230   4.064  0.0026
    ##  timepoint0 - timepoint24     0.031958 0.00692 230   4.618  0.0003
    ##  timepoint0 - timepoint36     0.013167 0.00692 230   1.902  0.6675
    ##  timepoint0 - timepoint48     0.028625 0.00692 230   4.136  0.0020
    ##  timepoint0 - timepoint72     0.012208 0.00692 230   1.764  0.7570
    ##  timepoint0 - timepoint100    0.018917 0.00692 230   2.733  0.1668
    ##  timepoint0 - timepoint120    0.031417 0.00692 230   4.539  0.0004
    ##  timepoint1 - timepoint6      0.007292 0.00692 230   1.054  0.9884
    ##  timepoint1 - timepoint12     0.004417 0.00692 230   0.638  0.9998
    ##  timepoint1 - timepoint24     0.008250 0.00692 230   1.192  0.9730
    ##  timepoint1 - timepoint36    -0.010542 0.00692 230  -1.523  0.8817
    ##  timepoint1 - timepoint48     0.004917 0.00692 230   0.710  0.9994
    ##  timepoint1 - timepoint72    -0.011500 0.00692 230  -1.662  0.8156
    ##  timepoint1 - timepoint100   -0.004792 0.00692 230  -0.692  0.9995
    ##  timepoint1 - timepoint120    0.007708 0.00692 230   1.114  0.9829
    ##  timepoint6 - timepoint12    -0.002875 0.00692 230  -0.415  1.0000
    ##  timepoint6 - timepoint24     0.000958 0.00692 230   0.138  1.0000
    ##  timepoint6 - timepoint36    -0.017833 0.00692 230  -2.577  0.2348
    ##  timepoint6 - timepoint48    -0.002375 0.00692 230  -0.343  1.0000
    ##  timepoint6 - timepoint72    -0.018792 0.00692 230  -2.715  0.1738
    ##  timepoint6 - timepoint100   -0.012083 0.00692 230  -1.746  0.7679
    ##  timepoint6 - timepoint120    0.000417 0.00692 230   0.060  1.0000
    ##  timepoint12 - timepoint24    0.003833 0.00692 230   0.554  0.9999
    ##  timepoint12 - timepoint36   -0.014958 0.00692 230  -2.161  0.4865
    ##  timepoint12 - timepoint48    0.000500 0.00692 230   0.072  1.0000
    ##  timepoint12 - timepoint72   -0.015917 0.00692 230  -2.300  0.3932
    ##  timepoint12 - timepoint100  -0.009208 0.00692 230  -1.330  0.9456
    ##  timepoint12 - timepoint120   0.003292 0.00692 230   0.476  1.0000
    ##  timepoint24 - timepoint36   -0.018792 0.00692 230  -2.715  0.1738
    ##  timepoint24 - timepoint48   -0.003333 0.00692 230  -0.482  1.0000
    ##  timepoint24 - timepoint72   -0.019750 0.00692 230  -2.854  0.1252
    ##  timepoint24 - timepoint100  -0.013042 0.00692 230  -1.884  0.6796
    ##  timepoint24 - timepoint120  -0.000542 0.00692 230  -0.078  1.0000
    ##  timepoint36 - timepoint48    0.015458 0.00692 230   2.234  0.4370
    ##  timepoint36 - timepoint72   -0.000958 0.00692 230  -0.138  1.0000
    ##  timepoint36 - timepoint100   0.005750 0.00692 230   0.831  0.9980
    ##  timepoint36 - timepoint120   0.018250 0.00692 230   2.637  0.2068
    ##  timepoint48 - timepoint72   -0.016417 0.00692 230  -2.372  0.3477
    ##  timepoint48 - timepoint100  -0.009708 0.00692 230  -1.403  0.9254
    ##  timepoint48 - timepoint120   0.002792 0.00692 230   0.403  1.0000
    ##  timepoint72 - timepoint100   0.006708 0.00692 230   0.969  0.9937
    ##  timepoint72 - timepoint120   0.019208 0.00692 230   2.775  0.1512
    ##  timepoint100 - timepoint120  0.012500 0.00692 230   1.806  0.7308
    ## 
    ## P value adjustment: tukey method for comparing a family of 10 estimates

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
    ## REML criterion at convergence: -1142.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2103 -0.5202  0.0944  0.6087  3.1054 
    ## 
    ## Random effects:
    ##  Groups            Name        Variance  Std.Dev.
    ##  treatment:tank_id (Intercept) 2.662e-05 0.00516 
    ##  Residual                      2.516e-04 0.01586 
    ## Number of obs: 240, groups:  treatment:tank_id, 6
    ## 
    ## Fixed effects:
    ##                              Estimate Std. Error         df t value Pr(>|t|)
    ## (Intercept)                  0.662333   0.005463  28.054916 121.245  < 2e-16
    ## treatmentHeat               -0.002167   0.007725  28.054916  -0.280  0.78118
    ## timepoint1                  -0.012917   0.006476 216.000001  -1.995  0.04734
    ## timepoint6                  -0.016417   0.006476 216.000001  -2.535  0.01195
    ## timepoint12                 -0.017083   0.006476 216.000001  -2.638  0.00894
    ## timepoint24                 -0.007250   0.006476 216.000001  -1.120  0.26413
    ## timepoint36                 -0.002583   0.006476 216.000001  -0.399  0.69034
    ## timepoint48                 -0.006833   0.006476 216.000001  -1.055  0.29249
    ## timepoint72                  0.007833   0.006476 216.000001   1.210  0.22773
    ## timepoint100                -0.005667   0.006476 216.000001  -0.875  0.38250
    ## timepoint120                -0.009833   0.006476 216.000001  -1.519  0.13035
    ## treatmentHeat:timepoint1    -0.021583   0.009158 216.000001  -2.357  0.01933
    ## treatmentHeat:timepoint6    -0.029167   0.009158 216.000001  -3.185  0.00166
    ## treatmentHeat:timepoint12   -0.022083   0.009158 216.000001  -2.411  0.01673
    ## treatmentHeat:timepoint24   -0.049417   0.009158 216.000001  -5.396 1.79e-07
    ## treatmentHeat:timepoint36   -0.021167   0.009158 216.000001  -2.311  0.02176
    ## treatmentHeat:timepoint48   -0.043583   0.009158 216.000001  -4.759 3.56e-06
    ## treatmentHeat:timepoint72   -0.040083   0.009158 216.000001  -4.377 1.87e-05
    ## treatmentHeat:timepoint100  -0.026500   0.009158 216.000001  -2.894  0.00420
    ## treatmentHeat:timepoint120  -0.043167   0.009158 216.000001  -4.714 4.36e-06
    ##                               
    ## (Intercept)                ***
    ## treatmentHeat                 
    ## timepoint1                 *  
    ## timepoint6                 *  
    ## timepoint12                ** 
    ## timepoint24                   
    ## timepoint36                   
    ## timepoint48                   
    ## timepoint72                   
    ## timepoint100                  
    ## timepoint120                  
    ## treatmentHeat:timepoint1   *  
    ## treatmentHeat:timepoint6   ** 
    ## treatmentHeat:timepoint12  *  
    ## treatmentHeat:timepoint24  ***
    ## treatmentHeat:timepoint36  *  
    ## treatmentHeat:timepoint48  ***
    ## treatmentHeat:timepoint72  ***
    ## treatmentHeat:timepoint100 ** 
    ## treatmentHeat:timepoint120 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ treatment | timepoint)
pairs(emm)
```

    ## timepoint = 0:
    ##  contrast       estimate      SE df t.ratio p.value
    ##  Control - Heat  0.00217 0.00773 28   0.280  0.7812
    ## 
    ## timepoint = 1:
    ##  contrast       estimate      SE df t.ratio p.value
    ##  Control - Heat  0.02375 0.00773 28   3.074  0.0047
    ## 
    ## timepoint = 6:
    ##  contrast       estimate      SE df t.ratio p.value
    ##  Control - Heat  0.03133 0.00773 28   4.056  0.0004
    ## 
    ## timepoint = 12:
    ##  contrast       estimate      SE df t.ratio p.value
    ##  Control - Heat  0.02425 0.00773 28   3.139  0.0040
    ## 
    ## timepoint = 24:
    ##  contrast       estimate      SE df t.ratio p.value
    ##  Control - Heat  0.05158 0.00773 28   6.677  <.0001
    ## 
    ## timepoint = 36:
    ##  contrast       estimate      SE df t.ratio p.value
    ##  Control - Heat  0.02333 0.00773 28   3.020  0.0053
    ## 
    ## timepoint = 48:
    ##  contrast       estimate      SE df t.ratio p.value
    ##  Control - Heat  0.04575 0.00773 28   5.922  <.0001
    ## 
    ## timepoint = 72:
    ##  contrast       estimate      SE df t.ratio p.value
    ##  Control - Heat  0.04225 0.00773 28   5.469  <.0001
    ## 
    ## timepoint = 100:
    ##  contrast       estimate      SE df t.ratio p.value
    ##  Control - Heat  0.02867 0.00773 28   3.711  0.0009
    ## 
    ## timepoint = 120:
    ##  contrast       estimate      SE df t.ratio p.value
    ##  Control - Heat  0.04533 0.00773 28   5.868  <.0001
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

    ##    Timepoint Estimate (Control-Heat)     SE t-ratio  p-value Significant?
    ## 1          0                  0.0022 0.0077    0.28 7.81e-01             
    ## 2          1                  0.0237 0.0077    3.07 4.67e-03           **
    ## 3          6                  0.0313 0.0077    4.06 3.61e-04          ***
    ## 4         12                  0.0242 0.0077    3.14 3.97e-03           **
    ## 5         24                  0.0516 0.0077    6.68 2.99e-07          ***
    ## 6         36                  0.0233 0.0077    3.02 5.34e-03           **
    ## 7         48                  0.0457 0.0077    5.92 2.24e-06          ***
    ## 8         72                  0.0422 0.0077    5.47 7.66e-06          ***
    ## 9        100                  0.0287 0.0077    3.71 9.06e-04          ***
    ## 10       120                  0.0453 0.0077    5.87 2.59e-06          ***

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

``` r
PAM_exp  %>% ggplot(aes(x = dark_adapt_mins, y = fv_fm_y_1000)) + 
    geom_point(aes(color=treatment)) +
  geom_smooth(aes(group = treatment, color = treatment), method = "lm", se = FALSE) +  # or method = "loess"
  theme_minimal() + scale_color_manual(values = custom_colors)
```

<img src="PAM_files/figure-gfm/unnamed-chunk-9-2.png" style="display: block; margin: auto;" />
