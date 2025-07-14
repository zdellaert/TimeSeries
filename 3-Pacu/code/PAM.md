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
    ## -0.09783 -0.01526  0.00125  0.01875  0.05175 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.661250   0.004859 136.093  < 2e-16 ***
    ## timepoint1   -0.023708   0.006871  -3.450 0.000666 ***
    ## timepoint6   -0.031000   0.006871  -4.511 1.03e-05 ***
    ## timepoint12  -0.028125   0.006871  -4.093 5.89e-05 ***
    ## timepoint24  -0.031958   0.006871  -4.651 5.57e-06 ***
    ## timepoint36  -0.013167   0.006871  -1.916 0.056584 .  
    ## timepoint48  -0.028625   0.006871  -4.166 4.39e-05 ***
    ## timepoint72  -0.012208   0.006871  -1.777 0.076941 .  
    ## timepoint100 -0.011625   0.006871  -1.692 0.092039 .  
    ## timepoint120 -0.029417   0.006871  -4.281 2.73e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0238 on 230 degrees of freedom
    ## Multiple R-squared:  0.1652, Adjusted R-squared:  0.1326 
    ## F-statistic: 5.058 on 9 and 230 DF,  p-value: 3.077e-06

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ timepoint)
pairs(emm)
```

    ##  contrast                     estimate      SE  df t.ratio p.value
    ##  timepoint0 - timepoint1      0.023708 0.00687 230   3.450  0.0229
    ##  timepoint0 - timepoint6      0.031000 0.00687 230   4.511  0.0004
    ##  timepoint0 - timepoint12     0.028125 0.00687 230   4.093  0.0023
    ##  timepoint0 - timepoint24     0.031958 0.00687 230   4.651  0.0002
    ##  timepoint0 - timepoint36     0.013167 0.00687 230   1.916  0.6581
    ##  timepoint0 - timepoint48     0.028625 0.00687 230   4.166  0.0018
    ##  timepoint0 - timepoint72     0.012208 0.00687 230   1.777  0.7492
    ##  timepoint0 - timepoint100    0.011625 0.00687 230   1.692  0.7991
    ##  timepoint0 - timepoint120    0.029417 0.00687 230   4.281  0.0011
    ##  timepoint1 - timepoint6      0.007292 0.00687 230   1.061  0.9878
    ##  timepoint1 - timepoint12     0.004417 0.00687 230   0.643  0.9997
    ##  timepoint1 - timepoint24     0.008250 0.00687 230   1.201  0.9717
    ##  timepoint1 - timepoint36    -0.010542 0.00687 230  -1.534  0.8771
    ##  timepoint1 - timepoint48     0.004917 0.00687 230   0.716  0.9994
    ##  timepoint1 - timepoint72    -0.011500 0.00687 230  -1.674  0.8092
    ##  timepoint1 - timepoint100   -0.012083 0.00687 230  -1.759  0.7603
    ##  timepoint1 - timepoint120    0.005708 0.00687 230   0.831  0.9980
    ##  timepoint6 - timepoint12    -0.002875 0.00687 230  -0.418  1.0000
    ##  timepoint6 - timepoint24     0.000958 0.00687 230   0.139  1.0000
    ##  timepoint6 - timepoint36    -0.017833 0.00687 230  -2.595  0.2259
    ##  timepoint6 - timepoint48    -0.002375 0.00687 230  -0.346  1.0000
    ##  timepoint6 - timepoint72    -0.018792 0.00687 230  -2.735  0.1662
    ##  timepoint6 - timepoint100   -0.019375 0.00687 230  -2.820  0.1361
    ##  timepoint6 - timepoint120   -0.001583 0.00687 230  -0.230  1.0000
    ##  timepoint12 - timepoint24    0.003833 0.00687 230   0.558  0.9999
    ##  timepoint12 - timepoint36   -0.014958 0.00687 230  -2.177  0.4757
    ##  timepoint12 - timepoint48    0.000500 0.00687 230   0.073  1.0000
    ##  timepoint12 - timepoint72   -0.015917 0.00687 230  -2.316  0.3825
    ##  timepoint12 - timepoint100  -0.016500 0.00687 230  -2.401  0.3300
    ##  timepoint12 - timepoint120   0.001292 0.00687 230   0.188  1.0000
    ##  timepoint24 - timepoint36   -0.018792 0.00687 230  -2.735  0.1662
    ##  timepoint24 - timepoint48   -0.003333 0.00687 230  -0.485  1.0000
    ##  timepoint24 - timepoint72   -0.019750 0.00687 230  -2.874  0.1190
    ##  timepoint24 - timepoint100  -0.020333 0.00687 230  -2.959  0.0958
    ##  timepoint24 - timepoint120  -0.002542 0.00687 230  -0.370  1.0000
    ##  timepoint36 - timepoint48    0.015458 0.00687 230   2.250  0.4262
    ##  timepoint36 - timepoint72   -0.000958 0.00687 230  -0.139  1.0000
    ##  timepoint36 - timepoint100  -0.001542 0.00687 230  -0.224  1.0000
    ##  timepoint36 - timepoint120   0.016250 0.00687 230   2.365  0.3520
    ##  timepoint48 - timepoint72   -0.016417 0.00687 230  -2.389  0.3373
    ##  timepoint48 - timepoint100  -0.017000 0.00687 230  -2.474  0.2882
    ##  timepoint48 - timepoint120   0.000792 0.00687 230   0.115  1.0000
    ##  timepoint72 - timepoint100  -0.000583 0.00687 230  -0.085  1.0000
    ##  timepoint72 - timepoint120   0.017208 0.00687 230   2.504  0.2717
    ##  timepoint100 - timepoint120  0.017792 0.00687 230   2.589  0.2288
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
    ## REML criterion at convergence: -1123
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3433 -0.4726  0.0937  0.5478  3.9740 
    ## 
    ## Random effects:
    ##  Groups            Name        Variance  Std.Dev.
    ##  treatment:tank_id (Intercept) 2.759e-05 0.005252
    ##  Residual                      2.753e-04 0.016592
    ## Number of obs: 240, groups:  treatment:tank_id, 6
    ## 
    ## Fixed effects:
    ##                              Estimate Std. Error         df t value Pr(>|t|)
    ## (Intercept)                  0.662333   0.005669  29.526651 116.837  < 2e-16
    ## treatmentHeat               -0.002167   0.008017  29.526651  -0.270 0.788841
    ## timepoint1                  -0.012917   0.006774 216.000001  -1.907 0.057858
    ## timepoint6                  -0.016417   0.006774 216.000001  -2.424 0.016189
    ## timepoint12                 -0.017083   0.006774 216.000001  -2.522 0.012388
    ## timepoint24                 -0.007250   0.006774 216.000001  -1.070 0.285663
    ## timepoint36                 -0.002583   0.006774 216.000001  -0.381 0.703293
    ## timepoint48                 -0.006833   0.006774 216.000001  -1.009 0.314188
    ## timepoint72                  0.007833   0.006774 216.000001   1.156 0.248772
    ## timepoint100                -0.005667   0.006774 216.000001  -0.837 0.403751
    ## timepoint120                -0.011333   0.006774 216.000001  -1.673 0.095742
    ## treatmentHeat:timepoint1    -0.021583   0.009579 216.000001  -2.253 0.025256
    ## treatmentHeat:timepoint6    -0.029167   0.009579 216.000001  -3.045 0.002618
    ## treatmentHeat:timepoint12   -0.022083   0.009579 216.000001  -2.305 0.022098
    ## treatmentHeat:timepoint24   -0.049417   0.009579 216.000001  -5.159 5.62e-07
    ## treatmentHeat:timepoint36   -0.021167   0.009579 216.000001  -2.210 0.028181
    ## treatmentHeat:timepoint48   -0.043583   0.009579 216.000001  -4.550 8.96e-06
    ## treatmentHeat:timepoint72   -0.040083   0.009579 216.000001  -4.184 4.16e-05
    ## treatmentHeat:timepoint100  -0.011917   0.009579 216.000001  -1.244 0.214846
    ## treatmentHeat:timepoint120  -0.036167   0.009579 216.000001  -3.776 0.000206
    ##                               
    ## (Intercept)                ***
    ## treatmentHeat                 
    ## timepoint1                 .  
    ## timepoint6                 *  
    ## timepoint12                *  
    ## timepoint24                   
    ## timepoint36                   
    ## timepoint48                   
    ## timepoint72                   
    ## timepoint100                  
    ## timepoint120               .  
    ## treatmentHeat:timepoint1   *  
    ## treatmentHeat:timepoint6   ** 
    ## treatmentHeat:timepoint12  *  
    ## treatmentHeat:timepoint24  ***
    ## treatmentHeat:timepoint36  *  
    ## treatmentHeat:timepoint48  ***
    ## treatmentHeat:timepoint72  ***
    ## treatmentHeat:timepoint100    
    ## treatmentHeat:timepoint120 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ treatment | timepoint)
pairs(emm)
```

    ## timepoint = 0:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.00217 0.00802 29.5   0.270  0.7888
    ## 
    ## timepoint = 1:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.02375 0.00802 29.5   2.962  0.0060
    ## 
    ## timepoint = 6:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.03133 0.00802 29.5   3.908  0.0005
    ## 
    ## timepoint = 12:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.02425 0.00802 29.5   3.025  0.0051
    ## 
    ## timepoint = 24:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.05158 0.00802 29.5   6.434  <.0001
    ## 
    ## timepoint = 36:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.02333 0.00802 29.5   2.910  0.0068
    ## 
    ## timepoint = 48:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.04575 0.00802 29.5   5.707  <.0001
    ## 
    ## timepoint = 72:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.04225 0.00802 29.5   5.270  <.0001
    ## 
    ## timepoint = 100:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.01408 0.00802 29.5   1.757  0.0893
    ## 
    ## timepoint = 120:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.03833 0.00802 29.5   4.782  <.0001
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

    ##    Timepoint Estimate (Control-Heat)    SE t-ratio  p-value Significant?
    ## 1          0                  0.0022 0.008    0.27 7.89e-01             
    ## 2          1                  0.0237 0.008    2.96 5.98e-03           **
    ## 3          6                  0.0313 0.008    3.91 5.01e-04          ***
    ## 4         12                  0.0242 0.008    3.02 5.11e-03           **
    ## 5         24                  0.0516 0.008    6.43 4.49e-07          ***
    ## 6         36                  0.0233 0.008    2.91 6.80e-03           **
    ## 7         48                  0.0457 0.008    5.71 3.35e-06          ***
    ## 8         72                  0.0422 0.008    5.27 1.14e-05          ***
    ## 9        100                  0.0141 0.008    1.76 8.93e-02            .
    ## 10       120                  0.0383 0.008    4.78 4.47e-05          ***

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
