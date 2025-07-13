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
    ##      9      9      9      9      9      9      9      9      9      9      9 
    ##   1473   1474   1614   1626   1691   1753   1761   2195   2370   2565   2666 
    ##      9      9      9      9      9      9      9      9      9      9      9 
    ##   2730   2986 Parent 
    ##      9      9      0

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
    ## -0.06908 -0.01434  0.00125  0.01840  0.05175 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.661250   0.004641 142.478  < 2e-16 ***
    ## timepoint1   -0.023708   0.006563  -3.612 0.000381 ***
    ## timepoint6   -0.031000   0.006563  -4.723 4.28e-06 ***
    ## timepoint12  -0.028125   0.006563  -4.285 2.80e-05 ***
    ## timepoint24  -0.031958   0.006563  -4.869 2.22e-06 ***
    ## timepoint36  -0.013167   0.006563  -2.006 0.046152 *  
    ## timepoint48  -0.028625   0.006563  -4.361 2.04e-05 ***
    ## timepoint72  -0.012208   0.006563  -1.860 0.064298 .  
    ## timepoint100 -0.011625   0.006563  -1.771 0.078004 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02274 on 207 degrees of freedom
    ## Multiple R-squared:  0.1825, Adjusted R-squared:  0.151 
    ## F-statistic: 5.778 on 8 and 207 DF,  p-value: 1.181e-06

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ timepoint)
pairs(emm)
```

    ##  contrast                    estimate      SE  df t.ratio p.value
    ##  timepoint0 - timepoint1     0.023708 0.00656 207   3.612  0.0112
    ##  timepoint0 - timepoint6     0.031000 0.00656 207   4.723  0.0001
    ##  timepoint0 - timepoint12    0.028125 0.00656 207   4.285  0.0009
    ##  timepoint0 - timepoint24    0.031958 0.00656 207   4.869  0.0001
    ##  timepoint0 - timepoint36    0.013167 0.00656 207   2.006  0.5410
    ##  timepoint0 - timepoint48    0.028625 0.00656 207   4.361  0.0007
    ##  timepoint0 - timepoint72    0.012208 0.00656 207   1.860  0.6419
    ##  timepoint0 - timepoint100   0.011625 0.00656 207   1.771  0.7011
    ##  timepoint1 - timepoint6     0.007292 0.00656 207   1.111  0.9721
    ##  timepoint1 - timepoint12    0.004417 0.00656 207   0.673  0.9991
    ##  timepoint1 - timepoint24    0.008250 0.00656 207   1.257  0.9423
    ##  timepoint1 - timepoint36   -0.010542 0.00656 207  -1.606  0.8006
    ##  timepoint1 - timepoint48    0.004917 0.00656 207   0.749  0.9980
    ##  timepoint1 - timepoint72   -0.011500 0.00656 207  -1.752  0.7133
    ##  timepoint1 - timepoint100  -0.012083 0.00656 207  -1.841  0.6548
    ##  timepoint6 - timepoint12   -0.002875 0.00656 207  -0.438  1.0000
    ##  timepoint6 - timepoint24    0.000958 0.00656 207   0.146  1.0000
    ##  timepoint6 - timepoint36   -0.017833 0.00656 207  -2.717  0.1483
    ##  timepoint6 - timepoint48   -0.002375 0.00656 207  -0.362  1.0000
    ##  timepoint6 - timepoint72   -0.018792 0.00656 207  -2.863  0.1040
    ##  timepoint6 - timepoint100  -0.019375 0.00656 207  -2.952  0.0827
    ##  timepoint12 - timepoint24   0.003833 0.00656 207   0.584  0.9997
    ##  timepoint12 - timepoint36  -0.014958 0.00656 207  -2.279  0.3597
    ##  timepoint12 - timepoint48   0.000500 0.00656 207   0.076  1.0000
    ##  timepoint12 - timepoint72  -0.015917 0.00656 207  -2.425  0.2761
    ##  timepoint12 - timepoint100 -0.016500 0.00656 207  -2.514  0.2314
    ##  timepoint24 - timepoint36  -0.018792 0.00656 207  -2.863  0.1040
    ##  timepoint24 - timepoint48  -0.003333 0.00656 207  -0.508  0.9999
    ##  timepoint24 - timepoint72  -0.019750 0.00656 207  -3.009  0.0710
    ##  timepoint24 - timepoint100 -0.020333 0.00656 207  -3.098  0.0555
    ##  timepoint36 - timepoint48   0.015458 0.00656 207   2.355  0.3146
    ##  timepoint36 - timepoint72  -0.000958 0.00656 207  -0.146  1.0000
    ##  timepoint36 - timepoint100 -0.001542 0.00656 207  -0.235  1.0000
    ##  timepoint48 - timepoint72  -0.016417 0.00656 207  -2.501  0.2375
    ##  timepoint48 - timepoint100 -0.017000 0.00656 207  -2.590  0.1972
    ##  timepoint72 - timepoint100 -0.000583 0.00656 207  -0.089  1.0000
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
    ## REML criterion at convergence: -1043.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3064 -0.5360  0.0850  0.5716  3.2167 
    ## 
    ## Random effects:
    ##  Groups            Name        Variance  Std.Dev.
    ##  treatment:tank_id (Intercept) 3.134e-05 0.005598
    ##  Residual                      2.322e-04 0.015238
    ## Number of obs: 216, groups:  treatment:tank_id, 6
    ## 
    ## Fixed effects:
    ##                              Estimate Std. Error         df t value Pr(>|t|)
    ## (Intercept)                  0.662333   0.005459  21.550844 121.338  < 2e-16
    ## treatmentHeat               -0.002167   0.007720  21.550844  -0.281  0.78164
    ## timepoint1                  -0.012917   0.006221 194.000000  -2.076  0.03918
    ## timepoint6                  -0.016417   0.006221 194.000000  -2.639  0.00899
    ## timepoint12                 -0.017083   0.006221 194.000000  -2.746  0.00660
    ## timepoint24                 -0.007250   0.006221 194.000000  -1.165  0.24527
    ## timepoint36                 -0.002583   0.006221 194.000000  -0.415  0.67840
    ## timepoint48                 -0.006833   0.006221 194.000000  -1.098  0.27336
    ## timepoint72                  0.007833   0.006221 194.000000   1.259  0.20946
    ## timepoint100                -0.005667   0.006221 194.000000  -0.911  0.36347
    ## treatmentHeat:timepoint1    -0.021583   0.008798 194.000000  -2.453  0.01504
    ## treatmentHeat:timepoint6    -0.029167   0.008798 194.000000  -3.315  0.00109
    ## treatmentHeat:timepoint12   -0.022083   0.008798 194.000000  -2.510  0.01288
    ## treatmentHeat:timepoint24   -0.049417   0.008798 194.000000  -5.617 6.66e-08
    ## treatmentHeat:timepoint36   -0.021167   0.008798 194.000000  -2.406  0.01707
    ## treatmentHeat:timepoint48   -0.043583   0.008798 194.000000  -4.954 1.58e-06
    ## treatmentHeat:timepoint72   -0.040083   0.008798 194.000000  -4.556 9.20e-06
    ## treatmentHeat:timepoint100  -0.011917   0.008798 194.000000  -1.355  0.17714
    ##                               
    ## (Intercept)                ***
    ## treatmentHeat                 
    ## timepoint1                 *  
    ## timepoint6                 ** 
    ## timepoint12                ** 
    ## timepoint24                   
    ## timepoint36                   
    ## timepoint48                   
    ## timepoint72                   
    ## timepoint100                  
    ## treatmentHeat:timepoint1   *  
    ## treatmentHeat:timepoint6   ** 
    ## treatmentHeat:timepoint12  *  
    ## treatmentHeat:timepoint24  ***
    ## treatmentHeat:timepoint36  *  
    ## treatmentHeat:timepoint48  ***
    ## treatmentHeat:timepoint72  ***
    ## treatmentHeat:timepoint100    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ treatment | timepoint)
pairs(emm)
```

    ## timepoint = 0:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.00217 0.00772 21.6   0.281  0.7816
    ## 
    ## timepoint = 1:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.02375 0.00772 21.6   3.077  0.0056
    ## 
    ## timepoint = 6:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.03133 0.00772 21.6   4.059  0.0005
    ## 
    ## timepoint = 12:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.02425 0.00772 21.6   3.141  0.0048
    ## 
    ## timepoint = 24:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.05158 0.00772 21.6   6.682  <.0001
    ## 
    ## timepoint = 36:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.02333 0.00772 21.6   3.023  0.0064
    ## 
    ## timepoint = 48:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.04575 0.00772 21.6   5.926  <.0001
    ## 
    ## timepoint = 72:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.04225 0.00772 21.6   5.473  <.0001
    ## 
    ## timepoint = 100:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.01408 0.00772 21.6   1.824  0.0820
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
    ## 1         0                  0.0022 0.0077    0.28 7.82e-01             
    ## 2         1                  0.0238 0.0077    3.08 5.61e-03           **
    ## 3         6                  0.0313 0.0077    4.06 5.41e-04          ***
    ## 4        12                  0.0243 0.0077    3.14 4.82e-03           **
    ## 5        24                  0.0516 0.0077    6.68 1.14e-06          ***
    ## 6        36                  0.0233 0.0077    3.02 6.35e-03           **
    ## 7        48                  0.0458 0.0077    5.93 6.29e-06          ***
    ## 8        72                  0.0423 0.0077    5.47 1.81e-05          ***
    ## 9       100                  0.0141 0.0077    1.82 8.20e-02            .

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
