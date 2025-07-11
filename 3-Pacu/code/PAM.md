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
    ##      7      7      7      7      7      7      7      7      7      7      7 
    ##   1473   1474   1614   1626   1691   1753   1761   2195   2370   2565   2666 
    ##      7      7      7      7      7      7      7      7      7      7      7 
    ##   2730   2986 Parent 
    ##      7      7      0

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
    ## -0.069083 -0.014031 -0.000396  0.018750  0.051750 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.661250   0.004697 140.794  < 2e-16 ***
    ## timepoint1  -0.023708   0.006642  -3.569 0.000472 ***
    ## timepoint6  -0.031000   0.006642  -4.667 6.39e-06 ***
    ## timepoint12 -0.028125   0.006642  -4.234 3.84e-05 ***
    ## timepoint24 -0.031958   0.006642  -4.812 3.42e-06 ***
    ## timepoint36 -0.013167   0.006642  -1.982 0.049143 *  
    ## timepoint48 -0.028625   0.006642  -4.310 2.84e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02301 on 161 degrees of freedom
    ## Multiple R-squared:  0.1886, Adjusted R-squared:  0.1584 
    ## F-statistic: 6.238 on 6 and 161 DF,  p-value: 6.548e-06

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ timepoint)
pairs(emm)
```

    ##  contrast                   estimate      SE  df t.ratio p.value
    ##  timepoint0 - timepoint1    0.023708 0.00664 161   3.569  0.0084
    ##  timepoint0 - timepoint6    0.031000 0.00664 161   4.667  0.0001
    ##  timepoint0 - timepoint12   0.028125 0.00664 161   4.234  0.0007
    ##  timepoint0 - timepoint24   0.031958 0.00664 161   4.812  0.0001
    ##  timepoint0 - timepoint36   0.013167 0.00664 161   1.982  0.4299
    ##  timepoint0 - timepoint48   0.028625 0.00664 161   4.310  0.0006
    ##  timepoint1 - timepoint6    0.007292 0.00664 161   1.098  0.9280
    ##  timepoint1 - timepoint12   0.004417 0.00664 161   0.665  0.9943
    ##  timepoint1 - timepoint24   0.008250 0.00664 161   1.242  0.8765
    ##  timepoint1 - timepoint36  -0.010542 0.00664 161  -1.587  0.6907
    ##  timepoint1 - timepoint48   0.004917 0.00664 161   0.740  0.9898
    ##  timepoint6 - timepoint12  -0.002875 0.00664 161  -0.433  0.9995
    ##  timepoint6 - timepoint24   0.000958 0.00664 161   0.144  1.0000
    ##  timepoint6 - timepoint36  -0.017833 0.00664 161  -2.685  0.1088
    ##  timepoint6 - timepoint48  -0.002375 0.00664 161  -0.358  0.9998
    ##  timepoint12 - timepoint24  0.003833 0.00664 161   0.577  0.9974
    ##  timepoint12 - timepoint36 -0.014958 0.00664 161  -2.252  0.2741
    ##  timepoint12 - timepoint48  0.000500 0.00664 161   0.075  1.0000
    ##  timepoint24 - timepoint36 -0.018792 0.00664 161  -2.829  0.0759
    ##  timepoint24 - timepoint48 -0.003333 0.00664 161  -0.502  0.9988
    ##  timepoint36 - timepoint48  0.015458 0.00664 161   2.327  0.2375
    ## 
    ## P value adjustment: tukey method for comparing a family of 7 estimates

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
    ## REML criterion at convergence: -801.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3697 -0.6248  0.0960  0.5933  3.0914 
    ## 
    ## Random effects:
    ##  Groups            Name        Variance  Std.Dev.
    ##  treatment:tank_id (Intercept) 2.081e-05 0.004561
    ##  Residual                      2.486e-04 0.015766
    ## Number of obs: 168, groups:  treatment:tank_id, 6
    ## 
    ## Fixed effects:
    ##                             Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)                 0.662333   0.005258  28.764422 125.959  < 2e-16 ***
    ## treatmentHeat              -0.002167   0.007436  28.764422  -0.291  0.77287    
    ## timepoint1                 -0.012917   0.006436 150.000001  -2.007  0.04657 *  
    ## timepoint6                 -0.016417   0.006436 150.000001  -2.551  0.01176 *  
    ## timepoint12                -0.017083   0.006436 150.000001  -2.654  0.00881 ** 
    ## timepoint24                -0.007250   0.006436 150.000001  -1.126  0.26180    
    ## timepoint36                -0.002583   0.006436 150.000001  -0.401  0.68873    
    ## timepoint48                -0.006833   0.006436 150.000001  -1.062  0.29010    
    ## treatmentHeat:timepoint1   -0.021583   0.009103 150.000001  -2.371  0.01900 *  
    ## treatmentHeat:timepoint6   -0.029167   0.009103 150.000001  -3.204  0.00165 ** 
    ## treatmentHeat:timepoint12  -0.022083   0.009103 150.000001  -2.426  0.01645 *  
    ## treatmentHeat:timepoint24  -0.049417   0.009103 150.000001  -5.429 2.23e-07 ***
    ## treatmentHeat:timepoint36  -0.021167   0.009103 150.000001  -2.325  0.02139 *  
    ## treatmentHeat:timepoint48  -0.043583   0.009103 150.000001  -4.788 4.00e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ treatment | timepoint)
pairs(emm)
```

    ## timepoint = 0:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.00217 0.00744 28.8   0.291  0.7729
    ## 
    ## timepoint = 1:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.02375 0.00744 28.8   3.194  0.0034
    ## 
    ## timepoint = 6:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.03133 0.00744 28.8   4.214  0.0002
    ## 
    ## timepoint = 12:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.02425 0.00744 28.8   3.261  0.0029
    ## 
    ## timepoint = 24:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.05158 0.00744 28.8   6.937  <.0001
    ## 
    ## timepoint = 36:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.02333 0.00744 28.8   3.138  0.0039
    ## 
    ## timepoint = 48:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.04575 0.00744 28.8   6.152  <.0001
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
    ## 1         0                  0.0022 0.0074    0.29 7.73e-01             
    ## 2         1                  0.0238 0.0074    3.19 3.39e-03           **
    ## 3         6                  0.0313 0.0074    4.21 2.26e-04          ***
    ## 4        12                  0.0243 0.0074    3.26 2.86e-03           **
    ## 5        24                  0.0516 0.0074    6.94 1.32e-07          ***
    ## 6        36                  0.0233 0.0074    3.14 3.91e-03           **
    ## 7        48                  0.0458 0.0074    6.15 1.09e-06          ***

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
