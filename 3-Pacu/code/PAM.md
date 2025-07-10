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
    ##      5      5      5      5      5      5      5      5      5      5      5 
    ##   1473   1474   1614   1626   1691   1753   1761   2195   2370   2565   2666 
    ##      5      5      5      5      5      5      5      5      5      5      5 
    ##   2730   2986 Parent 
    ##      5      5      0

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
    ## -0.051250 -0.016719 -0.001208  0.018875  0.051750 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.661250   0.004537 145.739  < 2e-16 ***
    ## timepoint1  -0.023708   0.006417  -3.695 0.000338 ***
    ## timepoint6  -0.031000   0.006417  -4.831 4.23e-06 ***
    ## timepoint12 -0.028125   0.006417  -4.383 2.60e-05 ***
    ## timepoint24 -0.031958   0.006417  -4.981 2.25e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02223 on 115 degrees of freedom
    ## Multiple R-squared:  0.2282, Adjusted R-squared:  0.2014 
    ## F-statistic: 8.501 on 4 and 115 DF,  p-value: 4.8e-06

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ timepoint)
pairs(emm)
```

    ##  contrast                   estimate      SE  df t.ratio p.value
    ##  timepoint0 - timepoint1    0.023708 0.00642 115   3.695  0.0031
    ##  timepoint0 - timepoint6    0.031000 0.00642 115   4.831  <.0001
    ##  timepoint0 - timepoint12   0.028125 0.00642 115   4.383  0.0002
    ##  timepoint0 - timepoint24   0.031958 0.00642 115   4.981  <.0001
    ##  timepoint1 - timepoint6    0.007292 0.00642 115   1.136  0.7869
    ##  timepoint1 - timepoint12   0.004417 0.00642 115   0.688  0.9587
    ##  timepoint1 - timepoint24   0.008250 0.00642 115   1.286  0.7005
    ##  timepoint6 - timepoint12  -0.002875 0.00642 115  -0.448  0.9916
    ##  timepoint6 - timepoint24   0.000958 0.00642 115   0.149  0.9999
    ##  timepoint12 - timepoint24  0.003833 0.00642 115   0.597  0.9752
    ## 
    ## P value adjustment: tukey method for comparing a family of 5 estimates

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
    ## REML criterion at convergence: -575.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4318 -0.6307  0.0608  0.6314  3.0746 
    ## 
    ## Random effects:
    ##  Groups            Name        Variance  Std.Dev.
    ##  treatment:tank_id (Intercept) 1.813e-05 0.004258
    ##  Residual                      2.416e-04 0.015542
    ## Number of obs: 120, groups:  treatment:tank_id, 6
    ## 
    ## Fixed effects:
    ##                             Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)                 0.662333   0.005116  24.649104 129.465  < 2e-16 ***
    ## treatmentHeat              -0.002167   0.007235  24.649104  -0.299  0.76709    
    ## timepoint1                 -0.012917   0.006345 106.000000  -2.036  0.04428 *  
    ## timepoint6                 -0.016417   0.006345 106.000000  -2.587  0.01103 *  
    ## timepoint12                -0.017083   0.006345 106.000000  -2.692  0.00825 ** 
    ## timepoint24                -0.007250   0.006345 106.000000  -1.143  0.25577    
    ## treatmentHeat:timepoint1   -0.021583   0.008973 106.000000  -2.405  0.01789 *  
    ## treatmentHeat:timepoint6   -0.029167   0.008973 106.000000  -3.250  0.00155 ** 
    ## treatmentHeat:timepoint12  -0.022083   0.008973 106.000000  -2.461  0.01547 *  
    ## treatmentHeat:timepoint24  -0.049417   0.008973 106.000000  -5.507 2.57e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) trtmnH tmpnt1 tmpnt6 tmpn12 tmpn24 trtH:1 trtH:6 trH:12
    ## treatmentHt -0.707                                                        
    ## timepoint1  -0.620  0.439                                                 
    ## timepoint6  -0.620  0.439  0.500                                          
    ## timepoint12 -0.620  0.439  0.500  0.500                                   
    ## timepoint24 -0.620  0.439  0.500  0.500  0.500                            
    ## trtmntHt:t1  0.439 -0.620 -0.707 -0.354 -0.354 -0.354                     
    ## trtmntHt:t6  0.439 -0.620 -0.354 -0.707 -0.354 -0.354  0.500              
    ## trtmntHt:12  0.439 -0.620 -0.354 -0.354 -0.707 -0.354  0.500  0.500       
    ## trtmntHt:24  0.439 -0.620 -0.354 -0.354 -0.354 -0.707  0.500  0.500  0.500

``` r
# Estimated marginal means (adjusted for random effects and model structure)
emm <- emmeans(model, ~ treatment | timepoint)
pairs(emm)
```

    ## timepoint = 0:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.00217 0.00723 24.6   0.299  0.7671
    ## 
    ## timepoint = 1:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.02375 0.00723 24.6   3.283  0.0031
    ## 
    ## timepoint = 6:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.03133 0.00723 24.6   4.331  0.0002
    ## 
    ## timepoint = 12:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.02425 0.00723 24.6   3.352  0.0026
    ## 
    ## timepoint = 24:
    ##  contrast       estimate      SE   df t.ratio p.value
    ##  Control - Heat  0.05158 0.00723 24.6   7.130  <.0001
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
    ## 1         0                  0.0022 0.0072    0.30 7.67e-01             
    ## 2         1                  0.0238 0.0072    3.28 3.07e-03           **
    ## 3         6                  0.0313 0.0072    4.33 2.17e-04          ***
    ## 4        12                  0.0243 0.0072    3.35 2.59e-03           **
    ## 5        24                  0.0516 0.0072    7.13 1.95e-07          ***

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
