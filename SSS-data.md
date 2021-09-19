SSS data
================

# set working directory

``` r
library(ggplot2)
library(dbplyr)
library(psych)
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

\#read in the data

``` r
library(readxl)
survey <- read_excel("data.xlsx")
View(survey)
```

\#\#\#1. emotion

``` r
# positive emotion
survey$positive_emotion <- rowMeans(survey[,c("Q153_1","Q153_4","Q153_6","Q145_2","Q145_3","Q145_4","Q145_5","Q145_6","Q145_7","Q146_6")], na.rm = TRUE)

# negative emotion
survey$negative_emotion <- rowMeans(survey[,c("Q153_2","Q153_3","Q153_5","Q153_7","Q145_1","Q146_1","Q146_2","Q146_3","Q146_4","Q146_5")],na.rm = TRUE)

# test internal reliability
alpha(survey[,c("Q153_1","Q153_4","Q153_6","Q145_2","Q145_3","Q145_4","Q145_5","Q145_6","Q145_7","Q146_6","Q153_2","Q153_3","Q153_5","Q153_7","Q145_1","Q146_1","Q146_2","Q146_3","Q146_4","Q146_5")])
```

    ## Warning in alpha(survey[, c("Q153_1", "Q153_4", "Q153_6", "Q145_2", "Q145_3", : Some items were negatively correlated with the total scale and probably 
    ## should be reversed.  
    ## To do this, run the function again with the 'check.keys=TRUE' option

    ## Some items ( Q153_2 Q153_3 Q153_5 Q153_7 Q145_1 Q146_1 Q146_2 Q146_3 ) were negatively correlated with the total scale and 
    ## probably should be reversed.  
    ## To do this, run the function again with the 'check.keys=TRUE' option

    ## 
    ## Reliability analysis   
    ## Call: alpha(x = survey[, c("Q153_1", "Q153_4", "Q153_6", "Q145_2", 
    ##     "Q145_3", "Q145_4", "Q145_5", "Q145_6", "Q145_7", "Q146_6", 
    ##     "Q153_2", "Q153_3", "Q153_5", "Q153_7", "Q145_1", "Q146_1", 
    ##     "Q146_2", "Q146_3", "Q146_4", "Q146_5")])
    ## 
    ##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
    ##       0.83      0.81    0.89      0.18 4.4 0.014  2.4 0.51     0.18
    ## 
    ##  lower alpha upper     95% confidence boundaries
    ## 0.8 0.83 0.86 
    ## 
    ##  Reliability if an item is dropped:
    ##        raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    ## Q153_1      0.81      0.80    0.88      0.17 4.0    0.015 0.082  0.14
    ## Q153_4      0.81      0.79    0.88      0.17 3.8    0.016 0.083  0.14
    ## Q153_6      0.83      0.81    0.89      0.19 4.3    0.015 0.079  0.14
    ## Q145_2      0.81      0.80    0.88      0.17 4.0    0.016 0.074  0.14
    ## Q145_3      0.81      0.80    0.88      0.17 4.0    0.016 0.078  0.14
    ## Q145_4      0.81      0.80    0.88      0.17 3.9    0.016 0.074  0.14
    ## Q145_5      0.81      0.80    0.88      0.17 3.9    0.016 0.079  0.14
    ## Q145_6      0.81      0.80    0.88      0.17 4.0    0.016 0.081  0.14
    ## Q145_7      0.81      0.80    0.88      0.17 4.0    0.016 0.076  0.14
    ## Q146_6      0.81      0.79    0.88      0.17 3.9    0.016 0.086  0.13
    ## Q153_2      0.84      0.82    0.89      0.19 4.5    0.013 0.081  0.20
    ## Q153_3      0.83      0.81    0.89      0.19 4.3    0.014 0.081  0.20
    ## Q153_5      0.83      0.81    0.89      0.19 4.4    0.014 0.083  0.20
    ## Q153_7      0.83      0.81    0.88      0.18 4.3    0.014 0.084  0.20
    ## Q145_1      0.83      0.81    0.89      0.19 4.3    0.014 0.086  0.20
    ## Q146_1      0.83      0.81    0.89      0.19 4.4    0.014 0.086  0.20
    ## Q146_2      0.84      0.82    0.89      0.20 4.6    0.013 0.076  0.20
    ## Q146_3      0.84      0.82    0.89      0.19 4.5    0.013 0.082  0.20
    ## Q146_4      0.82      0.80    0.88      0.17 4.0    0.015 0.087  0.14
    ## Q146_5      0.81      0.79    0.88      0.17 3.8    0.016 0.085  0.14
    ## 
    ##  Item statistics 
    ##          n raw.r std.r r.cor r.drop mean   sd
    ## Q153_1 284 0.619  0.56  0.54  0.535  2.9 1.18
    ## Q153_4 284 0.728  0.69  0.68  0.659  2.5 1.20
    ## Q153_6 284 0.422  0.35  0.31  0.324  3.2 1.13
    ## Q145_2 284 0.663  0.59  0.59  0.593  3.1 1.14
    ## Q145_3 284 0.626  0.56  0.55  0.553  3.7 1.11
    ## Q145_4 284 0.689  0.62  0.63  0.620  3.4 1.19
    ## Q145_5 284 0.688  0.64  0.63  0.623  3.2 1.13
    ## Q145_6 284 0.628  0.57  0.55  0.547  3.1 1.22
    ## Q145_7 284 0.620  0.55  0.54  0.542  3.1 1.17
    ## Q146_6 285 0.675  0.65  0.63  0.600  2.7 1.21
    ## Q153_2 284 0.170  0.25  0.21  0.079  2.0 0.97
    ## Q153_3 285 0.241  0.35  0.31  0.168  1.5 0.79
    ## Q153_5 284 0.203  0.31  0.26  0.137  1.3 0.76
    ## Q153_7 283 0.291  0.40  0.37  0.233  1.3 0.69
    ## Q145_1 284 0.282  0.35  0.30  0.192  1.7 0.97
    ## Q146_1 285 0.248  0.33  0.28  0.177  1.4 0.74
    ## Q146_2 285 0.072  0.16  0.12 -0.024  1.9 1.01
    ## Q146_3 285 0.164  0.23  0.18  0.066  1.8 0.99
    ## Q146_4 284 0.575  0.55  0.51  0.487  2.0 1.19
    ## Q146_5 285 0.686  0.67  0.65  0.623  2.0 1.08
    ## 
    ## Non missing response frequency for each item
    ##           1    2    3    4    5 miss
    ## Q153_1 0.16 0.23 0.27 0.28 0.07 0.00
    ## Q153_4 0.28 0.25 0.24 0.18 0.05 0.00
    ## Q153_6 0.07 0.23 0.27 0.31 0.12 0.00
    ## Q145_2 0.05 0.30 0.23 0.30 0.13 0.00
    ## Q145_3 0.04 0.14 0.20 0.37 0.25 0.00
    ## Q145_4 0.07 0.19 0.24 0.30 0.19 0.00
    ## Q145_5 0.08 0.19 0.30 0.30 0.13 0.00
    ## Q145_6 0.11 0.23 0.26 0.26 0.14 0.00
    ## Q145_7 0.08 0.27 0.28 0.23 0.13 0.00
    ## Q146_6 0.20 0.23 0.28 0.22 0.07 0.00
    ## Q153_2 0.38 0.30 0.25 0.06 0.01 0.00
    ## Q153_3 0.70 0.18 0.09 0.02 0.00 0.00
    ## Q153_5 0.80 0.12 0.05 0.03 0.01 0.00
    ## Q153_7 0.78 0.13 0.07 0.01 0.00 0.01
    ## Q145_1 0.55 0.26 0.10 0.07 0.01 0.00
    ## Q146_1 0.74 0.16 0.07 0.02 0.00 0.00
    ## Q146_2 0.43 0.35 0.13 0.08 0.01 0.00
    ## Q146_3 0.51 0.30 0.11 0.06 0.02 0.00
    ## Q146_4 0.48 0.22 0.15 0.11 0.04 0.00
    ## Q146_5 0.43 0.27 0.20 0.08 0.02 0.00

\#\#\#2. HEALTH PERCEPTION

``` r
# current health
survey$current_health <- rowMeans(survey[,c("Q67_1","Q67_2","Q67_3","Q67_4","Q67_5","Q67_6")], na.rm = TRUE)

# future health
survey$future_health <- rowMeans(survey[,c("Q123_1","Q123_2","Q123_3","Q123_4")], na.rm = TRUE)

# test internal reliability
alpha(survey[,c("Q67_1","Q67_2","Q67_3","Q67_4","Q67_5","Q67_6","Q123_1","Q123_2","Q123_3","Q123_4")])
```

    ## Warning in alpha(survey[, c("Q67_1", "Q67_2", "Q67_3", "Q67_4", "Q67_5", : Some items were negatively correlated with the total scale and probably 
    ## should be reversed.  
    ## To do this, run the function again with the 'check.keys=TRUE' option

    ## Some items ( Q67_2 Q67_3 Q67_6 Q123_1 Q123_4 ) were negatively correlated with the total scale and 
    ## probably should be reversed.  
    ## To do this, run the function again with the 'check.keys=TRUE' option

    ## 
    ## Reliability analysis   
    ## Call: alpha(x = survey[, c("Q67_1", "Q67_2", "Q67_3", "Q67_4", "Q67_5", 
    ##     "Q67_6", "Q123_1", "Q123_2", "Q123_3", "Q123_4")])
    ## 
    ##   raw_alpha std.alpha G6(smc) average_r  S/N   ase mean   sd median_r
    ##        0.3      0.31    0.52     0.043 0.44 0.063    3 0.41     0.02
    ## 
    ##  lower alpha upper     95% confidence boundaries
    ## 0.18 0.3 0.43 
    ## 
    ##  Reliability if an item is dropped:
    ##        raw_alpha std.alpha G6(smc) average_r  S/N alpha se var.r    med.r
    ## Q67_1       0.32      0.30    0.51     0.045 0.42    0.060 0.085  0.02802
    ## Q67_2       0.13      0.19    0.43     0.025 0.23    0.081 0.086 -0.06776
    ## Q67_3       0.33      0.36    0.54     0.058 0.55    0.061 0.074  0.02802
    ## Q67_4       0.29      0.26    0.48     0.038 0.35    0.062 0.080  0.02802
    ## Q67_5       0.36      0.34    0.52     0.054 0.52    0.057 0.073  0.00723
    ## Q67_6       0.32      0.35    0.52     0.055 0.53    0.062 0.073  0.02802
    ## Q123_1      0.21      0.26    0.47     0.038 0.35    0.072 0.080  0.02802
    ## Q123_2      0.25      0.22    0.45     0.030 0.28    0.067 0.084 -0.00085
    ## Q123_3      0.29      0.28    0.48     0.040 0.38    0.064 0.078 -0.00085
    ## Q123_4      0.26      0.28    0.51     0.042 0.40    0.068 0.092  0.00723
    ## 
    ##  Item statistics 
    ##          n raw.r std.r r.cor r.drop mean   sd
    ## Q67_1  283  0.30  0.35 0.203  0.017  3.2 1.12
    ## Q67_2  283  0.62  0.54 0.515  0.354  3.3 1.35
    ## Q67_3  284  0.32  0.22 0.065  0.012  3.1 1.27
    ## Q67_4  284  0.36  0.42 0.326  0.081  3.1 1.17
    ## Q67_5  284  0.16  0.26 0.140 -0.084  2.5 1.01
    ## Q67_6  284  0.33  0.25 0.118  0.039  3.0 1.23
    ## Q123_1 282  0.49  0.42 0.346  0.256  3.5 1.08
    ## Q123_2 282  0.40  0.49 0.442  0.196  2.5 0.88
    ## Q123_3 282  0.28  0.39 0.316  0.090  1.9 0.80
    ## Q123_4 282  0.40  0.37 0.218  0.163  3.6 1.03
    ## 
    ## Non missing response frequency for each item
    ##           1    2    3    4    5 miss
    ## Q67_1  0.03 0.30 0.22 0.30 0.14 0.01
    ## Q67_2  0.10 0.27 0.13 0.27 0.24 0.01
    ## Q67_3  0.10 0.31 0.19 0.24 0.17 0.00
    ## Q67_4  0.08 0.26 0.30 0.22 0.14 0.00
    ## Q67_5  0.13 0.44 0.25 0.14 0.04 0.00
    ## Q67_6  0.13 0.29 0.19 0.29 0.11 0.00
    ## Q123_1 0.04 0.13 0.30 0.33 0.20 0.01
    ## Q123_2 0.11 0.41 0.38 0.07 0.02 0.01
    ## Q123_3 0.34 0.48 0.15 0.03 0.00 0.01
    ## Q123_4 0.02 0.12 0.30 0.35 0.21 0.01

\#\#\#3. HAPPINESS

``` r
# reverse coding
survey$happiness1_reverse <- 8 - survey$Q177
survey$happiness2_reverse <- 8 - survey$Q102
# happiness wake
survey$happiness_wake <- rowMeans(survey[,c("Q174","Q175","Q176","happiness1_reverse")], na.rm = TRUE)
# happiness general
survey$happiness_general <- rowMeans(survey[,c("Q96","Q98","Q100","happiness2_reverse")], na.rm = TRUE)
```

\#\#\#4. EXERCISE INTENT

``` r
survey$exercise_intent <- rowMeans(survey[,c("Q124_1","Q124_2","Q124_3","Q124_4","Q124_5","Q124_6","Q124_7","Q124_8","Q124_9","Q124_11","Q124_12","Q124_13","Q124_14","Q124_15","Q124_17","Q124_18")],na.rm = TRUE)
```

\#\#\#5. EMPATHY

``` r
# reverse coding
survey$EC2_reverse <- 6 - survey$Q102_2
survey$EC4_reverse <- 6 - survey$Q102_4
survey$EC5_reverse <- 6 - survey$Q102_5
survey$PT1_reverse <- 6 - survey$Q103_1
survey$PT4_reverse <- 6 - survey$Q103_4
# Empathetic concern
survey$empathetic_concern <- rowMeans(survey[,c("Q102_1","EC2_reverse","Q102_3","EC4_reverse","EC5_reverse","Q102_6","Q102_7")], na.rm = TRUE)
# Perspective taking
survey$perspective_taking <- rowMeans(survey[,c("PT1_reverse","Q103_2","Q103_3","Q103_5","Q103_6","Q103_7","PT4_reverse")], na.rm = TRUE)
```

### Create new data frame

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:dbplyr':
    ## 
    ##     ident, sql

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
thesis <- survey %>%
  select(id,condition,national_sss,procedure,manipulation_check,Q142,positive_emotion,negative_emotion,current_health,future_health,happiness_wake,happiness_general,exercise_intent,empathetic_concern,perspective_taking,Q128,Q130,Q132,Q134)
write.csv(survey, "survey.csv")
write.csv(thesis,"thesis.csv")
```
