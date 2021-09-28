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
#reverse coding
survey$CH1_reverse <- 6 - survey$Q67_2
survey$CH2_reverse <- 6 - survey$Q67_3
survey$CH3_reverse <- 6 - survey$Q67_6
survey$FH1_reverse <- 6 - survey$Q123_1
survey$FH2_reverse <- 6 - survey$Q123_4
# current health
survey$current_health <- rowMeans(survey[,c("Q67_1","CH1_reverse","CH2_reverse","Q67_4","Q67_5","CH3_reverse")], na.rm = TRUE)

# future health
survey$future_health <- rowMeans(survey[,c("FH1_reverse","Q123_2","Q123_3","FH2_reverse")], na.rm = TRUE)

# test internal reliability
alpha(survey[,c("Q67_1","CH1_reverse","CH2_reverse","Q67_4","Q67_5","CH3_reverse","FH1_reverse","Q123_2","Q123_3","FH2_reverse")],check.keys=TRUE)
```

    ## 
    ## Reliability analysis   
    ## Call: alpha(x = survey[, c("Q67_1", "CH1_reverse", "CH2_reverse", "Q67_4", 
    ##     "Q67_5", "CH3_reverse", "FH1_reverse", "Q123_2", "Q123_3", 
    ##     "FH2_reverse")], check.keys = TRUE)
    ## 
    ##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
    ##       0.76      0.77     0.8      0.25 3.3 0.021  2.7 0.63     0.26
    ## 
    ##  lower alpha upper     95% confidence boundaries
    ## 0.72 0.76 0.8 
    ## 
    ##  Reliability if an item is dropped:
    ##             raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    ## Q67_1            0.75      0.75    0.78      0.25 3.1    0.022 0.021  0.26
    ## CH1_reverse      0.76      0.76    0.78      0.26 3.2    0.021 0.015  0.27
    ## CH2_reverse      0.72      0.73    0.77      0.23 2.7    0.024 0.022  0.26
    ## Q67_4            0.74      0.75    0.77      0.25 2.9    0.022 0.020  0.26
    ## Q67_5            0.73      0.74    0.76      0.24 2.8    0.024 0.019  0.26
    ## CH3_reverse      0.72      0.73    0.76      0.23 2.7    0.025 0.020  0.26
    ## FH1_reverse      0.73      0.74    0.77      0.24 2.9    0.023 0.021  0.27
    ## Q123_2           0.75      0.75    0.78      0.25 3.1    0.022 0.018  0.28
    ## Q123_3           0.74      0.74    0.77      0.24 2.9    0.022 0.019  0.26
    ## FH2_reverse      0.76      0.77    0.79      0.27 3.3    0.021 0.019  0.28
    ## 
    ##  Item statistics 
    ##               n raw.r std.r r.cor r.drop mean   sd
    ## Q67_1       283  0.53  0.53  0.45   0.38  3.2 1.12
    ## CH1_reverse 283  0.52  0.46  0.39   0.34  2.7 1.35
    ## CH2_reverse 284  0.68  0.66  0.61   0.55  2.9 1.27
    ## Q67_4       284  0.57  0.58  0.51   0.42  3.1 1.17
    ## Q67_5       284  0.65  0.64  0.60   0.53  2.5 1.01
    ## CH3_reverse 284  0.68  0.66  0.62   0.56  3.0 1.23
    ## FH1_reverse 282  0.61  0.61  0.56   0.49  2.5 1.08
    ## Q123_2      282  0.47  0.52  0.46   0.34  2.5 0.88
    ## Q123_3      282  0.54  0.60  0.55   0.44  1.9 0.80
    ## FH2_reverse 282  0.41  0.42  0.31   0.26  2.4 1.03
    ## 
    ## Non missing response frequency for each item
    ##                1    2    3    4    5 miss
    ## Q67_1       0.03 0.30 0.22 0.30 0.14 0.01
    ## CH1_reverse 0.24 0.27 0.13 0.27 0.10 0.01
    ## CH2_reverse 0.17 0.24 0.19 0.31 0.10 0.00
    ## Q67_4       0.08 0.26 0.30 0.22 0.14 0.00
    ## Q67_5       0.13 0.44 0.25 0.14 0.04 0.00
    ## CH3_reverse 0.11 0.29 0.19 0.29 0.13 0.00
    ## FH1_reverse 0.20 0.33 0.30 0.13 0.04 0.01
    ## Q123_2      0.11 0.41 0.38 0.07 0.02 0.01
    ## Q123_3      0.34 0.48 0.15 0.03 0.00 0.01
    ## FH2_reverse 0.21 0.35 0.30 0.12 0.02 0.01

\#\#\#3. HAPPINESS

``` r
# reverse coding
survey$happiness1_reverse <- 8 - survey$Q177
survey$happiness2_reverse <- 8 - survey$Q102
# happiness wake
survey$happiness_wake <- rowMeans(survey[,c("Q174","Q175","Q176","happiness1_reverse")], na.rm = TRUE)
# happiness general
survey$happiness_general <- rowMeans(survey[,c("Q96","Q98","Q100","happiness2_reverse")], na.rm = TRUE)

alpha(survey[,c("Q174","Q175","Q176","happiness1_reverse","Q96","Q98","Q100","happiness2_reverse")])
```

    ## 
    ## Reliability analysis   
    ## Call: alpha(x = survey[, c("Q174", "Q175", "Q176", "happiness1_reverse", 
    ##     "Q96", "Q98", "Q100", "happiness2_reverse")])
    ## 
    ##   raw_alpha std.alpha G6(smc) average_r S/N    ase mean  sd median_r
    ##       0.93      0.93    0.95      0.63  14 0.0065  4.8 1.1     0.67
    ## 
    ##  lower alpha upper     95% confidence boundaries
    ## 0.92 0.93 0.94 
    ## 
    ##  Reliability if an item is dropped:
    ##                    raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r
    ## Q174                    0.92      0.92    0.94      0.62  12   0.0077 0.014
    ## Q175                    0.92      0.92    0.94      0.63  12   0.0077 0.014
    ## Q176                    0.92      0.92    0.94      0.62  11   0.0079 0.015
    ## happiness1_reverse      0.93      0.93    0.93      0.66  14   0.0065 0.011
    ## Q96                     0.92      0.92    0.94      0.63  12   0.0076 0.016
    ## Q98                     0.92      0.92    0.94      0.62  11   0.0078 0.015
    ## Q100                    0.92      0.92    0.94      0.62  12   0.0077 0.016
    ## happiness2_reverse      0.93      0.93    0.93      0.66  13   0.0066 0.012
    ##                    med.r
    ## Q174                0.63
    ## Q175                0.69
    ## Q176                0.63
    ## happiness1_reverse  0.69
    ## Q96                 0.63
    ## Q98                 0.63
    ## Q100                0.65
    ## happiness2_reverse  0.69
    ## 
    ##  Item statistics 
    ##                      n raw.r std.r r.cor r.drop mean  sd
    ## Q174               285  0.85  0.85  0.83   0.80  4.9 1.3
    ## Q175               285  0.84  0.85  0.83   0.79  4.5 1.4
    ## Q176               285  0.86  0.86  0.84   0.81  4.6 1.4
    ## happiness1_reverse 285  0.75  0.73  0.71   0.66  4.9 1.5
    ## Q96                284  0.83  0.84  0.81   0.78  5.2 1.2
    ## Q98                285  0.86  0.87  0.85   0.81  4.8 1.3
    ## Q100               284  0.84  0.85  0.82   0.79  4.6 1.4
    ## happiness2_reverse 284  0.76  0.74  0.72   0.67  4.9 1.5
    ## 
    ## Non missing response frequency for each item
    ##                       1    2    3    4    5    6    7 miss
    ## Q174               0.01 0.04 0.09 0.19 0.35 0.22 0.09    0
    ## Q175               0.03 0.05 0.11 0.28 0.31 0.18 0.06    0
    ## Q176               0.02 0.06 0.15 0.22 0.29 0.16 0.09    0
    ## happiness1_reverse 0.02 0.05 0.16 0.15 0.22 0.27 0.14    0
    ## Q96                0.01 0.00 0.06 0.17 0.37 0.22 0.16    0
    ## Q98                0.02 0.03 0.10 0.21 0.37 0.21 0.07    0
    ## Q100               0.02 0.02 0.18 0.26 0.26 0.17 0.09    0
    ## happiness2_reverse 0.01 0.05 0.13 0.19 0.22 0.24 0.15    0

\#\#\#4. EXERCISE INTENT

``` r
survey$exercise_intent <- rowMeans(survey[,c("Q124_1","Q124_2","Q124_3","Q124_4","Q124_5","Q124_6","Q124_7","Q124_8","Q124_9","Q124_11","Q124_12","Q124_13","Q124_14","Q124_15","Q124_17","Q124_18")],na.rm = TRUE)

alpha(survey[,c("Q124_1","Q124_2","Q124_3","Q124_4","Q124_5","Q124_6","Q124_7","Q124_8","Q124_9","Q124_11","Q124_12","Q124_13","Q124_14","Q124_15","Q124_17","Q124_18")])
```

    ## Number of categories should be increased  in order to count frequencies.

    ## 
    ## Reliability analysis   
    ## Call: alpha(x = survey[, c("Q124_1", "Q124_2", "Q124_3", "Q124_4", 
    ##     "Q124_5", "Q124_6", "Q124_7", "Q124_8", "Q124_9", "Q124_11", 
    ##     "Q124_12", "Q124_13", "Q124_14", "Q124_15", "Q124_17", "Q124_18")])
    ## 
    ##   raw_alpha std.alpha G6(smc) average_r S/N    ase mean sd median_r
    ##       0.91      0.91    0.93       0.4  11 0.0076   53 21     0.38
    ## 
    ##  lower alpha upper     95% confidence boundaries
    ## 0.9 0.91 0.93 
    ## 
    ##  Reliability if an item is dropped:
    ##         raw_alpha std.alpha G6(smc) average_r  S/N alpha se  var.r med.r
    ## Q124_1       0.90      0.91    0.92      0.39  9.6   0.0083 0.0104  0.37
    ## Q124_2       0.91      0.91    0.93      0.40 10.0   0.0080 0.0094  0.38
    ## Q124_3       0.91      0.91    0.93      0.40 10.1   0.0079 0.0109  0.38
    ## Q124_4       0.91      0.91    0.93      0.40 10.1   0.0079 0.0103  0.39
    ## Q124_5       0.91      0.91    0.93      0.40  9.8   0.0082 0.0100  0.38
    ## Q124_6       0.91      0.91    0.92      0.40  9.8   0.0082 0.0098  0.38
    ## Q124_7       0.91      0.91    0.93      0.40  9.9   0.0081 0.0095  0.38
    ## Q124_8       0.91      0.91    0.92      0.40  9.9   0.0081 0.0104  0.39
    ## Q124_9       0.91      0.91    0.93      0.40 10.2   0.0079 0.0106  0.38
    ## Q124_11      0.91      0.91    0.92      0.40  9.9   0.0080 0.0097  0.38
    ## Q124_12      0.91      0.91    0.93      0.41 10.2   0.0078 0.0107  0.39
    ## Q124_13      0.91      0.91    0.93      0.40 10.0   0.0080 0.0109  0.38
    ## Q124_14      0.91      0.91    0.93      0.40 10.0   0.0080 0.0108  0.39
    ## Q124_15      0.91      0.91    0.93      0.40 10.2   0.0079 0.0101  0.39
    ## Q124_17      0.91      0.91    0.93      0.40  9.9   0.0081 0.0103  0.38
    ## Q124_18      0.91      0.91    0.93      0.40  9.9   0.0081 0.0103  0.38
    ## 
    ##  Item statistics 
    ##           n raw.r std.r r.cor r.drop mean sd
    ## Q124_1  277  0.77  0.76  0.75   0.72   53 28
    ## Q124_2  278  0.66  0.65  0.64   0.59   56 31
    ## Q124_3  278  0.64  0.64  0.60   0.57   61 32
    ## Q124_4  274  0.62  0.62  0.60   0.56   48 30
    ## Q124_5  280  0.70  0.70  0.68   0.65   65 29
    ## Q124_6  270  0.71  0.70  0.69   0.65   53 35
    ## Q124_7  276  0.69  0.68  0.66   0.63   62 31
    ## Q124_8  272  0.68  0.68  0.66   0.62   54 31
    ## Q124_9  266  0.60  0.61  0.57   0.54   42 30
    ## Q124_11 266  0.66  0.67  0.65   0.60   43 31
    ## Q124_12 273  0.60  0.60  0.56   0.53   48 32
    ## Q124_13 273  0.66  0.66  0.63   0.60   49 30
    ## Q124_14 282  0.65  0.64  0.61   0.58   66 29
    ## Q124_15 278  0.62  0.61  0.58   0.54   65 33
    ## Q124_17 269  0.67  0.67  0.65   0.62   39 29
    ## Q124_18 276  0.69  0.69  0.67   0.63   60 31

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

alpha(survey[,c("Q102_1","EC2_reverse","Q102_3","EC4_reverse","EC5_reverse","Q102_6","Q102_7","PT1_reverse","Q103_2","Q103_3","Q103_5","Q103_6","Q103_7","PT4_reverse")])
```

    ## 
    ## Reliability analysis   
    ## Call: alpha(x = survey[, c("Q102_1", "EC2_reverse", "Q102_3", "EC4_reverse", 
    ##     "EC5_reverse", "Q102_6", "Q102_7", "PT1_reverse", "Q103_2", 
    ##     "Q103_3", "Q103_5", "Q103_6", "Q103_7", "PT4_reverse")])
    ## 
    ##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
    ##       0.82      0.81    0.85      0.24 4.4 0.016  3.8 0.56     0.22
    ## 
    ##  lower alpha upper     95% confidence boundaries
    ## 0.78 0.82 0.85 
    ## 
    ##  Reliability if an item is dropped:
    ##             raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    ## Q102_1           0.80      0.80    0.83      0.23 3.9    0.018 0.021  0.22
    ## EC2_reverse      0.81      0.81    0.84      0.25 4.2    0.016 0.020  0.23
    ## Q102_3           0.81      0.81    0.84      0.24 4.1    0.017 0.021  0.22
    ## EC4_reverse      0.81      0.81    0.84      0.25 4.2    0.016 0.020  0.23
    ## EC5_reverse      0.81      0.81    0.84      0.25 4.2    0.016 0.020  0.22
    ## Q102_6           0.80      0.80    0.83      0.23 4.0    0.017 0.021  0.22
    ## Q102_7           0.80      0.80    0.83      0.23 3.9    0.017 0.020  0.22
    ## PT1_reverse      0.81      0.81    0.84      0.25 4.3    0.017 0.022  0.23
    ## Q103_2           0.80      0.80    0.83      0.23 4.0    0.017 0.018  0.22
    ## Q103_3           0.79      0.79    0.83      0.23 3.9    0.018 0.019  0.22
    ## Q103_5           0.80      0.80    0.84      0.24 4.0    0.017 0.020  0.22
    ## Q103_6           0.80      0.80    0.83      0.23 3.9    0.018 0.018  0.22
    ## Q103_7           0.80      0.80    0.83      0.23 4.0    0.018 0.020  0.22
    ## PT4_reverse      0.82      0.82    0.85      0.25 4.4    0.016 0.020  0.23
    ## 
    ##  Item statistics 
    ##               n raw.r std.r r.cor r.drop mean   sd
    ## Q102_1      284  0.62  0.63  0.59   0.53  3.8 1.04
    ## EC2_reverse 284  0.46  0.47  0.42   0.35  4.2 0.97
    ## Q102_3      284  0.50  0.51  0.46   0.41  3.9 0.94
    ## EC4_reverse 284  0.46  0.47  0.41   0.35  4.2 0.97
    ## EC5_reverse 284  0.44  0.47  0.41   0.34  4.5 0.88
    ## Q102_6      284  0.59  0.59  0.56   0.49  3.7 1.05
    ## Q102_7      284  0.60  0.60  0.58   0.50  3.7 1.08
    ## PT1_reverse 285  0.45  0.45  0.38   0.34  4.1 0.96
    ## Q103_2      285  0.60  0.58  0.56   0.50  3.6 1.12
    ## Q103_3      282  0.65  0.64  0.62   0.57  3.7 1.01
    ## Q103_5      284  0.58  0.56  0.52   0.47  3.6 1.06
    ## Q103_6      284  0.63  0.61  0.59   0.53  3.0 1.14
    ## Q103_7      284  0.61  0.60  0.57   0.51  3.3 1.07
    ## PT4_reverse 284  0.40  0.38  0.30   0.26  3.5 1.14
    ## 
    ## Non missing response frequency for each item
    ##                1    2    3    4    5 miss
    ## Q102_1      0.01 0.12 0.22 0.36 0.29 0.00
    ## EC2_reverse 0.02 0.04 0.13 0.32 0.49 0.00
    ## Q102_3      0.01 0.10 0.18 0.44 0.27 0.00
    ## EC4_reverse 0.00 0.08 0.13 0.31 0.48 0.00
    ## EC5_reverse 0.01 0.05 0.06 0.21 0.68 0.00
    ## Q102_6      0.02 0.12 0.23 0.37 0.26 0.00
    ## Q102_7      0.03 0.11 0.24 0.34 0.27 0.00
    ## PT1_reverse 0.01 0.07 0.12 0.36 0.43 0.00
    ## Q103_2      0.03 0.16 0.23 0.32 0.25 0.00
    ## Q103_3      0.02 0.12 0.21 0.41 0.24 0.01
    ## Q103_5      0.03 0.13 0.28 0.34 0.22 0.00
    ## Q103_6      0.07 0.30 0.29 0.23 0.12 0.00
    ## Q103_7      0.05 0.20 0.31 0.31 0.13 0.00
    ## PT4_reverse 0.05 0.17 0.21 0.37 0.20 0.00

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
