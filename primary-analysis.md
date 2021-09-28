primary analysis
================

``` r
library(readr)
thesis <- read_csv("thesis.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## 
    ## ─ Column specification ────────────────────────────
    ## cols(
    ##   .default = col_double(),
    ##   condition = col_character(),
    ##   procedure = col_character(),
    ##   Q132 = col_number()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
View(thesis)
```

``` r
library(psych)
library(sjPlot)
```

    ## Registered S3 methods overwritten by 'parameters':
    ##   method                           from      
    ##   as.double.parameters_kurtosis    datawizard
    ##   as.double.parameters_skewness    datawizard
    ##   as.double.parameters_smoothness  datawizard
    ##   as.numeric.parameters_kurtosis   datawizard
    ##   as.numeric.parameters_skewness   datawizard
    ##   as.numeric.parameters_smoothness datawizard
    ##   print.parameters_distribution    datawizard
    ##   print.parameters_kurtosis        datawizard
    ##   print.parameters_skewness        datawizard
    ##   summary.parameters_kurtosis      datawizard
    ##   summary.parameters_skewness      datawizard

    ## Learn more about sjPlot with 'browseVignettes("sjPlot")'.

``` r
library(tidyverse)
```

    ## ─ Attaching packages ──────────────────── tidyverse 1.3.0 ─

    ## ✓ ggplot2 3.3.3     ✓ dplyr   1.0.4
    ## ✓ tibble  3.1.0     ✓ stringr 1.4.0
    ## ✓ tidyr   1.1.2     ✓ forcats 0.5.1
    ## ✓ purrr   0.3.4

    ## ─ Conflicts ───────────────────── tidyverse_conflicts() ─
    ## x ggplot2::%+%()   masks psych::%+%()
    ## x ggplot2::alpha() masks psych::alpha()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()

``` r
thesis <- thesis %>%
  rename(exercise_frequency = Q142, 
         birthdate = Q128,
         gender = Q130,
         ethinicity = Q132,
         SES = Q134)
thesis$age <- 2021 - thesis$birthdate
```

\#\#\#correlation matrix

``` r
#drop non-numeric variables
thesis$X1 <- NULL
thesis$id <- NULL
thesis$manipulation_check <- NULL
thesis$procedure <- NULL
thesis$birthdate <- NULL
thesis$ethinicity <- NULL
```

``` r
#recode condition, high_sss = 1, low_sss = 0
thesis$condition[thesis$condition == "high_sss"] <- 1
thesis$condition[thesis$condition == "low_sss"] <- 0
#set the condition variable to type numeric
thesis$condition <- as.numeric(thesis$condition)
thesis$gender <- as.numeric(thesis$gender)
str(thesis)
```

    ## spec_tbl_df [285 × 15] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ condition         : num [1:285] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ national_sss      : num [1:285] 8 3 7 8 9 7 7 7 5 7 ...
    ##  $ exercise_frequency: num [1:285] 1 1 1 2 1 4 1 2 1 3 ...
    ##  $ positive_emotion  : num [1:285] 2.8 2.7 3.9 2.1 4.4 3.4 1.5 2.3 3.5 1.2 ...
    ##  $ negative_emotion  : num [1:285] 2.1 1.2 1.9 1.3 1.4 2.9 2 2.7 2.2 2.5 ...
    ##  $ current_health    : num [1:285] 2.5 2.67 2.5 3.17 3.83 ...
    ##  $ future_health     : num [1:285] 1.5 3.25 2.25 2.25 1.75 3.5 3.75 2.5 1.75 2.5 ...
    ##  $ happiness_wake    : num [1:285] 4.25 5.25 6.5 5.25 5.75 4.5 1.5 3 4 2.5 ...
    ##  $ happiness_general : num [1:285] 4.5 3.75 6.25 5.5 6 3.75 3.5 3.5 4.25 2.75 ...
    ##  $ exercise_intent   : num [1:285] 70.4 78.3 52.3 53.1 61.8 ...
    ##  $ empathetic_concern: num [1:285] 4.57 4.43 3.57 4.14 3.71 ...
    ##  $ perspective_taking: num [1:285] 3.71 3 3.43 3.57 3.29 ...
    ##  $ gender            : num [1:285] 2 2 2 2 1 2 1 2 1 2 ...
    ##  $ SES               : num [1:285] 5 3 5 4 5 4 5 4 4 4 ...
    ##  $ age               : num [1:285] 23 20 19 19 19 20 20 21 20 20 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   X1 = col_double(),
    ##   ..   id = col_double(),
    ##   ..   condition = col_character(),
    ##   ..   national_sss = col_double(),
    ##   ..   procedure = col_character(),
    ##   ..   manipulation_check = col_double(),
    ##   ..   Q142 = col_double(),
    ##   ..   positive_emotion = col_double(),
    ##   ..   negative_emotion = col_double(),
    ##   ..   current_health = col_double(),
    ##   ..   future_health = col_double(),
    ##   ..   happiness_wake = col_double(),
    ##   ..   happiness_general = col_double(),
    ##   ..   exercise_intent = col_double(),
    ##   ..   empathetic_concern = col_double(),
    ##   ..   perspective_taking = col_double(),
    ##   ..   Q128 = col_double(),
    ##   ..   Q130 = col_double(),
    ##   ..   Q132 = col_number(),
    ##   ..   Q134 = col_double()
    ##   .. )

``` r
#correlation matrix
tab_corr(thesis[,2:15],
         triangle = "lower",
         file = "correlation_matrix.doc")
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
 
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
national\_sss
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
exercise\_frequency
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
positive\_emotion
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
negative\_emotion
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
current\_health
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
future\_health
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
happiness\_wake
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
happiness\_general
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
exercise\_intent
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
empathetic\_concern
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
perspective\_taking
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
gender
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
SES
</th>
<th style="font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;">
age
</th>
</tr>
<tr>
<td style="font-style:italic;">
national\_sss
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
exercise\_frequency
</td>
<td style="padding:0.2cm; text-align:center;">
-0.154<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
positive\_emotion
</td>
<td style="padding:0.2cm; text-align:center;">
0.172<span style="vertical-align:super;font-size:0.8em;">\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.134<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
negative\_emotion
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.022<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.036<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.067<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
current\_health
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.009<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.246<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.280<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.281<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
future\_health
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.049<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.252<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.266<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.112<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.436<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
happiness\_wake
</td>
<td style="padding:0.2cm; text-align:center;">
0.159<span style="vertical-align:super;font-size:0.8em;">\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.149<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.547<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.340<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.429<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.319<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
happiness\_general
</td>
<td style="padding:0.2cm; text-align:center;">
0.153<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.186<span style="vertical-align:super;font-size:0.8em;">\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.460<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.301<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.376<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.359<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.845<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
exercise\_intent
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.085<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.554<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.064<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.056<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.189<span style="vertical-align:super;font-size:0.8em;">\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.218<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.123<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.126<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
empathetic\_concern
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.070<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.091<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.237<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.225<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.145<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.206<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.263<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.293<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.042<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
perspective\_taking
</td>
<td style="padding:0.2cm; text-align:center;">
-0.123<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.017<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.133<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.027<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.014<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.125<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.144<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.156<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.037<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.355<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
gender
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.008<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.061<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.000<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.145<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.109<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.070<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.020<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.044<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.153<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.233<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.065<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
SES
</td>
<td style="padding:0.2cm; text-align:center;">
0.415<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.125<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.112<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.068<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.063<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.005<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.092<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.109<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.079<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.070<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.015<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.198<span style="vertical-align:super;font-size:0.8em;">\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td style="font-style:italic;">
age
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.007<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.014<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.030<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.109<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.172<span style="vertical-align:super;font-size:0.8em;">\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.075<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.011<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.015<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
0.076<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.070<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center; color:#999999;">
-0.046<span style="vertical-align:super;font-size:0.8em;"></span>
</td>
<td style="padding:0.2cm; text-align:center;">
0.413<span style="vertical-align:super;font-size:0.8em;">\*\*\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
-0.155<span style="vertical-align:super;font-size:0.8em;">\*</span>
</td>
<td style="padding:0.2cm; text-align:center;">
 
</td>
</tr>
<tr>
<td colspan="15" style="border-bottom:double black; border-top:1px solid black; font-style:italic; font-size:0.9em; text-align:right;">
Computed correlation used pearson-method with listwise-deletion.
</td>
</tr>
</table>

### T test

``` r
#H0: mean positive emotion of high_sss = mean positive emotion of low_sss
t.test(positive_emotion~condition, var.equal=TRUE, data=thesis)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  positive_emotion by condition
    ## t = -0.47699, df = 283, p-value = 0.6337
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.2515551  0.1534193
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##        3.047518        3.096586

``` r
#negative emotion
t.test(negative_emotion~condition, var.equal=TRUE, data=thesis)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  negative_emotion by condition
    ## t = -0.19964, df = 283, p-value = 0.8419
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1349828  0.1101230
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##        1.684318        1.696748

``` r
#current_health
t.test(current_health~condition, var.equal=TRUE, data=thesis)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  current_health by condition
    ## t = 0.087184, df = 282, p-value = 0.9306
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1733301  0.1893958
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##        2.924350        2.916317

``` r
#future health
t.test(future_health~condition, var.equal=TRUE, data=thesis)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  future_health by condition
    ## t = -0.091985, df = 280, p-value = 0.9268
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1588646  0.1446802
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##        2.310284        2.317376

``` r
#happiness_wake
t.test(happiness_wake~condition, var.equal=TRUE, data=thesis)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  happiness_wake by condition
    ## t = -0.68713, df = 283, p-value = 0.4926
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.3760131  0.1814209
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##        4.663121        4.760417

``` r
#happiness_general
t.test(happiness_general~condition, var.equal=TRUE, data=thesis)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  happiness_general by condition
    ## t = -0.42467, df = 283, p-value = 0.6714
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.3232598  0.2085287
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##        4.829787        4.887153

``` r
#exercise
t.test(exercise_intent~condition, var.equal=TRUE, data=thesis)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  exercise_intent by condition
    ## t = -0.66974, df = 281, p-value = 0.5036
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -6.446386  3.173386
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##        52.62797        54.26447

``` r
t.test(exercise_frequency~condition, var.equal=TRUE, data=thesis)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  exercise_frequency by condition
    ## t = 1.4006, df = 283, p-value = 0.1624
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.06559442  0.38917598
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##        2.099291        1.937500

``` r
#empathetic_concern
t.test(empathetic_concern~condition, var.equal=TRUE, data=thesis)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  empathetic_concern by condition
    ## t = 1.4489, df = 282, p-value = 0.1485
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.03967675  0.26099194
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##        4.055102        3.944444

``` r
t.test(perspective_taking~condition, var.equal=TRUE, data=thesis)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  perspective_taking by condition
    ## t = 1.4253, df = 283, p-value = 0.1552
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.04546575  0.28409571
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##        3.616008        3.496693

### one way ANCOVA

``` r
anova_positive_emotion <- aov(positive_emotion~condition+national_sss,data = thesis)
summary(anova_positive_emotion)
```

    ##               Df Sum Sq Mean Sq F value  Pr(>F)   
    ## condition      1   0.17   0.172   0.236 0.62784   
    ## national_sss   1   7.97   7.972  10.946 0.00106 **
    ## Residuals    282 205.38   0.728                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova_negative_emotion <- aov(negative_emotion~condition+national_sss,data = thesis)
summary(anova_negative_emotion)
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## condition      1   0.01 0.01101   0.040  0.842
    ## national_sss   1   0.12 0.11869   0.429  0.513
    ## Residuals    282  78.04 0.27672

``` r
anova_current_health <- aov(current_health~condition+national_sss,data = thesis)
summary(anova_current_health)
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## condition      1   0.00  0.0046   0.008  0.931
    ## national_sss   1   0.31  0.3063   0.507  0.477
    ## Residuals    281 169.66  0.6038               
    ## 1 observation deleted due to missingness

``` r
anova_future_health <- aov(future_health~condition+national_sss,data = thesis)
summary(anova_future_health)
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## condition      1   0.00  0.0035   0.008  0.927
    ## national_sss   1   0.43  0.4341   1.036  0.310
    ## Residuals    279 116.91  0.4190               
    ## 3 observations deleted due to missingness

``` r
anova_happiness_wake <- aov(happiness_wake~condition+national_sss,data = thesis)
summary(anova_happiness_wake)
```

    ##               Df Sum Sq Mean Sq F value  Pr(>F)   
    ## condition      1    0.7   0.674   0.484 0.48707   
    ## national_sss   1   11.5  11.504   8.260 0.00436 **
    ## Residuals    282  392.7   1.393                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova_happiness_general <- aov(happiness_general~condition+national_sss,data = thesis)
summary(anova_happiness_general)
```

    ##               Df Sum Sq Mean Sq F value  Pr(>F)   
    ## condition      1    0.2   0.234   0.185 0.66741   
    ## national_sss   1   10.6  10.582   8.352 0.00415 **
    ## Residuals    282  357.3   1.267                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova_exercise_intent <- aov(exercise_intent~condition+national_sss,data = thesis)
summary(anova_exercise_intent)
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)  
    ## condition      1    189   189.5   0.451 0.5023  
    ## national_sss   1   1152  1152.4   2.745 0.0987 .
    ## Residuals    280 117536   419.8                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 2 observations deleted due to missingness

``` r
anova_perspective_taking <- aov(perspective_taking~condition+national_sss,data = thesis)
summary(anova_perspective_taking)
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)  
    ## condition      1   1.01  1.0142   2.045 0.1538  
    ## national_sss   1   1.44  1.4369   2.897 0.0898 .
    ## Residuals    282 139.85  0.4959                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova_empathetic_concern <- aov(empathetic_concern~condition+national_sss,data = thesis)
summary(anova_empathetic_concern)
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## condition      1   0.87  0.8692   2.096  0.149
    ## national_sss   1   0.20  0.2044   0.493  0.483
    ## Residuals    281 116.56  0.4148               
    ## 1 observation deleted due to missingness

### linear regression

``` r
regression_ <- lm(data=thesis, )
```
