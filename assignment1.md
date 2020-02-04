Statistical assignment 1
================
Jenson Wong 670034697
2/2/2020

Open data (10 points)
---------------------

In this assignment you will work with the individual level data from wave 8 of the Understanding Society survey. First, you need to open the data set. Please complete the code below.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.3
    ## ✓ tidyr   1.0.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
d <- read_tsv("~/Documents/University/Year 3/Data 3/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

Now you have got your data frame stored as Data.

Select variables (10 points)
----------------------------

The data for Wave 8 of the Understanding Society were collected in 2016-18. Among other things, people were asked the following question: "Should the United Kingdom remain a member of the European Union or leave the European Union?" In this assignment, we will explore how answers to this question depend on sex and age.

First, you need to select the variables for the analysis. You want to keep the following variables: cross-wave individual identifier (*pidp*), support for the UK remaining or leaving the EU (*h\_eumem*), sex (*h\_sex\_dv*), age (*h\_age\_dv*), and sample origin (*h\_memorig*).

Complete the code below to select those variables from the data frame and save the result.

``` r
d <- d %>%
        select(pidp, h_eumem, h_sex_dv, h_age_dv, h_memorig)
```

``` r
## Checking

colnames(d)
```

    ## [1] "pidp"      "h_eumem"   "h_sex_dv"  "h_age_dv"  "h_memorig"

Filter observations (10 points)
-------------------------------

To make nationally representative estimates from the Understanding Society data we would need to use weight coefficients. There are many different types of weight coefficients that can be used depending on the question and the level of analysis (see the User Guide, pp. 65-71). We will not do this in this assignment. However, what we want to do is to keep data from the original Understanding Society sample only (ukhls gb 2009-10), dropping data for Northern Ireland, the BHPS cohort members and ethnic minority boost samples. This will make data closer to be representative for Great Britain. You need to choose the observations where *h\_memorig* has the value of 1.

``` r
d <- d %>%
        filter(!h_memorig == "1")
```

``` r
## Checking

d[d$h_memorig == 1,]
```

    ## # A tibble: 0 x 5
    ## # … with 5 variables: pidp <dbl>, h_eumem <dbl>, h_sex_dv <dbl>,
    ## #   h_age_dv <dbl>, h_memorig <dbl>

Recode data (20 points)
-----------------------

Let us tabulate the variables for EU support, sex, and age.

``` r
table(d$h_eumem)
```

    ## 
    ##   -9   -8   -7   -2   -1    1    2 
    ##   22  892  804  197  602 8932 4887

``` r
table(d$h_sex_dv)
```

    ## 
    ##    0    1    2 
    ##    1 7452 8883

``` r
table(d$h_age_dv)
```

    ## 
    ##  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35 
    ## 270 316 294 275 258 247 288 233 265 237 218 241 231 235 226 221 273 249 243 260 
    ##  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55 
    ## 334 322 301 309 285 291 306 273 303 315 335 288 328 311 322 325 279 260 279 282 
    ##  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
    ## 277 262 234 220 221 218 211 235 184 193 171 213 192 209 190 183 161 141 151 152 
    ##  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95 
    ## 142 119 105 109  94  93  75  78  60  68  47  43  40  24  22  16  12  12   6   9 
    ##  96  97  98  99 100 101 102 
    ##   4   3   2   1   3   2   1

You will see that all these variables are numeric. You can learn what the numeric codes mean by checking the codebook here: <https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/indresp/wave/8> .

We want to do the following:

1.  Recode the variable for EU support as binary (1 for Remain, 0 for Leave), coding all types of missing values (including refusals and "don't know") as NA.
2.  Recode sex into a character vector with the values "male" or "female".
3.  Recode age into a variable with the following categories: 16 to 25, 26 to 40, 41 to 55, 56 to 70, over 70.

In all cases, we want to create new variables.

``` r
d <- d %>%
        filter(!h_eumem == "-9", !h_eumem == "-8", !h_eumem == "-7", !h_eumem == "-2", !h_eumem == "-1") %>% 
        mutate(EU = ifelse(h_eumem == 1, 1, 0 )
        ) %>%
        mutate(sex = ifelse(h_sex_dv == 1, "Male", "Female")) %>%
        mutate(agegr = cut(h_age_dv, breaks=c(15, 25, 40, 55, 70, Inf), labels=c("16-25","26-40","41-55", "56-70", "over 70"))
        )
```

``` r
## Checking

d1 <- d %>% 
        select(h_eumem, EU, h_sex_dv, sex, h_age_dv, agegr)
```

Summarise data (20 points)
--------------------------

Let us **dplyr** to calculate how many people in the sample supported Remain and Leave, both as absolute numbers and percentages.

``` r
d %>%
  group_by(EU) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))
```

    ## # A tibble: 2 x 3
    ##      EU     n  freq
    ##   <dbl> <int> <dbl>
    ## 1     0  4887 0.354
    ## 2     1  8932 0.646

Write a couple of sentences with the interpretation of this result. How this compares with the result of the 2016 referendum? Why?

There were a greater number of people who supported remain than before, as the referendum result was for Leave. This may be due to the fact that the process of Brexit led to many having little faith or confidence in the future of the UK post Brexit both economically and politically.

Summarise data by sex and age (30 points)
-----------------------------------------

Now let us look at the support for Leave and Remain by sex and age. Use your newly created variables.

``` r
d %>%
  group_by(sex, EU) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))  
```

    ## # A tibble: 4 x 4
    ## # Groups:   sex [2]
    ##   sex       EU     n  freq
    ##   <chr>  <dbl> <int> <dbl>
    ## 1 Female     0  2507 0.331
    ## 2 Female     1  5073 0.669
    ## 3 Male       0  2380 0.381
    ## 4 Male       1  3859 0.619

Write a couple of sentences interpreting your results.

Females were more likely to vote Remain than Males, and also as age increased, the likelihood that one voted Remain decreased.
