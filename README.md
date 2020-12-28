
<!-- README.md is generated from README.Rmd. Please edit that file -->

# M19

<!-- badges: start -->

<!-- badges: end -->

Use this package to generate cut-offs using the maximal rank statistic
method.

## Installation

First install and load the ‘devtools’ package;

``` r
install.packages("devtools")
library(devtools)
```

Next, you need to install the ‘M19’ package;

``` r
install_github("HHayman/M19")
```

## Preparing Data

You need to carry out any exclusions etc in your data set - include the
minimum amount of data possible. Your final data should include the
following columns (example); an identifier (TMA ID), survival time(s)
(CSS\_Time, PFS\_Time), corresponding survival status (CSS\_status,
PFS\_status), your variables (CD8\_Tumour, CD8\_Healthy).

CSS/OS should be coded as 0 = alive, 1 = CD, 2 = NCD. The function will
split it into CSS and OS for you. PFS should be coded as 0 = disease
free, 1 = recurred.

First, you need to read your data file. It should be a CSV file with no
spaces in the column names, use ’\_’. To do this, use this code;

``` r
Data <- read.csv(file.choose(), fileEncoding = 'UTF-8-BOM')
```

## Prroduce Cut-Offs

Check your data file in R and if you’re happy with it, edit and use the
code below to call the function;

All variable names must match exactly.

For CSS, OS and PFS, Use ‘Yes’ if you want that cut-off to be generated,
and ‘No’ if you do not. It is case sensitive.

For ‘Survival’, ‘SurvivalStatus’, ‘Progression’ and ‘ProgressionStatus’,
replace the text within the quotation marks with your variable names. If
you are not using PFS, delete the ‘Progression’ and ‘ProgressionStatus’
arguments, and likewise for CSS/OS if you want neither, but keep it if
you want one or the other.

Replace ‘Variable1’ etc with your variables. There is no limit so just
add/remove arguments as appropriate.

``` r
rCut(Data, CSS = "Yes", OS = "Yes", PFS = "Yes", ID = "ID", Survival = "Survival", SurvivalStatus = "Survival_Status", Progression = "Progression", ProgressionStatus = "ProgressionStatus", Variables = c("Variable1", "Variable2", "Variable3", "Variable4"))
```
