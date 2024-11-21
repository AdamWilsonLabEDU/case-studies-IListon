# Mauna Loa CO2 Data


Load packages

``` r
library(readr)
library(ggplot2)
library(tidyverse)
library(knitr)
```

Load data from website

``` r
CO2_data_MaunaLoa <- readr::read_table("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.txt", skip = 45, col_names= c("year", "mean", "unc"))
```

Plot CO2 levels over time

``` r
ggplot(CO2_data_MaunaLoa, aes(year, mean)) +
        geom_line(linewidth = 1) +
        labs(x = "Year", y = "Mean CO^2 (in ppm)", 
        title = "CO^2 Concentrations Over Time",
        subtitle = "Measurements taken at Mauna Loa", 
        caption = "Data Source: NOAA Global Monitoring Laboratory")
```

![](case_study_08_files/figure-commonmark/unnamed-chunk-3-1.png)

Table of data

``` r
table <- CO2_data_MaunaLoa %>%
          arrange(desc(mean)) %>%
          slice_head(n=5)

knitr::kable(head(table))
```

| year |   mean |  unc |
|-----:|-------:|-----:|
| 2023 | 421.08 | 0.12 |
| 2022 | 418.53 | 0.12 |
| 2021 | 416.41 | 0.12 |
| 2020 | 414.21 | 0.12 |
| 2019 | 411.65 | 0.12 |
