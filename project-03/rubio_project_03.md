---
title: "Visualizing Text and Distributions"
author: "Brandon Rubio"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

# Data Visualization Project 03


In this exercise you will explore methods to visualize text data and practice how to recreate charts that show the distributions of a continuous variable. 


## Part 1: Density Plots

Using the dataset obtained from FSU's [Florida Climate Center](https://climatecenter.fsu.edu/climate-data-access-tools/downloadable-data), for a station at Tampa International Airport (TPA) from 2016 to 2017, attempt to recreate the charts shown below


```r
library(tidyverse)
weather_tpa <- read_csv("../data/tpa_weather_16_17.csv")
# random sample 
sample_n(weather_tpa, 4)
```

```
## # A tibble: 4 x 6
##    year month   day precipitation max_temp min_temp
##   <dbl> <dbl> <dbl>         <dbl>    <dbl>    <dbl>
## 1  2016    10    28             0       85       69
## 2  2016     6    24             0       90       80
## 3  2016     9    19             0       91       80
## 4  2016     3    12             0       85       64
```

See https://www.reisanar.com/slides/relationships-models#10 for a reminder on how to use this dataset with the `lubridate` package for dates and times.

(a) Recreate the plot below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_facet.png" width="80%" style="display: block; margin: auto;" />

Hint: the option `binwidth = 3` was used with the `geom_histogram()` function.

__My attempt to recreate__


```r
weather_tpa$month[weather_tpa$month == 1]<-"January"
weather_tpa$month[weather_tpa$month == 2]<-"February"
weather_tpa$month[weather_tpa$month == 3]<-"March"
weather_tpa$month[weather_tpa$month == 4]<-"April"
weather_tpa$month[weather_tpa$month == 5]<-"May"
weather_tpa$month[weather_tpa$month == 6]<-"June"
weather_tpa$month[weather_tpa$month == 7]<-"July"
weather_tpa$month[weather_tpa$month == 8]<-"August"
weather_tpa$month[weather_tpa$month == 9]<-"September"
weather_tpa$month[weather_tpa$month == 10]<-"October"
weather_tpa$month[weather_tpa$month == 11]<-"November"
weather_tpa$month[weather_tpa$month == 12]<-"December"

weather_tpa$month <-factor(weather_tpa$month, # Reordering group factor levels
levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September","October","November","December"))
```



```r
ggplot(weather_tpa, aes(x = max_temp, fill = month)) +
  geom_histogram(color = "white", size = 0.8, binwidth = 3, show.legend = FALSE) +
  scale_y_continuous(name = "Number of Days") +
  scale_x_continuous(name = "Maximum temperatures" ) +
  facet_wrap(~ month) +
  scale_fill_viridis_d() +
  theme_bw(16)
```

<img src="rubio_project_03_files/figure-html/unnamed-chunk-4-1.png" width="80%" height="80%" style="display: block; margin: auto;" />



(b) Recreate the plot below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_density.png" width="80%" style="display: block; margin: auto;" />

Hint: check the `kernel` parameter of the `geom_density()` function, and use `bw = 0.5`.


```r
ggplot(data = weather_tpa, mapping = aes(x = max_temp)) +
  geom_density(kernel = "epanechnikov", bw = 0.5, size = 1, fill = "#7f7f7f") +
  labs(x = "Maximum temperature") +
  theme_minimal(16)
```

<img src="rubio_project_03_files/figure-html/unnamed-chunk-6-1.png" width="80%" height="80%" style="display: block; margin: auto;" />



(c) Recreate the chart below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_density_facet.png" width="80%" style="display: block; margin: auto;" />

Hint: default options for `geom_density()` were used. 


```r
ggplot(weather_tpa, aes(x = max_temp, fill = month)) +
  geom_density(show.legend = FALSE, color = "black", size = 1, alpha = 0.5) +
  scale_x_continuous(name = "Maximum temperatures" ) +
  scale_y_continuous(name = "") +
  labs(title = "Density plots for each month in 2016") +
  facet_wrap(~ month) +
  scale_fill_viridis_d() +
  theme_bw(18)
```

<img src="rubio_project_03_files/figure-html/unnamed-chunk-8-1.png" width="80%" height="80%" style="display: block; margin: auto;" />


(d) Recreate the chart below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_ridges.png" width="80%" style="display: block; margin: auto;" />

Hint: default options for `geom_density()` were used. 


```r
library(ggridges)

ggplot(weather_tpa, aes(x = max_temp, y = fct_rev(month), fill = month)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2, size = 1, bandwidth = 1.49) +
  guides(fill = "none") +
  scale_fill_viridis_d() +
  labs(x = "Maximum temperature", y = "") +
  scale_y_discrete(limits=rev) +
  theme_minimal(16)
```

<img src="rubio_project_03_files/figure-html/unnamed-chunk-10-1.png" width="80%" height="80%" style="display: block; margin: auto;" />


(e) Recreate the plot below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_ridges.png" width="80%" style="display: block; margin: auto;" />

Hint: use the`ggridges` package, and the `geom_density_ridges()` function paying close attention to the `quantile_lines` and `quantiles` parameters.


```r
ggplot(weather_tpa, aes(x = max_temp, y = fct_rev(month), fill = month),quantile_lines = TRUE, quantiles = 2, size = 1, bandwidth = 1.49) +
  geom_density_ridges() +
  guides(fill = "none") +
  scale_fill_viridis_d() +
  labs(x = "Maximum temperature", y = "") +
  scale_y_discrete(limits=rev) +
  theme_minimal(16)
```

```
## Picking joint bandwidth of 1.49
```

![](rubio_project_03_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


(f) Recreate the chart below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_ridges_plasma.png" width="80%" style="display: block; margin: auto;" />

Hint: this uses the `plasma` option (color scale) for the _viridis_ palette.


```r
ggplot(weather_tpa, aes(x = max_temp, y = fct_rev(month), fill = ..x..)) +
  geom_density_ridges_gradient(quantile_lines = TRUE, quantiles = 2, size = 1, bandwidth = 1.49) +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = "Maximum temperature (in Fahrenheit degrees", y = NULL) +
  theme_minimal(16) +
  theme(legend.title = element_blank()) 
```

![](rubio_project_03_files/figure-html/unnamed-chunk-14-1.png)<!-- -->



## Part 2: Visualizing Text Data

Review the set of slides (and additional resources linked in it) for visualizing text data: https://www.reisanar.com/slides/text-viz#1

Choose any dataset with text data, and create at least one visualization with it. For example, you can create a frequency count of most used bigrams, a sentiment analysis of the text data, a network visualization of terms commonly used together, and/or a visualization of a topic modeling approach to the problem of identifying words/documents associated to different topics in the text data you decide to use. 

Make sure to include a copy of the dataset in the `data/` folder, and reference your sources if different from the ones listed below:

- [Billboard Top 100 Lyrics](https://github.com/reisanar/datasets/blob/master/BB_top100_2015.csv)

- [RateMyProfessors comments](https://github.com/reisanar/datasets/blob/master/rmp_wit_comments.csv)

- [FL Poly News 2020](https://github.com/reisanar/datasets/blob/master/poly_news_FL20.csv)

- [FL Poly News 2019](https://github.com/reisanar/datasets/blob/master/poly_news_FL19.csv)

(to get the "raw" data from any of the links listed above, simply click on the `raw` button of the GitHub page and copy the URL to be able to read it in your computer using the `read_csv()` function)