---
title: "Forex Pairs Trading - test ride"
author: "Konrad Birycki"
date: "19 listopada 2018"
output: 
  html_document:
    keep_md: yes
---


```r
library(PerformanceAnalytics)
```

```
## Loading required package: xts
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```
## 
## Attaching package: 'PerformanceAnalytics'
```

```
## The following object is masked from 'package:graphics':
## 
##     legend
```

```r
library(tseries)
library(XML)


setwd('C://Users/Kasia/Documents/ProjektyR/Trading')

download_files <- F

# 1 data handling ---------------------------------------------------------------

#TODO: deduplicate each series
#read.table('http://bossafx.pl/pub/metastock/forex/sesjafx/sesjafx.prn')

if(download_files) {
  temp <- tempfile()
  download.file("http://bossafx.pl/pub/metastock/forex/mstfx.zip",temp)
  mstfx <- lapply(unzip(temp, list = T)$Name,
                  function(x) read.csv(unzip(temp, x), stringsAsFactors = F))
  mstfx <- setNames(mstfx, gsub('mst$', '', unzip(temp, list = T)$Name))
  unlink(temp)
  
  lapply(mstfx, function(x) colnames(x) <<- gsub('\\.|^X', '', names(x)))
  
  data <- setNames(lapply(mstfx,
                          function(x) xts(x[,-1:-2],
                                          order.by = as.Date(as.character(x[,2]),
                                                             '%Y%m%d'))), names(mstfx))
  
  paired_names <- lapply(unique(gsub('\\.', '', names(data))),
                         function(x) grep(x, names(data), value = T))
  
  data <- setNames(lapply(paired_names, function(x) rbind.xts(data[[x[1]]], data[[x[2]]])),
                   unique(gsub('\\.', '', names(data))))
  rm(paired_names)
  
  data <- lapply(data, setNames, nm = c('Open', 'High', 'Low', 'Close', 'Vol'))
  saveRDS(data, './Data/mstfx.rds')
}

download_files <- FALSE


# 2 Instrument specs --------------------------------------------------------------

tbls_xml <- readHTMLTable('http://bossafx.pl/fx/oferta/instrumenty/specyfikacja/')


# 3 Model development ---------------------------------------------------------------

data <- readRDS('./Data/mstfx.rds')
data_tmp <- setNames(do.call('cbind.xts', lapply(data, function(x) x[,'Close'])),
                     names(data))
data_tmp <- data_tmp[ ! duplicated( index(data_tmp), fromLast = TRUE ),  ] 
  
m1 <- lm( USDRUB ~ FOIL, data_tmp)
summary(m1)
```

```
## 
## Call:
## lm(formula = USDRUB ~ FOIL, data = data_tmp)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.9472  -3.9510  -0.3258   2.4231  18.8416 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 91.221436   0.443053  205.89   <2e-16 ***
## FOIL        -0.578678   0.006184  -93.58   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.641 on 1588 degrees of freedom
##   (12666 observations deleted due to missingness)
## Multiple R-squared:  0.8465,	Adjusted R-squared:  0.8464 
## F-statistic:  8757 on 1 and 1588 DF,  p-value: < 2.2e-16
```

```r
plot(xts(m1$residuals, as.Date(names(m1$residuals))), type = 'l')
```

![](ForexPairsTrading_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
(tmp <- tseries::adf.test(m1$residuals))
```

```
## 
## 	Augmented Dickey-Fuller Test
## 
## data:  m1$residuals
## Dickey-Fuller = -2.8888, Lag order = 11, p-value = 0.2021
## alternative hypothesis: stationary
```

```r
m2 <- lm( 1/USDRUB~FOIL, data_tmp)
summary(m2)
```

```
## 
## Call:
## lm(formula = 1/USDRUB ~ FOIL, data = data_tmp)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -0.0081384 -0.0014562  0.0002188  0.0014854  0.0066181 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.653e-03  2.171e-04   12.22   <2e-16 ***
## FOIL        2.719e-04  3.030e-06   89.73   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.002764 on 1588 degrees of freedom
##   (12666 observations deleted due to missingness)
## Multiple R-squared:  0.8352,	Adjusted R-squared:  0.8351 
## F-statistic:  8051 on 1 and 1588 DF,  p-value: < 2.2e-16
```

```r
plot(xts(m2$residuals, as.Date(names(m2$residuals))), type = 'l')
```

![](ForexPairsTrading_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
(tmp <- tseries::adf.test(m2$residuals))
```

```
## 
## 	Augmented Dickey-Fuller Test
## 
## data:  m2$residuals
## Dickey-Fuller = -2.7648, Lag order = 11, p-value = 0.2546
## alternative hypothesis: stationary
```

```r
# WE PICK M1

# 4 combinations ---------------------------------------------------------------

formulas <- lapply(combn(names(data_tmp), 2, simplify = F),
                   function(x) as.formula(paste0(x[1],'~', x[2])))

#tmp <- strsplit(as.character(formulas),' ~ ')
#lapply(tmp, function(x) x<<-ifelse(grepl('USD|EUR|GBP|JPY|CHF', x[1]) & grepl('USD|EUR|GBP|JPY|CHF', x[2]), c('a', 'b'), x))

models <- lapply(formulas, function(x) lm(x, data_tmp))
names(models) <- as.character(formulas)
results <- lapply(models,
                  function(x)
                    data.frame(r.squared = summary(x)$r.squared,
                               N = x$df.residual,
                               adf.parameter = adf.test(x$residuals)$parameter,
                               adf.p.value = adf.test(x$residuals)$p.value,
                               scaled.last.resid = last(x$residuals)/summary(x)$sigma,
                               coef0 <- x$coef[1], coef1 <- x$coef[2])
                  
                  )
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value

## Warning in adf.test(x$residuals): p-value smaller than printed p-value
```

```
## Warning in adf.test(x$residuals): p-value greater than printed p-value

## Warning in adf.test(x$residuals): p-value greater than printed p-value
```

```r
results <- do.call('rbind', results)
results_selected <- subset(results, r.squared > .75 & N > 800 & adf.p.value < 0.25
                           & (scaled.last.resid > 1.96 |  scaled.last.resid < -1.96))
models_selected <- models[names(models) %in% row.names(results_selected)]

lapply(models_selected,
       function(x) plot(xts(x$residuals, as.Date(names(x$residuals))),
                        main = formula(x$terms)))
```

```
## $`FCOPPER ~ PLATINUM`
```

![](ForexPairsTrading_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```
## 
## $`FEU50 ~ FFR40`
```

![](ForexPairsTrading_files/figure-html/unnamed-chunk-1-4.png)<!-- -->

```
## 
## $`FSCHATZ2 ~ FTNOTE10`
```

![](ForexPairsTrading_files/figure-html/unnamed-chunk-1-5.png)<!-- -->
