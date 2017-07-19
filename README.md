Regressing Sales Price on (Tax) Assessed Market Values
================

NYC makes the following data sets available for public use: -[PLUTO](https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page) contains tax-lot information, including the city's assessed market value of each property (for taxation purposes) -[Rolling Sales Data](http://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page) contains property transactions and sale prices

Marrying these two sources, we can quickly determine if the `Market Value` of a property (i.e., the Assessment Value) correlates at all with the `Sale Price`. What we find is that certain property classes are more homogeneous than others. For example, Office buildings have a stronger correlation than Hotels. Overall, however, the market price rarely seems to track the sale price.

``` r
library(tidyverse)
library(modelr)
library(purrr)

knitr::opts_chunk$set(
    echo = TRUE,
    message = FALSE,
    warning = FALSE,
    fig.width = 10,
    fig.height = 8
)
```

For this modeling exercise, the data has already been downloaded and joined:

``` r
sale_augmented <- read_rds("data/sales_augmented.rds")
```

A Case Example
==============

We will look at a specific case, 31 West 27th Street, to guide the discussion. This office building appears in the sales data 4 times in 10 years.

A couple convenience functions for quickly glimpsing our example property:

``` r
lookat <- function(boro= 1,blck = 829,lt = 16, data = sale_augmented) {
  data %>% 
    filter(BOROUGH == boro, BLOCK == blck, LOT == lt) %>% 
    arrange(desc(SALE_DATE)) %>% 
    glimpse()
  }

lookhead <- function(boro= 1,blck = 829,lt = 16, data = sale_augmented) {
  data %>% 
    filter(BOROUGH == boro, BLOCK == blck, LOT == lt)
}

lookat()
```

    ## Observations: 4
    ## Variables: 64
    ## $ BOROUGH                        <int> 1, 1, 1, 1
    ## $ NEIGHBORHOOD                   <chr> "FLATIRON", "FLATIRON", "FLATIR...
    ## $ BUILDING.CLASS.CATEGORY        <chr> "23  LOFT BUILDINGS", "23  LOFT...
    ## $ TAX.CLASS.AT.PRESENT           <chr> "4", "4", "4", "4"
    ## $ BLOCK                          <chr> "829", "829", "829", "829"
    ## $ LOT                            <chr> "16", "16", "16", "16"
    ## $ EASE.MENT                      <chr> NA, NA, NA, NA
    ## $ BUILDING.CLASS.AT.PRESENT      <chr> "L1", "L1", "L1", "L1"
    ## $ ADDRESS                        <chr> "31 WEST 27TH STREET", "31 WEST...
    ## $ APARTMENT.NUMBER               <chr> "", "", "", ""
    ## $ ZIP.CODE                       <chr> "10001", "10001", "10001", "10001"
    ## $ RESIDENTIAL.UNITS              <dbl> 0, 0, 0, 0
    ## $ COMMERCIAL.UNITS               <dbl> 17, 17, 17, 17
    ## $ TOTAL.UNITS                    <dbl> 17, 17, 17, 17
    ## $ LAND.SQUARE.FEET               <dbl> 9876, 9876, 9876, 9876
    ## $ GROSS.SQUARE.FEET              <dbl> 106800, 106800, 106800, 106800
    ## $ YEAR.BUILT                     <dbl> 1910, 1910, 1910, 1910
    ## $ TAX.CLASS.AT.TIME.OF.SALE      <chr> "4", "4", "4", "4"
    ## $ BUILDING.CLASS.AT.TIME.OF.SALE <chr> "L1", "L1", "L1", "L1"
    ## $ SALE.PRICE                     <dbl> 80775000, 65000000, 45700000, 3...
    ## $ SALE_DATE                      <date> 2014-03-28, 2012-07-16, 2009-1...
    ## $ SALE_YEAR                      <dbl> 2014, 2012, 2009, 2006
    ## $ Address                        <chr> "31 WEST 27 STREET", "31 WEST 2...
    ## $ lat                            <dbl> 40.77033, 40.36161, 40.36162, NA
    ## $ lon                            <dbl> -76.82009, -77.29831, -77.29831...
    ## $ ZipCode                        <chr> "10001", "10001", "10001", NA
    ## $ BldgClass                      <chr> "L1", "L1", "L1", NA
    ## $ Building_Type                  <chr> "L", "L", "L", NA
    ## $ BBL_derive                     <chr> "1008290016", "1008290016", "10...
    ## $ OwnerType                      <chr> NA, "", "", NA
    ## $ OwnerName                      <chr> "31 W27ST 9 OWNER, LLC", "31 W ...
    ## $ LotArea                        <dbl> 9876, 9876, 9876, NA
    ## $ BldgArea                       <dbl> 106800, 106800, 108594, NA
    ## $ ComArea                        <dbl> 106800, 106800, 100000, NA
    ## $ ResArea                        <dbl> 0, 0, 8594, NA
    ## $ NumBldgs                       <dbl> 1, 1, 1, NA
    ## $ NumFloors                      <dbl> 12, 12, 12, NA
    ## $ UnitsRes                       <dbl> 0, 0, 0, NA
    ## $ UnitsTotal                     <dbl> 17, 17, 22, NA
    ## $ LotFront                       <dbl> 100, 100, 100, NA
    ## $ LotDepth                       <dbl> 98.75, 98.75, 98.75, NA
    ## $ BldgFront                      <dbl> 100, 100, 100, NA
    ## $ BldgDepth                      <dbl> 89, 89, 89, NA
    ## $ ProxCode                       <dbl> 3, 3, 3, NA
    ## $ IrrLotCode                     <chr> "N", "N", "N", NA
    ## $ CornerLot                      <chr> NA, NA, NA, NA
    ## $ AssessLand                     <dbl> 2700000, 2700000, 2700000, NA
    ## $ AssessTotal                    <dbl> 12874950, 12886650, 5220000, NA
    ## $ ExemptLand                     <dbl> 0, 0, 0, NA
    ## $ ExemptTotal                    <dbl> 0, 0, 0, NA
    ## $ YearBuilt                      <dbl> 1910, 1910, 1910, NA
    ## $ YearAlter1                     <dbl> 1987, 1987, 1987, NA
    ## $ YearAlter2                     <dbl> 0, 0, 0, NA
    ## $ OfficeArea                     <dbl> 96800, 0, 0, NA
    ## $ RetailArea                     <dbl> 10000, 0, 0, NA
    ## $ GarageArea                     <dbl> 0, 0, 0, NA
    ## $ StrgeArea                      <dbl> 0, 106800, 100000, NA
    ## $ FactryArea                     <dbl> 0, 0, 0, NA
    ## $ OtherArea                      <dbl> 0, 0, 0, NA
    ## $ LotType                        <dbl> 5, 5, 5, NA
    ## $ BsmtCode                       <dbl> 2, 2, 5, NA
    ## $ BuiltFAR                       <dbl> 10.81, 10.81, 11.00, NA
    ## $ BBL                            <chr> "1008290016", "1008290016", "10...
    ## $ CondoNo                        <dbl> 0, 0, 0, NA

From 2006 to 2014, `SALE.PRICE` increases from `$31,500,000` to `$80,775,000`, a 265% increase. In roughly the same period, `AssessTotal` went from `$5,220,000` (in 2009) to `$12,874,950`, a similar increase.

Let's look at price as a multiple of the Assessed Value:

``` r
sale_augmented %>% filter(BOROUGH == 1, BLOCK == 829, LOT == 16) %>% 
  select(ADDRESS,SALE_DATE, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal)
```

    ## # A tibble: 4 x 5
    ##                 ADDRESS  SALE_DATE SALE.PRICE AssessTotal
    ##                   <chr>     <date>      <dbl>       <dbl>
    ## 1     31 WEST 27 STREET 2006-06-08   31500000          NA
    ## 2 31 WEST 27TH   STREET 2009-10-22   45700000     5220000
    ## 3 31 WEST 27TH   STREET 2012-07-16   65000000    12886650
    ## 4   31 WEST 27TH STREET 2014-03-28   80775000    12874950
    ## # ... with 1 more variables: SalePriceToAssesstmentRatio <dbl>

Is this a consistent pattern across all the sales data?

``` r
sale_augmented %>% 
  select(ADDRESS,SALE_DATE, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal) %>% 
  filter(!is.na(SalePriceToAssesstmentRatio), !is.infinite(SalePriceToAssesstmentRatio)) %>% 
  mutate(Ratio_bin = ntile(SalePriceToAssesstmentRatio,5)) %>% 
  group_by(Ratio_bin) %>% 
  summarise(average_ratio = mean(SalePriceToAssesstmentRatio)) %>% 
  ggplot()+
  aes(x = Ratio_bin, y = average_ratio)+
  geom_col()+
  theme_minimal()+
  labs(title = "Sale Price To Assesstment Ratio")
```

![](README_files/figure-markdown_github/histogram%20of%20ratios-1.png)

In many instances, the ratio is wildly off. This begs the question: which types of buildings are more homogeneous?

``` r
sale_augmented %>% 
  select(ADDRESS,SALE_DATE,Building_Type, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal) %>% 
  filter(!is.na(SalePriceToAssesstmentRatio)) %>% 
  filter(AssessTotal<500000000) %>% 
  #filter(SALE.PRICE>5e+07) %>% 
  ggplot()+
  aes(x = SALE.PRICE, y=AssessTotal, group = Building_Type, color = Building_Type)+
  geom_point()+
  geom_smooth(se=F, method = "lm")+
  scale_y_continuous(labels=scales::comma)+
  theme_minimal()
```

![Correlations by group](README_files/figure-markdown_github/correlations%20by%20group.png)

The building designations can be looked up in the [PLUTO Data Dictionary Appendix C](https://www1.nyc.gov/assets/planning/download/pdf/data-maps/open-data/pluto_datadictionary.pdf?r=16v2)

A few of the building classes stand out. For example, "O" stands for 'Office'. In addition, "H" Hotels and "I" Hospitals seem to correlate more than other classes.

Modleing
========

We will fit a simple linear model to each building class using the nested `modelr` approach.

``` r
library(modelr)

f1 <- lm(AssessTotal~SALE.PRICE, data = sale_augmented)
summary(f1)
```

    ## 
    ## Call:
    ## lm(formula = AssessTotal ~ SALE.PRICE, data = sale_augmented)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -158958485   -2569836   -2512504   -1870750 2621421738 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2.533e+06  1.431e+04   177.0   <2e-16 ***
    ## SALE.PRICE  1.335e-01  1.034e-03   129.2   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10590000 on 550095 degrees of freedom
    ##   (390644 observations deleted due to missingness)
    ## Multiple R-squared:  0.02944,    Adjusted R-squared:  0.02944 
    ## F-statistic: 1.669e+04 on 1 and 550095 DF,  p-value: < 2.2e-16

Clearly the overall model does not fair well with an Adj R Squared sub 0.2, but what about individual classes?

``` r
# helpfer functions for modeling with groups:
group_model <- function(df) {
  lm(AssessTotal ~ SALE.PRICE, data = df)
}

extract_coef <- function(model) coef(model)["SALE.PRICE"]
```

First we nest the data frames by category:

``` r
by_group<-
  sale_augmented %>% 
  select(ADDRESS,SALE_DATE,Building_Type, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal) %>% 
  filter(!is.na(SalePriceToAssesstmentRatio)) %>% 
  group_by(Building_Type) %>% 
  nest()

by_group
```

    ## # A tibble: 27 x 2
    ##    Building_Type                   data
    ##            <chr>                 <list>
    ##  1             B <tibble [126,091 x 5]>
    ##  2             C  <tibble [93,813 x 5]>
    ##  3             D <tibble [114,460 x 5]>
    ##  4             S  <tibble [17,402 x 5]>
    ##  5             K   <tibble [9,499 x 5]>
    ##  6             L     <tibble [819 x 5]>
    ##  7             V  <tibble [14,529 x 5]>
    ##  8             E   <tibble [3,327 x 5]>
    ##  9             I     <tibble [520 x 5]>
    ## 10             M     <tibble [921 x 5]>
    ## # ... with 17 more rows

Next, using `purrr::map` and `broom::glance` we can apply various functions over the nested data frames in order to fit models and extract summary statistics:

``` r
by_group %>% 
  mutate(Number_of_Sales = map_dbl(data,nrow)) %>% 
  mutate(model = map(data, group_model)) %>% 
  mutate(Sale_Coef = map_dbl(model,extract_coef)) %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance) %>% 
  arrange(-adj.r.squared) %>% 
  filter(p.value <= (0.05)) %>% 
  glimpse()
```

    ## Observations: 17
    ## Variables: 16
    ## $ Building_Type   <chr> "L", "O", "N", "H", "F", "E", "B", "S", "G", "...
    ## $ data            <list> [<# A tibble: 819 x 5,                       ...
    ## $ Number_of_Sales <dbl> 819, 3503, 132, 8726, 2636, 3327, 126091, 1740...
    ## $ model           <list> [<1.594152e+06, 1.320010e-01, -1372752.29, 52...
    ## $ Sale_Coef       <dbl> 0.13200104, 0.13088686, 0.11407901, 0.11723693...
    ## $ r.squared       <dbl> 0.354059930, 0.332790155, 0.289172620, 0.20407...
    ## $ adj.r.squared   <dbl> 0.353269305, 0.332599578, 0.283704718, 0.20398...
    ## $ sigma           <dbl> 7875291.30, 22896266.51, 2403543.44, 5902366.8...
    ## $ statistic       <dbl> 447.823220, 1746.224730, 52.885471, 2236.84038...
    ## $ p.value         <dbl> 1.360568e-79, 5.494034e-310, 2.954454e-11, 0.0...
    ## $ df              <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
    ## $ logLik          <dbl> -14166.208, -64333.076, -2125.696, -148426.537...
    ## $ AIC             <dbl> 28338.415, 128672.153, 4257.392, 296859.073, 8...
    ## $ BIC             <dbl> 28352.539, 128690.637, 4266.041, 296880.295, 8...
    ## $ deviance        <dbl> 5.067051e+16, 1.835361e+18, 7.510127e+14, 3.03...
    ## $ df.residual     <int> 817, 3501, 130, 8724, 2634, 3325, 126089, 1740...

The top building classes are:

| Class | Name                     |
|:------|:-------------------------|
| L     | Lofts (a type of office) |
| O     | Office                   |
| N     | Asylums                  |
| H     | Hotel                    |
| F     | Factory/Industrial       |

The very highest adjusted R-Squared achieved with a linear model is 0.35 across the "Lofts Office" group, followed closely by general "Office". The coefficients for these classes are quite low: `0.13`. With more advanced modeling and residual analysis, these models could be improved dramatically, especially for the office classes.
