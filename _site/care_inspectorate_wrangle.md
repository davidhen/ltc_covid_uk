Introduction
============

There are issues with uploading raw data to RStudio.cloud. As a work
around I am going to create a simplified data object to create the care
home size boxplot. This is a hacky workaround.

**Note, this RMD won’t knit in RStudio cloud or on any machine other
than David Henderson’s. The raw data files are too big for Rstudio.cloud
to handle so I ran this code locally and saved smaller data objects that
could be imported into the `ltc_covid_uk.Rmd` file from the
`derived_data` folder. As I said - pretty hacky**

Packages
--------

R packages required…..

``` r
library(tidyverse)
library(curl)
library(readxl)
library(forcats)
library(ggthemes)
library(here)
#Helper function
`%nin%` <- negate(`%in%`)

#Short cut for csv output with html tables
my_datatable <- function(x){
  DT::datatable(x, extensions = "Buttons", options = list(dom = "Bfrtip", 
                                                          buttons = c("csv")))
}
```

Care Home size
==============

``` r
eng <- read_xlsx("raw_data/ENGLAND_care_homes.xlsx", sheet = "HSCA Active Locations")
scot <- read_xlsx("raw_data/SCOTLAND_care_homes.xlsx", sheet = "MDSF_data_31 May 2020")
ni <- read_xlsx("raw_data/NI_care_homes.xlsx")
w <- read_xlsx("raw_data/22 - CIW - 20200730 - list of adult care homes in Wales.xlsx", 
               range = "A4:K1060")
```

``` r
eng %<>% 
  filter(`Care home?` == "Y" & 
           `Location Inspection Directorate` == "Adult social care") %>% 
  select(`Location Name`, `Care homes beds`, `Service user band - Older People`) %>%
  #filter(!is.na(`Service user band - Older People`))
  mutate(country = "England")
eng
```

    ## # A tibble: 15,481 x 4
    ##    `Location Name`             `Care homes beds` `Service user band - O… country
    ##    <chr>                                   <dbl> <chr>                   <chr>  
    ##  1 Kingswood House Nursing Ho…                22 <NA>                    England
    ##  2 Little Haven                               15 <NA>                    England
    ##  3 Highlands Borders Care Home                28 Y                       England
    ##  4 Belmont Grange Nursing and…                30 Y                       England
    ##  5 Mayfield Adult Services                     4 <NA>                    England
    ##  6 Meadow Rose Nursing Home                   56 Y                       England
    ##  7 The Spinney Nursing Home                   35 Y                       England
    ##  8 Ashdale Care Home                          22 Y                       England
    ##  9 The Hall                                   10 Y                       England
    ## 10 Ashlea House                                4 <NA>                    England
    ## # … with 15,471 more rows

``` r
scot %<>% 
  filter(CareService == "Care Home Service" & 
           Subtype %nin% c("Children & Young People" ) &
           ServiceStatus == "Active") %>% 
  select(`Location Name` = ServiceName, 
         `Care homes beds` = TotalBeds, Subtype) %>% 
  mutate(country = "Scotland")
unique(scot$Subtype)
```

    ## [1] "Older People"                    "Mental Health Problems"         
    ## [3] "Alcohol & Drug Misuse"           "Learning Disabilities"          
    ## [5] "Physical and Sensory Impairment" "Respite Care and Short Breaks"  
    ## [7] "Blood Borne Virus"

``` r
ni %<>% 
  filter(Category %in% c("Nursing (NH)", "Residential (RC)")) %>%
  #filter(str_detect(`Categories of Care`, "I") | 
           #str_detect(`Categories of Care`, "E") |
           #str_detect(`Categories of Care`, "DE") |
          # str_detect(`Categories of Care`, "MP(E)") |
          # str_detect(`Categories of Care`, "LD(E)") |
          # str_detect(`Categories of Care`, "PH(E)")) %>% 
  select(`Location Name` = ServiceName,
         `Care homes beds` = `Max Approved Places`,
         `Categories of Care`) %>% 
  mutate(country = "Northern Ireland") 
ni
```

    ## # A tibble: 481 x 4
    ##    `Location Name`     `Care homes beds` `Categories of Care`         country   
    ##    <chr>                           <dbl> <chr>                        <chr>     
    ##  1 47 Somerton Road                   40 NH-LD                        Northern …
    ##  2 Abbey View                         25 NH-I, NH-PH, NH-PH(E), NH-TI Northern …
    ##  3 Abbeylands                         38 NH-I, NH-PH, NH-PH(E)        Northern …
    ##  4 Abingdon Manor Car…                60 NH-I, NH-TI, NH-PH, NH-PH(E… Northern …
    ##  5 Ailsa Lodge                        41 NH-I, NH-PH, NH-PH(E), NH-TI Northern …
    ##  6 Ambassador                         48 NH-A, NH-I, NH-PH, NH-PH(E)… Northern …
    ##  7 Annadale                           38 NH-I, NH-PH, NH-PH(E), NH-TI Northern …
    ##  8 Antrim Care Home                   51 NH-I, NH-PH, NH-PH(E), NH-T… Northern …
    ##  9 Apple Blossom Lodge                37 NH-DE, NH-MP, NH-MP(E)       Northern …
    ## 10 Apple Mews                         30 NH-LD, NH-LD(E)              Northern …
    ## # … with 471 more rows

``` r
w %<>% 
  select(`Location Name` = `Service Name`,
         `Care homes beds` = `Maximum Capacity`,
         `Service Sub Type`) %>% 
  mutate(country = "Wales")
w  
```

    ## # A tibble: 1,056 x 4
    ##    `Location Name`                 `Care homes bed… `Service Sub Type`   country
    ##    <chr>                                      <dbl> <chr>                <chr>  
    ##  1 Abbey Lodge                                    4 Adults Without Nurs… Wales  
    ##  2 Beechlea                                       4 Adults Without Nurs… Wales  
    ##  3 The Beeches                                    9 Adults Without Nurs… Wales  
    ##  4 The Fields Care Home                          34 Adults With Nursing  Wales  
    ##  5 Beacon Lodge                                   6 Adults Without Nurs… Wales  
    ##  6 Maes-y-felin Care Home LTD                    19 Adults Without Nurs… Wales  
    ##  7 walker road                                    3 Adults Without Nurs… Wales  
    ##  8 rachel kathryn residential home                4 Adults Without Nurs… Wales  
    ##  9 College Fields Nursing Home                   68 Adults With Nursing  Wales  
    ## 10 Belvedere House Residential Ca…               19 Adults Without Nurs… Wales  
    ## # … with 1,046 more rows

``` r
unique(w$`Service Sub Type`)
```

    ## [1] "Adults Without Nursing" "Adults With Nursing"

``` r
uk <- bind_rows(eng, scot, w, ni) %>% 
  mutate(country = factor(country,
                          levels = c("England", "Scotland", "Wales", "Northern Ireland")))
uk
```

    ## # A tibble: 18,075 x 7
    ##    `Location Name` `Care homes bed… `Service user b… country Subtype
    ##    <chr>                      <dbl> <chr>            <fct>   <chr>  
    ##  1 Kingswood Hous…               22 <NA>             England <NA>   
    ##  2 Little Haven                  15 <NA>             England <NA>   
    ##  3 Highlands Bord…               28 Y                England <NA>   
    ##  4 Belmont Grange…               30 Y                England <NA>   
    ##  5 Mayfield Adult…                4 <NA>             England <NA>   
    ##  6 Meadow Rose Nu…               56 Y                England <NA>   
    ##  7 The Spinney Nu…               35 Y                England <NA>   
    ##  8 Ashdale Care H…               22 Y                England <NA>   
    ##  9 The Hall                      10 Y                England <NA>   
    ## 10 Ashlea House                   4 <NA>             England <NA>   
    ## # … with 18,065 more rows, and 2 more variables: `Service Sub Type` <chr>,
    ## #   `Categories of Care` <chr>

``` r
feather::write_feather(uk, "derived_data/uk.feather")
```
