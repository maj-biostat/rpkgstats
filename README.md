# R Package Statistics

Based on daily download logs.
This has got to have been done somewhere else already.

## Getting started

Install 

```r
> library(devtools)
> install_github("maj-biostat/rpkgstats")
```

Add an environment variable for temp directory

```sh
$ echo TMPDIR=\'\\tmp\' >> ~/.Renviron
```

Restart session

```r
> library(pkgstats)
> d1 <- get_cran_logs(from = as.Date("2021-03-02"), to = as.Date("2021-03-04"))
> d2 <- d1[!is.na(package), .N, by = package][order(-N)]
> idx <- grep("json", as.character(d2$package), ignore.case = TRUE)
> d2[idx, ]
          package      N
 1:      jsonlite 141322
 2:         rjson  13051
 3:       RJSONIO  10054
 4:    rapidjsonr   2322
 5:       jsonify   1610
 6:     geojsonsf   1485
 7:     geojsonio   1405
 8:       geojson   1096
 9:  jsonvalidate    811
```

