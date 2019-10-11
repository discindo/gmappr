
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gmappr

<!-- badges: start -->

<!-- badges: end -->

A shiny app to plot maps with data. Currently plots only North Macedonia
but can be easily extended. The app can be found online here:
<https://novica.shinyapps.io/nmkmappr/>.

This is a version using [{golem}](https://github.com/ThinkR-open/golem)
– that’s what the ‘g’ stands for. Otherwise it is the same app as
<https://github.com/discindo/mappr>.

## Some notes

  - The map data are from <https://wambachers-osm.website/boundaries/>

  - The demo data are from <http://www.stat.gov.mk/>

## Installation

You can install the released version of gmappr from
[github](https://github.com) with:

``` r
devtools::install_github("discindo/gmappr", build=FALSE)
```

Note: `build=FALSE` is needed because
<https://github.com/r-lib/remotes/issues/262>.
