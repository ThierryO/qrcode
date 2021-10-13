
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/qrcode)](https://CRAN.R-project.org/package=qrcode)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable-1)
![GitHub](https://img.shields.io/github/license/thierryo/qrcode)
[![Codecov test
coverage](https://codecov.io/gh/thieryo/qrcode/branch/main/graph/badge.svg)](https://app.codecov.io/gh/thierryo/qrcode?branch=main)
![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/thierryo/qrcode.svg)
![GitHub repo
size](https://img.shields.io/github/repo-size/thierryo/qrcode.svg)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5040089.svg)](https://doi.org/10.5281/zenodo.5040089)
<!-- badges: end -->

# QRcode Generator for R <img src="man/figures/logo.svg" align="right" alt="A hexagon with the word qrcode and a qrcode linking to the DOI" width="120" />

A [QR code](https://en.wikipedia.org/wiki/QR_code) is a two-dimensional
barcode developed by the [Denso Wave](https://www.denso.com/global/en/)
company.

The current implementation handles three modes: numeric, alphanumeric
and byte. Kanji is currently not supported. Please contact the
maintainer if you need it.

-   **Numeric**: only digits from 0 to 9
-   **Alphanumeric**: all numeric characters, upper case `LETTERS`, and
    the characters `" "` (space), `"$"`, `"%"`, `"*"`, `"+"`, `"-"`,
    `"."`, `"/"` and `":"`
-   **Byte**: All characters from the Latin 1 (ISO 8859-1) character
    set.

## Main functions

The main `qr_code()` function returns an object of the `qr_code` class.
This is a logical matrix with specific methods for printing and
plotting. The print version uses UTF-8 drawing symbols and the result
depend on the font used. For a better quality QR code it best to use
`plot()` or `generate_svg()`.

``` r
library(qrcode)
code <- qr_code("QR CODE")
print(code)
```

    ##               
    ##  ▗▄▄▄  ▗▗▄▄▄  
    ##  ▐▗▄▐ ▐▟▐▗▄▐  
    ##  ▐▐█▐▐▞▐▐▐█▐  
    ##  ▐▄▄▟▗▚▚▐▄▄▟  
    ##  ▗▖ ▄▖ ▖ ▗▖   
    ##  ▝▄▜▄▌▘▜▚▛▚█  
    ##  ▐▜█▚▀▐██▙▙▞  
    ##  ▗▄▄▄▐▄▞▖▗▟▄  
    ##  ▐▗▄▐▝▖▖▞▗▟▘  
    ##  ▐▐█▐ ▞█▛▄▐▄  
    ##  ▐▄▄▟▐▘▜▀▐▜▝  
    ##               
    ##               
    ## 
    ## use plot() for a better quality image

``` r
plot(code)
```

<img src="man/figures/example-1.png" title="A QR code displaying the value 'QR CODE'" alt="A QR code displaying the value 'QR CODE'"  />

``` r
generate_svg(code, filename = "man/figures/qr.svg")
```

![A QR code displaying the value ‘QR CODE’](man/figures/qr.svg)

## Installation

The `qrcode` package is available from CRAN, R universe and GitHub.

``` r
# install from CRAN
install.packages("qrcode")
# install from R universe
install.packages("qrcode", repos = "https://thierryo.r-universe.dev")
# install from GitHub use the remotes package
remotes::install_github("ThierryO/qrcode")
```

## Legacy code

The legacy function `qrcode_gen()` has several known bugs. Do check any
QR code you create with that function extra careful. Some of them will
not work.

Because of these bugs, we re-implemented the code from scratch in
version 0.1.4. We created new functions and left the old version in
place. Please note that we deprecated the legacy functions. We plan to
keep the deprecated functions around until at least June 2022. Update
your code before that date.
