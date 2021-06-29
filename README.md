# QRcode Generator for R <img src="man/figures/logo.svg" align="right" alt="A hexagon with the word qrcode and a qrcode linking to the DOI" width="120" />

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle: stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable-1)
![GitHub](https://img.shields.io/github/license/thierryo/qrcode)
[![Codecov test coverage](https://codecov.io/gh/thieryo/qrcode/branch/main/graph/badge.svg)](https://codecov.io/gh/thierryo/qrcode?branch=main)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/thierryo/qrcode.svg)
![GitHub repo size](https://img.shields.io/github/repo-size/thierryo/qrcode.svg)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5040089.svg)](https://doi.org/10.5281/zenodo.5040089)

Current version support all QRcode version(1 - 40) and different levels of Error Correction (L, M, Q and H). Meanwhile the encoding method only supports Alphanumeric (Uppercase, Numeric and some basic symbol) and UTF-8 (Byte). 
The example below creates a QR code.

```
qrcode_gen("www.r-project.org"")
```

Current version has a software limit to version 10.
This is because not all APP can decode large QRcode.
To generate QRcode that larger than version 10, use the code below.

```
inputString <- paste0(
  rep("abcdefghijklmnopqrstuvwxyz1234567890"", 63), collapse = ""
)
qrcode_gen(inputString, softLimitFlag = FALSE)
```

This package implement the whole process to create a QRcode.
It is also beneficial to those user who are interested to learn more about how a QRcode is created. 
