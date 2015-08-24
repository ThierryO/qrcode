#QRcode Generator for R

Generate QRcode for R. Current version support all QRcode version(1-40) and different level of Error Correction Level(L,M,Q and H). Meanwhile the encoding method only support Alphanumeric(Uppercase,Numeric and some basic symbol) and UTF-8(Byte). 

To create a QRcode, user can follow the following example.

```
qrcode_gen('www.r-project.org')

```
Current version has a software limit to version 10. This is because not all APP can decode large QRcode. To generate QRcode that larger than version 10.

```
inputString <- paste0(rep('abcdefghijklmnopqrstuvwxyz1234567890',63),collapse = '')
qrcode_gen(inputString,softLimitFlag = FALSE)
```


This package implement the whole process to create a QRcode. It is also beneficial to those user who interested to learn more about how a QRcode is create. 
