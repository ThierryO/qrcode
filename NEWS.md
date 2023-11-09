# qrcode 0.2.2

* New `coordinates()` function returning the coordinates of the dark cells.
* Test the QR code with `opencv::ocv_qr_detect()`.
* Remove the defunct legacy functions.

# qrcode 0.2.1

* Replace the unexported `qr_logo()` by `add_logo`().
* `add_logo()` returns a `qr_code` object.
* `plot()` and `generate_svg()` handle the output of `add_logo()`.

# qrcode 0.2.0

* Defunct legacy functions.
* Generate QR codes for calender events with `qr_event()`.
* Generate QR codes for WiFi network settings with `qr_wifi()`.
* Add a logo to a QR code with `qr_logo()`.
* Add [DocSearch](https://docsearch.algolia.com/) support to pkgdown website.
* Use [`checklist`](https://inbo.github.io/checklist/) to monitor the quality of
  the package.

# qrcode 0.1.4

* Re-implemented the main functions to fix bug in the legacy functions.
* Deprecated the legacy functions.

# qrcode 0.1.3

* Added a `NEWS.md` file to track changes to the package.
* Add `generate_svg()`.
* Change maintainer.
* Set-up a pkgdown website.
* Add a logo.
* Add GitHub actions to test the packages.
* Add a few unit tests.
* Streamline some of the functions and reduce their cyclomatic complexity.

# qrcode 0.1.2

* Make functions importable by other packages.
