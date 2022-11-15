library(qrcode)
if (!require(checklist)) {
  install.packages(
    "checklist", repos = c(getOption("repos"), "https://inbo.r-universe.dev")
  )
  library(checklist)
}
icon <- tempfile(fileext = ".svg")
generate_svg("https://doi.org/10.5281/zenodo.5040088", filename = icon)
checklist::create_hexsticker(
  "qrcode", icon = icon, scale = 0.35, x = 170, y = -108
)
