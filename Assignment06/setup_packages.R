# BDA400 Assignment 6 - Package Setup
# Student: Parvana Babali

required_packages <- c(
  "shiny",
  "ggplot2",
  "quantmod",
  "TTR"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}

cat("Package setup complete.\n")
cat("Installed/available packages:\n")
print(required_packages)
