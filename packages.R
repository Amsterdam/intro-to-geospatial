# Packages voor de workshop
packages <- c(
  "sf",
  "terra",
  "tidyverse",
  "httr2",
  "jsonlite",
  "cbsodataR",
  "spdep"
)

# Controleer welke packages geïnstalleerd zijn
installed <- sapply(
  packages,
  requireNamespace,
  quietly = TRUE
)

# Installeer ontbrekende packages inclusief dependencies
if (any(!installed)) {
  missing_packages <- packages[!installed]
  
  message(
    "Deze packages worden geïnstalleerd: ",
    paste(missing_packages, collapse = ", ")
  )
  
  install.packages(
    missing_packages,
    dependencies = TRUE
  )
}

# Laad alle packages
for (pkg in packages) {
  library(pkg, character.only = TRUE)
}