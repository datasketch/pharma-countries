## Load data - csv
library(dplyr)

name <- "data-raw/pharma_exported_171122.csv"
data <- read.csv(name)
DataExplorer::create_report(data)
usethis::use_data(data, overwrite = TRUE)
