## Load data - csv
library(dplyr)

name <- "data-raw/pharma_exported_171122.csv"
data <- read.csv(name)
DataExplorer::create_report(data)
usethis::use_data(data, overwrite = TRUE)

sample_data=NULL
sample_data <- data %>%
  group_by(country) %>%
  do(sample_n(., 10))

usethis::use_data(sample_data, overwrite = TRUE)
