## Load data - csv
library(dplyr)

name <- "data-raw/pharma_exported_171122.csv"
data <- read.csv(name)
DataExplorer::create_report(data)


#https://www.iban.com/country-codes
data$country[data$country == "UA"] <- "UKRAINE"
data$country[data$country == "UY"] <- "URUGUAY"
data$country[data$country == "MX"] <- "MEXICO"
data$country[data$country == "BR"] <- "BRAZIL"
data$country[data$country == "DO"] <- "DOMINICAN REP."
data$country[data$country == "CL"] <- "CHILE"
data$country[data$country == "KZ"] <- "KAZAKHSTAN"
data$country[data$country == "AM"] <- "ARMENIA"
data$country[data$country == "RU"] <- "RUSSIA"
data$country <- stringr::str_to_title(data$country)
usethis::use_data(data, overwrite = TRUE)
sample_data=NULL
sample_data <- data %>%
  group_by(country) %>%
  do(sample_n(., 10))

usethis::use_data(sample_data, overwrite = TRUE)

