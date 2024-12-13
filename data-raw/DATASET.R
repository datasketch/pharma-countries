## Load data - csv
library(dplyr)
library(googledrive)
googledrive::drive_deauth()

drive_download(as_id("https://drive.google.com/file/d/1C6An-Se10Cgt4YDf2VjEQkaexq9kCDPC"),overwrite = TRUE,  path = "data-raw/pharma.csv")
data <- read.csv("data-raw/pharma.csv")

colnames(data)  <- c("tender_id","lot_id","Tender Title","Tender Value Amount (USD)","lot_value_amount","tender_value_currency",
                     "Unit Price (USD)","quantity","Tender Year","ATC.code","Country","tender_publications_firstcallfortenderdate",
                     "tender_awarddecisiondate","Signature Date","tender_biddeadline","Drug type")
#https://www.iban.com/Country-codes
data$Country[data$Country == "UA"] <- "UKRAINE"
data$Country[data$Country == "UY"] <- "URUGUAY"
data$Country[data$Country == "MX"] <- "MEXICO"
data$Country[data$Country == "BR"] <- "BRAZIL"
data$Country[data$Country == "DO"] <- "DOMINICAN REP."
data$Country[data$Country == "CL"] <- "CHILE"
data$Country[data$Country == "KZ"] <- "KAZAKHSTAN"
data$Country[data$Country == "AM"] <- "ARMENIA"
data$Country[data$Country == "RU"] <- "RUSSIA"
data$Country <- stringr::str_to_title(data$Country)

data$`Drug type` <- stringr::str_to_title(data$`Drug type`)
# data <- data |> rename(`Drug type` = `Drug Name`)
usethis::use_data(data, overwrite = TRUE)
sample_data=NULL
sample_data <- data %>%
  group_by(Country) %>%
  do(sample_n(., 10))

usethis::use_data(sample_data, overwrite = TRUE)

