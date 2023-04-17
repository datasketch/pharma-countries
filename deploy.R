
# readRenviron("~/.Renviron")

rsconnect::setAccountInfo(name = Sys.getenv("SHINYAPPS_ACCOUNT"),
                          token = Sys.getenv("SHINYAPPS_TOKEN"),
                          secret = Sys.getenv("SHINYAPPS_SECRET"))
rsconnect::deployApp(
  appDir = "inst/pharma-countries-app/",
  appName = "pharma-countries-app",
  appTitle = "pharma-countries-app",
  logLevel = "verbose",
  forceUpdate= TRUE
  # # exclude hidden files and renv directory (if present)
  # appFiles = setdiff(list.files(), "renv")
)
