# library(shinypanels)
# library(shiny)
# library(DT)
# library(hgchmagic)
# library(lfltmagic)
# library(dsmodules)
# library(parmesan)
# library(webshot2)
# library(DT)
# # library(pharma.countries)
# library(shinypanels)
# webshot::install_phantomjs()
#
#
# ui <- panelsPage(
#   includeCSS("www/custom.css"),
#   panel(title = "Filters",
#         id = "pharma-panel",
#         can_collapse = TRUE,
#         width = 350,
#         body =  div(
#           uiOutput("controls")
#         )
#   ),
#   panel(title = "Visualization",
#         id = "pharma-panel",
#         can_collapse = FALSE,
#         header_right = div(
#           class = "head-viz",
#           div(class = "viz-style",
#               uiOutput("viz_icons")),
#           uiOutput("downloads")
#         ),
#         body =  div(
#           uiOutput("viz_view")
#         )
#   ),
#   panel(title = "Detail",
#         id = "pharma-panel",
#         can_collapse = TRUE,
#         width = 300,
#         color = "chardonnay",
#         body =  div(
#           shinycustomloader::withLoader(
#             uiOutput("click_info"),
#             type = "html", loader = "loader4"
#           )
#         ),
#         footer =  div(class = "footer-logos",
#                       tags$a(
#                         href="https://www.datasketch.co", target="blank",
#                         img(src= 'img/logos/logo_ds.svg',
#                             align = "left", width = 130, height = 70)),
#                       img(src= 'img/logos/logo_ins.svg',
#                           width = 150, height = 150)
#         )
#   )
# )
#
#
# server <- function(input, output, session) {
#
#
#
# }
#
# shinyApp(ui, server)
