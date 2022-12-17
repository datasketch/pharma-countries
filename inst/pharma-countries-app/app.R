library(shinypanels)
library(shiny)
library(DT)
library(hgchmagic)
library(lfltmagic)
library(dsmodules)
library(parmesan)
library(webshot2)
library(DT)
library(pharma.countries)
library(shinypanels)
webshot::install_phantomjs()


ui <- panelsPage(
  includeCSS("www/custom.css"),
  panel(title = "Filters",
        id = "pharma-panel",
        can_collapse = TRUE,
        width = 350,
        body =  div(
          uiOutput("controls"),
          uiOutput("sel_slide_opts")
        )
  ),
  panel(title = "Visualization",
        id = "pharma-panel",
        can_collapse = FALSE,
        header_right = div(
          class = "head-viz",
          div(class = "viz-style",
              uiOutput("viz_icons")),
          uiOutput("downloads")
        ),
        body =  div(
          uiOutput("viz_view")
        )
  ),
  panel(title = "Detail",
        id = "pharma-panel",
        can_collapse = TRUE,
        width = 300,
        color = "chardonnay",
        body =  div(style="overflow: scroll;max-height: 620px",
          shinycustomloader::withLoader(
            uiOutput("click_info"),
            type = "html", loader = "loader4"
          )
        ),
        footer =  div(class = "footer-logos",
                      tags$a(
                        href="https://www.datasketch.co", target="blank",
                        img(src= 'img/logos/logo_ds.svg',
                            align = "left", width = 130, height = 70)),
                      img(src= 'img/logos/logo_ins.svg',
                          width = 150, height = 150)
        )
  )
)


server <- function(input, output, session) {
###Panel izquierdo
  observe({
    if (is.null(input$viz_selection)) return()
     viz_rec <- c("map", "line", "bar", "treemap", "table")

    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }
  })

  ####### Generación de componentes de entrada
  sel_date_mean_opts <- reactive({
    req(sel_date_min_opts())
    req(sel_date_max_opts())
    c(sel_date_min_opts(),  sel_date_max_opts())
    })

  sel_date_min_opts <- reactive({
    min(data$tender_year, na.rm=TRUE)
  })

  sel_date_max_opts <- reactive({
    max(data$tender_year, na.rm=TRUE)
  })

  sel_country <- reactive({
    unique(data$country)
  })

  sel_atc <- reactive({
    unique(data$ATC.product_name)
  })



  output$sel_slide_opts <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active == "bar")   sliderInput("sel_slide_opts","Number of ATC to display",min=10, max=50, value=c(1,20))

  })


####### Generación Parmensan

  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)

  output_parmesan("controls",
                  input = input, output = output, session = session,
                  env = environment())



  ########################################## Panel derecho
  actual_but <- reactiveValues(active = NULL)


   ############################## Data required
  data_down <-reactive({

    ###print(actual_but)
    # req(parmesan_input())
    ls= parmesan_input()
    df <- data |> dplyr::select(contractsignaturedate, country, ATC.product_name, tender_value_amount, unit_price, tender_title, tender_year)
    #TODO hacer en preprocces
    df$unit_price <- as.numeric(df$unit_price)

    df <- filtering_list(df, ls, "tender_year")

    df
  })

  data_viz <- reactive({
    req(actual_but$active)
    if (actual_but$active == "table") return()
    req(data_down())
    ls= parmesan_input()

    if(actual_but$active == "map" | actual_but$active=="treemap") {
      df <- selecting_viz_data(data_down(), actual_but$active, ls$InsId_rb, "country")
    }
    if(actual_but$active == "bar") {
      # req(input$sel_slide_opts)
      df <- selecting_viz_data(data_down(), actual_but$active,  ls$InsId_rb, "ATC.product_name")

      df <- df |> filter(!is.na(mean))
      if(!is.null(input$sel_slide_opts)) df <- df[c(as.numeric(input$sel_slide_opts[1]):as.numeric(input$sel_slide_opts[2])),]
    }
    if(actual_but$active == "line") {
      df <- selecting_viz_data(data_down(), actual_but$active, ls$InsId_rb, "tender_year", "country")
    }
     # if(all(is.na(df$mean))) df <- NULL
    df
  })

 ########################### type viz

  vizFrtype <- reactive({
      req(actual_but$active)
      req(data_viz())
      selecting_viz_typeGraph(data_viz(),actual_but$active)
  })

  viz_down  <- reactive({
    req(actual_but$active)
    req(viz_opts())
    if (is.null(vizFrtype())) return()
    viz=""

    if(actual_but$active == "bar" | actual_but$active == "line" | actual_but$active == "treemap") {

        viz <- paste0("hgchmagic::", paste0("hgch_",actual_but$active, "_", vizFrtype()))
        library(hgchmagic)
    }

    if(actual_but$active == "map") { #TODO update with vizFrtype())
      viz <- paste0("lfltmagic::", "lflt_choropleth_GnmNum")
      library(lfltmagic)
    }

    try({
      do.call(eval(parse(text=viz)),
              viz_opts()
      )
    })
  })



  viz_opts <- reactive({
    tryCatch({
        req(data_viz())
        req(actual_but$active)
        print(colnames(data_viz()))


        myFunc <- NULL
        if (actual_but$active %in% c("bar", "treemap")) {
          myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {id:event.point.name, timestamp: new Date().getTime()});}")
        }
        if (actual_but$active %in% c("line")) {
          myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")
        }



      opts <- list(
          data = data_viz(),
          orientation = "hor",
          ver_title = " ",
          hor_title = " ",
          label_wrap_legend = 100,
          label_wrap = 40,
          background_color = "#ffffff",
          axis_line_y_size = 1,
          axis_line_color = "#dbd9d9",
          grid_y_color = "#dbd9d9",
          grid_x_color = "#fafafa",
          cursor = "pointer",
          map_tiles = "OpenStreetMap",
          legend_position = "bottomleft",
          border_weight = 0.3
        )
        if (actual_but$active == "map") {

          opts$map_bins <- 3
          opts$map_color_scale = "Bins"
          opts$na_color <- "transparent"
          opts$palette_colors <- rev(c("#ef4e00", "#f66a02", "#fb8412", "#fd9d29",
                                       "#ffb446", "#ffca6b", "#ffdf98"))
        } else {
          opts$clickFunction <- htmlwidgets::JS(myFunc)
          opts$palette_colors <- "#ef4e00"
          if (actual_but$active == "line") {
            opts$marker_enabled <- FALSE
            opts$palette_colors <- c("#ef4e00", "#ffe700", "#6fcbff", "#62ce00",
                                     "#ffeea8", "#da3592","#0000ff")
          }
        }

        if (actual_but$active == "treemap") {
          opts$dataLabels_align <- "middle"
          opts$dataLabels_inside <- TRUE
          opts$dataLabels_show <- TRUE
          opts$legend_show <- FALSE
        }

        opts
      },
      error = function(cond) {
        return()
      })
    })


  ############################### Render
  output$hgch_viz <- highcharter::renderHighchart({
    tryCatch({
        req(actual_but$active)
        if (actual_but$active %in% c("table", "map")) return()
        viz_down()
    },
    error = function(cond) {
      return()
    })
  })
  #
  output$lflt_viz <- leaflet::renderLeaflet({
    req(actual_but$active)
    if (!actual_but$active %in% c("map")) return()
    viz_down()
  })

  output$dt_viz <- DT::renderDataTable({
    req(actual_but$active)
    if (actual_but$active != "table") return()
    req(data_down())
    df <- data_down()
    dtable <- DT::datatable(df,
                            rownames = F,
                            selection = 'none',
                            options = list(
                              scrollX = T,
                              fixedColumns = TRUE,
                              fixedHeader = TRUE,
                              scrollY = "500px"
                            ))

    dtable
  })


  output$viz_view <- renderUI({
    tryCatch({

      req(actual_but$active)
      viz <- actual_but$active
      if (viz %in% c("map")) {
        print(data_viz())
        if(all(is.na(data_viz()$mean))) return("No information available")

        shinycustomloader::withLoader(
          leaflet::leafletOutput("lflt_viz", height = 600),
          type = "html", loader = "loader4"
        )
      } else if (viz == "table") {
        shinycustomloader::withLoader(
          DT::dataTableOutput("dt_viz", height = 600, width = 600 ),
          type = "html", loader = "loader4"
        )
      } else {
        if(all(is.na(data_viz()$mean))) return("No information available")

        #shinycustomloader::withLoader(
        highcharter::highchartOutput("hgch_viz", height = 600)#,
        #   type = "html", loader = "loader4"
        # )
      }
    },
    error = function(cond) {
      return()
    })
  })

  ############################## Graph

  output$viz_icons <- renderUI({
    viz <- c("map", "line", "bar", "treemap", "table")
    viz_label <- c("Map", "Line", "Bar", "Treemap", "Table")


    suppressWarnings(
      shinyinvoer::buttonImageInput("viz_selection",
                                    " ",
                                    images = viz,
                                    tooltips = viz_label,
                                    path = "img/viz_icons/",
                                    active = actual_but$active,
                                    imageStyle = list(shadow = TRUE,
                                                      borderColor = "#ffffff",
                                                      padding = "3px")
      )
    )
  })

  output$downloads <- renderUI({

    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      dsmodules::downloadImageUI("download_viz",
                                 dropdownLabel ="Download",
                                 formats = c("jpeg", "pdf", "png", "html"),
                                 display = "dropdown",
                                 text = "Descargar")
    } else {
      dsmodules::downloadTableUI("dropdown_table",
                                 dropdownLabel = "Download",
                                 formats = c("csv", "xlsx", "json"),
                                 display = "dropdown", text = "Descargar")
    }
  })

  observe({
    dsmodules::downloadTableServer("dropdown_table",
                                   element = data_down(),
                                   formats = c("csv", "xlsx", "json"))
    dsmodules::downloadImageServer("download_viz",
                                   element = viz_down(),
                                   lib = "highcharter",
                                   formats = c("jpeg", "pdf", "png", "html"),
                                   file_prefix = "plot")
  })

  output$click_info  <- renderUI({

    tx <- HTML("<div class = 'click'>
                <img src='img/click/click.svg' class = 'click-img'/><br/>
                <b>Click</b> on the visualization to see more information.")



    req(data_down())



    req(actual_but$active)

     if (actual_but$active == "map") {
       if(is.null(input$lflt_viz_shape_click$id)) return(tx)
          dt <- list("country" =input$lflt_viz_shape_click$id)
          df_filtered <- filtering_list(data_down(),dt)
          print(nrow(df_filtered))
          df_filtered <- df_filtered |> head(1000)
          tx <- creating_detail_data(df_filtered , input$lflt_viz_shape_click$id, actual_but$active)
          print(tx)
          tx
    }
    if (actual_but$active == "line") {
      if(is.null(input$hcClicked$id)) return(tx)

          dt <- list("tender_year"=input$hcClicked$id)
          df_filtered <- filtering_list(data_down(),dt)
          df_filtered <- df_filtered |> head(1000)
          tx <- creating_detail_data(df_filtered , input$hcClicked$id, actual_but$active)
          tx
    }

    if (actual_but$active == "bar") {
      if(is.null(input$hcClicked$id)) return(tx)
          dt <- list("ATC.product_name"=input$hcClicked$id)
          df_filtered <- filtering_list(data_down(),dt)
          df_filtered <- df_filtered |> head(1000)
          tx <- creating_detail_data(df_filtered , input$hcClicked$id, actual_but$active)
          tx
    }

    if (actual_but$active == "treemap") {

      if(is.null(input$hcClicked$id)) return(tx)
          dt <- list("country"=input$hcClicked$id)
          df_filtered <- filtering_list(data_down(),dt)
          df_filtered <- df_filtered |> head(1000)
          tx <- creating_detail_data(df_filtered , input$hcClicked$id, actual_but$active)
          tx
    }
    # print(tx)
    if(tx == "" | is.null(tx))
      tx ="No information available"
    tx
  })


}

shinyApp(ui, server)
