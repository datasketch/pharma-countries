library(shinypanels)
library(shiny)
library(DT)
library(hgchmagic)
library(lfltmagic)
library(dsmodules)
library(parmesan)
library(webshot2)
library(DT)
library(reactable)
library(pharma.countries)
library(shinypanels)
library(bsplus)
webshot::install_phantomjs()

ui <- panelsPage(
  includeCSS("www/custom.css"),
  panel(title = "Filters",
        id = "pharma-panel",
        can_collapse = TRUE,
        width = 350,
        body =  div(
          uiOutput("controls"),
          uiOutput("sel_slide_opts"),
          uiOutput("sel_check_opt"),
          uiOutput("sel_check_opt_asc")
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
        body =  div(style="overflow: auto;",

                    uiOutput("viz_view_side_limit"),
                    shinycustomloader::withLoader(

                      uiOutput("viz_view_side"),
                      type = "html", loader = "loader4",
                    )
        ),
        footer =  div(class = "footer-logos",
                      img(src= 'img/logos/GTI_logo.png',
                          width = 90, height = 90),
                      img(src= 'img/logos/logo_ins.svg',
                          width = 160, height = 160),
                      tags$a(
                        href="https://www.datasketch.co", target="blank",
                        img(src= 'img/logos/logo_ds.svg',
                            align = "left", width = 170, height = 110))
        )
  )
)


server <- function(input, output, session) {



  library(dplyr)

  ###Panel izquierdo
  observe({

    if (is.null(input$viz_selection)) return()
    viz_rec <- c("map", "line", "bar", "treemap", "table")


    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
      click_viz$id <- NULL
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
    min(data$`Tender Year`, na.rm=TRUE)
  })

  sel_date_max_opts <- reactive({
    max(data$`Tender Year`, na.rm=TRUE)
  })

  sel_country <- reactive({
    unique(c("All", data$Country)) |>
      setdiff("NA")
  })

  sel_atc <- reactive({
    unique(data$`Drug type`)
  })



  output$sel_slide_opts <- renderUI({
    # req(sel_slide_opts_max())
    if (is.null(actual_but$active)) return()
    if (actual_but$active == "bar" ){
      sliderInput("sel_slide_opts","Number of drug types to display",list(icon("paw"),"Select a variable:"),step=10,
                                                  min=1, max=sel_slide_opts_max(), value=c(0,10)) |>
          bs_embed_tooltip(title = "We recommend that you choose no more than 10 categories to compare.")
    }else{

    if(actual_but$active == "treemap") {
      # req(input$sel_slide_opts)

       if(length(unique(input$Country)) >= 1) {
         if (!"All" %in% input$Country) {
           sliderInput("sel_slide_opts","Number of drug types to display",list(icon("paw"),"Select a variable:"),step=10,
                       min=1, max=sel_slide_opts_max(), value=c(0,10)) |> #'
             bs_embed_tooltip(title = "We recommend that you choose no more than 10 categories to compare.")
         }
       }
     }
    }
  })

  sel_slide_opts_max <- reactive({
    req(data_down())
    req(actual_but$active)
    req(parmesan_input())
    if (actual_but$active == "bar" |  actual_but$active == "treemap") {
      ls <- parmesan_input()
        df <- data_down()
       try( df <- filtering_list_side(df, ls, "Tender Year"))

      df$counter_c <-NULL
      if(length(unique(input$Country)) > 1){
        if (!"All" %in% input$Country){
          df <- selecting_viz_data(df, actual_but$active,  ls$InsId_rb,  "Drug type","Country")
          total <- length(input$Country)
          df2 <- df |> group_by(`Drug type`) |> summarize(count = n()) |> filter(count == total)
          df <- df |> filter(`Drug type` %in% as.vector(df2$`Drug type`))
          #
        }
        else{
          df <- selecting_viz_data(df, actual_but$active,  ls$InsId_rb, "Drug type")
        }

      }

      else{
        if(actual_but$active == "treemap" & !"All" %in% input$Country){
          df <- selecting_viz_data(df, actual_but$active,  ls$InsId_rb,  "Drug type","Country")
          total <- length(input$Country)
          df2 <- df |> group_by(`Drug type`) |> summarize(count = n()) |> filter(count == total)
          df <- df |> filter(`Drug type` %in% as.vector(df2$`Drug type`))
        }
         else {
           df <- selecting_viz_data(df, actual_but$active,  ls$InsId_rb, "Drug type")
         }
      }
      # df <- selecting_viz_data(df, actual_but$active,  ls$InsId_rb, "Drug type")
      df <- df |> filter(!is.na(mean) | mean==0)

      length(unique(df$`Drug type`))

    }

  })

  output$sel_check_opt <- renderUI({
    req(data_down())
    req(actual_but$active)
    req(sel_slide_opts_max())

    if (actual_but$active == "bar") {
        checkboxInput("sel_check_opt","Sort by name",FALSE)
    }

  })
  output$sel_check_opt_asc <- renderUI({
    req(data_down())
    req(actual_but$active)
    req(sel_slide_opts_max())

    if (actual_but$active == "treemap") {
      if(length(unique(input$Country)) >= 1){
        if (!"All" %in% input$Country){
            radioButtons("sel_check_opt_asc","Order type:",
                   c("Desc" = "Desc",
                     "Asc" = "Asc"))

        }
      }


    }

  })


  observe({
    if ("All" %in% input$Country) {
      updateSelectizeInput(session, inputId = "Country", selected = "All")
    }

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
      # tryCatch({

      req(parmesan_input())
      ls <- parmesan_input()

      click_viz$id <- NULL
      df <- data |> dplyr::select(`Signature Date`, Country, `Drug type`, `Tender Value Amount (USD)`, `Unit Price (USD)`, `Tender Title`, `Tender Year`)
      #TODO hacer en preprocces

      df$`Unit Price (USD)` <- as.numeric(df$`Unit Price (USD)`)
     # try(
        df <- filtering_list(df, ls, "Tender Year")
      #)

      # },
      df

    # error = function(cond) {
    #   return()
    # })
  })

  data_viz <- reactive({
    req(actual_but$active)
    if (actual_but$active == "table") return()
    req(data_down())
    req( parmesan_input())

    ls= parmesan_input()

    if(actual_but$active == "map") {
      df <- selecting_viz_data(data_down(), actual_but$active, ls$InsId_rb, "Country", "Drug type")

    }
    if(actual_but$active == "treemap") {
      # req(input$sel_slide_opts)


       if(length(unique(input$Country)) >  1){

            if (!"All" %in% input$Country){
              if(is.null(input$sel_check_opt_asc)) return()

              df <- selecting_viz_data(data_down(), actual_but$active,  ls$InsId_rb,  "Country", "Drug type")
              df <- df |> filter(!is.na(mean))
              df <- df |> arrange(desc(mean),`Drug type`,Country)
              df <- df |> select(Country,`Drug type`,mean)
              total <- length(input$Country)
              df2 <- df |> group_by(`Drug type`) |> summarize(count = n()) |> filter(count == total)
              df <- df |> filter(`Drug type` %in% as.vector(df2$`Drug type`))
              df <- df |> filter(!is.na(mean))

              if(!is.null(input$sel_check_opt_asc)){
                if(input$sel_check_opt_asc != "Desc"){
                  df = df|>  dplyr::arrange(mean)
                }
                else {

                  df = df|>  dplyr::arrange(desc(mean))
                }
              }
              if(nrow(df) > 0) {
                #
                if(!is.null(input$sel_slide_opts)) df <- df[c(as.integer(input$sel_slide_opts[1]):as.integer(input$sel_slide_opts[2])),]
              }
              if(nrow(df) < 0) {
                #
                 HTML("There are no common vaccines for the selected countries")
              }

              if(!is.null(input$sel_check_opt_asc)){
                if(input$sel_check_opt_asc != "Desc"){
                     df = df|>  dplyr::arrange(mean)
                }
                else {

                  df = df|>  dplyr::arrange(desc(mean))
                }
              }




              # df <- df |> arrange(Country,`Drug type`,desc(mean))


            }
            else{

              # df <- df |> filter(!is.na(mean))
              # df <- selecting_viz_data(data_down(), actual_but$active,  ls$InsId_rb, "Country")
              # df <- df |> filter(!is.na(mean))
              df <- selecting_viz_data(data_down(), actual_but$active,  ls$InsId_rb,"Country", "Drug type")
              df <- df |> filter(!is.na(mean))
              df <- df |> arrange(desc(mean),Country,`Drug type`)
              df1 <-  df %>% select(Country,`Drug type`,mean) |> top_n(10,mean)
              # df1$temp_c <- 1
              df2 <-  df |> filter(!`Drug type` %in% as.vector(df1$`Drug type`))

              others_treemap$list_country_drug  <-  unique(df2 |> select(Country,`Drug type`))
              # others_treemap$list_country_drug <- 1

              df2$`Drug type` = "Others"
              df2 <- df2   |>  group_by(Country,`Drug type`) |> summarize(mean=mean(mean,na.rm=TRUE))
              # df2$temp_c <- 2
              df <- as.data.frame(rbind(df1,df2))
              df <- df |> arrange(desc(mean),Country,`Drug type`)
              df


            }


      }

      else{
        if(length(unique(input$Country)) == 1){

                      if ("All" %in% input$Country){
                        # df <- df |> filter(!is.na(mean))
                        df <- selecting_viz_data(data_down(), actual_but$active,  ls$InsId_rb,"Country", "Drug type")
                        df <- df |> filter(!is.na(mean))
                        df <- df |> arrange(desc(mean),Country,`Drug type`)
                        df1 <-  df %>% select(Country,`Drug type`,mean) |> top_n(10,mean)
                        # df1$temp_c <- 1
                        df2 <-  df |> filter(!`Drug type` %in% as.vector(df1$`Drug type`))

                        others_treemap$list_country_drug  <-  unique(df2 |> select(Country,`Drug type`))
                        # others_treemap$list_country_drug <- 1

                        df2$`Drug type` = "Others"
                        df2 <- df2   |>  group_by(Country,`Drug type`) |> summarize(mean=mean(mean,na.rm=TRUE))
                        # df2$temp_c <- 2
                        df <- as.data.frame(rbind(df1,df2))
                        df <- df |> arrange(desc(mean),Country,`Drug type`)
                        df


                      }
                      else {
                      #######################################
                      df <- selecting_viz_data(data_down(), actual_but$active,  ls$InsId_rb,  "Country", "Drug type")


                      df <- df |> filter(!is.na(mean))
                      df <- df |> arrange(desc(mean),`Drug type`,Country)
                      df <- df |> select(Country,`Drug type`,mean)
                      total <- length(input$Country)
                      df2 <- df |> group_by(`Drug type`) |> summarize(count = n()) |> filter(count == total)
                      df <- df |> filter(`Drug type` %in% as.vector(df2$`Drug type`))
                      df <- df |> filter(!is.na(mean))
                      #
                      # df1 <-  df %>% select(Country,`Drug type`,mean) |> top_n(10,mean)
                      # df1$temp_c <- 1
                      #
                      # df2 <-  df |> filter(!`Drug type` %in% as.vector(df1$`Drug type`))
                      # # print(others_treemap$list_country_drug )
                      # others_treemap$list_country_drug  <-  unique(df2 |> select(Country,`Drug type`))
                      # # others_treemap$list_country_drug <- 1

                      # df2$`Drug type` = "Others"
                      # df2 <- df2   |>  group_by(Country,`Drug type`) |> summarize(mean=mean(mean,na.rm=TRUE))
                      # df2$temp_c <- 2
                      # df <- as.data.frame(rbind(df1,df2))
                      # df <- df |> arrange(desc(mean),Country,`Drug type`)
                      # df

                      if(!is.null(input$sel_check_opt_asc)){
                        if(input$sel_check_opt_asc != "Desc"){
                          df = df|>  dplyr::arrange(mean)
                        }
                        else {

                          df = df|>  dplyr::arrange(desc(mean))
                        }
                      }

                      if(nrow(df) > 0) {
                        #
                        if(!is.null(input$sel_slide_opts)) df <- df[c(as.integer(input$sel_slide_opts[1]):as.integer(input$sel_slide_opts[2])),]
                      }
                      if(nrow(df) < 0) {
                        #
                        HTML("There are no common vaccines for the selected countries")
                      }








                      ########################
                      }

          }
        else{
        # df <- selecting_viz_data(data_down(), actual_but$active,  ls$InsId_rb, "Country")
        # df <- df |> filter(!is.na(mean))
          df <- selecting_viz_data(data_down(), actual_but$active,  ls$InsId_rb,"Country", "Drug type")
          df <- df |> filter(!is.na(mean))
          df <- df |> arrange(desc(mean),Country,`Drug type`)
          df1 <-  df %>% select(Country,`Drug type`,mean) |> top_n(10,mean)
          # df1$temp_c <- 1
          df2 <-  df |> filter(!`Drug type` %in% as.vector(df1$`Drug type`))

          others_treemap$list_country_drug  <-  unique(df2 |> select(Country,`Drug type`))
          # others_treemap$list_country_drug <- 1

          df2$`Drug type` = "Others"
          df2 <- df2   |>  group_by(Country,`Drug type`) |> summarize(mean=mean(mean,na.rm=TRUE))
          # df2$temp_c <- 2
          df <- as.data.frame(rbind(df1,df2))
          df <- df |> arrange(desc(mean),Country,`Drug type`)
          df



            # group_by(Country,`Drug type`) %>%
            # arrange(mean, .by_group = TRUE) %>%
            # top_n(1)




          # df <- df |> select(Country,`Drug type`,mean)
        }

      }

    }
    if(actual_but$active == "bar") {
      # req(input$sel_slide_opts)

      if(length(unique(input$Country)) > 1){
        if (!"All" %in% input$Country){
          df <- selecting_viz_data(data_down(), actual_but$active,  ls$InsId_rb,  "Drug type","Country")
          df <- df |> filter(!is.na(mean))
          df <- df |> arrange(desc(mean),`Drug type`,Country)
          df <- df |> select(Country,`Drug type`,mean)
          total <- length(input$Country)
          df2 <- df |> group_by(`Drug type`) |> summarize(count = n()) |> filter(count == total)
          df <- df |> filter(`Drug type` %in% as.vector(df2$`Drug type`))

          if(nrow(df) > 0) {
                      #
          if(!is.null(input$sel_slide_opts)) df <- df[c(as.integer(input$sel_slide_opts[1]):as.integer(input$sel_slide_opts[2])),]
          }

          else{ HTML("There are no common vaccines for the selected countries")}

          # df <- df |> arrange(Country,`Drug type`,desc(mean))


        }
        else{
          df <- selecting_viz_data(data_down(), actual_but$active,  ls$InsId_rb, "Drug type")
          df <- df |> filter(!is.na(mean))
          if(!is.null(input$sel_slide_opts)) df <- df[c(as.integer(input$sel_slide_opts[1]):as.integer(input$sel_slide_opts[2])),]

          df <- df |> arrange(desc(mean))
        }


      }

       else{
        df <- selecting_viz_data(data_down(), actual_but$active,  ls$InsId_rb, "Drug type")
        df <- df |> filter(!is.na(mean))
        if(!is.null(input$sel_slide_opts)) df <- df[c(as.integer(input$sel_slide_opts[1]):as.integer(input$sel_slide_opts[2])),]

        df <- df |> arrange(desc(mean))
       }

    }
    if(actual_but$active == "line") {
      df <- selecting_viz_data(data_down(), actual_but$active, ls$InsId_rb, "Tender Year", "Country")
    }

    df
  })

  ########################### type viz

  vizFrtype <- reactive({
    req(actual_but$active)
    req(data_viz())
    selecting_viz_typeGraph(data_viz(),actual_but$active,input$Country)
  })

  viz_down  <- reactive({

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


  others_treemap <- reactiveValues(list_country_drug=NULL)

  click_viz <- reactiveValues(id = NULL,
                              cat =NULL)

  observeEvent(input$lflt_viz_shape_click, {
    if (is.null(data_viz())) return()

    if (!is.null(input$lflt_viz_shape_click$id)) {
      click_viz$id <- input$lflt_viz_shape_click$id
    }
    else {
      click_viz$id <- NULL

    }

  })


  observeEvent(input$hcClicked, {
    if (is.null(data_viz())) return()


   if(actual_but$active %in% c("treemap")) {

     click_viz$id <- input$hcClicked$cat$parent
     click_viz$cat <- input$hcClicked$cat$name


     if(length(unique(input$Country)) > 1){
       if (!"All" %in% input$Country){

          if (!is.null(input$hcClicked$cat$parent)) {
               click_viz$id <-  input$hcClicked$cat$parent
               click_viz$cat <- input$hcClicked$cat$name
             }
         else{
                 click_viz$id <- input$hcClicked$id }
       }
       else{

         click_viz$id <- input$hcClicked$id }
     }
     if(length(unique(input$Country)) == 1){
       if (!"All" %in% input$Country){

         if (!is.null(input$hcClicked$cat$parent)) {
           click_viz$id <-   input$hcClicked$cat$parent
           click_viz$cat <-   input$hcClicked$cat$name
         }
         else{   click_viz$id <- input$hcClicked$id }
       }
       else{   click_viz$id <-   input$hcClicked$cat$parent
               click_viz$cat <-   input$hcClicked$cat$name }
     }
     # else{
     #
     # if(is.null(input$Country)) print("entrooooo4")
     #
     # }

      # else {   click_viz$id <- NULL }
   }
   else{
     if(!actual_but$active %in% c("line","bar")){
       if (!is.null(input$hcClicked$id)) {
         click_viz$id <- input$hcClicked$id

       }
       else {   click_viz$id <- NULL

       }

     }
      else{
          if (!is.null(input$hcClicked$id)) {
            click_viz$id <- input$hcClicked$id
            click_viz$cat <- input$hcClicked$cat

          }
          else {   click_viz$id <- NULL
                   click_viz$cat <- NULL
              }
      }
   }


  })




  viz_opts <- reactive({
     tryCatch({
      req(data_viz())
      req(actual_but$active)

      myFunc <- NULL

      if (actual_but$active %in% c("treemap")) {
        myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "',{cat:event.point.node, id:event.point.name, timestamp: new Date().getTime()});}");

      }


      if(length(unique(input$Country)) > 1){
        if (!"All" %in% input$Country){
          if (actual_but$active %in% c("treemap")) {
            myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "',{cat:event.point.node, id:event.point.name, timestamp: new Date().getTime()});}");

          }
          if (actual_but$active %in% c("bar","line")) {
            myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")
          }
        }
        else {
            if (actual_but$active %in% c( "bar")) {
              myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {id:event.point.name, timestamp: new Date().getTime()});}")

            }
            if (actual_but$active %in% c("line")) {
              myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")

            }

            if (actual_but$active %in% c("treemap")) {
              myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "',{cat:event.point.node, id:event.point.name, timestamp: new Date().getTime()});}");

             }


        }
      }
      else {
        if (actual_but$active %in% c( "bar")) {
          myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {id:event.point.name, timestamp: new Date().getTime()});}")
        }
        if (actual_but$active %in% c("line")) {
          myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")

        }
        # if (actual_but$active %in% c("treemap")) {
        #   myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "',{cat:event.point.node, id:event.point.name, timestamp: new Date().getTime()});}");
        #
        # }
      }
      data_v <- data_viz()
      data_v <- data_v |> dplyr::mutate(mean_show = sitools::f2si(mean))
      data_v <- data_v |> dplyr::rename("Average Price" = "mean")

     # if(actual_but$active == "treemap") {

     # }

      opts <- list(
        data = data_v,
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
        border_weight = 0.3,
        map_provider_tile = "url",
        map_extra_layout = "https://maps.geoapify.com/v1/tile/osm-bright-smooth/{z}/{x}/{y}.png?apiKey=3ccf9d5f19894b32b502485362c99163",
        map_name_layout = "osm-brigh",
        # format_sample_num = "10M",
        format_numericSymbols = T
      )
      if (actual_but$active == "map") {
        opts$legend_title <- input$InsId_rb
        opts$legend_color <-  "Black"
        opts$map_bins <- 3
        opts$map_color_scale = "Bins"
        opts$na_color <- "transparent"
        opts$tooltip <- "<b>Country:</b> {Country}<br/><b>Average Price:</b> {mean_show} USD"
        opts$format_sample_num = "10M"
        # opts$palette_colors <- rev(c("#ef4e00", "#f66a02", "#fb8412", "#fd9d29",
                                     # "#ffb446", "#ffca6b", "#ffdf98"))
        opts$palette_colors <- rev(c( "#da3592","#FFF6FF"))
      } else {
        opts$clickFunction <- htmlwidgets::JS(myFunc)
        opts$palette_colors <- "#ef4e00"
        if (actual_but$active == "line") {
          opts$marker_enabled <- FALSE
          opts$palette_colors <- c("#ef4e00", "#ffe700", "#6fcbff", "#62ce00",
                                   "#ffeea8", "#da3592","#0000ff")
          opts$ver_title <- "Tender Year"
          opts$hor_title <- stringr::str_to_sentence(input$InsId_rb)
          opts$format_sample_num = "10M"
          opts$tooltip <- "<b>Country:</b> {Country}<br/><b>Tender Year:</b> {Tender Year}<br/><b>Average Price:</b> {mean_show} USD"
        }
      }

      if (actual_but$active == "treemap") {
        # opts$Labels <-  sitools::f2si(data_v$mean)

        opts$dataLabels_align <- "middle"
        opts$dataLabels_inside <- TRUE
        opts$dataLabels_show <- TRUE
        opts$legend_show <- FALSE


        if(length(unique(input$Country)) >= 1) {
          if (!"All" %in% input$Country){
            # if(input$sel_check_opt == FALSE){ opts$sort <- "desc" }
            opts$tooltip <- "<b>Country:</b> {Country}<br/><b>Drug type:</b> {Drug type}<br/><b>Average Price:</b> {mean_show} USD"
            opts$datalabel_formmater_js  <- TRUE

           if(length(unique(input$Country)) > 1){
                opts$palette_colors <- c("#ef4e00", "#ffe700", "#6fcbff", "#62ce00",
                                     "#ffeea8", "#da3592","#0000ff")
            }
            else{
              if(length(unique(input$Country)) == 1){
               #TODO, el color no cambia
                if ("All" %in% input$Country){
                  opts$palette_colors <- c("#ef4e00", "#ffe700", "#6fcbff", "#62ce00",
                                           "#ffeea8", "#da3592","#0000ff")

                  }
                else{ opts$color_by <- "Drug type"
                }
              }
           }
          }
          else{

            if ("All" %in% input$Country){
              opts$palette_colors <- c("#ef4e00", "#ffe700", "#6fcbff", "#62ce00",
                                       "#ffeea8", "#da3592","#0000ff")

            }
            else {
                opts$tooltip <- "<b>Country:</b> {Country}<br/><b>Drug type:</b> {Drug type}<br/><b>Average Price:</b> {mean_show} USD"
                opts$datalabel_formmater_js  <- TRUE
                opts$color_by <- "Country"
            }
          }

        }

        else{
          opts$tooltip <- "<b>Country:</b> {Country}<br/><b>Drug type:</b> {Drug type}<br/><b>Average Price:</b> {mean_show} USD"
          opts$datalabel_formmater_js  <- TRUE

          # opts$color_by <- "Country"
          opts$palette_colors <- c("#ef4e00", "#ffe700", "#6fcbff", "#62ce00",
                                   "#ffeea8", "#da3592","#0000ff")
        }

      }

      if (actual_but$active == "bar") {
        opts$palette_colors <- c("#ef4e00", "#ffe700", "#6fcbff", "#62ce00",
                                 "#ffeea8", "#da3592","#0000ff")
        opts$ver_title <- "Drug type"
        opts$hor_title <- stringr::str_to_sentence(input$InsId_rb)
        opts$format_sample_num = "10M"
        if(input$sel_check_opt == FALSE){ opts$sort <- "desc" }

        if(length(unique(input$Country)) > 1){
          if (!"All" %in% input$Country){
            opts$tooltip <- "<b>Drug type:</b> {Drug type}<br/><b>Country:</b> {Country}<br/><b>Average Price:</b> {mean_show} USD"

          }
          else{
            opts$tooltip <- "<b>Drug type:</b> {Drug type}<br/><b>Average Price:</b> {mean_show} USD"

          }

        }

        else{
          opts$tooltip <- "<b>Drug type:</b> {Drug type}<br/><b>Average Price:</b> {mean_show} USD"
        }
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
      req(data_viz())
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
    viz_down()  |>
      leaflet::setView(lng = 0, lat = -5, 1.25)
  })

  output$dt_viz <- reactable::renderReactable({
    req(actual_but$active)
    if (actual_but$active != "table") return()
    req(data_down())
    df <- data_down()
    reactable(df,
              showSortIcon = TRUE)
    # dtable <- DT::datatable(df,
    #                         rownames = F,
    #                         selection = 'none',
    #                         options = list(
    #                           scrollX = T,
    #                           fixedColumns = TRUE,
    #                           fixedHeader = TRUE,
    #                           scrollY = "500px"
    #                         ))
    #
    # dtable
  })


  output$viz_view <- renderUI({
     tryCatch({

      req(actual_but$active)
      viz <- actual_but$active

            if (viz %in% c("map")) {
        req(data_viz())
        if(all(is.na(data_viz()$mean))) return("No information available")


        shinycustomloader::withLoader(
          leaflet::leafletOutput("lflt_viz", height = 600),
          type = "html", loader = "loader4"
        )
      } else if (viz == "table") {
        shinycustomloader::withLoader(
          reactable::reactableOutput("dt_viz"),
          type = "html", loader = "loader4"
        )
      } else {
        req(data_viz())

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
                                 text = "Download")
    } else {
      dsmodules::downloadTableUI("dropdown_table",
                                 dropdownLabel = "Download",
                                 formats = c("csv", "xlsx", "json"),
                                 display = "dropdown", text = "Download")
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






  ##############################
  #Demo otuput side with DT
  output$viz_view_side_limit <- renderUI({
    #TODO update Country - create a metric for to choose
    req(actual_but$active)
    if(is.null(click_viz$id)) { return() }
    if ((actual_but$active == "treemap"   | actual_but$active == "map")  & click_viz$id == "Ukraine") {
        tx <- HTML("<div style='color:red;  font-size: 7px;'>
                  10,000 records are displayed. If you want to see the complete detail, access the table shown in the menu. </div><br/>" )
    }

  })

  output$viz_view_side <- renderUI({

    tx <- HTML("<div class = 'click'>
                <img src='img/click/click.svg' class = 'click-img'/><br/>
                <b>Click</b> on the visualization to see more information.")

    # req(data_viz())
    if(is.null(data_side())) return(tx)
    if(all(is.na(data_viz()$mean))) return("No information available")

    #shinycustomloader::withLoader(
    DT::dataTableOutput("dt_viz_side", height = 590, width = 230 )#,
    # type = "html", loader = "loader4"
    #)


  })

  data_side <- reactive({
    req(data_down())
    if (nrow(data_down()) == 0) return()
    req(actual_but$active)
    tx <- NULL
    if (actual_but$active == "map") {
      if(is.null(click_viz$id)) { return() }

      dt <- list("Country" = click_viz$id)
      df_filtered <- filtering_list_side(data_down(),dt)
      df_filtered <- df_filtered |> head(10000)

      # df_filtered <- df_filtered |> head(100)
      tx <- creating_detail_data(df_filtered , click_viz$id, actual_but$active)


    }
    if (actual_but$active == "line") {
      if(is.null(click_viz$id)) return(NULL)

      dt <- list("Tender Year"= click_viz$id)
      df_filtered <- filtering_list_side(data_down(),dt)
      if(!is.null(click_viz$cat)) {
        dt <- list("Country"= click_viz$cat)
        df_filtered <- filtering_list_side(df_filtered,dt)

      }
      # df_filtered <- df_filtered |> head(1000)
      tx <- creating_detail_data(df_filtered ,  click_viz$id, actual_but$active)

    }

    if (actual_but$active == "bar") {
      if(is.null( click_viz$id)) return(NULL)
      dt <- list("Drug type"= click_viz$id)
      df_filtered <- filtering_list_side(data_down(),dt)
      if(!is.null(click_viz$cat)) {
        dt <- list("Country"= click_viz$cat)
        df_filtered <- filtering_list_side(df_filtered,dt)

      }
      # df_filtered <- df_filtered |> head(1000)
      tx <- creating_detail_data(df_filtered ,  click_viz$id, actual_but$active)

    }

    if (actual_but$active == "treemap") {

      if(is.null( click_viz$id)) return(NULL)

      dt <- list("Country"= click_viz$id)
      df_filtered <- filtering_list_side(data_down(),dt)
      df_filtered$`Tender Title` <- str_trunc(  df_filtered$`Tender Title`,100,"right")

      if(!is.null(click_viz$cat)) {
        if(click_viz$cat == "Others"){

          dt_temp_others <- others_treemap$list_country_drug |> filter(Country == dt[[1]])
          dt <- list("Drug type"= dt_temp_others$`Drug type`)
          df_filtered <- filtering_list_side(df_filtered,dt)
        }
        else{
         dt <- list("Drug type" = click_viz$cat)
         df_filtered <- filtering_list_side(df_filtered,dt)
        }

      }
      df_filtered <- df_filtered |> head(10000)


      tx <- creating_detail_data(df_filtered ,  click_viz$id, actual_but$active)



    }
    if(!is.null(tx)) { if(nrow(tx)==0)
      tx = NULL
    }

    tx


  })

  output$dt_viz_side <- DT::renderDataTable({
    req(data_side())

    tx <- data_side()

    colnames(tx) <- c("")

    dtable <- DT::datatable(tx,
                            rownames = F,
                            class="stripe hover",
                            selection = 'none',
                            escape = FALSE,
                            height = 690,
                            options = list(
                              pageLength = 10,
                              dom = '<>t<"bottom"p>',
                              scrollCollapse = T,
                              fixedColumns = TRUE,
                              fixedHeader = TRUE,
                              paging = TRUE#,

                              #lengthPage = 2

                            ))

    dtable
  })




}

shinyApp(ui, server)
