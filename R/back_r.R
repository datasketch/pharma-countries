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
    tx <- creating_detail_data(df_filtered , input$lflt_viz_shape_click$id, actual_but$active)
    tx
  }
  if (actual_but$active == "line") {
    if(is.null(input$hcClicked$id)) return(tx)

    dt <- list("tender_year"=input$hcClicked$id)
    df_filtered <- filtering_list(data_down(),dt)
    print(df_filtered |> head(1))
    print(nrow(df_filtered))
    tx <- creating_detail_data(df_filtered , input$hcClicked$id, actual_but$active)
    tx
  }

  if (actual_but$active == "bar") {
    if(is.null(input$hcClicked$id)) return(tx)
    dt <- list("ATC.product_name"=input$hcClicked$id)
    df_filtered <- filtering_list(data_down(),dt)
    print(nrow(df_filtered))
    tx <- creating_detail_data(df_filtered , input$hcClicked$id, actual_but$active)
    tx
  }

  if (actual_but$active == "treemap") {

    if(is.null(input$hcClicked$id)) return(tx)
    dt <- list("country"=input$hcClicked$id)
    df_filtered <- filtering_list(data_down(),dt)
    print(nrow(df_filtered))
    tx <- creating_detail_data(df_filtered , input$hcClicked$id, actual_but$active)
    tx
  }
  # print(tx)
  if(tx == "" | is.null(tx))
    tx ="No information available"
  tx
})
