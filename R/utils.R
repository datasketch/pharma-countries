#TODO try catch

#' @import dplyr
#' @export
filtering_list <- function(df, list_params = NULL, seq_var=NULL){
  #A tomar en cuenta: si el valor en la lista es nulo no hace filtración, no evalua UPPER/TOUPPER; LIKE, solo "in"

  # list_params=list(id_country = NULL, id_anio=2010)
  # length(list_params)
  # names(list_params[1])
  if(is.null(seq_var)) seq_var=""

  list_params= list_params[-4]
  for(i in 1:length(list_params)) {
    if(!is.null(names(list_params[i]))){
        if(!is.null(list_params[[ i ]])) {
          df$temp <-  df[[names(list_params[i])]]
          if(names(list_params[i])!=seq_var)  df  <- df  |> dplyr::filter( temp %in% list_params[[i]]) |>  dplyr::select(!temp)
          else  df  <- df  |> dplyr::filter( temp >= list_params[[i]][1] & temp <= list_params[[i]][2] ) |>  dplyr::select(!temp)
        }
     }
  }
  ##print("Outlopp")
  df

}



#' @import dplyr
#' @export
meaning_r <- function(df,colname_group1,mean_variable="",colname_group2=NULL) {

  df$temp <-  df[[mean_variable]]
   if(is.null(colname_group2)) {

    df <- df |> group_by(across(all_of(colname_group1))) %>% summarize(mean = mean(temp ,na.rm=TRUE))
   }
   else{

    df <- df|> group_by(across(all_of(colname_group1)),across(all_of(colname_group2))) %>% summarize(mean =mean(temp,na.rm=TRUE))
   }
  df
}

#meaning_r(data,,colname_group1="country",mean_variable="tender_value_amount")
#a=filter_list(data,list(tender_value_currency="KZT", tender_year=2016))

#' @import dplyr
#' @export
selecting_viz_data <- function(df,type_viz,variable_viz, group_by_viz, desagregation_viz=NULL, operation="mean") {

  # Vizualizaciones requeridas:Clorepethc  Line  Bar   treemap   table, se pueden dejar en un solo if las que no necesitan desagregacion


  if(type_viz=="map"){
    if(operation=="mean")  df <- meaning_r(df,group_by_viz,variable_viz)
  }

  if(type_viz=="line"){ #add format graph hchmagic CatYeaNUm
    if(operation=="mean") {

      if(is.null(desagregation_viz)) df <- meaning_r(df,group_by_viz,variable_viz)
      else df <- meaning_r(df,desagregation_viz,variable_viz,group_by_viz)
    }
  }

  if(type_viz=="bar"){
    if(operation=="mean")  df <- meaning_r(df,group_by_viz,variable_viz)
  }

  if(type_viz=="treemap"){
    if(operation=="mean")  df <- meaning_r(df, group_by_viz, variable_viz)
  }

  df
}


#' @import dplyr
#' @export
selecting_viz_typeGraph <- function(df, type_viz) {
  # Vizualizaciones requeridas:Clorepethc  Line  Bar   treemap   table, se pueden dejar en un solo if las que no necesitan desagregacion
  prex <- "YeaNum"
  if(type_viz=="map") {  prex <- "GnmNum" }
  if(type_viz=="line") {
    if(ncol(df) > 2)  prex <- "CatYeaNum"
  }
  if(type_viz=="bar" | type_viz=="treemap" ) prex <- "CatNum"
  prex
}


#' @import dplyr
#' @import shiny
#' @export
#'
gen_html_detail_to_modal <- function(id,list_block) {
  # myBtn <- "afganistan"
  button_modal <- paste("<button id='",id,"'>i</button>",sep="")
  div_modal <- paste("<div id='",id,"_",id,"' class='modal'", sep="")
  div_modal_content <- "<div class='modal-content'>"
  span_modal <- "<span class='close'>&times;</span>"
  p_modal <- paste("<p>",list_block,"</p>")
  div_model_close_content <- "</div>"
  div_model_close <- "</div>"
  block_modal <- paste(button_modal,div_modal,div_modal_content, span_modal,p_modal,div_model_close_content, div_model_close, sep = " ")
  block_modal
    #eEjemplo tomaddo de:
    # <!-- The Modal -->
    # <div id='myModal' class='modal'>
    #
    #   <!-- Modal content -->
    #   <div class='modal-content'>
    #     <span class='close'>&times;</span>
    #       <p>Some text in the Modal..</p>
    #       </div>
    #
    #       </div>
}


#  var_test = "<p>Some text in the Modal..</p>"
#  id="Uk paper"
# #
# ##print(gen_html_detail_to_modal(id,var_test) )
#



#' @import dplyr
#' @import shiny
#' @export
gen_html_detail <- function(df, parameters_col=NULL, colnames_show=NULL, modal_col_show=NULL, tittle_paraph=NULL) {
  # da=get_data_api("request")
  # param="Request..doc."
  if(is.null(parameters_col)) parameters_col <- c("tender_title","country","tender_value_amount","unit_price","ATC.product_name")
  if(is.null(colnames_show)) colnames_show <- c("tender_title","Country","Tender amount","Unit Price","ATC product_name")
  if(is.null(modal_col_show)) modal_col_show <- "NNN" #"ATC code" #No está implementado
  if(is.null(tittle_paraph)) tittle_paraph <- "tender_title"
  da <- dplyr::select(df,one_of(parameters_col))
  colnames(da) <- colnames_show
  v <- vector()
  list_temp2 <- ""

  for(j in 1:nrow(da)) {
    v <- NULL
    title=""
    for(i in 1:ncol(da)) {
      if(colnames(da[i]) == tittle_paraph ) {
        title=da[j,i]
        v <- append(v,paste0("<B>",da[j,i],"</B>"))

      }
      # if(colnames(da[i])==modal_col_show ) {  # Esta sección llamará al modal
      #   v <- append(v,paste0("<B>",colnames(da[i]),"</B>",": ", to_modal_link(da[j,i])))
      #
      # }
      else {
        v <- append(v,paste0("<B>",colnames(da[i]),"</B>",": ", da[j,i]))

      }
    }
    list_temp <- paste(v, "</BR>",collapse = " ")
    list_temp <- paste(list_temp,gen_html_detail_to_modal(title,list_temp))
    list_temp2 <- paste(list_temp2,list_temp, "</BR> </BR>")
  }
  list_temp2 <- shiny::HTML(list_temp2)

}

#Versión sin desagregados y basada en la variable asociada a cada tipo de gráfica
#parameters col indica cuales columnas del dataset se incluirán en el detalle hmtl
#En una nueva versión la columna estática a filtrar podría ser reemplazada por selected_col
#' @import dplyr
#' @export
creating_detail_data <- function(df, clickId, type_viz, parameters_col=NULL,selected_col=NULL, modal_col=NULL) {
  # if (is.null(clickId)) return()
  if(type_viz=="map") { #Pendiente si la colunmna se llamará country
    eval <- list(country=clickId)
    df_filtered <- filtering_list(df,eval)
    html_detail <- gen_html_detail(df, parameters_col)

  }
  if(type_viz=="line") { #Pendiente si la colunmna se llamará tender_year
    eval <- list(tender_year=clickId)
    df_filtered <- filtering_list(df,eval)
    html_detail <- gen_html_detail(df, parameters_col)

  }
  if(type_viz=="bar") { #Pendiente si la colunmna se llamará ATC.product_nam
    eval <- list(ATC.product_name=clickId)
    df_filtered <- filtering_list(df,eval)
    html_detail <- gen_html_detail(df, parameters_col)

  }
  if(type_viz=="treemap") { #Pendiente si la colunmna se llamará country
    eval <- list(country=clickId)
    df_filtered <- filtering_list(df,eval)
    html_detail <- gen_html_detail(df, parameters_col)

  }
  html_detail
}

