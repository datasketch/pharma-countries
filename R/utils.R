#TODO try catch

#' @import dplyr
#' @export
filtering_list <- function(df, list_params = NULL, seq_var=NULL){
  #A tomar en cuenta: si el valor en la lista es nulo no hace filtración, no evalua UPPER/TOUPPER; LIKE, solo "in"
  #Descarta parametros NULL y All al momento de filtrar
  # list_params=list(id_country = NULL, id_anio=2010)
  # length(list_params)
  # names(list_params[1])
  if(is.null(seq_var)) seq_var=""

  list_params= list_params[-4]
  for(i in 1:length(list_params)) {
    if(!is.null(names(list_params[i]))){
       if(!is.null(list_params[[ i ]])) {
            df$temp <-  df[[names(list_params[i])]]
            if(names(list_params[i])!=seq_var) {

              if(list_params[[ i ]] != "All")   df  <- df  |> dplyr::filter( temp %in% list_params[[i]]) |>  dplyr::select(!temp)

            }
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
gen_html_detail_to_modal <- function(id,list_block,...) {
  idb <- paste("b",id,sep="")
  idm <- paste("m",id, sep="")
  idc <- paste("c",id, sep="")
  # myBtn <- "afganistan"
  open_style <- ("<style>")
  c <- paste(".",idc)
  c1 <-"{
  color: #aaaaaa;
  float: right;
  font-size: 28px;
  font-weight: bold;
 }"
  c11 <- paste(".",idc,":hover,.",idc,":focus")
  c111 <- "{
  color: #000;
  text-decoration: none;
  cursor: pointer;
  </style>"
  style_c <- paste(open_style,c ,c1,c111)
  button_modal <- paste(style_c ,"<button class='button-modal-det fa fa-info-circle' role='presentation' aria-label='info-circle icon' id='", idb,"'></button>", sep="")
  div_modal <- paste("<div id='", idm,"' class='modal-c'", sep="")
  div_modal_content <- "<div class='modal-content-c'>"
  span_modal <- paste("<span class='", idc,"'>&times;</span>", sep="")
  p_modal <- paste("<p>", list_block,"</p>")
  div_model_close_content <- "</div>"
  div_model_close <- "</div>"

  open_script <- '<script  type="text/javascript">'
  var1 <- paste("var ", idb,' = document.getElementById("', idb,'");', sep="")
  var2 <- paste("var ", idm,' = document.getElementById("', idm,'");', sep="")
  var3 <- paste("var ", idc,' = document.getElementsByClassName("', idc,'")[0];', sep="")
  varn <- paste(var1,var2,var3,sep="\n")
  fb1 <-  paste(idb,".onclick = function() {",sep="")
  fb11 <- paste(idm,'.style.display = "block";',sep="")
  fb111 <- "}"
  fbn <- paste(fb1,fb11,fb111,sep="\n")
  fc1 <-  paste(idc,".onclick = function() {",sep="")
  fc11 <- paste(idm,'.style.display = "none";',sep="")
  fc111 <- "}"
  fcn <- paste(fc1,fc11,fc111,sep="\n")
  w1 <- " window.onclick = function(event) {"
  w11 <- paste("if (event.target == ", idm, " ){" )
  w111 <- paste(idm,'.style.display = "none";',sep="")
  w1111 <- " } } </script>"
  wn <- paste(w1,w11,w111,w1111,sep="\n")
  jsblk <-paste( open_script, varn,fbn,fcn,wn,sep="\n")
  block_modal <- paste(button_modal,div_modal,div_modal_content, span_modal,p_modal,div_model_close_content, div_model_close, sep = " ")
  block_modal2 <- paste(block_modal,jsblk,sep="\n" )
  block_modal2
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
        v <- append(v,paste0("<div style='display: inline-flex; color:black !important;'><B>",da[j,i],"</B></div>"))

      }
      # if(colnames(da[i])==modal_col_show ) {  # Esta sección llamará al modal
      #   v <- append(v,paste0("<B>",colnames(da[i]),"</B>",": ", to_modal_link(da[j,i])))
      #
      # }
      else {
        a= stringi::stri_trim(da[j,i])
        if(is.numeric(da[j,i])) a = format(round(as.numeric(da[j,i]), 1), big.mark=",",big.interval=3)
        v <- append(v,paste("<div class='bloque_html_det-line' style='display: inline-flex;'> <div style='color:#3695D8 !important;'>&nbsp;&nbsp;",
                            stringi::stri_trim(colnames(da[i])),"</div>","<div><p>: ",a,"</p></div></div>", sep=""))

      }
    }
    list_temp <- paste(v, "</BR>",collapse = " ")
    # list_temp <- paste(list_temp,gen_html_detail_to_modal(paste("m",j, sep=""),list_temp))
    list_temp2 <- paste(list_temp2,"<div class='bloque_html_det'>",list_temp, "</div></BR> </BR>")
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

#' @import dplyr
#' @export
counting_r <- function(df,colname_group1, colname_group2=NULL, na_control=NULL){
  ######print(colname_group1)
  ######print(names(df))
  if(is.null(colname_group2)){
    df <- df %>% group_by(across(all_of(colname_group1))) %>% summarize(count =n())
  }else{

    df <- df %>% group_by(across(all_of(colname_group1)),across(all_of(colname_group2))) %>% summarize(count =n())
  }

  if(!is.null(na_control)) { df <- df |> dplyr::filter(!is.na(count)) }

  df
}

