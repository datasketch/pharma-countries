#TODO try catch

#' @import dplyr
#' @export
filtering_list <- function(df,list_params=NULL){
  #A tomar en cuenta: si el valor en la lista es nulo no hace filtración, no evalua UPPER/TOUPPER; LIKE, solo "in"

  # list_params=list(id_country = NULL, id_anio=2010)
  # length(list_params)
  # names(list_params[1])

  for(i in 1:length(list_params)) {

    df$temp <-  df[[names(list_params[i])]]
    if(!is.null(list_params[[i]]))  df  <- df  |> dplyr::filter( temp %in% list_params[[i]]) |>  dplyr::select(!temp)

  }
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
    if(operation=="mean")  df <- meaning_r(df,group_by_viz,variable_viz)
  }

  df
}

#' @import dplyr
#' @export
selecting_viz_typeGraph <- function(df,type_viz) {
  # Vizualizaciones requeridas:Clorepethc  Line  Bar   treemap   table, se pueden dejar en un solo if las que no necesitan desagregacion
  prex <- "YeaNum"
  if(type_viz=="map") {  prex <- "CatNum" }
  if(type_viz=="line") {
    if(ncol(df) > 2)  prex <- "CatYeaNum"
  }
  prex
}


