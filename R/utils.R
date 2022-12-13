

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


#a=filter_list(data,list(tender_value_currency="KZT", tender_year=2016))
