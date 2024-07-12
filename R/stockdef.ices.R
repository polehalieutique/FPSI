#' Get limits values for  stocks provided by NOAA stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' stockdef.ices.dta<-stockdef.ices(update=TRUE)
#'
#' @export
#'
stockdef.ices <- function(update=FALSE) {

  data(asfis)

  if (update)
  {
    if (is.null(to)) {to=as.numeric(format(Sys.time(), "%Y"))}
    assessments <- StockList(seq(from,to))

    assessments %>%  dplyr::filter(Purpose=="Advice")->assessments

    #En utilisant la fonction getSAG, on obtient en plus des information sur les pression (Fishing pressure) et FishingPressure Description quend on est pas que sur du F/Fmsy
    #Normalement le reste est identique - A checker sur 2022 / Je fais donc la moyenne sur 6 an des Fishing pressure et je considère cela comme une nouvelle limite

    maliste_SAG<-getSAG(stock=NULL, year=seq(from,to), data = "source", combine = TRUE, purpose = "Advice")

    stockdef.ices.dta<-
      maliste_SAG %>% dplyr::select(matches("StockKeyLabel|ICES_Areas")) %>% distinct() %>%
      dplyr::mutate(fishstock=StockKeyLabel,species_code=toupper(substr(StockKeyLabel,1,3)),scientific_name=NULL,sub_division_fao=toupper(gsub('~','/',ICES_Areas))) %>%
      dplyr::left_join(asfis %>% select(scientific_name,species_code) %>% dplyr::mutate(scientific_name=toupper(scientific_name))) %>%
      dplyr::select(fishstock,species_code,scientific_name,sub_division_fao)



    return(stockdef.ices.dta)
  }
  else
  {
    data(stockdef.ices.dta)
    return(stockdef.ices.dta) #I can store data within the package as stocksmart one
  }


}
