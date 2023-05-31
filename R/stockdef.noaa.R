#' Get limits values for  stocks provided by NOAA stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-limits.noaa(Stock_Name='Albacore - North Atlantic',Assessment_Year=2020)
#' @export
#'
stockdef.noaa <- function(Stock_Name=NULL,Assessment_Year=NULL,update=FALSE) {

  if (update) {
    get_latest_full_assessment(itis = NULL) # To replace StockassessmentData and stockAssessmentSummary
  }

  names(stockAssessmentSummary)<-gsub(' ','_',names(stockAssessmentSummary))
data("asfis")
asfis %>% dplyr::distinct(species_code,scientific_name) %>% dplyr::mutate(scientific_name=toupper(scientific_name))->spec.code

  stockdef.noaa.dta<-stockAssessmentSummary %>% dplyr::mutate(fishstock=Stock_Name,sub_division_fao=toupper(Stock_Area),scientific_name=toupper(Scientific_Name)) %>%
    dplyr::distinct(fishstock,sub_division_fao,scientific_name) %>%
    left_join(spec.code) %>% select(fishstock,species_code,sub_division_fao,scientific_name)

  return(stockdef.noaa.dta)
}
