#' Get limits values for  stocks provided by NOAA stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-limits.noaa(Stock_Name='Albacore - North Atlantic',Assessment_Year=2020)
#' @export
#'
limits.noaa <- function(Stock_Name=NULL,Assessment_Year=NULL,update=FALSE) {

  if (update) {
    get_latest_full_assessment(itis = NULL) # To replace StockassessmentData and stockAssessmentSummary
  }

  names(stockAssessmentSummary)<-gsub(' ','_',names(stockAssessmentSummary))


  limits<-stockAssessmentSummary %>% dplyr::mutate(evaluationyear=Assessment_Year,workinggroup=Jurisdiction,fishstock=Stock_Name,flim=Flimit,
                                            fpa=Ftarget,blim=Blimit,bpa=NA,fmsy=Fmsy,msybtrigger=Bmsy) %>%
    dplyr::select(evaluationyear,workinggroup,fishstock,flim,fpa,blim,bpa,fmsy,msybtrigger) %>%
    dplyr::group_by(evaluationyear,workinggroup,fishstock) %>%
    dplyr::summarize(flim=mean(flim),fpa=mean(fpa),blim=mean(blim),bpa=mean(bpa),fmsy=mean(fmsy),msybtrigger=mean(msybtrigger))
  return(limits)
}
