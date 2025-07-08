#' Get limits values for  stocks provided by NOAA stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-limits.noaa(Stock_Name='Albacore - North Atlantic',update=TRUE)
#' @export
#'
limits.noaa <- function(Stock_Name=NULL,Assessment_Year=NULL,update=FALSE) {

  if (update) {
    get_latest_full_assessment(itis = NULL) # To replace StockassessmentData and stockAssessmentSummary
  }

  names(stockAssessmentSummary)<-gsub(' |/','_',names(stockAssessmentSummary))
  stockAssessmentData %>% filter(Metric=='Fmort') %>%
    mutate(Stock_Name=StockName,Assessment_Year=AssessmentYear) %>%
    distinct(Assessment_Year,Stock_Name,Metric,Units)->F_type



#Passer en left_join et voir comment cele se passe
  limits<-stockAssessmentSummary %>% left_join(F_type) %>% dplyr::mutate(evaluationyear=Assessment_Year,workinggroup=Jurisdiction,fishstock=Stock_Name,flim=Flimit,
                                            fpa=Ftarget,blim=Blimit,bpa=NA,
                                            fmsy=case_when(Metric=='Fmort'  & Units %in% c('Rate','Ratio') ~1,
                                                        is.na(Estimated_F) & !is.na(F_Fmsy) ~1,
                                                                              TRUE ~Fmsy),msybtrigger=Bmsy,references=Citation) %>%
    dplyr::select(evaluationyear,workinggroup,fishstock,flim,fpa,blim,bpa,fmsy,msybtrigger,references) %>%
    dplyr::group_by(evaluationyear,workinggroup,fishstock,references) %>%
    dplyr::summarize(flim=mean(flim),fpa=mean(fpa),blim=mean(blim),bpa=mean(bpa),fmsy=mean(fmsy),msybtrigger=mean(msybtrigger)) %>%
    mutate(FishingPressure=NA,FishingPressureDescription=NA)


  return(limits)
}

