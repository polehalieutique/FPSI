#' Get fishadata values for stocks provided by NOAA stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-fishdata.noaa(Stock_Name='Albacore - North Atlantic',Assessment_Year=2020)
#'
#' @export
#'
fishdata.noaa <- function(Stock_Name=NULL,Assessment_Year=NULL,update=FALSE) {
#a rajouter les filtres
if (update) {
  get_latest_full_assessment(itis = NULL) # To replace StockassessmentData and stockAssessmentSummary
}

  names(stockAssessmentData)<-gsub(' ','_',names(stockAssessmentData))
  names(stockAssessmentSummary)<-gsub(' ','_',names(stockAssessmentSummary))

  names(stockAssessmentSummary)
  stockAssessmentSummary %>% select(Stock_Name,Assessment_Year,B_Year,Estimated_B) %>%
    mutate(AssessmentYear=Assessment_Year,StockName=Stock_Name,Year=B_Year,tbiomass=Estimated_B) %>%
    select(AssessmentYear,StockName,Year,tbiomass)->extract_biomass



    stockAssessmentData %>% dplyr::select(AssessmentYear,Jurisdiction,Stockid,StockName,Year,Metric,Value) %>%
    dplyr::group_by(Year,Jurisdiction,Stockid,StockName,AssessmentYear,Metric) %>% dplyr::summarise(Value=mean(Value)) %>%
    pivot_wider(names_from =Metric,values_from=Value) %>% arrange(Stockid,AssessmentYear,Year) %>%
      left_join(extract_biomass)->fisdata.new

  fishdata<-fisdata.new %>%ungroup() %>%
    dplyr::mutate(evaluationyear=AssessmentYear,workinggroup=Jurisdiction,fishstock=StockName,year=Year,
                  recruitment=Recruitment,tbiomass=tbiomass,ssb=NA,landings=Catch,yieldssb=NA,
                  meanf=Fmort,sop=NA,discards=NA,low_f=NA,high_f=NA) %>%
    dplyr::select(evaluationyear,workinggroup,fishstock,year,recruitment,tbiomass,ssb,landings,yieldssb,meanf,sop,discards,low_f,high_f)%>% dplyr::group_by(evaluationyear,workinggroup,fishstock,year) %>%
    dplyr::summarise(recruitment=mean(recruitment),tbiomass=mean(tbiomass),ssb=mean(ssb),landings=mean(landings),yieldssb=mean(yieldssb),meanf=mean(meanf),sop=mean(sop),discards=mean(discards),low_f=mean(low_f),high_f=mean(high_f))

  return(fishdata)
}
