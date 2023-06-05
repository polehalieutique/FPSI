#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-limits.ices(Stock_Name='Albacore - North Atlantic',Assessment_Year=2020)
#' @export
#'
fishdata.ices <- function(Stock_Name=NULL,Assessment_Year=NULL,update=FALSE,from=2017,to=NULL) {

  if (update)
  {
  if (is.null(to)) {to=as.numeric(format(Sys.time(), "%Y"))}
    assessments <- getListStocks(seq(from,to))
    assessments %>%  dplyr::filter(Purpose=="Advice")->assessments
    fishdata.tmp  <- do.call("rbind",getSummaryTable(assessments$AssessmentKey))
      fishdata.tmp %>% dplyr::mutate(evaluationyear=AssessmentYear,workinggroup='ICES',year=Year,ssb=as.numeric(SSB),meanf=as.numeric(F),low_f=as.numeric(low_F),high_f=as.numeric(high_F),tbiomass=NA,yieldssb=NA,sop=NA) %>%
        dplyr::select(evaluationyear,workinggroup,fishstock,year,recruitment,tbiomass,ssb,landings,yieldssb,meanf,sop,discards,low_f,high_f)->fishdata.ices.dta
return(fishdata.ices.dta)
  }
  else
  {
    data(fishdata.ices.dta)
    return(fishdata.ices.dta) #I can store data within the package as stocksmart one
      }




}
