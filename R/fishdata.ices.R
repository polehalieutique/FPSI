#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-limits.ices(Stock_Name='Albacore - North Atlantic',Assessment_Year=2020)
#' @export
#
fishdata.ices <- function(Stock_Name=NULL,Assessment_Year=NULL,update=FALSE,from=2017,to=NULL,exclude=NULL) {

  if (update)
  {
  if (is.null(to)) {to=as.numeric(format(Sys.time(), "%Y"))}
    if (is.null(exclude)) {exclude=c(-1)}
    #library(icesSAG)
    #library(dplyr)
    #exclude=NULL
    #assessments <- StockList(seq(2024,2024))

    assessments <- getListStocks(seq(from,to))
    #Deprectated function getListStocks(seq(from,to))

    #Attention spécificité sur les 3 sous stock  cod.27.46a7d20 3 évaluations pour le même stock
    #Après discussion avec Youen je ne garde que le stock sud qui correspond à ce qui a été gardé pour l'avis scientifique
    #C'est la partie du stock la plus en mauvais état et qui conduit à l'avis le plus précautioneux
    #A changer quand on aura les évals de stock 2024
    assessments %>%  dplyr::filter(Purpose=="Advice") %>%  filter(!AssessmentKey %in% exclude)->assessments
#    assessments<-data.frame(AssessmentKey=18195)
    fishdata.tmp  <- do.call("rbind",getSummaryTable(assessments$AssessmentKey))
      fishdata.tmp %>% dplyr::mutate(evaluationyear=AssessmentYear,workinggroup='ICES',year=Year,ssb=as.numeric(SSB),meanf=as.numeric(F),low_f=as.numeric(low_F),high_f=as.numeric(high_F),tbiomass=NA,yieldssb=NA,sop=NA,fishingPressureDescription,fishingPressureUnits) %>%
        dplyr::select(evaluationyear,workinggroup,fishstock,year,recruitment,tbiomass,ssb,landings,yieldssb,meanf,sop,discards,low_f,high_f,fishingPressureDescription,fishingPressureUnits)->fishdata.ices.dta
return(fishdata.ices.dta)
  }
  else
  {
    data(fishdata.ices.dta)
    return(fishdata.ices.dta) #I can store data within the package as stocksmart one
      }




}
