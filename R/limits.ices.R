#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-limits.ices(Stock_Name='Albacore - North Atlantic',Assessment_Year=2020)
#' @export
#'
limits.ices <- function(Stock_Name=NULL,Assessment_Year=NULL,update=FALSE,from=2017,to=NULL) {

  if (update)
  {
    if (is.null(to)) {to=as.numeric(format(Sys.time(), "%Y"))}
    assessments <- getListStocks(seq(from,to))

    assessments %>%  dplyr::filter(Purpose=="Advice")->assessments



    masliste<-lapply(getFishStockReferencePoints(assessments$AssessmentKey), function(x) x%>% select(matches("AssessmentKey|StockKeyLabel|StockDatabaseID|StockKey|AssessmentYear|FLim|Fpa|Bpa|Blim|FMSY|MSYBtrigger")))

    limits.tmp  <- do.call("bind_rows",masliste)
    head(limits.tmp)
    head(limits)
    limits.tmp %>% dplyr::mutate(evaluationyear=AssessmentYear,workinggroup='ICES',fishstock=StockKeyLabel,flim=FLim,fpa=Fpa,blim=Blim,bpa=Bpa,fmsy=FMSY,msybtrigger=MSYBtrigger) %>%
      dplyr::select(evaluationyear,workinggroup,fishstock,flim,fpa,blim,bpa,fmsy,msybtrigger)->limits.ices.dta


    return(limits.ices.dta)
  }
  else
  {
    data(limits.ices.dta)
    return(limits.ices.dta) #I can store data within the package as stocksmart one
      }

}
