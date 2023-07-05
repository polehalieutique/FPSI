#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-limits.ices(Stock_Name='Albacore - North Atlantic',Assessment_Year=2020)
#' @export
#'
limits.ices <- function(Stock_Name=NULL,Assessment_Year=NULL,update=FALSE,from=2018,to=NULL) {

  if (update)
  {
    if (is.null(to)) {to=as.numeric(format(Sys.time(), "%Y"))}
    assessments <- getListStocks(seq(from,to))

    assessments %>%  dplyr::filter(Purpose=="Advice")->assessments

#En utilisant la fonction getSAG, on obtient en plus des information sur les pression (Fishing pressure) et FishingPressure Description quend on est pas que sur du F/Fmsy
#Normalement le reste est identique - A checker sur 2022 / Je fais donc la moyenne sur 6 an des Fishing pressure et je considère cela comme une nouvelle limite

    maliste_SAG<-getSAG(stock=NULL, year=seq(from,to), data = "source", combine = TRUE, purpose = "Advice")
    extractmalisteSAG<-maliste_SAG %>% select(matches("AssessmentKey|StockKeyLabel|StockDatabaseID|StockKey|AssessmentYear|Year|FLim|Fpa|Bpa|Blim|FMSY|MSYBtrigger|FishingPressure|FishingPressureDescription"))
    extractmalisteSAG %>% group_by(StockKeyLabel,AssessmentYear) %>% summarize(Year.keep=max(Year)-6)->Last.year

masliste<-    extractmalisteSAG %>% inner_join(Last.year) %>% dplyr::filter(Year>=Year.keep) %>%
  dplyr::group_by(AssessmentKey,StockKeyLabel,StockDatabaseID,StockKey,AssessmentYear,FishingPressureDescription) %>%
      dplyr::summarise(Flim=mean(Flim,na.rm=TRUE),Fpa=mean(Fpa,na.rm=TRUE),Bpa=mean(Bpa,na.rm=TRUE),Blim=mean(Blim,na.rm=TRUE),FMSY=mean(FMSY,na.rm=TRUE),MSYBtrigger=mean(as.numeric(MSYBtrigger),na.rm=TRUE),FishingPressure=mean(as.numeric(FishingPressure),na.rm=TRUE),.groups = "drop")






    # masliste<-lapply(getFishStockReferencePoints(assessments$AssessmentKey), function(x) x%>% select(matches("AssessmentKey|StockKeyLabel|StockDatabaseID|StockKey|AssessmentYear|FLim|Fpa|Bpa|Blim|FMSY|MSYBtrigger")))
    #
    # limits.tmp  <- do.call("bind_rows",masliste)
    # head(limits.tmp)
    # head(limits)
#
#     limits.tmp %>% dplyr::mutate(evaluationyear=AssessmentYear,workinggroup='ICES',fishstock=StockKeyLabel,flim=FLim,fpa=Fpa,blim=Blim,bpa=Bpa,fmsy=FMSY,msybtrigger=MSYBtrigger) %>%
#       dplyr::select(evaluationyear,workinggroup,fishstock,flim,fpa,blim,bpa,fmsy,msybtrigger,FishingPressure,FishingPressureDescription)->limits.ices.dta

    masliste %>% dplyr::mutate(evaluationyear=AssessmentYear,workinggroup='ICES',fishstock=StockKeyLabel,flim=Flim,fpa=Fpa,blim=Blim,bpa=Bpa,fmsy=FMSY,msybtrigger=MSYBtrigger) %>%
      dplyr::select(evaluationyear,workinggroup,fishstock,flim,fpa,blim,bpa,fmsy,msybtrigger,FishingPressure,FishingPressureDescription)->limits.ices.dta

    return(limits.ices.dta)
  }
  else
  {
    data(limits.ices.dta)
    return(limits.ices.dta) #I can store data within the package as stocksmart one
      }

}
