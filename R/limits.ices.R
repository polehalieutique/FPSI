#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-limits.ices(Stock_Name='Albacore - North Atlantic',Assessment_Year=2020)
#' @export

limits.ices <- function(Stock_Name=NULL,Assessment_Year=NULL,update=FALSE,from=2018,to=NULL,average.nbyear=NULL,exclude=NULL) {

  if (update)
  {
    if (is.null(average.nbyear)) {average.nbyear=6}
    if (is.null(exclude)) {exclude=c(-1)}
    if (is.null(to)) {to=as.numeric(format(Sys.time(), "%Y"))}

    assessments <- StockList(seq(from,to))
    #Deprectated function ass.old<-getListStocks(seq(from,to))

    assessments %>%  dplyr::filter(Purpose=="Advice")->assessments

#En utilisant la fonction getSAG, on obtient en plus des information sur les pression (Fishing pressure) et FishingPressure Description quend on est pas que sur du F/Fmsy
#Normalement le reste est identique - A checker sur 2022 / Je fais donc la moyenne sur 6 an des Fishing pressure et je considère cela comme une nouvelle limite

    maliste_SAG<-getSAG(stock=NULL, year=seq(from,to), data = "source", combine = TRUE, purpose = "Advice")
#On est obligé de créer un diverticule pour ce stock qui est évalué en 3 sous stocks / C'est sur la même zone

      #Attention spécificité sur les 3 sous stock  cod.27.46a7d20 3 évaluations pour le même stock
    #Après discussion avec Youen je ne garde que le stock sud qui correspond à ce qui a été gardé pour l'avis scientifique
    #C'est la partie du stock la plus en mauvais état et qui conduit à l'avis le plus précautioneux
    #A changer quand on aura les évals de stock 2024
    #J(enleve aussi un stock ane.27.9 il y en a 2 et 2024)
    #maliste_SAG %>% filter(StockKeyLabel=='ane.27.9a')
    maliste_SAG<-maliste_SAG %>% filter(!AssessmentKey %in% exclude)

#    maliste_SAG<-getSAG(stock="cod.27.1-2.coastN", year=2024, data = "source", combine = TRUE, purpose = "Advice")

        extractmalisteSAG<-maliste_SAG %>% select(matches("AssessmentKey|StockKeyLabel|StockDatabaseID|StockKey|AssessmentYear|Year|FLim|Fpa|Bpa|Blim|FMSY|MSYBtrigger|FishingPressure|FishingPressureDescription|Fmanagement"))
    extractmalisteSAG %>% dplyr::group_by(StockKeyLabel,AssessmentYear) %>% dplyr::summarize(Year.keep=max(as.integer(Year))-average.nbyear)->Last.year

masliste<-    extractmalisteSAG %>% inner_join(Last.year) %>% dplyr::filter(Year>=Year.keep) %>%
  dplyr::group_by(AssessmentKey,StockKeyLabel,StockDatabaseID,StockKey,AssessmentYear,FishingPressureDescription) %>%
      dplyr::summarise(Flim=mean(as.numeric(Flim),na.rm=TRUE),Fpa=mean(as.numeric(Fpa),na.rm=TRUE),Bpa=mean(as.numeric(Bpa),na.rm=TRUE),Blim=mean(as.numeric(Blim),na.rm=TRUE),FMSY=mean(as.numeric(FMSY),na.rm=TRUE),MSYBtrigger=mean(as.numeric(MSYBtrigger),na.rm=TRUE),FishingPressure=mean(as.numeric(FishingPressure),na.rm=TRUE),Fmanagement=as.numeric(Fmanagement),.groups = "drop")




    # masliste<-lapply(getFishStockReferencePoints(assessments$AssessmentKey), function(x) x%>% select(matches("AssessmentKey|StockKeyLabel|StockDatabaseID|StockKey|AssessmentYear|FLim|Fpa|Bpa|Blim|FMSY|MSYBtrigger")))
    #
    # limits.tmp  <- do.call("bind_rows",masliste)
    # head(limits.tmp)
    # head(limits)
#
#     limits.tmp %>% dplyr::mutate(evaluationyear=AssessmentYear,workinggroup='ICES',fishstock=StockKeyLabel,flim=FLim,fpa=Fpa,blim=Blim,bpa=Bpa,fmsy=FMSY,msybtrigger=MSYBtrigger) %>%
#       dplyr::select(evaluationyear,workinggroup,fishstock,flim,fpa,blim,bpa,fmsy,msybtrigger,FishingPressure,FishingPressureDescription)->limits.ices.dta

    masliste %>% dplyr::mutate(evaluationyear=as.numeric(AssessmentYear),workinggroup='ICES',fishstock=StockKeyLabel,flim=Flim,fpa=Fpa,blim=Blim,bpa=Bpa,fmsy=FMSY,msybtrigger=MSYBtrigger,Fmanagement) %>%
      dplyr::select(evaluationyear,workinggroup,fishstock,flim,fpa,blim,bpa,fmsy,msybtrigger,FishingPressure,FishingPressureDescription,Fmanagement)->limits.ices.dta

    return(limits.ices.dta)
  }
  else
  {
    data(limits.ices.dta)
    return(limits.ices.dta) #I can store data within the package as stocksmart one
      }

}
