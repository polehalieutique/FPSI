#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param scientific_name taxonomic name of the product
#' @param area catches area mentionned for the product
#' @examples
#' pre.score.2<-system.2(sci_name='MOLVA MOLVA',area='27.1')
#' @export
#'
system.2 <- function(sci_name=NULL,area=NULL,stockdef=NULL,limits=NULL,fishdata=NULL) {

an_6<-2017 # We take into account time series from 2023 to 2017 (6 year)

if (is.null(stockdef) && is.null(limits) && is.null(fishdata))
{
  stockdef<-stockdef.other()

  limits<-rbind(limits.noaa(),limits.other(),limits.ices())

  fishdata<-rbind(fishdata.noaa(),fishdata.other(),fishdata.ices())
}

  sf::sf_use_s2(FALSE)
#To obtain last available Evaluationyear by stock
  fishdata %>%  dplyr::filter(evaluationyear>an_6) %>%  dplyr::group_by(fishstock) %>%
    dplyr::summarize(evaluationyear=max(evaluationyear)) ->last.Eval.year
#To obtain for the last EvaluationYear the last 6 years of the time serie
fishdata %>%  dplyr::inner_join(last.Eval.year) %>% dplyr::group_by(fishstock,evaluationyear) %>%
  dplyr::summarize(maxyear=max(year)-6)->last.ts.year


#Modification to take into account Fishing pressure alternatives

system2.dta<- stockdef %>%
  right_join(
    fishdata %>% left_join(fishdata_ext.ices.dta) %>%  inner_join(last.ts.year) %>% dplyr::filter(year>maxyear) %>%
      dplyr::inner_join (limits) %>%
      dplyr::mutate(f_fmsy=case_when(is.na(FishingPressureDescription) | FishingPressureDescription=='F' ~meanf/fmsy,
                              FishingPressureDescription %in% c('HRrel','Frel','Harvest rate')~ FishingPressure),
                    msybtrigger_or_prox=case_when(!is.na(msybtrigger)~ msybtrigger,is.na(msybtrigger) & !is.na(bpa)~bpa),  b_bmsy=ssb/msybtrigger_or_prox,
                    catch.advice=catches/catches_advice,
             FishingPressureDescription=case_when(!is.na(FishingPressureDescription)~FishingPressureDescription,
                                                                                           TRUE ~ 'F')) %>%
            dplyr::group_by(fishstock,FishingPressureDescription) %>% dplyr::summarize(mean.f_fmsy=mean(f_fmsy,na.rm=TRUE),mean.b_bmsy=mean(b_bmsy,na.rm=TRUE),
                                                                                       mean.catch.advice=mean(catch.advice,na.rm=TRUE),road.1=mean.b_bmsy<0.8,
              road.2=mean.b_bmsy>=0.8 && !is.na(mean.f_fmsy),
              road.3=mean.b_bmsy>=0.8 && is.na(mean.f_fmsy),
              road.4=is.na(mean.b_bmsy) && !is.na(mean.f_fmsy) ,
              road.5=is.na(mean.b_bmsy) && is.na(mean.f_fmsy) && !is.na(mean.catch.advice),
              nb.eval=n(),eval.year=mean(evaluationyear,na.rm=TRUE),
              f_fmsy=paste(f_fmsy,collapse='/'),b_bmsy=paste(b_bmsy,collapse='/'))) %>%
    dplyr::mutate(roadall=as.numeric(coalesce(road.1,0))+as.numeric(coalesce(road.2,0))+as.numeric(coalesce(road.3,0))+as.numeric(coalesce(road.4,0))+as.numeric(coalesce(road.5,0)))
  #filter(roadall!=0)

if (!is.null(sci_name)) {

  system2.dta %>% dplyr::filter(scientific_name==toupper(sci_name) & (grepl(paste(area,'.',sep=''),paste(sub_division_fao,' ',sep=''),fixed=TRUE)
| grepl(paste(area,' ',sep=''),paste(sub_division_fao,' ',sep=''),fixed=TRUE)))->results
  }
  else(results<-system2.dta )

#g1<-ggplot(results)+geom_sf(aes(fill=fishstock))
#  print(g1)


return(results)


}
