#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param scientific_name taxonomic name of the product
#' @param area catches area mentionned for the product
#' @examples
#' pre.score.2<-system.2(sci_name='SOLEA SOLEA',area='27')
#' @export
#'
system.2 <- function(sci_name=NULL,area=NULL,stockdef=NULL,limits=NULL,fishdata=NULL) {

an_6<-2018 # We take into account time series from 2023 to 2017 (6 year)

if (is.null(stockdef) && is.null(limits) && is.null(fishdata))
{
    stockdef<-stockdef.other()

  limits<-rbind(limits.noaa(),limits.other(),limits.ices()) #manque limit.ices()

  fishdata<-rbind(fishdata.noaa(),fishdata.other(),fishdata.ices()) #manque fishdata.ices()

  sf::sf_use_s2(FALSE)


  fishdata %>% filter(year>=an_6) %>% group_by(fishstock) %>%
    summarize(evaluationyear=max(evaluationyear)) ->last.Eval.year

  stockdef %>% right_join(
    fishdata %>% filter(year>=an_6) %>% inner_join(last.Eval.year) %>%
      inner_join (limits) %>%
      mutate(f_fmsy=meanf/fmsy,b_bmsy=ssb/msybtrigger) %>%
      group_by(fishstock) %>% summarize(mean.f_fmsy=mean(f_fmsy,na.rm=TRUE),mean.b_bmsy=mean(b_bmsy,na.rm=TRUE),road.1=mean.b_bmsy<0.8,
              road.2=mean.b_bmsy>=0.8 && !is.na(mean.f_fmsy),
              road.3=mean.b_bmsy>=0.8 && is.na(mean.f_fmsy),
              road.4=is.na(mean.b_bmsy) && !is.na(mean.f_fmsy) ,nb.eval=n(),eval.year=mean(evaluationyear,na.rm=TRUE),
              f_fmsy=paste(f_fmsy,collapse='/'),b_bmsy=paste(b_bmsy,collapse='/'))) %>%
    mutate(roadall=as.numeric(coalesce(road.1,0))+as.numeric(coalesce(road.2,0))+as.numeric(coalesce(road.3,0))+as.numeric(coalesce(road.4,0))) %>%
    filter(roadall!=0)  ->system2.dta


if (!is.null(sci_name)) {system2.dta %>% filter(scientific_name==toupper(sci_name),grepl(toupper(area), sub_division_fao))->results}
  else(results<-system2.dta )

g1<-ggplot(results)+geom_sf(aes(fill=fishstock))
  print(g1)


return(results)
}

}
