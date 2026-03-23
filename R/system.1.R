#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param scientific_name taxonomic name of the product
#' @param area catches area mentionned for the product
#' @examples
#' pre.score.2<-system.1(sci_name='LOLIGO VULGARIS',area='34.1.2')
#' @export
#'
system.1 <- function(sci_name=NULL,area=NULL,iucn.dta=NULL,sensitive.dta=NULL,area.dta=NULL,iucn_to_stock_area=NULL,longitude=NULL,latitude=NULL) {

#Update the 28/11/2023
#After discussion with  David Allen from IUCN
#I will use data downloadable from   https://www.iucnredlist.org/search?permalink=f48c3b99-8900-48e8-993c-590343ad5e4f
#Regarding the "no more available" webservice
if (!is.null(iucn.dta)) {data(iucn.dta)}
if (!is.null(sensitive.dta)) {data(sensitive.dta)}
#A test where we only keep Cheung 2005 / Source code renamed C
#
#  unique(sensitive.dta$Indicator_source)
sensitive.dta %>% filter(Indicator_source!='Cheung_et_al_2007')->sensitive.dta
#On revient pre cheunc
#sensitive.dta %>% filter(Indicator_source!='Cheung_et_al_2005_sealifebase_fishbase')->sensitive.dta
sf::sf_use_s2(FALSE)



  area.req.sens<-'Global'
  area.req.sens.iucn<-'Global'
    if (substr(area,1,2) %in% c('SA','37')) {area.req.sens.iucn<-'Mediterranean'
    area.req.sens<-'37'}
  if (substr(area,1,2) %in% c('27')) {area.req.sens.iucn<-'Europe'
  area.req.sens<-'27'}

  iucn.dta %>%
    dplyr::mutate(scientific_name=scientificName) %>%
    dplyr::filter(scientific_name==sci_name,scopes==area.req.sens.iucn) %>%
    dplyr::filter(!is.na(category)) %>%
    dplyr::mutate(area.req=area)-> system1.road.6

nlignes<-dim(system1.road.6)[1]

if (nlignes==0) {iucn.dta %>%
    dplyr::mutate(scientific_name=scientificName) %>%
    dplyr::filter(scientific_name==sci_name,scopes=='Global') %>%
    dplyr::filter(!is.na(category)) %>%
    dplyr::mutate(area.req=area)-> system1.road.6
}

nlignes<-dim(system1.road.6)[1]


  if (nlignes==0) {


    result.local<-data.frame(Sensitivity_indicator=NULL)
    result.local<-sensitive.dta %>%  dplyr::filter(scientific_name==sci_name & area.req==area.req.sens)%>%
      dplyr::mutate(id_no=NA,yrcompiled=NA,freshwater=NA,category=NA) %>%
      dplyr::select (id_no,scientific_name,yrcompiled,freshwater,category,Sensitivity_indicator,source_code,area.req)


    result.global<-sensitive.dta %>%
      dplyr::filter(scientific_name==sci_name & area.req=='Global') %>% dplyr::mutate(id_no=NA,yrcompiled=NA,freshwater=NA,category=NA) %>%
      dplyr::select (id_no,scientific_name,yrcompiled,freshwater,category,Sensitivity_indicator,source_code,area.req)

    if (dim(result.local)[1]==0) {results<-result.global} else {results<-result.local}

  }else
  {sensitive.dta
    result.local<-system1.road.6 %>% dplyr::mutate(area.req=area.req.sens) %>% left_join(sensitive.dta) %>%
      select(-Indicator_source) %>% distinct()
    result.global<-system1.road.6 %>% dplyr::mutate(area.req='Global') %>%
      left_join(sensitive.dta) %>%
      select(-Indicator_source) %>% distinct()
    if (is.na(result.local$Sensitivity_indicator)) {results<-result.global} else {results<-result.local}
  }

results %>% dplyr::mutate(fishstock=paste(scientific_name,area,sep='+'))->results

  return(results)

}
