#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param scientific_name taxonomic name of the product
#' @param area catches area mentionned for the product
#' @examples
#' pre.score.2<-system.2(sci_name='SQUATINA SQUATINA',area='27.')
#' @export
#'
system.1 <- function(sci_name=NULL,area=NULL,iucn.dta=NULL,sensitive.dta=NULL,area.dta=NULL,iucn_to_stock_area=NULL,longitude=NULL,latitude=NULL) {

if (!is.null(iucn.dta)) {data(iucn.dta)}
if (!is.null(sensitive.dta)) {data(sensitive.dta)}
if (!is.null(area.dta)) {data(area.dta)}
if (!is.null(iucn_to_stock_area)) {data(iucn_to_stock_area)}

sf::sf_use_s2(FALSE)

#Method with intersection / a little bit longer than using iucn_to_stock_area link table (for each gid from iucn you have the list og fao area concerned (in intersection with))
#  filter(area.dta,grepl(toupper(area), sub_division_fao)) %>% st_union() %>%
#    st_as_sf() %>% st_join(filter(iucn.dta,scientific_name==sci_name)) %>%
#    filter(!is.na(category)) %>% distinct()-> system1.road.5

  iucn.dta %>%
    filter(scientific_name==sci_name) %>%
    inner_join(dplyr::filter(iucn_to_stock_area,grepl(toupper(area),sub_division_fao))) %>%
    filter(!is.na(category)) %>% distinct(id_no,scientific_name,yrcompiled,freshwater,category) %>%
    mutate(area.req=area)-> system1.road.5


  nlignes<-dim(system1.road.5)[1]

  area.req.sens<-'Global'
  if (substr(area,1,2) %in% c('37','27','SA')) {

    if (substr(area,1,2)=='SA') {area.req.sens<-'37'} else {area.req.sens<-substr(area,1,2)}
  }


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
  {
    result.local<-system1.road.5 %>% mutate(area.req=area.req.sens) %>% left_join(sensitive.dta)
    result.global<-system1.road.5 %>% mutate(area.req='Global') %>%  st_drop_geometry() %>%
      left_join(sensitive.dta)
    if (is.na(result.local$Sensitivity_indicator)) {results<-result.global} else {results<-result.local}
  }

results %>% mutate(fishstock=paste(scientific_name,area,sep='+'))->results

  return(results)

}
