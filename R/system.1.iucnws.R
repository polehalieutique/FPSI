#' Get limits values for  stocks provided by IUCN API through Rpackage rredlist
#' @param scientific_name taxonomic name of the product
#' @param area catches area mentionned for the product
#' @examples
#' pre.score.2<-system.2(sci_name='SQUATINA SQUATINA',area='27.')
#' @export
#'
system.1.iucnws<- function(sci_name=NULL,area=NULL,iucn.dta=NULL,sensitive.dta=NULL,area.dta=NULL,iucn_to_stock_area=NULL,token=NULL,longitude=NULL,latitude=NULL) {


if (!is.null(sensitive.dta)) {data(sensitive.dta)}
if (!is.null(area.dta)) {data(area.dta)}
if (!is.null(iucn_to_stock_area)) {data(iucn_to_stock_area)}

sf::sf_use_s2(FALSE)

#Method with intersection / a little bit longer than using iucn_to_stock_area link table (for each gid from iucn you have the list og fao area concerned (in intersection with))
#  filter(area.dta,grepl(toupper(area), sub_division_fao)) %>% st_union() %>%
#    st_as_sf() %>% st_join(filter(iucn.dta,scientific_name==sci_name)) %>%
#    filter(!is.na(category)) %>% distinct()-> system1.road.5
region.global<-'global'
region.local<-''
iucn.ws<-data.frame(result=NULL)
system1.road.5<-data.frame()
if (substr(area,1,2)=='27') region.local<-'europe'
if (substr(area,1,2)=='37') region.local<-'mediterranean'
if (substr(area,1,2)=='34') region.local<-'western_africa'

if (region.local!='') {iucn.ws<-rl_search(sci_name, region = region.local,key=token)
ass.level<-'local'}
if (is.null(dim(iucn.ws$result))) {
iucn.ws<-rl_search(sci_name, region = region.global,key=token)
ass.level<-'global'
}

if (!is.null(dim(iucn.ws$result)))
    {
iucn.ws$result %>%
    filter(!is.na(category)) %>% distinct(taxonid,scientific_name,published_year,freshwater_system,category) %>%
    mutate(area.req=area,ass=ass.level)-> system1.road.5
}

  nlignes<-dim(system1.road.5)[1]
  if (nlignes==0) {
    sensitive.dta %>% filter(scientific_name==sci_name) %>% mutate(id_no=NA,yrcompiled=NA,freshwater=NA,category=NA) %>%
      select (id_no,scientific_name,yrcompiled,freshwater,category,Rindorf_precautionary_F,Cheung_vulnerabilty)->results
  }else
  {
    system1.road.5 %>% st_drop_geometry() %>% left_join(sensitive.dta) %>% select(-c(Species,X))->results
  }

results %>% mutate(fishstock=paste(scientific_name,area,sep='+'))->results

  return(results)

}

