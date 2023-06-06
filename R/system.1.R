#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param scientific_name taxonomic name of the product
#' @param area catches area mentionned for the product
#' @examples
#' pre.score.2<-system.2(sci_name='SQUATINA SQUATINA',area='27.')
#' @export
#'
system.1 <- function(sci_name=NULL,area=NULL,longitude=NULL,latitude=NULL) {

data(iucn.dta)
data(sensitive.dta)
data(area.dta)
data(iucn_to_stock_area)

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
  if (nlignes==0) {
    sensitive.dta %>% filter(scientific_name==sci_name) %>% mutate(id_no=NA,yrcompiled=NA,freshwater=NA,category=NA) %>%
      select (id_no,scientific_name,yrcompiled,freshwater,category,Rindorf_precautionary_F,Cheung_vulnerabilty)->results
  }else
  {
    system1.road.5 %>% st_drop_geometry() %>% left_join(sensitive.dta) %>% select(-c(Species,X))->results
  }



  return(results)

}

