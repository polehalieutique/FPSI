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

  sf::sf_use_s2(FALSE)


  filter(area.dta,grepl(toupper(area), sub_division_fao)) %>% st_union() %>%
    st_as_sf() %>% st_join(filter(iucn.dta,scientific_name==sci_name)) %>%
    filter(!is.na(category)) %>% distinct()-> system1.road.5

  g1<-ggplot(system1.road.5)+geom_sf(aes())+ggtitle("Lien avec les zones IUCN")
  print(g1)

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

