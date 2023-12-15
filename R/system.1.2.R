#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param scientific_name taxonomic name of the product
#' @param area catches area mentionned for the product
#' @examples
#' all<-system.1.2(sci_name='SQUATINA SQUATINA',area='27.')
#' @export
#'

system.1.2 <- function(sci_name=NULL,area=NULL,stockdef=NULL,limits=NULL,fishdata=NULL,iucn.dta=NULL,sensitive.dta=NULL,area.dta=NULL,iucn_to_stock_area=NULL,iucn.token=NULL) {

print(paste('First Step for ',sci_name,' in ',area,'for system2 ',sep=''))

system.2.road1.2.3.4.5<-system.2(sci_name,area,stockdef,limits,fishdata)

mix.systems<-length(unique(system.2.road1.2.3.4.5$roadall))==2
if (dim(system.2.road1.2.3.4.5)[1]==0 | sum(as.numeric(system.2.road1.2.3.4.5$roadall))==0 |mix.systems)
{
  print(paste('No answer for system2 or mixed system 1 and 2, Second step for System 1',sep=''))
if (is.null(iucn.token)){
  system.1.road.5.6<-system.1(sci_name,area,iucn.dta,sensitive.dta,area.dta,iucn_to_stock_area) %>% mutate(method='system1')
} else
{
  system.1.road.5.6<-system.1.iucnws(sci_name,area,iucn.dta,sensitive.dta,area.dta,iucn_to_stock_area,token=iucn.token) %>% mutate(method='system1')

}
  if (dim(system.1.road.5.6)[1]==0 & !mix.systems)
  { tmp<-data.frame(method="No answer, nor for system2, nor system1")
    print(tmp)
  }
  else {
    if (mix.systems)
     {
      tmp<-system.2.road1.2.3.4.5 %>% mutate(method="system2") %>% filter(roadall==1) %>% mutate(category=NA,freshwater_system=NA,area.req=NA,ass=NA,source_code=NA,Sensitivity_indicator=NA,Indicator_source=NA) %>%
        st_drop_geometry()

      if (length(system.1.road.5.6$id_no)>0)
      {
      tmp.system1<-system.2.road1.2.3.4.5 %>% filter(roadall==0) %>% st_drop_geometry()%>%
        inner_join(select(system.1.road.5.6,-fishstock),by=c('scientific_name'))
      results<-tmp %>% dplyr::bind_rows(tmp.system1)
      }else
      {results<-tmp}


      return(results)
     }
    else( return(system.1.road.5.6 ))
    }
}
else
{
  tmp<-system.2.road1.2.3.4.5%>% mutate(method="system2")
  return(tmp)
}


}

