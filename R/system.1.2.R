#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param scientific_name taxonomic name of the product
#' @param area catches area mentionned for the product
#' @examples
#' all<-system.1.2(sci_name='SQUATINA SQUATINA',area='27.')
#' @export
#'
system.1.2 <- function(sci_name=NULL,area=NULL,stockdef=NULL,limits=NULL,fishdata=NULL,iucn.dta=NULL,sensitive.dta=NULL,area.dta=NULL,iucn_to_stock_area=NULL,iucn.token=NULL) {

print(paste('First Step for ',sci_name,' in ',area,'for system2 ',sep=''))
system.2.road1.2.3.4<-system.2(sci_name,area,stockdef,limits,fishdata)

if (dim(system.2.road1.2.3.4)[1]==0 | sum(as.numeric(system.2.road1.2.3.4$roadall))==0)
{
  print(paste('No answer for system2, Second step for System 1',sep=''))
if (is.null(iucn.token)){  system.1.road.5.6<-system.1(sci_name,area,iucn.dta,sensitive.dta,area.dta,iucn_to_stock_area) %>% mutate(method='system1')
} else
{
  system.1.road.5.6<-system.1.iucnws(sci_name,area,iucn.dta,sensitive.dta,area.dta,iucn_to_stock_area,token=iucn.token) %>% mutate(method='system1')

}
  if (dim(system.1.road.5.6)[1]==0)
  { tmp<-data.frame(method="No answer, nor for system2, nor system1")
    print(tmp)
  }
  else {return(system.1.road.5.6)}
}
else
{
  tmp<-system.2.road1.2.3.4 %>% mutate(method='system2')
  return(tmp)
}


}

