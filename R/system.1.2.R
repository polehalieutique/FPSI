#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param scientific_name taxonomic name of the product
#' @param area catches area mentionned for the product
#' @examples
#' all<-system.1.2(sci_name='SQUATINA SQUATINA',area='27.')
#' @export
#'
system.1.2 <- function(sci_name=NULL,area=NULL,longitude=NULL,latitude=NULL) {

print(paste('First Step for ',sci_name,' in ',area,'for system2 ',sep=''))
system.2.road1.2.3.4<-system.2(sci_name,area)

if (dim(system.2.road1.2.3.4)[1]==0)
{
  print(paste('No answer for system2, Second step for System 1',sep=''))
  system.1.road.5.6<-system.1(sci_name,area) %>% mutate(method='system1')
  if (dim(system.1.road.5.6)[1]==0)
  {
    print("No answer, nor for system2, nor system1")
  }
  else {return(system.1.road.5.6)}
}
else
{
  tmp<-system.2.road1.2.3.4 %>% mutate(method='system2')
  return(tmp)
}


}

