#' Get sf object of all stocks area available
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-limits.ices(Stock_Name='Albacore - North Atlantic',Assessment_Year=2020)
#' @export
#'
get_area <- function(Stock_Name=NULL,update=FALSE,user=NULL,password=NULL,server=NULL,db=NULL) {

  if (update)
  {
 noaa.include<-paste("('",paste(unique(stockAssessmentSummary$Jurisdiction),collapse="','"),"')",sep='')


    library(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    stock <- dbConnect(drv, host=server, user=user, password=password, dbname=db)
    area.dta<-st_read(stock,query="select * from geo.fao_area_compilation ")
    dbDisconnect(stock)
    return(area.dta)
  }
  else
  {

    data(area.dta)
    return(area.dta) #I can store data within the package as stocksmart one
      }



}
