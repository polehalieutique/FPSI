#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-limits.ices(Stock_Name='Albacore - North Atlantic',Assessment_Year=2020)
#' @export
#'
limits.other <- function(Stock_Name=NULL,Assessment_Year=NULL,update=FALSE,user=NULL,password=NULL,server=NULL,db=NULL) {

  if (update)
  {
 noaa.include<-paste("('",paste(unique(stockAssessmentSummary$Jurisdiction),collapse="','"),"')",sep='')


    library(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    stock <- dbConnect(drv, host=server, user=user, password=password, dbname=db)
    limits<-dbGetQuery(stock,paste("select evaluationyear,workinggroup,fishstock,flim,fpa,blim,bpa,fmsy,msybtrigger from limits where workinggroup not like 'ICES%' and workinggroup not in ",noaa.include,sep=''))
    dbDisconnect(stock)
    limits<-limits %>% mutate(FishingPressure=NULL,FishingPressureDescription=NULL)
    return(limits)
  }
  else
  {

    data(limits.other.dta)
    limits.other.dta<-limits.other.dta %>% mutate(FishingPressure=NA,FishingPressureDescription=NA)

      return(limits.other.dta) #I can store data within the package as stocksmart one
      }



}
