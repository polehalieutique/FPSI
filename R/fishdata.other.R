#' Get fishadata values for stocks provided by NOAA stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-fishdata.noaa(Stock_Name='Albacore - North Atlantic',Assessment_Year=2020)
#'
#' @export
#'
fishdata.other <- function(Stock_Name=NULL,Assessment_Year=NULL,update=FALSE,user=NULL,password=NULL,server=NULL,db=NULL) {
#a rajouter les filtres
  if (update)
  {
    noaa.include<-paste("('",paste(unique(stockAssessmentSummary$Jurisdiction),collapse="','"),"')",sep='')


    library(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    stock <- dbConnect(drv, host=server, user=user, password=password, dbname=db)
    fishdata<-dbGetQuery(stock,paste("select evaluationyear,workinggroup,fishstock,year,recruitment,tbiomass,ssb,landings,yieldssb,meanf,sop,discards
                                    from fishdata where workinggroup not like 'ICES%' and workinggroup not in ",noaa.include,sep=''))
    dbDisconnect(stock)
    return(fishdata)
  }
  else
  {
    data(fishdata.other.dta)
    return(fishdata.other.dta) #I can store data within the package as stocksmart one
  }

}
