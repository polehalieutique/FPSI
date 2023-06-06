#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param Stock_Name to get data for a specified stock
#' @param Assessment_Year to get data for a specified stock
#' @examples
#' limits<-limits.ices(Stock_Name='Albacore - North Atlantic',Assessment_Year=2020)
#' @export
#'
stockdef.other <- function(Stock_Name=NULL,Assessment_Year=NULL,update=FALSE,user=NULL,password=NULL,server=NULL,db=NULL) {

    if (update)
  {
 noaa.include<-paste("('",paste(unique(stockAssessmentSummary$Jurisdiction),collapse="','"),"')",sep='')


    library(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    stock <- dbConnect(drv, host=server, user=user, password=password, dbname=db)
    stockdef.other.dta<-st_read(stock,query="with part1 as
(select distinct fishstock,species_code,trim(sub_division_fao) as sub_division_fao,st_buffer(st_simplify(geom,0.1),0) as geom,upper(scientific_name) as scientific_name from def_stock inner join asfis using(species_code)
 inner join limits using(fishstock) left join geo.fao_area_compilation using(sub_division_fao)
 ) select distinct fishstock,species_code,scientific_name,string_agg(sub_division_fao,' / ') as sub_division_fao,st_buffer(st_collect(geom),0)as geom
 from part1  group by fishstock,species_code,scientific_name
")
    dbDisconnect(stock)
    return(stockdef.other.dta)
  }
  else
  {

    data(stockdef.other.dta)
    return(stockdef.other.dta) #I can store data within the package as stocksmart one
      }



}
