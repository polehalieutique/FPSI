#' Get SHI values by fleets using either totvallandg (values in euros) or totwghtlandg (landings weight)
#' @param fleets.aer a set of aer data
#' @param stockdef stock definition DF
#' @param limits limits for stocks
#' @param fishdata time series for stocks
#' @examples
#' library(stocksmart)
#' library(dplyr)
#' library(sf)
#' library(ggplot2)
#' library(tidyr)#' test <-shi(fleet.aer.samp,NULL,NULL,NULL,2024)
#' ggplot(test) + geom_bar(aes(x=year,y=shi,fill=variable_code),stat='identity',position='dodge')
#'
#' @export
#'
shi <- function(fleets.aer=NULL,stockdef=NULL,limits=NULL,fishdata=NULL,limit.assYear=NULL) {

  if (is.null(stockdef) && is.null(limits) && is.null(fishdata))
  {
    stockdef<-stockdef.other()

    limits<-rbind(limits.noaa(),limits.other(),limits.ices())

    fishdata<-rbind(fishdata.noaa(),fishdata.other(),fishdata.ices())
  }

  #To obtain last available Evaluationyear by stock
  fishdata %>%  dplyr::filter(evaluationyear>limit.assYear) %>% dplyr::group_by(fishstock) %>%
    dplyr::summarize(evaluationyear=max(evaluationyear)) ->last.Eval.year

  fishdata.last<-fishdata %>% inner_join(last.Eval.year)
  limits.last<-limits %>% inner_join(last.Eval.year)

  # C'est ici que l'on fait le calcul du F/fmsy =f_etoile2 sur les dernières éval de stock disponibles
  f_etoile2_year<- fishdata.last %>% inner_join(limits) %>%
    mutate(year_wg=evaluationyear,f_etoile2=meanf/fmsy,overfished=case_when(f_etoile2>1 ~ 1, NULL))  %>%
    filter(f_etoile2<9) %>%
    ungroup() %>%
    dplyr::select(year_wg,fishstock,year,f_etoile2,overfished)


  stockdef.wider<-stockdef  %>% st_drop_geometry() %>%
    mutate(sub_reg=substr(sub_division_fao,2,1000000L)) %>% separate_longer_delim(sub_reg,"./.") %>%
    select (-sub_division_fao) %>% inner_join(f_etoile2_year)


  jointure<-fleet.aer.sam %>% mutate(sub_reg=toupper(sub_reg)) %>%
    left_join(stockdef.wider,relationship = "many-to-many") %>% mutate(numerateur=values*f_etoile2)

  somme.jointure <- jointure %>% filter(!is.na(f_etoile2)) %>% group_by(fleet_code,year,variable_code) %>%
    summarise(denominateur=sum(values,na.rm=TRUE))

  somme.jointure.stock <- jointure %>% filter(!is.na(f_etoile2)) %>%
    mutate(fishstock=case_when(overfished==1 ~ paste('*',fishstock,sep=''),TRUE ~ fishstock)) %>%
    distinct(fleet_code,year,variable_code,fishstock,f_etoile2) %>%
    group_by(fleet_code,year,variable_code) %>%
    summarise(stock_assessed=paste(fishstock,collapse = ' '),nb_stock=n(),f_etoile2_concat=paste(fishstock,':',f_etoile2,collapse=' / '))

  somme.jointure <- jointure %>% filter(!is.na(f_etoile2)) %>% group_by(fleet_code,year,variable_code) %>%
    summarise(denominateur=sum(values,na.rm=TRUE)) %>%
    inner_join(somme.jointure.stock)

  somme.jointure.tout <- jointure  %>% group_by(fleet_code,year,variable_code) %>% summarise(capt.totales=sum(values,na.rm=TRUE))

  results<-jointure %>% group_by(year,fleet_code,variable_code) %>% summarise(numerateur=sum(numerateur,na.rm=TRUE)) %>%
    inner_join(somme.jointure.tout) %>%
    inner_join(somme.jointure) %>% mutate(shi=numerateur/denominateur,ratio_f2=100*denominateur/capt.totales)

return(results)


}
