#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param scientific_name taxonomic name of the product
#' @param area catches area mentionned for the product
#' @examples
#' pre.score.2<-system.1(sci_name='CYPRINUS CARPIO',area='27',sensitive.dta=sensitive.dta,iucn.dta=iucn.dta)
#' @export
#'
system.1 <- function(sci_name=NULL,area=NULL,iucn.dta=NULL,sensitive.dta=NULL,area.dta=NULL,iucn_to_stock_area=NULL,longitude=NULL,latitude=NULL) {

val.name1<-NA
#Update the 28/11/2023
#After discussion with  David Allen from IUCN
#I will use data downloadable from   https://www.iucnredlist.org/search?permalink=f48c3b99-8900-48e8-993c-590343ad5e4f
#Regarding the "no more available" webservice
if (!is.null(iucn.dta)) {data(iucn.dta)}

if (!is.null(sensitive.dta)) {data(sensitive.dta)}
#A test where we only keep Cheung 2005 / Source code renamed C
#
#  unique(sensitive.dta$Indicator_source)
sensitive.dta %>% dplyr::filter(Indicator_source!='Cheung_et_al_2007')->sensitive.dta
#On revient pre cheunc
#sensitive.dta %>% filter(Indicator_source!='Cheung_et_al_2005_sealifebase_fishbase')->sensitive.dta
sf::sf_use_s2(FALSE)



  area.req.sens<-'Global'
  area.req.sens.iucn<-'Global'
    if (substr(area,1,2) %in% c('SA','37')) {area.req.sens.iucn<-'Mediterranean'
    area.req.sens<-'37'}
  if (substr(area,1,2) %in% c('27')) {area.req.sens.iucn<-'Europe'
  area.req.sens<-'27'}

  iucn.dta %>%
    dplyr::mutate(scientific_name=scientificName,valid_name=scientificName) %>%
    dplyr::filter(scientific_name==sci_name,scopes==area.req.sens.iucn) %>%
    dplyr::filter(!is.na(category)) %>%
    dplyr::mutate(area.req=area)-> system1.road.6

nlignes<-dim(system1.road.6)[1]

if (nlignes==0) {iucn.dta %>%
   dplyr::mutate(scientific_name=scientificName,valid_name=scientificName) %>%
    dplyr::filter(scientific_name==sci_name,scopes=='Global') %>%
    dplyr::filter(!is.na(category)) %>%
    dplyr::mutate(area.req=area)-> system1.road.6
}

nlignes<-dim(system1.road.6)[1]

# Je rajoute une interrogation en ligne des données IUCN
CapStr <- function(y) {

  paste(toupper(substring(y, 1,1)), tolower(substring(y, 2)),
        sep="", collapse=" ")
}

if (nlignes==0) {
  #Quand on a pas trouver on fait un test de synonymie et on va chercher dans sealifebase
  #Attention il faut le token dans le .Renviron de shiny pour utiliser le rredlist / Limitation quand même
  library(rredlist)
  library(rfishbase)
  val.name1<-validate_names(CapStr(sci_name),server='sealifebase')

  if (!is.na(val.name1))
  {
  val.name<-unlist(strsplit(val.name1," "))
  genus=val.name[[1]]
  species<-paste(val.name[seq(2,length(val.name))],collapse=' ')
  enligne.iucn <- rl_species(genus = genus, species = species)
  system1.road.6<-enligne.iucn$assessments %>% unnest(scopes)  %>% mutate(scopes=description$en) %>%  dplyr::filter(scopes==area.req.sens.iucn) %>%
    arrange(desc(year_published)) %>% slice(1L)   %>% mutate(assessmentId=assessment_id,internalTaxonId=sis_taxon_id,scientificName=sci_name,scientific_name=sci_name,valid_name=toupper(paste(val.name,collapse=' ')),category=red_list_category_code,area.req=area) %>%
    select(assessmentId,internalTaxonId,scientificName,category,scopes,scientific_name,area.req,valid_name)
  }else #Denière tentative, on a bien le bon nom mais on a pas tout en local
  {
    val.name<-unlist(strsplit(sci_name," "))
    genus=val.name[[1]]
    species<-paste(val.name[seq(2,length(val.name))],collapse=' ')
    enligne.iucn <- rl_species(genus = genus, species = species)
    system1.road.6<-enligne.iucn$assessments %>% unnest(scopes)  %>% mutate(scopes=description$en) %>%  dplyr::filter(scopes==area.req.sens.iucn) %>%
      arrange(desc(year_published)) %>% slice(1L) %>%
      mutate(assessmentId=assessment_id,internalTaxonId=sis_taxon_id,scientificName=sci_name,scientific_name=sci_name,valid_name=toupper(paste(val.name,collapse=' ')),category=red_list_category_code,area.req=area) %>%
      select(assessmentId,internalTaxonId,scientificName,category,scopes,scientific_name,area.req,valid_name)

  }

}
nlignes<-dim(system1.road.6)[1]
  if (nlignes==0) {


    result.local<-data.frame(Sensitivity_indicator=NULL)
    result.local<-sensitive.dta %>%  dplyr::filter(scientific_name==sci_name & area.req==area.req.sens)%>%
      dplyr::mutate(id_no=NA,yrcompiled=NA,freshwater=NA,category=NA,valid_name=NA) %>%
      dplyr::select (id_no,scientific_name,valid_name,yrcompiled,freshwater,category,Sensitivity_indicator,source_code,area.req)


    result.global<-sensitive.dta %>%
      dplyr::filter(scientific_name==sci_name & area.req=='Global') %>% dplyr::mutate(id_no=NA,yrcompiled=NA,freshwater=NA,category=NA,valid_name=NA) %>%
      dplyr::select (id_no,scientific_name,yrcompiled,freshwater,category,Sensitivity_indicator,source_code,area.req,valid_name)

    if (dim(result.local)[1]==0) {results<-result.global} else {results<-result.local}

  }else
  {
    if (is.na(val.name1))
    {
    result.local<-system1.road.6 %>% dplyr::mutate(area.req=area.req.sens) %>% left_join(sensitive.dta) %>%
      select(-Indicator_source) %>% distinct()
    result.global<-system1.road.6 %>% dplyr::mutate(area.req='Global') %>%
      left_join(sensitive.dta) %>%
      select(-Indicator_source) %>% distinct()
    }
    else
    {
      result.local<-system1.road.6 %>% dplyr::mutate(area.req=area.req.sens) %>% left_join(sensitive.dta,by=c('valid_name'='scientific_name')) %>%
        select(-Indicator_source) %>% distinct()
      result.global<-system1.road.6 %>% dplyr::mutate(area.req='Global') %>%
        left_join(sensitive.dta,by=c('valid_name'='scientific_name')) %>%
        select(-Indicator_source) %>% distinct()

    }
    if (is.na(result.local$Sensitivity_indicator)) {results<-result.global} else {results<-result.local}

  }

results %>% dplyr::mutate(fishstock=paste(scientific_name,area,sep='+'))->results

  return(results)

}
