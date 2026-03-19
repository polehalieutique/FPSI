#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param species.req Alpha code of species
#' @param gear.req FAO code of gear
#' @examples
#' impact<-bottom.impact(gear.req='OTB',species.req='SQC')
#' @export
#
bottom.impact <- function(gear.req=NULL,species.req=NULL) {

#To be in line with Grati and hal, score on sensitivity in between 1 and 3 (not 0 to 3), following he article, it seems that pelagic are 0 and rocky 3, so all the other are 2.
#That means than initial STECF sheet (Processed_species) used here (bottom.impact.species.dta) has to be modified.
#score 0 become 1, score(1,2) --> 2 and score(3) is 3
bottom.impact.species.dta.2<-bottom.impact.species.dta %>% dplyr::rename(gearimpactinit=gearimpact2) %>%
  dplyr::mutate(gearimpact2=case_when(gearimpactinit==0~1,TRUE ~ gearimpactinit)) %>% # On passe les pélagiques à 1 (indiqué dans l'article Grati)
  dplyr::select(-gearimpactinit)
tmp.impact1<-(bottom.impact.gear.dta %>% dplyr::filter(gear==gear.req))$gearimpact
tmp.impact2<-(bottom.impact.species.dta.2 %>% filter(species==species.req))$gearimpact2
if (length(tmp.impact1)==0) {tmp.impact1<-3}
if (length(tmp.impact2)==0) {tmp.impact2<-3}

result<-data.frame(gear.impact=tmp.impact1,habitat.impact=tmp.impact2)

    return(result) #I can store data within the package as stocksmart one

}
