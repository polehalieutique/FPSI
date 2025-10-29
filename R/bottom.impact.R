#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param species.req Alpha code of species
#' @param gear.req FAO code of gear
#' @examples
#' impact<-bottom.impact(gear.req='PS',species.req='ANE')
#' @export
#
bottom.impact <- function(gear.req=NULL,species.req=NULL) {

result<-data.frame(gear.impact=(bottom.impact.gear.dta %>% filter(gear==gear.req))$gearimpact,habitat.impact=(bottom.impact.species.dta %>% filter(species==species.req))$gearimpact2)

    return(result) #I can store data within the package as stocksmart one

}
