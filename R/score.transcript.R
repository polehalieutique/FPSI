#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param system.1.2.dta data provided by system.1.2 function
#' @examples
#' score<-score.transcript(system.2(sci_name='SQUATINA SQUATINA',area='27.'))
#' @export

score.transcript<-function(system.1.2.dta=NULL) {

if (system.1.2.dta$method[1]=="No answer, nor for system2, nor system1")
{
tmp<-data.frame(method="No score for this Stock without data")
print(tmp)

return(tmp)
}
else
{
  mix.systems<-(length(unique(system.1.2.dta$method))==2)
  if (system.1.2.dta$method[1]=='system2' |mix.systems)
  {

    print('System2 detected')


    system.1.2.dta %>% filter(method=='system2') %>% mutate(road=case_when(road.1~'road.1',
                                             road.2~'road.2',
                                             road.3~'road.3',
                                             road.4~'road.4',
                                             road.5~'road.5'
                                            )
                                             ,score=case_when(road.1 & mean.b_bmsy <0.5~'E',
                                                            road.1 & (mean.b_bmsy>=0.5 & mean.b_bmsy <0.8)~'D',
                                                            road.2 & (mean.f_fmsy>=0 & mean.f_fmsy <1)~'A',
                                                            road.2 & (mean.f_fmsy>=1 & mean.f_fmsy <1.2)~'B',
                                                            road.2 & (mean.f_fmsy>=1.2 & mean.f_fmsy <1.5)~'C',
                                                            road.2 & (mean.f_fmsy>=1.5 & mean.f_fmsy <2)~'D',
                                                            road.2 & (mean.f_fmsy>=2)~'E',
                                                            road.3 & (mean.b_bmsy>=0.8 & mean.b_bmsy <0.9)~'C',
                                                            road.3 & (mean.b_bmsy>=0.9 & mean.b_bmsy <1)~'B',
                                                            road.3 & (mean.b_bmsy>=1 )~'A',
                                                            road.4 & (mean.f_fmsy>=0 & mean.f_fmsy <1)~'B',
                                                            road.4 & (mean.f_fmsy>=1 & mean.f_fmsy <1.5)~'C',
                                                            road.4 & (mean.f_fmsy>=1.5 & mean.f_fmsy <2)~'D',
                                                            road.4 & (mean.f_fmsy>=2)~'E',
                                                            road.5 & (mean.catch.advice<=1)~'B',
                                                            road.5 & (mean.catch.advice>1 & mean.catch.advice<=1.5)~'C',
                                                            road.5 & (mean.catch.advice>1.5 & mean.catch.advice<=2)~'D',
                                                            road.5 & (mean.catch.advice>2)~'E'


    )) %>%
      select (fishstock,species_code,scientific_name,method,road,score,sub_division_fao,FishingPressureDescription,mean.f_fmsy,mean.b_bmsy,mean.catch.advice, road.1,road.2,road.3,road.4,road.5,nb.eval,eval.year,f_fmsy,b_bmsy,roadall)        ->system.1.2.dta.system2

    if (!mix.systems) result<-system.1.2.dta.system2

      }







    if (system.1.2.dta$method[1]=='system1' |mix.systems)
  {
      print('System1 detected')

      system.1.2.dta %>% filter(method=='system1') %>%  mutate(road=case_when(category %in% c('NT','VU','CR','EN') ~'road.6',
                                               category %in% c('LC')~'road.7',
                                               is.na(category) || category %in% c('DD') ~'road.8'

      )#First use of more precise and local sensitive list Rindorf and if it's not available then Cheung # Add area 27 maybe
      ,score=case_when(category=='NT' ~'D',
                       category %in% c('VU','CR','EN') ~'E',
                       category=='LC' & source_code=='R' & Sensitivity_indicator>3 ~'B',
                       category=='LC' & source_code=='R' & Sensitivity_indicator<=3 ~'C',
                       category=='LC' & source_code=='O' & Sensitivity_indicator<=1.6 ~'B',
                       category=='LC' & source_code=='O' & Sensitivity_indicator>1.6 ~'C',
                       category=='LC' & source_code=='C'  & Sensitivity_indicator<=40 ~'B',
                       category=='LC' & source_code=='C'  & Sensitivity_indicator>40 ~'C',
                       (is.na(category) | category %in% c('DD')) & source_code=='R' & Sensitivity_indicator>3   ~'C',
                       (is.na(category) | category %in% c('DD')) & source_code=='R' & (Sensitivity_indicator>0.41 & Sensitivity_indicator<=3) ~'D',
                       (is.na(category) | category %in% c('DD')) & source_code=='R' & Sensitivity_indicator<=0.41 ~'E',
                       (is.na(category) | category %in% c('DD')) & source_code=='C' & Sensitivity_indicator <=40   ~'C',
                       (is.na(category) | category %in% c('DD')) & source_code=='C' & (Sensitivity_indicator >40 &  Sensitivity_indicator<70)  ~'D',
                       (is.na(category) | category %in% c('DD')) & source_code=='C' & (Sensitivity_indicator >=70 ) ~'E',
                       (is.na(category) | category %in% c('DD')) & source_code=='O' & Sensitivity_indicator <=1.6 ~'C',
                       (is.na(category) | category %in% c('DD')) & source_code=='O' & (Sensitivity_indicator >1.6 &  Sensitivity_indicator<=2)  ~'D',
                       (is.na(category) | category %in% c('DD')) & source_code=='O' & (Sensitivity_indicator >2 ) ~'E'
                     )) %>%
        select(scientific_name,fishstock,method,road,score,category,source_code,Sensitivity_indicator)->system.1.2.dta.system1



      if (!mix.systems) result<-system.1.2.dta.system1
    }
  if (mix.systems) result<-bind_rows(system.1.2.dta.system2,system.1.2.dta.system1)

  return(result)
}
}
