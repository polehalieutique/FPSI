#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param system.1.2.dta data provided by system.1.2 function
#' @examples
#' score<-score.transcript(system.2(sci_name='SQUATINA SQUATINA',area='27.'))
#'
#' @export
#'
score.transcript<- function(system.1.2.dta=NULL) {

if (system.1.2.dta$method[1]=="No answer, nor for system2, nor system1")
{
tmp<-data.frame(method="No score for this Stock without data")
print(tmp)

return(tmp)
}
else
{
  if (system.1.2.dta$method[1]=='system2')
  {

    print('System2 detected')


    system.1.2.dta %>% mutate(road=case_when(road.1~'road.1',
                                             road.2~'road.2',
                                             road.3~'road.3',
                                             road.4~'road.4'
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
                                                            road.4 & (mean.f_fmsy>=2)~'E'

    )) %>%
      select (fishstock,species_code,scientific_name,method,road,score,sub_division_fao,FishingPressureDescription,mean.f_fmsy,mean.b_bmsy,road.1,road.2,road.3,road.4,nb.eval,eval.year,f_fmsy,b_bmsy,roadall)        ->system.1.2.dta

    return(system.1.2.dta)
      }


  }




    if (system.1.2.dta$method[1]=='system1')
  {
      print('System1 detected')

      system.1.2.dta %>% mutate(road=case_when(category %in% c('NT','VU','CR','EN') ~'road.6',
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
        select(scientific_name,fishstock,method,road,score,category,source_code,Sensitivity_indicator)->system.1.2.dta



  return(system.1.2.dta)
  }
}
