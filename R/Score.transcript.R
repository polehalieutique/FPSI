#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param system.1.2 date provided by system.1.2 function

#' @examples
#' score<-score.transcript(system.2(ssci_name='SQUATINA SQUATINA',area='27.'))
#' @export
#'
score.transcript <- function(system.1.2.dta=NULL) {
  #system.1.2.dta<-tmp
  score<-''
  chemin<-''

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
      select (fishstock,species_code,scientific_name,method,road,score,sub_division_fao,mean.f_fmsy,mean.b_bmsy,road.1,road.2,road.3,road.4,nb.eval,eval.year,f_fmsy,b_bmsy,roadall)        ->system.1.2.dta

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
                       category=='LC' & Rindorf_precautionary_F>3 ~'B',
                       category=='LC' & Rindorf_precautionary_F<=3 ~'C',
                       category=='LC' & is.na(Rindorf_precautionary_F) & Cheung_vulnerabilty<=40 ~'B',
                       category=='LC' & is.na(Rindorf_precautionary_F) & Cheung_vulnerabilty>40 ~'C',
                       (is.na(category) | category %in% c('DD'))& Rindorf_precautionary_F>3   ~'C',
                       (is.na(category) | category %in% c('DD')) & (Rindorf_precautionary_F>0.41 & Rindorf_precautionary_F<=3) ~'D',
                       (is.na(category) | category %in% c('DD')) & Rindorf_precautionary_F<=0.41 ~'E',
                       (is.na(category) | category %in% c('DD')) & is.na(Rindorf_precautionary_F) & Cheung_vulnerabilty <=40   ~'C',
                       (is.na(category) | category %in% c('DD')) & is.na(Rindorf_precautionary_F) & (Cheung_vulnerabilty >40 &  Cheung_vulnerabilty<70)  ~'D',
                       (is.na(category) | category %in% c('DD')) & is.na(Rindorf_precautionary_F) & (Cheung_vulnerabilty >=70 ) ~'E'
                     )) %>%
        select(scientific_name,fishstock,method,road,score,category,Rindorf_precautionary_F,Cheung_vulnerabilty)->system.1.2.dta

sensitive.dta
      #       if (is.na(system.1.2.dta$category)) {
# if (!is.na(system.1.2.dta$Rindorf_precautionary_F))
# {
#         if (system.1.2.dta$Rindorf_precautionary_F>3) {score<-'C'} #Low sensitivity
#         if (system.1.2.dta$Rindorf_precautionary_F>0.41 & system.1.2.dta$Rindorf_precautionary_F<=3) {score<-'D'} #Medium
#         if (system.1.2.dta$Rindorf_precautionary_F<=0.41) {score<-'E'} #High sensitivity
# }
#         chemin<-'road.8'
#       }
#       else
#       {
#       if (system.1.2.dta$category=='NT') {score<-'D'}
#       if (system.1.2.dta$category %in% c('VU','CR','EN')) {
#         print("On devrait passer par ici ")
#         score<-'E'
#         }
#       if (system.1.2.dta$category=='LC') {
#         if (!is.na(system.1.2.dta$Rindorf_precautionary_F))
#         {
#           if (system.1.2.dta$Rindorf_precautionary_F>3) {score<-'B'} #Low sensitivity
#         if (system.1.2.dta$Rindorf_precautionary_F<=3) {score<-'C'} #Medium and High sensitivity
#         }
#         chemin<-'road.7'
#       }
#       }
#   system.1.2.dta$score<-score
#   system.1.2.dta$road<-chemin
  names(system.1.2.dta)
  return(system.1.2.dta)
  }
}


