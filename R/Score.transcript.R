#' Get limits values for  stocks provided by ICES stocksmart packages
#' @param system.1.2 date provided by system.1.2 function

#' @examples
#' score<-score.transcript(system.2(ssci_name='SQUATINA SQUATINA',area='27.'))
#' @export
#'
score.transcript <- function(system.1.2.dta=NULL) {

  score<-''
  chemin<-''

if (is.null(system.1.2.dta))
{
print("No score for this Stock without data")
#return(FALSE)
}
else
{
  if (system.1.2.dta$method[1]=='system2')
  {

    print('System2 detected')
  if (coalesce(system.1.2.dta$road.1,FALSE)) {
    chemin<-'road.1'
                              if (min(system.1.2.dta$b_bmsy)<0.5) {score<-'E'}
                              if (min(system.1.2.dta$b_bmsy)>=0.5 && min(system.1.2.dta$b_bmsy)<0.8) {score<-'D'}
  }
  if (coalesce(system.1.2.dta$road.2,FALSE)) {
    chemin<-'road.2'
                                  if (min(system.1.2.dta$f_fmsy)>=0 && min(system.1.2.dta$f_fmsy)<1) {score<-'A'}
                                  if (min(system.1.2.dta$f_fmsy)>=1 && min(system.1.2.dta$f_fmsy)<1.2) {score<-'B'}
                                  if (min(system.1.2.dta$f_fmsy)>=1.2 && min(system.1.2.dta$f_fmsy)<1.5) {score<-'C'}
                                  if (min(system.1.2.dta$f_fmsy)>=1.5 && min(system.1.2.dta$f_fmsy)<2) {score<-'D'}
                                  if (min(system.1.2.dta$f_fmsy)>=2 ) {score<-'E'}

  }
  if (coalesce(system.1.2.dta$road.3,FALSE)) {
    chemin<-'road.3'
      if (min(system.1.2.dta$b_bmsy)>=0.8 && min(system.1.2.dta$b_bmsy)<0.9) {score<-'C'}
    if (min(system.1.2.dta$b_bmsy)>=0.9 && min(system.1.2.dta$b_bmsy)<1) {score<-'B'}
    if (min(system.1.2.dta$b_bmsy)>=1) {score<-'A'}
    }
    if (coalesce(system.1.2.dta$road.4,FALSE)) {
      chemin<-'road.4'
      if (min(system.1.2.dta$f_fmsy)>=0 && min(system.1.2.dta$f_fmsy)<1) {score<-'B'}
      if (min(system.1.2.dta$f_fmsy)>=1 && min(system.1.2.dta$f_fmsy)<1.5) {score<-'C'}
      if (min(system.1.2.dta$f_fmsy)>=1.5 && min(system.1.2.dta$f_fmsy)<2) {score<-'D'}
      if (min(system.1.2.dta$f_fmsy)>=2)  {score<-'E'}
    }
    system.1.2.dta$score<-score
    system.1.2.dta$road<-chemin
    return(system.1.2.dta)
  }




    if (system.1.2.dta$method[1]=='system1')
  {
      print('System1 detected')
      chemin<-'road.6'
      if (is.na(system.1.2.dta$category)) {

        if (system.1.2.dta$Rindorf_precautionary_F>3) {score<-'C'} #Low sensitivity
        if (system.1.2.dta$Rindorf_precautionary_F>0.41 & system.1.2.dta$Rindorf_precautionary_F<=3) {score<-'D'} #Medium
        if (system.1.2.dta$Rindorf_precautionary_F<=0.41) {score<-'E'} #High sensitivity
        chemin<-'road.8'
      }
      else
      {
      if (system.1.2.dta$category=='NT') {score<-'D'}
      if (system.1.2.dta$category %in% c('VU','CR','EN')) {
        print("On devrait passer par ici ")
        score<-'E'
        }
      if (system.1.2.dta$category=='LC') {
        if (system.1.2.dta$Rindorf_precautionary_F>3) {score<-'B'} #Low sensitivity
        if (system.1.2.dta$Rindorf_precautionary_F<=3) {score<-'C'} #Medium and High sensitivity
        chemin<-'road.7'
      }
      }
  system.1.2.dta$score<-score
  system.1.2.dta$road<-chemin
  return(system.1.2.dta)
  }
}

}
