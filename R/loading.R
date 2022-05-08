#' Load dataset from SPSS
#'
#' Function enables loading from SPSS by using sjlabelled method.
#' This means that variables will be loaded as atomic vectors and not of class labelled.
#' Variable and value labels can be accessed with check.labs function
#'
#' @param path path to dataset (default = file.choose())
#' @return loaded data.frame
#' @export
#'
load.SPSS<-function(path="file.choose()"){
  if (path=="file.choose()"){
    baza<-sjlabelled::read_spss(file.choose())
  } else {
    baza<-sjlabelled::read_spss(path)
  }
  return(baza)

}
