
#' Add variable and value labels to a variable
#'
#' @param data data frame
#' @param vari variable that needs to be changed as string
#' @param label label for the variable
#' @param value_labs named vector
#'
#' @return data frame with labelled variable
#' @export
#'
#' @examples
#' data.frame(a=c(1,1,1,2,2,2), b=c(1,2,3,1,2,3)) %>%
#'   var_to_labelled("a", "Age", c("1"="Young", "2"="Old"))
var_to_labelled=function(data, vari, label=NULL, value_labs=NULL){
  if (is.null(label)==FALSE) {data[[vari]]=sjlabelled::set_label(data[[vari]], label=label)}
  if (is.null(value_labs)==FALSE) {data[[vari]]=sjlabelled::set_labels(data[[vari]], labels=value_labs)}
  return(data)
}

