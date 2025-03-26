#' Generate a Codebook for a Data Frame
#'
#' Creates a codebook summarizing the structure and labeling of variables in a data frame.
#' The function extracts information on variable names, positions, types, variable labels,
#' and value labels (if available). It uses the auxiliary function `check.labs()`
#' to retrieve label information for each variable.
#'
#' @param data A data frame for which the codebook is to be generated.
#'
#' @return A data frame with five columns:
#' \describe{
#'   \item{Variable}{Name of the variable in the data frame.}
#'   \item{Position}{Column position of the variable in the data frame.}
#'   \item{Type}{Data type of the variable (e.g., "character", "double", "integer").}
#'   \item{Label}{Variable label (if available).}
#'   \item{Value label}{Formatted string of value labels (if available), or "-" if none exist.}
#' }
#'
#' @details
#' This function relies on the `check.labs()` function to extract variable and value labels.
#' The `check.labs()` function is expected to return a list with the following elements:
#' \itemize{
#'   \item \code{var_lab} - a string with the variable label.
#'   \item \code{val_lab} - a data frame with value labels (two columns: code and label).
#'   \item \code{has_val_lab} - logical, indicating if value labels are present.
#' }
#'
#' @examples
#' \dontrun{
#' codebook_result <- codebook(rxR_data)
#' View(codebook_result)
#' }
#'
#' @export
codebook=function(data){
  res=as.data.frame(matrix(nrow=0,ncol=5))
  names(res)=c("Variable","Position","Type","Label", "Value label")
  for (i in 1:length(names(data))){

    #collect the data on the variable
    var_info=check.labs(data[[names(data)[i]]])

    if (var_info$has_val_lab==TRUE){
      value_labels=""
      for (j in 1:nrow(var_info$val_lab)){
        value_labels=paste(value_labels, var_info$val_lab[j,1], " - ", var_info$val_lab[j,2], "\n", sep="")
      }
    } else {
      value_labels="-"
    }
    temp=c(names(data)[i], i, typeof(data[[names(data)[i]]]), var_info$var_lab, value_labels)
    res=rbind(res,temp)
  }
  names(res)=c("Variable","Position","Type","Label", "Value label")
  return(res)
}

