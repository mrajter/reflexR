#' Check variable and value labels
#'
#' Helper function to determine if there is a defined variable label and value labels. Also checks if there are more unique values than defined value labels.
#' @param vari Variable to be checked
#'
#' @return list with four elements:
#' \itemize{
#'     \item var_lab - variable label (NA if not defined)
#'     \item val_lab - data frame with values and value labels, NA if not defined
#'     \item val_lab_ok - logical. FALSE if there are more values than value labels, otherwise TRUE. NA if value labels are not defined.
#' }
#' @export
check.labs <- function(vari) {
  res <- list(var_lab = NA, has_val_lab = NA, val_lab = NA, val_lab_ok = NA)

  # variable label
  if (is.null(sjlabelled::get_label(vari))) {
    res$var_lab <- NA
  } else {
    res$var_lab <- sjlabelled::get_label(vari)
  }

  # value labels
  if (is.null(sjlabelled::get_labels(vari, values = "n", non.labelled = T, attr.only = T))) {
    res$has_val_lab <- FALSE
    res$val_lab <- NA
  } else {
    res$has_val_lab <- TRUE
    res$val_lab <- data.frame(
      value = as.numeric(names(sjlabelled::get_labels(vari, values = "n", non.labelled = T))),
      label = unname(sjlabelled::get_labels(vari, values = "n", non.labelled = T))
    )
  }


  # check unknown labels
  if (typeof(res$val_lab) != "logical") {
    if (length(sjlabelled::get_labels(vari, values = "n", non.labelled = T)) != length(sjlabelled::get_labels(vari, values = "n", non.labelled = F))) {
      res$val_lab_ok <- FALSE
    } else {
      res$val_lab_ok <- TRUE
    }
  }
  return(res)
}




#' Get variable name or variable label
#'
#' helper function that returns variable name or variable label if existing as string. Used for values in tables.
#'
#' @param data data.frame
#' @param vari variable as string
#'
#' @return variable name or variable label if existing as string
#' @export
get.var.name <- function(data, vari) {
  if (is.na(check.labs(data[[vari]])$var_lab) == TRUE) {
    var.name <- vari
  } else {
    var.name <- check.labs(data[[vari]])$var_lab
  }
  return(var.name)
}
