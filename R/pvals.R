#' Converts number to APA 7 format and adds optional stars for pvalues
#'
#' @param num numeric - number
#' @param pval numeric - p value. If there is no need for stars this parameter should be set to NA
#' @param has.zero boolean - should it include leading zero (see APA 7 rules)
#' @param round integer - number of rounding places
#' @param option - r.flex.opts
#'
#' @return string formated in a desired style
#' @export
#'
#' @examples
#' # number higher than 1
#' star.pval.apa(num = 2.223, pval = 0.02, round = 3, has.zero = TRUE, option = list(d.p = "."))
#' # number lower than 1, no leading zero
#' star.pval.apa(num = 0.223, pval = 0.02, round = 3, has.zero = FALSE, option = list(d.p = "."))
#'# no p value
#' star.pval.apa(num = 0.223, pval = NA, round = 3, has.zero = FALSE, option = list(d.p = "."))
star.pval.apa <- function(num, pval, has.zero, round = 3, option = r.flex.opts) {
  num <- round(num, round)
  num.char <- format(num, nsmall = round)
  # controls
  if (is.na(pval) == F & pval > 1) {
    tcltk::tkmessageBox(type = "ok", message = "pval cannot be higher than 1")
    stop("pval cannot be higher than 1")
  }
  if (has.zero == F & num >= 1) {
    tcltk::tkmessageBox(type = "ok", message = "When the number is 1 or higher, has.zero should be set to TRUE by APA7 rules")
    stop("When the number is 1 or higher, has.zero should be set to TRUE by APA7 rules")
  }

  if (num < 1 & has.zero == F) {
    num.char <- substr(num.char, 2, nchar(num.char))
  }
  num.char <- gsub(".", option$d.p, num.char, fixed = TRUE)


  # add stars

  if (is.na(pval) == FALSE) {
    if (pval < 0.05) {
      num.char <- paste0(num.char, "*")
    }
    if (pval < 0.01) {
      num.char <- paste0(num.char, "*")
    }
    if (pval < 0.001) {
      num.char <- paste0(num.char, "*")
    }
  }
  return(num.char)
}
