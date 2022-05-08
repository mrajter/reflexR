


#' Set up everything needed to start an analysis
#'
#' This function will do necessary steps to set up an environment for an analysis
#' Basically, it sets up a working directory, creates options variable in global environment, creates folders and downloads template file for officer package.
#'
#' You should really start with this
#'
#' @return environment setup and variables:
#' \itemize{
#'    \item working directory
#'    \item directories in working directory: Data, Pictures, Templates, Outputs
#'    \item downloaded file template.docx into Templates directory
#'    \item global variable in r.flex.opts environment \strong{doc} based on template.docx to be used with officer
#'    \item \strong{r.flex.opts} environment which is used in functions for options:
#'    \itemize{
#'        \item lang - language - defaults to "hr", can be changed to "en"
#'        \item d.p - decimal point type - defaults to ",", can be changed to "."
#'        \item lead.zero (boolean) - will the leading zero be shown in numbers. e.g. 0.05 or .05. Defaults to TRUE
#'        \item p.type - how do you show p values. Options are
#'        \itemize{
#'            \item "<>" - default - they will be shown as compared to critical points (<0.05, <0.01, <0.001)
#'            \item "exact" - shown as exact values
#'            \item "star" - shown as stars as compared to critical points (<0.05, 0.01, 0.001)
#'        }
#'        \item tab.caption (boolean) - should table captions be put in word file. Defaults to TRUE
#'    }
#' }
#' @export
im_on_fire <- function() {
  # set up working directory
  setwd(utils::choose.dir(default = "", caption = "Select folder for analysis"))

  # Create directories
  dir.create("Data")
  dir.create("Pictures")
  dir.create("Templates")
  dir.create("Outputs")

  # download template
  utils::download.file("https://github.com/mrajter/flex_support_files/raw/main/template.docx", "Templates/template.docx", mode = "wb")

  # assign to officer variable
  assign("doc", officer::read_docx("Templates/template.docx"), envir = r.flex.opts)

}



#' Set r.flex.opts variable
#'
#' Used as a direct way to set the options variable used in other functions. The preferred way is by using im_on_fire() function.
#' This should be used when you want to work without other perks of im_on_fire().
#'
#' @return r.flex.opts environment used in various functions
#' @export
set.r.flex.opts <- function() {
  lang=readline(prompt="Set language (hr or en): ")
  d.p=readline(prompt="Set decimal point type(. or ,): ")
  lead.zero=readline(prompt="Would you like to have a leading zero (T/F): ")
  p.type=readline(prompt="How should p values be displayed(<>, exact, star): ")
  tab.caption=readline(prompt="Should table captions be put in word file (T/F): ")

  r.flex.opts$lang=lang
  r.flex.opts$d.p=d.p
  r.flex.opts$lead.zero=as.logical(lead.zero)
  r.flex.opts$p.type=p.type
  r.flex.opts$tab.caption=as.logical(tab.caption)


}




# ovo radi za sve brojeve kad se briše leading zero
pval.apa <- function(num, deci = 3, equal = F, option = r.flex.opts) {
  if (num > 1 | num < -1) {
    tcltk::tk_messageBox(type = "ok", message = "This is not a p-value")
    stop("This is not a p-value")
  }

  if (abs(num) < (10^(-1 * deci))) {
    res <- paste0("<", option$d.p, paste0(rep("0", deci - 1), collapse = ""), "1")
  } else {
    res <- format(round(num, deci), nsmall = deci, decimal.mark = option$d.p)

    if (abs(num)==1) {
      res <- res
    } else if (num < 0) {
      res <- substring(res, 3)
      res <- paste0("-", res)
    } else {
      res <- substring(res, 2)
    }

    if (equal == T) {
      res <- paste0("=", res)
    }
  }
  return(res)
}

#--------------------------
# formatiranje broja - decimalna točka i broj decimala
format.n <- function(num, deci, option = r.flex.opts) {
  res <- format(round(num, deci), nsmall = deci, decimal.mark = option$d.p)
  return(res)
}
