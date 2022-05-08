
#' Frequencies table
#'
#' @param data dataset
#' @param form variables as formulas (e.g. 1~var1+var2+var3)
#' @param deci number of decimals for percentages (default=1)
#' @param has.NA should NA values be included (default=FALSE)
#' @param title table title. If omitted it will be guessed
#' @param option inherited from r.flex.opts
#'
#' @return list with four elements
#' \itemize{
#'     \item type - table type - used for inserting in word document (x-is there a section, desc - are there descriptives)
#'     \item title - used for table title. Can be set manually or automatically
#'     \item table - flextable with results
#'     \item tab.df - results as data.frame
#'     \item orientation - suggested page orientation (P/L)
#' }
#' @export
#' @examples
#' set.r.flex.opts()
#' data(mtcars)
#' freq.flex(mtcars, 1 ~ gear + cyl)
freq.flex <- function(data, form, deci = 1, has.NA = FALSE, title = "", option = r.flex.opts) {
  vars <- names(stats::get_all_vars(form, data))
  # set title
  if (title == "") {
    if (length(vars) == 1) {
      if (is.null(sjlabelled::get_label(data[[vars[1]]]))) {
        title <- vars[1]
      } else {
        title <- sjlabelled::get_label(data[[vars[1]]])
      }
    } else {
      title <- paste(vars[1], " - ", vars[length(vars)], sep = "")
    }
  }

  # create frq table
  len.t <- c() # for rows
  v.names <- c() # for variable names
  for.N <- c() # for N-s
  res.t <- data.frame() # frq tables
  for (i in 1:length(vars)) {
    res.temp <- sjmisc::frq(data[[vars[i]]])[[1]]
    if (check.labs(data[[vars[i]]])$has_val_lab == FALSE) {
      res.temp$label <- as.character(res.temp$val)
    }
    len.t <- c(len.t, nrow(res.temp))
    v.names <- c(v.names, get.var.name(data, vars[i]))
    for.N <- c(for.N, sum(res.temp$frq, na.rm = TRUE))
    v.names[i] <- paste0(v.names[i], " (N=", for.N[i], ")") # add N to variable name
    if (i == 1) {
      res.t <- res.temp
    } else {
      res.t <- rbind(res.t, res.temp)
    }
  }

  # create results data.frame
  # based on desired options
  # variable names are always in separate column
  names.col <- c()
  for (i in 1:length(v.names)) {
    names.col <- c(names.col, rep(v.names[i], len.t[i]))
  }

  res <- cbind(names.col, res.t)
  if (has.NA == FALSE) {
    res <- res %>% dplyr::filter(is.na(res$val) == FALSE)
    len.t <- len.t - 1
  }

  res <- res %>% dplyr::select(-.data$val)
  res <- res %>% dplyr::select(-.data$cum.prc)
  if (has.NA == FALSE) {
    res <- res %>% dplyr::select(-.data$raw.prc)
  }

  # options
  if (option$lang == "hr") {
    if (has.NA == FALSE) {
      names(res) <- c("Varijabla", "Odgovor", "N", "%")
    } else {
      names(res) <- c("Varijabla", "Odgovor", "N", "Total %", "%")
    }
  } else {
    if (has.NA == FALSE) {
      names(res) <- c("Variable", "Response", "N", "%")
    } else {
      names(res) <- c("Variable", "Response", "N", "Total %", "%")
    }
  }
  table <- freq.to.flex(res, deci, len.t, has.NA, option)
  type <- "freq"
  result <- list(type = type, title = title, table = table, tab.df = res, orientation="P")

  return(result)
}



freq.to.flex <- function(t, deci, len.t, has.NA, option) {

  # arrange df

  # number formats
  t[, 3] <- format(t[, 3], nsmall = 0)
  t[, 4] <- format(round(t[, 4], deci), nsmall = deci, decimal.mark = option$d.p)
  if (has.NA == TRUE) {
    t[, 5] <- format(round(t[, 5], deci), nsmall = deci, decimal.mark = option$d.p)
  }


  if (has.NA == TRUE) {
    if (option$lang == "hr") {
      t[is.na(t[, 2]), 2] <- "B.O."
    } else {
      t[is.na(t[, 2]), 2] <- "NA"
    }
    t <- t %>% dplyr::mutate_all(trimws)
    t[t[, 5] == "NA", 5] <- ""
  }


  t <- t %>%
    flextable::flextable() %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(align = "center", part = "body") %>%
    flextable::align(j = 1, align = "left", part = "body") %>%
    flextable::hline_top(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "body") %>%
    flextable::font(fontname="Calibri", part="all") %>%
    flextable::padding(padding.top = 0, padding.bottom = 0, part="all") %>%
    flextable::width(j=1, width=5, unit="cm") %>%
    flextable::width(j=2, width=5, unit="cm")

  # lines within
  i2 <- 0
  for (i in len.t) {
    i2 <- i2 + i
    t <- t %>% flextable::hline(i = i2, border = officer::fp_border(color = "black", width = 1), part = "body")
    if (has.NA == TRUE) {
      t <- t %>% flextable::hline(i = i2 - 1, j = 2:5, border = officer::fp_border(color = "black", width = 1), part = "body")
    }
  }
  t <- t %>% flextable::merge_v(j = 1, part = "body")
  t <- t %>% flextable::valign(j = 1, valign = "top", part = "body")
  t <- t %>% flextable::align(j = 2, align = "left", part = "body")
  t <- t %>% flextable::fix_border_issues()
  return(t)
}


