#' Performs paired t-test for one or more pairs of variables.
#'
#' @param data data frame with variables
#' @param ... pairs of variables written as formulas and separated by comma. E.g. v1 ~ v2, v3 ~ v4
#' @param deci number of decimal places for M (default=2)
#' @param opts options - defaults to r.flex.opts
#'
#' @return list with
#' \itemize{
#'     \item type - table type - used for inserting in word document
#'     \item table - flextable with results
#'     \item df - data frame with results as numerics
#'     \item p.vals - pvalues for t-tests. This is used for future implementation of Benjamini-Hochbergs correction}
#' @export
#'
ttest.paired.flex <- function(data, ..., deci = 2, opts=r.flex.opts) {
  lang <- r.flex.opts$lang
  # check if ... has only formulas
  test_konz <- unlist(lapply(list(...), function(x) {
    return(class(x) == "formula")
  }))
  if (sum(test_konz) != length(list(...))) {
    stop("This function accepts only formulas!!!")
  }


  brojac <- 1
  for (form in list(...)) {
    # extract data frame
    df2 <- stats::model.frame(form, data = data)
    # check if formula has two variables
    if (ncol(df2) != 2) {
      stop(paste("Formula", format(form), " needs to have exactly one variable at each side!!!"))
    }

    # calculate parameters
    res_t <- data.frame(matrix(nrow = 2, ncol = 9))
    names(res_t) <- c(
      "Varijabla",
      "N",
      "M",
      "SD",
      "r",
      "p(r)",
      "t",
      "df",
      "p(t)"
    )
    if (lang=="en") {
      names(res_t)[1]="Variable"
    }
    res_t[, 1] <- names(df2)
    res_t[, 2] <- rep(nrow(df2[stats::complete.cases(df2), ]), 2)
    res_t[, 3] <- c(mean(df2[, 1], na.rm = T), mean(df2[, 2], na.rm = T))
    res_t[, 4] <- c(stats::sd(df2[, 1], na.rm = T), stats::sd(df2[, 2], na.rm = T))

    kor <- stats::cor.test(df2[, 1], df2[, 2], method = "pearson")
    res_t[1, 5] <- unname(kor$estimate)
    res_t[1, 6] <- kor$p.value

    q <- stats::t.test(df2[, 1], df2[, 2], paired = TRUE)
    q$statistic
    res_t[1, 7] <- q$statistic
    res_t[1, 8] <- q$parameter
    res_t[1, 9] <- q$p.value

    if (brojac == 1) {
      res <- res_t
    } else {
      res <- rbind(res, res_t)
    }
    brojac <- brojac + 1
  }




  res_list <- list(
    type="paired_t",
    table=ttest.paired.to.flex(res, deci,lang),
    df = res,
    p.vals=res$`p(t)`[stats::complete.cases(res$`p(t)`)]
  )
  return(res_list)
}



ttest.paired.to.flex <- function(data, deci, lang) {
#helper function for creating flextable
    br_usp <- nrow(data) / 2

  if (lang == "hr") {
    d.p <- ","
  } else {
    d.p <- "."
  }

  data[, 3] <- format(round(data[, 3], deci), decimal.mark = d.p, nsmall = deci)
  data[, 4] <- format(round(data[, 4], deci + 1), decimal.mark = d.p, nsmall = deci + 1)
  data[, 5] <- substr(format(round(data[, 5], 3), decimal.mark = d.p, nsmall = 3), 2, 5)
  data[, 6] <- substr(format(round(data[, 6], 3), decimal.mark = d.p, nsmall = 3), 2, 5)
  data[, 7] <- format(round(data[, 7], 2), decimal.mark = d.p, nsmall = 2)
  data[, 9] <- substr(format(round(data[, 9], 3), decimal.mark = d.p, nsmall = 3), 2, 5)

  for (i in 1:br_usp) {
    data[i * 2, 5:9] <- c("", "", "", NA, "")
  }

  tab <- data %>%
    flextable::flextable() %>%
    flextable::align(j = 2:9, align = "center", part = "all") %>%
    flextable::align(i = 1, j = 1, align = "center", part = "head") %>%
    flextable::valign(valign = "center", part = "all")

  for (m in 1:br_usp) {
    for (n in 5:9) {
      tab <- tab %>% flextable::merge_at(i = (m * 2 - 1):(m * 2), j = n, part = "body")
    }
    tab <- tab %>% flextable::hline(i = m * 2, border = officer::fp_border(color = "black", width = 1), part = "body")
  }
  tab <- tab %>%
    flextable::hline(i = 1, border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline(i = (br_usp * 2), border = officer::fp_border(color = "black", width = 1), part = "body") %>%
    flextable::border(i = 1, border.top = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::fix_border_issues() %>%
    flextable::autofit()
  return(tab)
}
