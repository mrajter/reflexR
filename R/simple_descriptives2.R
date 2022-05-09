#' Provide basic descriptives
#'
#' Simple descriptive statistics
#'
#' @param data data.frame with variables
#' @param vars variables as formula. e.g. 1 ~ var1 + var2
#' @param param_set list of parameters with order in the table (as strings):
#' \itemize{
#'   \item default - "standard"
#'   \item "simple" - N, Min, Max, M, SD
#'   \item "standard" - N, M, SD, Min, Q1, C, Q3, Max, Skew, Kurt, SW, SWp
#'   \item "report" - N, M, SD, Min, Q1, C, Q3, Max
#'   \item other available statistics - missing values(Miss), Kolmogorov-Smirnov (KS, KSp), standard errors for Skewness and Kurtosis (SEskew, SEkurt)
#' }
#' @param by section variable, default=FALSE, otherwise put variable name in quotes, e.g. "section_var". Section variable should have labels
#' @param by_total boolean. If there is a section variable, should the total result be included in descriptives (default=TRUE)
#' @param deci number of decimals for M, everything else is done automatically (default=1)
#' @param option options variable. To change, modify r.flex.opts variable created by im_on_fire()
#' @param title table title. If not set it will be determined automatically
#' @return list with elements
#' \itemize{
#'   \item type - table type - used for inserting in word document
#'   \item title - used for table title. Can be set manually or automatically
#'   \item section - section variable title
#'   \item table - flextable with results
#'   \item tab.df - results as data.frame
#'   \item orientation - suggested page orientation (P/L)
#' }
#' @export
des.flex <- function(data, vars, param_set = "standard", by = NA, by_total = TRUE, deci = 1, option = r.flex.opts, title = "") {
  if (missing(vars)) {
    vars <- names(data)
  } else {
    vars <- all.vars(vars)
  }

  res.names <- c()
  for (i in vars) {
    # names
    if (is.null(labelled::var_label(data[[i]]))) {
      res.names <- append(res.names, i)
    } # if not labelled use variable name
    else {
      res.names <- append(res.names, labelled::var_label(data[[i]]))
    }
  }

  # define data frame for section variable
  if (is.na(by) == FALSE) {
    if (sum(is.na(check.labs(data[[by]])$val_lab)) > 0) {
      tcltk::tk_messageBox(type = "ok", message = "Please provide labels for the section variable (by-argument)")
      stop("Please provide labels for the section variable (by-argument)")
    }
    if (is.numeric(data[[by]]) == FALSE) {
      tcltk::tk_messageBox(type = "ok", message = "Sections only work for numeric variables for now...")
      stop("Sections only work for numeric variables for now...")
    }

    labels_vector_by <- sjlabelled::get_labels(data[[by]], values = T)
    by_df <- data.frame(vals = as.numeric(names(labels_vector_by)), labs = unname(labels_vector_by))
  }

  # workflow
  if (is.na(by) == TRUE) {
    res <- descriptives(data, vars, param_set, deci = deci, option = option, res.names = res.names)
  } else {
    res_list <- list()
    for (counter_by in 1:nrow(by_df)) {
      res_list[[counter_by]] <- descriptives(data[data[[by]] == by_df[counter_by, 1], ], vars, param_set, deci = deci, option = option, res.names = res.names)
      res_list[[counter_by]]$Variable <- res.names #subsetting looses variable labels. This is the correction
    }
    if (by_total == TRUE) {
      res_list[[nrow(by_df) + 1]] <- descriptives(data, vars, param_set, deci = deci, option = option)
      by_df <- rbind(by_df, c(nrow(by_df) + 1, "Total"))
    }
    # create data frame with results
    res <- data.frame()

    for (counter_by in 1:length(res_list)) {
      res_list[[counter_by]] <- cbind(
        no_var = seq(1, nrow(res_list[[counter_by]])),
        no_gr = rep(counter_by, nrow(res_list[[counter_by]])),
        Group = rep(by_df[counter_by, 2], nrow(res_list[[counter_by]])),
        res_list[[counter_by]]
      )
      res <- rbind(res, res_list[[counter_by]])
    }
    res <- res[order(res$no_var, res$no_gr), ]
    res <- res %>% dplyr::select(.data$Variable, .data$Group, tidyselect::everything())
    res <- dplyr::select(res, -c(.data$no_var, .data$no_gr))
  }

  # set title
  if (title == "") {
    if (length(vars) > 1) {
      title <- paste(vars[1], " - ", vars[length(vars)], sep = "")
    } else {
      title <- res.names[1]
    }
  }
  title.by=NA
  if (is.na(by) == FALSE) {
    title.by <- sjlabelled::get_label(data[[by]])
  }

  if (option$lang == "hr") {
    names(res)[1] <- "Varijabla"
    if (is.na(by) == FALSE) {
      names(res)[2] <- sjlabelled::get_label(data[[by]])
    }
  } else {
    names(res)[1] <- "Variable"
  }

  if (is.na(by) == FALSE) {
    type <- "desc"
    table <- des.to.flex.by(res, nrow(by_df), by_total)
  } else {
    type <- "desc"
    table <- des.to.flex(res)
  }
  #calculate orientation
  if (sum(table$body$colwidths)*2.54<19.8) {
    orientation <- "P"
  } else {
    orientation <- "L"
  }
  result <- list(type = type, title = title, section=title.by, table = table, tab.df = res, orientation=orientation)
  return(result)
}

descriptives <- function(data, vars, param_set = "standard", by = NA, deci = 1, option = r.flex.opts, title = "", res.names) {


  # empty list for results
  res.names <- c()
  res.N <- c()
  res.Miss <- c()
  res.min <- c()
  res.max <- c()
  res.M <- c()
  res.SD <- c()
  res.C <- c()
  res.Q1 <- c()
  res.Q3 <- c()
  res.Var <- c()
  res.Skew <- c()
  res.SE_skew <- c()
  res.Kurt <- c()
  res.SE_kurt <- c()
  res.KS <- c()
  res.KSp <- c()
  res.SW <- c()
  res.SWp <- c()


  for (i in vars) {
    # names
    if (is.null(labelled::var_label(data[[i]]))) {
      res.names <- append(res.names, i)
    } # if not labelled use variable name
    else {
      res.names <- append(res.names, labelled::var_label(data[[i]]))
    }
    # N
    res.N <- append(res.N, length(stats::na.omit(data[[i]])))

    # Miss
    res.Miss <- append(res.Miss, sum(is.na(data[[i]])))

    # Min
    res.min <- append(res.min, min(data[[i]], na.rm = TRUE))

    # Max
    res.max <- append(res.max, max(data[[i]], na.rm = TRUE))

    # M
    res.M <- append(res.M, mean(data[[i]], na.rm = TRUE))

    # SD
    res.SD <- append(res.SD, stats::sd(data[[i]], na.rm = TRUE))

    # C
    res.C <- append(res.C, stats::median(data[[i]], na.rm = TRUE))

    # Q1
    res.Q1 <- append(res.Q1, unname(stats::quantile(data[[i]], na.rm = TRUE)[2]))

    # Q3
    res.Q3 <- append(res.Q3, unname(stats::quantile(data[[i]], na.rm = TRUE)[4]))

    # Variance
    res.Var <- append(res.Var, stats::var(data[[i]], na.rm = TRUE))

    # skew
    res.Skew <- append(res.Skew, r.skewness(data[[i]]))

    # skew SE
    res.SE_skew <- append(res.SE_skew, r.skewness.se(data[[i]]))

    # kurt
    res.Kurt <- append(res.Kurt, r.kurtosis(data[[i]]))

    # kurt SE
    res.SE_kurt <- append(res.SE_kurt, r.kurtosis.se(data[[i]]))

    # KS test
    ks <- r.ks(data[[i]])
    res.KS <- append(res.KS, unname(ks$statistic))
    res.KSp <- append(res.KSp, unname(ks$p.value))

    # SW test
    sw <- r.sw(data[[i]])
    res.SW <- append(res.SW, unname(sw$statistic))
    res.SWp <- append(res.SWp, unname(sw$p.value))
  }


  ## decimals

  #test if int is equal to res --> for setting rounding
  #Neccessary if min and max are decimal numbers

  if (sum(res.min==as.integer(res.min))!=length(res.min) |
      sum(res.max==as.integer(res.max))!=length(res.max)) {
    deci.mm <- deci
  } else {
    deci.mm <- 0
  }

  res.min=format(round(res.min, deci.mm), nsmall = deci.mm, decimal.mark = option$d.p)
  res.max=format(round(res.max, deci.mm), nsmall = deci.mm, decimal.mark = option$d.p)
  res.M <- format(round(res.M, deci), nsmall = deci, decimal.mark = option$d.p)
  res.SD <- format(round(res.SD, (deci + 1)), nsmall = (deci + 1), decimal.mark = option$d.p)
  res.C <- format(round(res.C, deci), nsmall = deci, decimal.mark = option$d.p)
  res.Q1 <- format(round(res.Q1, deci), nsmall = deci, decimal.mark = option$d.p)
  res.Q3 <- format(round(res.Q3, deci), nsmall = deci, decimal.mark = option$d.p)
  res.Var <- format(round(res.Var, (deci + 1)), nsmall = (deci + 1), decimal.mark = option$d.p)
  res.Skew <- format(round(res.Skew, 2), nsmall = 2, decimal.mark = option$d.p)
  res.SE_skew <- format(round(res.SE_skew, 2), nsmall = 2, decimal.mark = option$d.p)
  res.Kurt <- format(round(res.Kurt, 2), nsmall = 2, decimal.mark = option$d.p)
  res.SE_kurt <- format(round(res.SE_kurt, 2), nsmall = 2, decimal.mark = option$d.p)
  res.KS <- format(round(res.KS, 2), nsmall = 2, decimal.mark = option$d.p)

  # special case for p values
  res.KSp <- p.val.transf(res.KSp, option)

  res.SW <- format(round(res.SW, 2), nsmall = 2, decimal.mark = option$d.p)

  res.SWp <- p.val.transf(res.SWp, option)







  res <- data.frame(
    res.names, res.N, res.Miss, res.min, res.max, res.M, res.SD,
    res.C, res.Q1, res.Q3, res.Skew, res.SE_skew, res.Kurt, res.SE_kurt,
    res.KS, res.KSp, res.SW, res.SWp
  )




  names(res) <- c(
    "Variable", "N", "Miss", "Min", "Max", "M", "SD", "C", "Q1", "Q3",
    "Skew", "SE_skew", "Kurt", "SE_kurt", "KS", "KSp", "SW", "SWp"
  )
  if (param_set == "simple") {
    res <- res %>% dplyr::select("Variable", "N", "Min", "Max", "M", "SD")
  } else if (param_set == "standard") {
    res <- res %>% dplyr::select("Variable", "N", "M", "SD", "Min", "Q1", "C", "Q3", "Max", "Skew", "Kurt", "SW", "SWp")
  } else if (param_set == "report") {
    res <- res %>% dplyr::select("Variable", "N", "M", "SD", "Min", "Q1", "C", "Q3", "Max")
  } else if (param_set == "full") {
  } else {
    res <- res %>% dplyr::select(append("Variable", param_set))
  }

  # define decimal mark and leading zero
  if (option$lead.zero == FALSE) {
    for (i in 2:ncol(res)) {
      for (j in 1:nrow(res)) {
        if (nchar(res[j, i]) > 2 & substring(res[j, i], 1, 2) == "0.") {
          res[j, i] <- paste(".", substring(res[j, i], 3), sep = "")
        }
      }
    }
  }
  if (option$d.p == ",") {
    res[-1] <- lapply(res[-1], gsub, pattern = ".", replacement = ",", fixed = TRUE)
  }

  return(as.data.frame(res))
}






# Skewness ----------------------------------------------------------------


#' Calculates skewness of the variable
#'
#' uses Kirk (2008) method for adjusted Fisher-Pearson standardized moment coefficient.
#'
#' This is the same method Excel uses when calculating skewness.
#' @param target_variable numeric variable
#' @return variable skewness
#' @export
r.skewness <- function(target_variable) {
  a <- scale(target_variable)
  a <- a^3

  n <- length(stats::na.omit(a))
  skew <- sum(a, na.rm = TRUE) / (n - 1)
  skew <- skew * (n / (n - 2))

  return(skew)
}



# Skewness standard error -------------------------------------------------

#' Calculates standard error for the skewness of the variable
#'
#' uses SPSS book of algorithms
#'
#' @param target_variable numeric variable
#' @return standard error for variable skewness
#' @export
r.skewness.se <- function(target_variable) {
  n <- length(stats::na.omit(target_variable))
  se.skew <- sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))
  return(se.skew)
}



# Kurtosis ----------------------------------------------------------------

#' Calculates excess kurtosis of the variable
#'
#' uses SPSS Book of algorithms v20
#'
#' @param target_variable numeric variable
#' @return excess kurtosis
#' @export
r.kurtosis <- function(target_variable) {
  n <- length(stats::na.omit(target_variable))
  a <- target_variable - mean(target_variable, na.rm = TRUE)
  a2 <- a^2
  a4 <- a^4

  m2 <- sum(a2, na.rm = TRUE)
  m4 <- sum(a4, na.rm = TRUE)

  up_div <- (n * (n + 1) * m4) - (3 * m2 * m2 * (n - 1))
  down_div <- (n - 1) * (n - 2) * (n - 3) * (stats::sd(target_variable, na.rm = TRUE)^4)

  kurt <- up_div / down_div
  return(kurt)
}



# Kurtosis SE -------------------------------------------------------------

#' Calculates standard error for excess kurtosis of the variable
#'
#' uses SPSS Book of algorithms v20
#'
#' @param target_variable numeric variable
#' @return SE for excess kurtosis
#' @export
r.kurtosis.se <- function(target_variable) {
  n <- length(stats::na.omit(target_variable))
  up_div <- 4 * (n^2 - 1) * r.skewness.se(target_variable)^2
  down_div <- (n - 3) * (n + 5)
  kurt_se <- sqrt(up_div / down_div)
  return(kurt_se)
}



# Kolmogorov Smirnov ------------------------------------------------------

#' Calculates Kolmogorov-Smirnov test with Lilliefors correction
#'
#' uses nortest package
#'
#' @param target_variable numeric variable
#' @return list with statistic and p value
#' @export
r.ks <- function(target_variable) {
  res <- nortest::lillie.test(target_variable)
  ks <- list(unname(res$statistic), unname(res$p.value))
  names(ks) <- c("statistic", "p.value")
  return(ks)
}



# Shapiro Wilk ------------------------------------------------------------

#' Calculates Shapiro-Wilk test
#'
#' The maximum data length is 5000 points by definition. If the sample size is larger than 5000 (without NAs), function selects a random sample with 5000 cases and issues a warning.
#'
#' @param target_variable numeric variable
#' @return list with statistic and p value
#' @export
r.sw <- function(target_variable) {
  if (length(stats::na.omit(target_variable)) > 5000) {
    a <- sample(target_variable, 5000)
    warning("Number of cases is larger than 5000. Test is calculated based on random sample with 5000 cases")
  } else {
    a <- target_variable
  }
  res <- stats::shapiro.test(a)
  sw <- list(unname(res$statistic), unname(res$p.value))
  names(sw) <- c("statistic", "p.value")
  return(sw)
}

des.to.flex <- function(res) {
  res <- res %>%
    flextable::flextable() %>%
    flextable::hline_top(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "body") %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(align = "center", part = "body") %>%
    flextable::align(j = 1, align = "left", part = "body") %>%
    flextable::font(fontname="Calibri", part="all") %>%
    flextable::padding(padding.top = 0, padding.bottom = 0, part="all") %>%
    flextable::width(j=1, width=5, unit="cm") %>%
    flextable::width(j=2:ncol(res), width=1.4, unit="cm")
  res
}


p.val.transf <- function(vari, option) {
  if (option$lead.zero == TRUE) {
    lead.zero.char <- "0"
  } else {
    lead.zero.char <- ""
  } # lead.zero.char

  if (option$p.type == "exact") {
    vari <- format(round(vari, 3), nsmall = 3, decimal.mark = option$d.p)
  } else if (option$p.type == "<>") {
    vari2=c()
    for (i in 1:length(vari)) {
      if (vari[i] >= 0.05) {
        vari2 <- c(vari2, paste(">", lead.zero.char, option$d.p, "05", sep = ""))
      } else if (vari[i] < 0.001) {
        vari2 <- c(vari2, paste("<", lead.zero.char, option$d.p, "001", sep = ""))
      } else if (vari[i] < 0.01) {
        vari2 <- c(vari2, paste("<", lead.zero.char, option$d.p, "01", sep = ""))
      } else if (vari[i] < 0.05) {
        vari2 <- c(vari2, paste("<", lead.zero.char, option$d.p, "05", sep = ""))
      }
    }
    vari <- vari2
  } else if (option$p.type == "star") {
    vari2=c()
    for (i in 1:length(vari)) {
      if (vari[i] >= 0.05) {
        vari2 <- c(vari2, "-")
      } else if (vari[i] < 0.001) {
        vari2 <- c(vari2, "***")
      } else if (vari[i] < 0.01) {
        vari2 <- c(vari2, "**")
      } else if (vari[i] < 0.05) {
        vari2 <- c(vari2, "*")
      }
    }
    vari=vari2
  }
  return(vari)
}




des.to.flex.by <- function(res, by_row, by_total) {
  ff <- flextable::flextable(res)


  for (i in 1:(nrow(res) / by_row)) {
    ff <- ff %>% flextable::hline(i = i * by_row, j = 1:ncol(res)) # add line after last group
    if (by_total == TRUE) {
      ff <- ff %>% flextable::hline(i = (i * by_row) - 1, j = 1:ncol(res))
    }
  }
  ff <- ff %>% flextable::merge_v(j = 1, part = "body")

  ff <- ff %>% flextable::hline_top(part = "header")
  ff <- ff %>% flextable::hline_bottom(part = "header")
  ff <- ff %>% flextable::hline_bottom(part = "body")

  ff <- ff %>% flextable::align(i = 1, j = NULL, align = "center", part = "header")


  ff <- ff %>% flextable::align(i = NULL, j = 3:ncol(res), align = "center", part = "body") %>%
    flextable::font(fontname="Calibri", part="all") %>%
    flextable::padding(padding.top = 0, padding.bottom = 0, part="all")


  flex_wids <- flextable::dim_pretty(ff)$widths
  ff <- ff %>% flextable::width(width = flex_wids)
  ff <- ff %>%
    flextable::width(j=1, width=5, unit="cm") %>%
    flextable::width(j=2, width=3, unit="cm")


  ff <- ff %>% flextable::fix_border_issues(part = "all")
  return(ff)
}
