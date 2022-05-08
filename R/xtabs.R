#' Create crosstabs (contingency tables)
#'
#' Function creates crosstabulation with multiple variables in rows and one variable in column.
#' @param data dataset
#' @param form formula specified as row variables on left hand side and one section variable (columns) on right hand side. E.g. var1 + var2 + var3 ~ var_sect
#' @param deci number of decimal points (default=1)
#' @param title Table title (optional)
#' @param option inherits from r.flex.opts
#' @return list with elements
#' \itemize{
#'     \item type - table type - used for inserting in word document (, desc - are there descriptives)
#'     \item title - used for table title. Can be set manually or automatically
#'     \item section - section variable title
#'     \item table - flextable with results
#'     \item tab.df - results as data.frame
#'     \item orientation - suggested page orientation (P/L)
#' }
#' @export
xtabs.flex <- function(data, form, deci = 1, title = "", option = r.flex.opts) {
  vars.all <- formula.tools::lhs.vars(form)
  vars.by <- formula.tools::rhs.vars(form)

  # check if there are two variables in formula
  if (length(vars.by) != 1) {
    tcltk::tk_messageBox(type = "ok", message = "Formula for crosstabs needs one variable on right side")
    stop("Formula for crosstabs needs one variable on right side")
  }

  # title
  if (title == "") {
    if (length(vars.all) == 1) {
      if (is.na(check.labs(data[[vars.all]])$var_lab)) {
        title <- vars.all
      } else {
        title <- check.labs(data[[vars.all]])$var_lab
      }
    } else {
      title <- paste0(vars.all[1], " - ", vars.all[length(vars.all)])
    }
  }
  # section
  if (is.na(check.labs(data[[vars.by]])$var_lab)) {
    section <- vars.by
  } else {
    section <- check.labs(data[[vars.by]])$var_lab
  }

  # is there more than one variable for xtabs
  if (length(vars.all) == 1) {
    multi <- 0
  } else {
    multi <- 1
  }


  # harvest results
  res <- data.frame()
  n_cat <- c() # number of rows for each var
  for (i in 1:length(vars.all)) {
    vars <- c(vars.all[i], vars.by)
    res.t <- xtabs_single(data, vars, deci)
    if (multi == 1) {
      res.t <- cbind(rep(names(res.t)[1], nrow(res.t)), res.t)
      if (i > 1) {
        names(res.t) <- names(res)
      }
      res <- rbind(res, res.t)
      n_cat <- c(n_cat, nrow(res.t))
    } else {
      res <- res.t
    }
  }

  # set names
  if (multi == 1) {
    if (option$lang == "hr") {
      names(res)[1] <- "Varijabla"
      names(res)[2] <- "Odgovor"
    } else {
      names(res)[1] <- "Variable"
      names(res)[2] <- "Response"
    }
  }
  # send to flextable
  table <- xtabs_to_flex(res, multi, deci, n_cat, option)

  # calculate orientation
  if (sum(table$body$colwidths) * 2.54 < 19.8) {
    orientation <- "P"
  } else {
    orientation <- "L"
  }

  result <- list(type = "xtabs", title = title, section = section, table = table, tab.df = res, orientation = orientation)
  return(result)
}


#------------------------

# convert ftable to data.frame
ftable_to_df <- function(ftable) {
  res <- as.data.frame(ftable)
  res <- tidyr::pivot_wider(res, names_from = names(res)[2], values_from = names(res)[3])
  return(res)
}


#------------------------

xtabs_single <- function(data, vars, deci) {

  # get two tables
  df <- data %>% dplyr::select(dplyr::all_of(vars))
  frq <- sjmisc::flat_table(df, margin = "counts")
  pct <- sjmisc::flat_table(df, margin = "row", digits = deci)

  # convert to data.frames
  frq <- ftable_to_df(frq)
  pct <- ftable_to_df(pct)

  # create res df
  res <- data.frame(frq[1], rowSums(frq %>% dplyr::select_if(is.numeric)), pct %>% dplyr::select_if(is.numeric))
  res[1] <- lapply(res[1], as.character)

  # add total row
  tot_n <- sum(res[2])
  tot_pct <- round(colSums(frq %>% dplyr::select_if(is.numeric)) * 100 / tot_n, deci)
  last_row <- list("Total")
  last_row <- append(last_row, tot_n)
  last_row <- append(last_row, unname(tot_pct))
  res <- rbind(res, last_row)

  # make pretty
  if (is.na(check.labs(df[[vars[1]]])$var_lab)) {
    names(res)[1] <- vars[1]
  } else {
    names(res)[1] <- check.labs(df[[vars[1]]])$var_lab
  }

  names(res)[2] <- "N"

  # other columns

  if (check.labs(df[[vars[1]]])$has_val_lab == TRUE) {
    names(res)[3:length(names(res))] <- check.labs(df[[vars[2]]])$val_lab$label
  }

  return(res)
}

#------------------------

xtabs_to_flex <- function(tab, multi, deci, n_cat, option) {

  # formats

  tab[2 + multi] <- format(tab[2 + multi], nsmall = 0, big.mark = "")
  tab[(3 + multi):ncol(tab)] <- format(tab[(3 + multi):ncol(tab)], decimal.mark = option$d.p)

  # radi za jednu var pa ćeš dalje
  col.names <- c(names(tab)[1:(2 + multi)], rep("%", ncol(tab) - 2 - multi))
  ff <- tab %>%
    flextable::flextable() %>%
    flextable::hline_top(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "body") %>%
    flextable::add_header_row(top = FALSE, values = col.names) %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::merge_v(part = "header") %>%
    flextable::hline(i = nrow(tab) - 1, border = officer::fp_border(color = "black", width = 1), part = "body") %>%
    flextable::fix_border_issues() %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(align = "center", part = "body") %>%
    flextable::align(j = 1:(1 + multi), align = "left", part = "body")

  # multi
  if (multi == 1) {
    row_counter <- 0
    for (i in 1:length(n_cat)) {
      row_counter <- row_counter + n_cat[i]
      ff <- ff %>%
        flextable::hline(i = row_counter - 1, j = 2:ncol(tab), border = officer::fp_border(color = "black", width = 1), part = "body") %>%
        flextable::hline(i = row_counter, border = officer::fp_border(color = "black", width = 1), part = "body")
    }
    ff <- ff %>%
      flextable::merge_v(j = 1, part = "body") %>%
      flextable::width(j = 1, width = 4, unit = "cm") %>%
      flextable::fix_border_issues() %>%
      flextable::valign(j = 1, valign = "top", part = "body")
  }

  # widths and padding
  ff <- ff %>%
    flextable::width(j = (1 + multi), width = 4, unit = "cm") %>%
    flextable::width(j = (2 + multi), width = 1.2, unit = "cm") %>%
    flextable::width(j = (3 + multi):ncol(tab), width = 2, unit = "cm") %>%
    flextable::font(fontname = "Calibri", part = "all") %>%
    flextable::padding(padding.top = 0, padding.bottom = 0, part = "all")

  return(ff)
}
