#' Create comperimeter tables
#'
#' Function creates basic comperimeter flextable with optional descriptives. Variables must be labelled.
#' @param data dataset
#' @param form formula specified as 1 ~ var1 + var2 +varX
#' @param by section variable specified with quotes
#' @param by_total should totals be included when using section variable (default=TRUE)
#' @param param should parametrics (M and SD) be included (default=TRUE)
#' @param vals_to_labs if TRUE values will be put in column headings and value labels in result list (default=TRUE)
#' @param m.deci number of decimal points for parametrics (default=1)
#' @param p.deci number of decimal points for percentages (default=1)
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
#'     \item legend - vector of value labels for column headings
#' }
#'
#' @examples
#' \dontrun{
#' comp.flex(data=rxR_data, form=1~education, param=TRUE, vals_to_labs=FALSE)
#' }
#'
#' @export
comp.flex <- function(data, form, by = NA, by_total = TRUE, param = TRUE, vals_to_labs=TRUE, m.deci = 1, p.deci = 1, title = "", option = r.flex.opts) {
  # get variable names - important only for title
  vars <- NULL
  vars <- attr(stats::terms(form), which = "variables")
  vars <- as.character(vars[3:length(vars)])
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
  if (check.labs(data[[vars[1]]])$val_lab_ok == FALSE) {
    tcltk::tk_messageBox(type = "ok", message = paste0("There are more unique values in variable -", vars[1], "- then defined value labels"))
    stop(paste0("There are more unique values in variable -", vars[1], "- then defined value labels"))
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


    labels_vector_by <- check.labs(data[[by]])$val_lab
    by_df <- data.frame(vals = labels_vector_by$value, labs = labels_vector_by$label)
  }

  # workflow
  if (is.na(by) == TRUE) {
    res <- comperimeter(data = data, form = form, by = by, by_total = by_total, param = param, m.deci = m.deci, p.deci = p.deci, option = option)
  } else {
    res_list <- list()
    for (counter_by in 1:nrow(by_df)) {
      res_list[[counter_by]] <- comperimeter(sjlabelled::copy_labels(data[data[[by]] == by_df[counter_by, 1], ], data), form = form, by = by, by_total = by_total, param = param, m.deci = m.deci, p.deci = p.deci, option = option)
    }
    if (by_total == TRUE) {
      res_list[[nrow(by_df) + 1]] <- comperimeter(data = data, form = form, by = by, by_total = by_total, param = param, m.deci = m.deci, p.deci = p.deci, option = option)
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
    res <- res %>% dplyr::select(.data$Name, .data$Group, tidyselect::everything())
    res <- dplyr::select(res, -c(.data$no_var, .data$no_gr))
  }

  # set title

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
    if (is.na(by) == FALSE) {
      names(res)[2] <- sjlabelled::get_label(data[[by]])
    }
  }
  #table

  #define column names if vals_to_labs
  if (vals_to_labs==TRUE) {
    start_comp <- ncol(res)-nrow(check.labs(data[[vars[1]]])$val_lab)+1 #column where the comperimeter starts
    end_comp <- ncol(res) #end column
    names(res)[start_comp:end_comp] <- check.labs(data[[vars[1]]])$val_lab$value
  } else {
    start_comp <- ncol(res)-nrow(check.labs(data[[vars[1]]])$val_lab)+1 #column where the comperimeter starts
    end_comp <- ncol(res) #end column
    names(res)[start_comp:end_comp] <- check.labs(data[[vars[1]]])$val_lab$label
  }

  if (is.na(by) == FALSE) {
    type <- "comp"
    table <- comp.to.flex.by(res, param = param, by_row = nrow(by_df), by_total = by_total)
  } else {
    type <- "comp"
    table <- comp.to.flex(res, param = param)
  }
  if (param == TRUE) {
    type <- paste0(type, "_desc")
  } else {
    type <- paste0(type, "_nodesc")
  }


  if (sum(table$body$colwidths)*2.54<19.8) {
    orientation <- "P"
  } else {
    orientation <- "L"
  }

  if (vals_to_labs==TRUE){
    legend <- check.labs(data[[vars[1]]])$val_lab
    legend$legend <- paste0(legend$value, " - ", legend$label)
  } else {
    legend$legend <- "not needed"
  }
  result <- list(type = type, title = title, section = title.by, table = table, tab.df = res, orientation=orientation, legend=legend$legend)

  return(result)
}









comperimeter <- function(data, form, by = NA, by_total = TRUE, param = TRUE, m.deci = 1, p.deci = 1, option = r.flex.opts, lang = "hr") {
  # collect variable names
  v.names <- attr(stats::terms(form), which = "variables")
  v.names <- as.character(v.names[3:length(v.names)])

  # collect value labels - the function will work only with labelled data
  if (check.labs(data[[v.names[1]]])$has_val_lab == FALSE) {
    warning("Value labels should be defined for comperimeter table")
  } else {
    v.labs <- data.frame(labs = names(attributes(data[[v.names[1]]])$labels), vals = unname(attributes(data[[v.names[1]]])$labels))
  }


  # radi? prvo za jednu varijablu pa tek onda za ostale
  # tri varijable - imena, deskriptiva, postotci


  r.names <- data.frame(Name = sjlabelled::get_label(data[[v.names[1]]]), N = sum(is.na(data[[v.names[1]]]) == FALSE))
  if (param == TRUE) {
    if (is.na(r.names$N) | r.names$N == 0) {
      r.desc <- data.frame(M = NA, SD = NA) # if there is no data under the category
    } else {
      r.desc <- data.frame(
        M = mean(data[[v.names[1]]], na.rm = TRUE),
        SD = stats::sd(data[[v.names[1]]], na.rm = TRUE)
      )
    }
  }

  fre.t <- c()
  for (i in 1:nrow(v.labs)) {
    if (is.na(r.names$N) | r.names$N == 0) {
      fre.t <- c(fre.t, NA)
    } else {
      fre.t <- c(fre.t, length(data[[v.names[1]]][data[[v.names[1]]] == v.labs$vals[i] & is.na(data[[v.names[1]]])==FALSE]) * 100 / r.names$N[1])
    }
  }
  r.fre <- data.frame(t(fre.t)) #transpose
  names(r.fre) <- v.labs$labs


  # start with other variables
  if (length(v.names) > 1) {
    # provjeri jesu li labeli isti
    res <- 1
    for (i in 2:length(v.names)) {
      # check for errors in value labels
      if (check.labs(data[[v.names[i]]])$val_lab_ok == FALSE) {
        tcltk::tk_messageBox(type = "ok", message = paste0("There are more unique values in variable -", v.names[1], "- then defined value labels"))
        stop(paste0("There are more unique values in variable -", v.names[i], "- then defined value labels"))
      }


      test.labs <- data.frame(labs = names(attributes(data[[v.names[i]]])$labels), vals = unname(attributes(data[[v.names[i]]])$labels))
      tryCatch(if (all(v.labs == test.labs) == TRUE) {
        res <- 1
      } else {
        res <- 0
      }, error = function(e) {
        stop("Something is wrong, probably with labels of variables in comperimeter")
      })
    }
    if (res == 0) {
      warning("Variables in comperimeter tables should have same values and value labels")
    }

    # imena
    for (i in 2:length(v.names)) {
      r.names <- rbind(r.names, list(sjlabelled::get_label(data[[v.names[i]]]), sum(is.na(data[[v.names[i]]]) == FALSE)))

      # parametrija
      if (param == TRUE) {
        if (is.na(r.names$N[i]) | r.names$N[i] == 0) {
          r.desc <- rbind(r.desc, NA, NA)
        } else {
          r.desc <- rbind(r.desc, c(mean(data[[v.names[i]]], na.rm = TRUE), stats::sd(data[[v.names[i]]], na.rm = TRUE)))
        }
      }


      # freqs
      fre.t <- c()
      for (j in 1:nrow(v.labs)) {
        if (r.names$N[i] == 0) {
          fre.t <- c(fre.t, NA)
        } else {
          fre.t <- c(fre.t, length(data[[v.names[i]]][data[[v.names[i]]] == v.labs$vals[j] & is.na(data[[v.names[i]]])==FALSE]) * 100 / r.names$N[i])
        }
      }
      r.fre <- rbind(r.fre, fre.t)
    }
  }


  # format cells and create data.frame
  r.names[2] <- format(r.names[2], nsmall = 0)

  if (param == TRUE) {
    r.desc$M <- format(round(r.desc$M, m.deci), nsmall = m.deci, decimal.mark = option$d.p)
    r.desc$SD <- format(round(r.desc$SD, m.deci + 1), nsmall = m.deci + 1, decimal.mark = option$d.p)
    r.desc <- r.desc %>% dplyr::mutate_all(trimws)
    r.desc[r.desc == "NA"] <- "-"
    r.fre <- format(round(r.fre, p.deci), nsmall = p.deci, decimal.mark = option$d.p)
    r.fre <- r.fre %>% dplyr::mutate_all(trimws) # necessary because format puts spaces around NA's
    r.fre[r.fre == "NA"] <- "-"
    res.df <- cbind(r.names, r.desc, r.fre)
  } else {
    r.fre <- format(round(r.fre, p.deci), nsmall = p.deci, decimal.mark = option$d.p)
    r.fre <- r.fre %>% dplyr::mutate_all(trimws)
    res.df <- cbind(r.names, r.fre)
    r.fre[r.fre == "NA"] <- "-"
  }

  # if (option$lang == "hr") {
  #   names(res.df)[1] <- "Varijabla"
  # } else {
  #   names(res.df)[1] <- "Variable"
  # }

  # res.df <- comp.to.flex(res.df, param = param)
  return(res.df)
}






comp.to.flex <- function(df, param) {

  # definicije za stupce
  if (param == TRUE) {
    col.names <- c(names(df)[1], names(df)[2], names(df)[3], names(df)[4])
    for (i in 5:ncol(df)) {
      col.names <- c(col.names, "%")
    }
  } else {
    col.names <- c(names(df)[1], names(df)[2])
    for (i in 3:ncol(df)) {
      col.names <- c(col.names, "%")
    }
  }

  # tablica
  ff <- df %>%
    flextable::flextable() %>%
    flextable::hline_top(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "body") %>%
    flextable::add_header_row(top = FALSE, values = col.names) %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::merge_v(part = "header") %>%
    flextable::fix_border_issues() %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(align = "center", part = "body") %>%
    flextable::align(j = 1, align = "left", part = "body")

  #widths, font and padding
  if (param==TRUE) {
    ff <- ff %>% flextable::width(j=2:4, width=1.4, unit="cm")
    col_min=5
  } else {
    col_min=2
  }
  ff <- ff %>% flextable::width(j=1, width=5, unit="cm") %>%
    flextable::width(j=col_min:ncol(df), width=1.6, unit="cm") %>%
    flextable::font(fontname="Calibri", part="all") %>%
    flextable::padding(padding.top = 0, padding.bottom = 0)

  return(ff)
}


comp.to.flex.by <- function(df, param, by_row, by_total) {

  # definicije za stupce
  if (param == TRUE) {
    col.names <- c(names(df)[1], names(df)[2], names(df)[3], names(df)[4], names(df)[5])
    for (i in 6:ncol(df)) {
      col.names <- c(col.names, "%")
    }
  } else {
    col.names <- c(names(df)[1], names(df)[2], names(df)[3])
    for (i in 4:ncol(df)) {
      col.names <- c(col.names, "%")
    }
  }

  # tablica
  ff <- df %>%
    flextable::flextable() %>%
    flextable::hline_top(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "body") %>%
    flextable::add_header_row(top = FALSE, values = col.names) %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::merge_v(part = "header") %>%
    flextable::fix_border_issues() %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(align = "center", part = "body") %>%
    flextable::align(j = 1, align = "left", part = "body")

  # lines for subtotals and groups
  for (i in 1:(nrow(df) / by_row)) {
    ff <- ff %>% flextable::hline(i = i * by_row, j = 1:ncol(df)) # add line after last group
    if (by_total == TRUE) {
      ff <- ff %>% flextable::hline(i = (i * by_row) - 1, j = 1:ncol(df))
    }
  }
  ff <- ff %>% flextable::merge_v(j = 1, part = "body")
  ff <- ff %>% flextable::align(i = NULL, j = 2, align = "left", part = "body")
  ff <- ff %>% flextable::fix_border_issues()

  #widths, font and padding
  if (param==TRUE) {
    ff <- ff %>% flextable::width(j=3:5, width=1.4, unit="cm")
    col_min=6 #minimum columns if parametrics are present
  } else {
    col_min=3
  }
  ff <- ff %>% flextable::width(j=1, width=5, unit="cm") %>%
    flextable::width(j=2, width=3, unit="cm") %>%
    flextable::width(j=col_min:ncol(df), width=1.2, unit="cm") %>%
    flextable::font(fontname="Calibri", part="all") %>%
    flextable::padding(padding.top = 0, padding.bottom = 0, part="all")

  return(ff)
}
