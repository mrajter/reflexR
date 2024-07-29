

#' Claculate chi-square test with post-hocs
#'
#' @param data labelled data frame
#' @param form formula with two variables
#' @param post_hoc_chi logical. If TRUE (default), post-hoc chi-square tests are calculated
#'
#' @return Flextable with results
#' @export
chisq.flex <- function(data, form, post_hoc_chi = TRUE) {
  vari_c <- formula.tools::rhs.vars(form)[1]
  vari_r <- formula.tools::lhs.vars(form)[1]


  table <- xtabs.flex(data, form)$tab.df



  # uvjet za hi ili fischer

  if (nrow(table) == 3 & ncol(table) == 4) {
    vrsta <- "f"
    chisq <- stats::fisher.test(data[[vari_c]], data[[vari_r]])
    test <- format(round(chisq$p.value, 3), nsmall = 3, decimal.mark = ",")
    efekt <- format(round(DescTools::Phi(data[[vari_c]], data[[vari_r]]), 3), nsmall = 3, decimal.mark = ",")
  } else {
    vrsta <- "hi"
    chisq <- stats::chisq.test(data[[vari_c]], data[[vari_r]])
    test <- format(round(unname(chisq$statistic), 2), nsmall = 2, decimal.mark = ",")
    df <- chisq$parameter
    p <- format(round(chisq$p.value, 3), nsmall = 3, decimal.mark = ",")
    efekt <- format(round(DescTools::CramerV(data[[vari_c]], data[[vari_r]]), 3), nsmall = 3, decimal.mark = ",")
  }


  # tablica

  if (vrsta == "hi") {
    rez <- paste(test, df, p, sep = "\n")
    imena <- names(table)
    br_stup <- length(imena) - 2
    table <- cbind(table, rep(rez, nrow(table)), rep(efekt, nrow(table)))
    names(table) <- c(imena, paste0("\U03c7", "\U00B2", "/df/p"), "V")
    imena2 <- c(imena[1:2], rep("%", br_stup), paste0("\U03c7", "\U00B2", "/df/p"), "V")



    if (post_hoc_chi == TRUE & chisq$p.value < 0.05) {
      posthoc <- as.data.frame(
        matrix(
          nrow = length(check.labs(data[[vari_r]])$val_lab$value),
          ncol = length(check.labs(data[[vari_r]])$val_lab$value)
        )
      )

      # napravi varijantu za Xx2 tablicu. kod većih tablica je ovo problem za izvesti...bar za sada




      brojac_i <- 0
      brojac_j <- 0
      for (i in check.labs(data[[vari_r]])$val_lab$value) {
        brojac_i <- brojac_i + 1
        brojac_j <- 0
        for (j in check.labs(data[[vari_r]])$val_lab$value) {
          brojac_j <- brojac_j + 1
          if (i == j) {
            posthoc[brojac_i, brojac_j] <- "-"
          } else {
            ph_data <- data %>% dplyr::filter(get(vari_r) == i | get(vari_r) == j)
            chisq <- stats::fisher.test(ph_data[[vari_c]], ph_data[[vari_r]])$p.value
            efekt <- format(round(DescTools::Phi(ph_data[[vari_c]], ph_data[[vari_r]]), 3), nsmall = 3, decimal.mark = ",")
            if (chisq < 0.05) {
              efekt <- paste0(efekt, "*")
            }
            if (chisq < 0.01) {
              efekt <- paste0(efekt, "*")
            }
            if (chisq < 0.001) {
              efekt <- paste0(efekt, "*")
            }
            posthoc[brojac_i, brojac_j] <- efekt
          }
        }
      }




      posthoc <- rbind(posthoc, rep("", ncol(posthoc)))
      names(posthoc) <- check.labs(data[[vari_r]])$val_lab$label

      or_tab_width <- ncol(table)

      table <- cbind(table, posthoc[, -1])



      # tablica
      imena2 <- c(imena2, rep("\U03C6", ncol(posthoc) - 1))
      out <- table %>%
        flextable::flextable() %>%
        flextable::add_header_row(top = FALSE, values = imena2) %>%
        flextable::merge_v(part = "header") %>%
        flextable::merge_v(j = (or_tab_width - 1), part = "body") %>%
        flextable::merge_v(j = or_tab_width, part = "body") %>%
        flextable::hline(i = 2, border = officer::fp_border(), part = "header") %>%
        flextable::hline(i = 1, border = officer::fp_border(), part = "header") %>%
        flextable::hline(i = (nrow(table) - 1), j = (1:or_tab_width), border = officer::fp_border(), part = "body") %>%
        flextable::hline(i = nrow(table), border = officer::fp_border(), part = "body") %>%
        flextable::hline_top(border = officer::fp_border(), part = "header") %>%
        flextable::align(align = "center", part = "all") %>%
        flextable::align(j = 1, align = "left", part = "body") %>%
        flextable::padding(padding = 0, part = "all") %>%
        flextable::fix_border_issues()
    } else {
      out <- table %>%
        flextable::flextable() %>%
        flextable::add_header_row(top = FALSE, values = imena2) %>%
        flextable::merge_v(part = "header") %>%
        flextable::merge_v(j = (ncol(table) - 1), part = "body") %>%
        flextable::merge_v(j = ncol(table), part = "body") %>%
        flextable::hline(i = 2, border = officer::fp_border(), part = "header") %>%
        flextable::hline(i = 1, border = officer::fp_border(), part = "header") %>%
        flextable::hline(i = (nrow(table) - 1), border = officer::fp_border(), part = "body") %>%
        flextable::hline(i = nrow(table), border = officer::fp_border(), part = "body") %>%
        flextable::hline_top(border = officer::fp_border(), part = "header") %>%
        flextable::align(align = "center", part = "all") %>%
        flextable::align(j = 1, align = "left", part = "body") %>%
        flextable::padding(padding = 0, part = "all") %>%
        flextable::fix_border_issues()
    }
  } else {
    rez <- test
    imena <- names(table)
    br_stup <- length(imena) - 2
    table <- cbind(table, rep(rez, nrow(table)), rep(efekt, nrow(table)))
    names(table) <- c(imena, "Exact p", "\U03C6")
    imena2 <- c(imena[1:2], rep("%", br_stup), "Exact p", "\U03C6")

    out <- table %>%
      flextable::flextable() %>%
      flextable::add_header_row(top = FALSE, values = imena2) %>%
      flextable::merge_v(part = "header") %>%
      flextable::merge_v(j = (ncol(table) - 1), part = "body") %>%
      flextable::merge_v(j = ncol(table), part = "body") %>%
      flextable::hline(i = 2, border = officer::fp_border(), part = "header") %>%
      flextable::hline(i = 1, border = officer::fp_border(), part = "header") %>%
      flextable::hline(i = (nrow(table) - 1), border = officer::fp_border(), part = "body") %>%
      flextable::hline(i = nrow(table), border = officer::fp_border(), part = "body") %>%
      flextable::hline_top(border = officer::fp_border(), part = "header") %>%
      flextable::align(align = "center", part = "all") %>%
      flextable::align(j = 1, align = "left", part = "body") %>%
      flextable::padding(padding = 0, part = "all") %>%
      flextable::fix_border_issues()
  }

  return(out)
}
