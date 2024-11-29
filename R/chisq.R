#' calculate chi square test for single independent and multiple dependent variables
#'
#' @param data dataset with variables with defined variable and value labels
#' @param form formula. Independent variable must be on the right side of the formula. Function allows for only one dependent variable which will be shown in the columns
#' @param pct.calc calculation of percentages. "by_dep" (default) calculates percentages by dependent variable, "by_ind" calculates percentages by independent variable
#'
#' @return flextable with results
#' @export
#'
#' @examples
#' #create data frame
#' dataset <- data.frame(ind=c(1,1,1,2,2,2,1,1,1,2,2,2),
#'                       dep1=c(1,2,3,1,2,3,1,2,3,1,2,3),
#'                       dep2=c(1,2,2,1,3,3,1,2,2,1,3,3)) %>%
#'   var_to_labelled("ind", "Independent", c("1"="No", "2"="Yes")) %>%
#'   var_to_labelled("dep1", "Dependent 1", c("1"="A", "2"="B", "3"="C")) %>%
#'   var_to_labelled("dep2", "Dependent 2", c("1"="D", "2"="E", "3"="F"))
#' #calculate test
#' chisq.flex(dataset, dep1+dep2~ind)
chisq.flex=function(data, form, pct.calc="by_dep") {
  #idi jednu po jednu varijablu
  vari_c <- formula.tools::rhs.vars(form)[1]
  vari_r <- formula.tools::lhs.vars(form)
  varijable=c()

  for (i in 1:length(vari_r)) {
    #pošalji formulu za jednostavni hi kvadrat test
    form2 <- stats::as.formula(paste0(vari_r[i], "~", vari_c))
    q=chisq.sing(data, form2, post_hoc_chi = FALSE)$body$dataset
    if (i==1) {
      out=q
      varijable=c(varijable, names(out)[1])
      names(out)[1]="Odgovor"

    } else {
      varijable=c(varijable, names(q)[1])
      names(q)[1]="Odgovor"
      out=rbind(out, q)
    }
  }

  #napravi tablicu
  stupac1=c()
  for (i in 1:length(vari_r)) {
    stupac1=c(stupac1, rep(check.labs(data[[vari_r[i]]])$var_lab, length(check.labs(data[[vari_r[i]]])$val_lab$value)+1))
  }

  out=cbind(stupac1,out)
  names(out)[1]="Varijabla"

  if(names(out)[length(out)]=="V") {
    vrsta="hi"
  } else {
    vrsta="f"
  }

  imena2=names(out)
  for (i in 4:(length(imena2)-3)) {
    imena2[i]="%"
  }
  #if(vrsta=="f") {
  imena2[length(imena2)-2]="%"
  #}


  tbl=out %>% flextable::flextable() %>%
    flextable::add_header_row(top = FALSE, values = imena2) %>%
    flextable::hline(i = 2, border = officer::fp_border(), part = "header") %>%
    flextable::hline(i = 1, border = officer::fp_border(), part = "header") %>%
    flextable::merge_v(part = "header")

  brojac=0
  for (k in 1:length(vari_r)) {
    duzina=length(check.labs(data[[vari_r[k]]])$val_lab$value)+1
    tbl=tbl %>%
      flextable::merge_at(i=(brojac+1):(brojac+duzina), j = 1, part = "body") %>%
      flextable::merge_at(i=(brojac+1):(brojac+duzina), j = (length(imena2)-1), part = "body") %>%
      flextable::merge_at(i=(brojac+1):(brojac+duzina), j = length(imena2), part = "body") %>%
      flextable::hline(i=(brojac+duzina-1), j=2:(length(imena2)-3), border = officer::fp_border(), part = "body")

    #if (vrsta=="f") {
    tbl=tbl %>% flextable::hline(i=(brojac+duzina-1), j=(length(imena2)-2), border = officer::fp_border(), part = "body")
    #}
    tbl=tbl %>% flextable::hline(i = (brojac+duzina), border = officer::fp_border(), part = "body")
    brojac=brojac+duzina
  }
  tbl=tbl %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "body") %>%
    flextable::padding(padding = 0, part = "all") %>%
    flextable::autofit() %>%
    flextable::fix_border_issues()




  return(tbl)

}











chisq.sing <- function(data, form, post_hoc_chi = TRUE, pct.calc="by_dep") {
  vari_c <- formula.tools::rhs.vars(form)[1]
  vari_r <- formula.tools::lhs.vars(form)[1]


  if (pct.calc=="by_dep") {table <- xtabs.flex(data, form)$tab.df
  # u table je sad napravljena tablica gdje se postotci računaju prema redu. Treba ti i varijanta kada se računaju prema stupcu. To se radi tako da su ti varijable zamijenjene
  } else {
  table <- xtabs.flex(data, stats::as.formula(paste0(vari_c, "~", vari_r)))$tab.df
  names(table)[1] <- check.labs(data[[vari_r]])$var_lab
  }



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
