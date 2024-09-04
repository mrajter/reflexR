#' Hierarhical regression analysis
#'
#' @param data dataset with variables
#' @param ... one or more formulas separated by comma (e.g out ~ v1 + v2, out ~ v3 + v4). Each model should not contain variables from previous step
#' @param option r.flex.opts
#'
#' @return list with two elements: a flextable and data frame with results
#' @export
hra.flex <- function(data, ..., option = r.flex.opts) {
  arguments <- list(...)
  #----------------------

  steps <- length(arguments)

  # how many variables in each step
  vars_per_step <- unlist(lapply(arguments, function(x) length(formula.tools::rhs.vars(x))))

  form_step <- list(arguments[[1]]) # this will be a list of formulas
  if (steps > 1) {
    for (i in 2:steps) {
      form_step <- append(
        form_step,
        stats::as.formula(
          paste0(
            as.character(form_step[[i - 1]][2]),
            " ~ ",
            as.character(form_step[[i - 1]][3]),
            " + ",
            as.character(arguments[[i]][3])
          )
        )
      )
    }
  }

  # variable names
  var.names <- data.frame(var = formula.tools::rhs.vars(form_step[[steps]]))
  var.names$labs <- unlist(lapply(var.names$var, function(x) {
    if (is.na(check.labs(data[[x]])$var_lab)) {
      return(x)
    } else {
      return(check.labs(data[[x]])$var_lab)
    }
  }))



  # scale data.frame
  data <- as.data.frame(scale(stats::model.frame(form_step[[steps]], data)))



  #-----------------------
  # models
  models <- list()
  for (i in 1:steps) {
    models <- append(models, list(stats::lm(form_step[[i]], data)))
  }

  # coefficients
  coefficients <- var.names
  for (i in 1:steps) {
    coef_df <- data.frame(summary(models[[i]])$coefficients)[-1, ]
    coef_temp <- c()
    for (j in 1:nrow(coef_df)) {
      coef_temp <- c(coef_temp, add.p.star(pval.apa(coef_df[j, 1]), coef_df[j, 4]))
    }




    coef_temp <- c(coef_temp, rep(NA, length(coefficients$var) - length(coef_temp)))
    coefficients <- cbind(coefficients, coef_temp)
    names(coefficients)[ncol(coefficients)] <- paste0("m", i)
  }

  # r, r2, r2cor, r2change
  mod_par <- data.frame(matrix(nrow = 4, ncol = steps)) # parameters
  mod_pval <- data.frame(matrix(nrow = 2, ncol = steps)) # f ratios (r and delta R)
  for (i in 1:steps) {
    a <- summary(models[[i]])
    mod_par[1, i] <- pval.apa(sqrt(a$r.squared)) # format to apa style
    mod_par[2, i] <- pval.apa(a$r.squared)
    mod_par[3, i] <- pval.apa(a$adj.r.squared)
    if (i > 1) {
      mod_par[4, i] <- pval.apa(a$r.squared - summary(models[[i - 1]])$r.squared)
    }

    # mod_pval
    mod_pval[1, i] <- stats::pf(a$fstatistic[1], df1 = a$fstatistic[2], df2 = a$fstatistic[3], lower.tail = FALSE)
    if (i > 1) {
      mod_pval[2, i] <- stats::anova(models[[i]], models[[i - 1]])[2, 6]
    }

    # apperance
    mod_par[1, i] <- add.p.star(mod_par[1, i], mod_pval[1, i])
    if (i > 1) {
      mod_par[4, i] <- add.p.star(mod_par[4, i], mod_pval[2, i])
    }
  }

  # names for model parameters
  names_mod_par <- "R"
  names_mod_par <- c(names_mod_par, paste0("R", "\U00B2"))
  names_mod_par <- c(names_mod_par, paste0("R", "\U00B2", "\U2090"))
  names_mod_par <- c(names_mod_par, paste0("\U0394", "R", "\U00B2"))

  # create table for parameters
  mod_par <- cbind(vars = c(NA, NA, NA, NA), a = c(NA, NA, NA, NA), b = c(NA, NA, NA, NA), names_mod_par, mod_par)

  # correlation bivariate

  a <- Hmisc::rcorr(as.matrix(data))
  cor_df <- data.frame(cor = a$r[-1, 1], p = a$P[-1, 1])
  cor_vector <- c()
  for (i in 1:nrow(cor_df)) {
    cor_vector <- c(cor_vector, add.p.star(pval.apa(cor_df[i, 1]), cor_df[i, 2]))
  }

  # VIF
  vif_vector <- unname(car::vif(models[[steps]]))
  vif_vector <- format(round(vif_vector, 2), nsmall = 2, decimal.mark = option$d.p)


  fin_tab <- cbind(coefficients[, 1:2], cor_vector, vif_vector, coefficients[, 3:ncol(coefficients)])
  for (i in 5:ncol(fin_tab)) {names(fin_tab)[i] <- paste0("m", i-4)}
  colnames(mod_par) <- colnames(fin_tab)
  fin_tab <- rbind(fin_tab, mod_par)
  if (steps == 1) {
    fin_tab <- fin_tab[-nrow(fin_tab), ]
  }

  res=list()
  res$df=fin_tab
  res$table=hra.to.flex(fin_tab, steps, vars_per_step)
  return(res)
}

#--------------------
# FLEXTABLE


hra.to.flex=function(df, steps, vars_per_step, option=r.flex.opts) {
  #prepare for flextable
  df=df[,-1]

  colnames(df)=c("Varijabla", paste0("rpk"), "VIF", colnames(df)[4:ncol(df)])


  tab_df=df%>%
    flextable::flextable()%>%
    #flextable::set_header_labels(values = col.names)
    flextable::hline_top(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "body")

  if (option$lang=="hr") {
    tab_df=tab_df %>% flextable::compose(part="header", j="Varijabla", value = flextable::as_paragraph("Varijabla"))
    tab_df=tab_df %>% flextable::compose(part="header", j="rpk", value = flextable::as_paragraph("r", flextable::as_sub("pk")))
  } else {
    tab_df=tab_df %>% flextable::compose(part="header", j="Varijabla", value = flextable::as_paragraph("Variable"))
    tab_df=tab_df %>% flextable::compose(part="header", j="rpk", value = flextable::as_paragraph("r", flextable::as_sub("pc")))
  }

  #borders
  temp_i=0
  for (i in 1:steps) {
    temp_i=temp_i+vars_per_step[i]
    tab_df=tab_df %>% flextable::hline(i=temp_i, border = officer::fp_border(color = "black", width = 1), part = "body")
  }


  #alignments
  tab_df=tab_df %>% flextable::align(align = "center", part = "header") %>%
    flextable::align(align = "center", part = "body") %>%
    flextable::align(j = 1, align = "left", part = "body") %>%
    flextable::align(i=sum(vars_per_step):nrow(df), j = 3, align = "right", part = "body")

  #  #widths
  tab_df <- tab_df %>% flextable::width(j=1, width=5, unit="cm") %>%
    flextable::width(j=2, width=1.8, unit="cm") %>%
    flextable::width(j=3, width=1.1, unit="cm") %>%
    flextable::width(j=4:ncol(df), width=1.8, unit="cm") %>%
    flextable::font(fontname="Calibri", part="all") %>%
    flextable::padding(padding.top = 0, padding.bottom = 0, part="all") %>%
    flextable::padding(padding.top = 1, padding.bottom = 2, part="header")

  return(tab_df)
}




#----------------------------------
add.p.star <- function(val, pval) {
  if (is.numeric(val)) {
    val <- as.character(val)
  }
  if (pval < 0.05) {
    val <- paste0(val, "*")
  }
  if (pval < 0.01) {
    val <- paste0(val, "*")
  }
  if (pval < 0.001) {
    val <- paste0(val, "*")
  }
  return(val)
}



pval.apa <- function(num, deci = 3, equal = F, option = r.flex.opts) {
  if (num > 1 | num < -1) {
    tcltk::tk_messageBox(type = "ok", message = "This is not a p-value")
    stop("This is not a p-value")
  }

  if (abs(num) < (10^(-1 * deci))) {
    res <- paste0("<", option$d.p, paste0(rep("0", deci - 1), collapse = ""), "1")
  } else {
    res <- format(round(num, deci), nsmall = deci, decimal.mark = option$d.p)

    if (abs(num) == 1) {
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



