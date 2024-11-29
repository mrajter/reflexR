#' Independent samples t-test
#'
#' @param data dataset
#' @param form formula in format dependent ~ independent. Can have multiple dependent variables (dep1+dep2+dep3~ind) or multiple independent variables (dep~ind1+ind2+ind3), but NOT BOTH.
#' @param groups vector with two values for independent variable indicating groups to be analyzed. The default is c(0,0) indicating that the groups will be determined from the variable itself. If only one value is provided, it will be used as cut-off point with criteria g1<=CUT_FF; g2>CUT-OFF
#' @param effect effect size "d" (default) for Cohen's d or "prob_sup" for probability of superiority.
#' @param type type of test (one or two sided). Options are "two.sided" (default), "greater" and "less".
#' @param deci number of decimal places for M
#' @param conf.level confidence level
#' @param lang language of the output. Options are "hr" (default) and "en".
#'
#' @return list with table as flextable and data frame with results
#' @export
#'
#' #' @examples
#' #create data frame
#' dataset <- data.frame(ind=c(1,1,1,2,2,2,1,1,1,2,2,2),
#'                       dep1=c(1,2,3,1,2,3,1,2,3,1,2,3),
#'                       dep2=c(1,2,2,1,3,3,1,2,2,1,3,3)) %>%
#'   var_to_labelled("ind", "Independent", c("1"="No", "2"="Yes")) %>%
#'   var_to_labelled("dep1", "Dependent 1") %>%
#'   var_to_labelled("dep2", "Dependent 2")
#'
#' #calculate test
#' ttest.flex(dataset, dep1+dep2~ind)
ttest.flex <- function(data, form, groups = c(0, 0), effect = "d", type = "two.sided", deci = 2, conf.level = 0.95, lang = "hr") {
  # data: data frame
  # form: formula
  # effect: effect size "d" or "prob_sup"
  # type: type of test (one or two sided)
  # deci: number of decimal places
  # conf.level: confidence level

  # the formula should allow for multiple dependent or multiple independent variables (not mixed)
  # this is resolved by loops

  # steps for single analysis:
  # 1. subset by levels of the independent variable
  # 2. perform t-test
  # 3. calculate effect size
  # 4. save to data frame
  # 5. return data frame

  # steps for major analysis
  # 1. check if there are multiple dependents or multiple independents
  # 2. if multiple dependents, loop through dependents
  # 3. if multiple independents, loop through independents
  # 4. save to data frame
  # 5. push to flextable
  # 6. return flextable

  # checks
  # 1. independent needs to have exactly 2 levels
  # 2. dependent needs to be numeric


  #---------------------------------------------------


  # check if there are multiple dependents or independents
  rhs <- formula.tools::rhs.vars(form)
  lhs <- formula.tools::lhs.vars(form)
  if (length(lhs) > 1 & length(rhs) > 1) {
    stop("The formula should have only one dependent or only one independent variable")
  }

  # check if the number of groups defined is at least 2
  if (length(groups) > 2) {
    stop("The groups should be defined with no more than 2 values")
  }

  rez <- data.frame(matrix(nrow = 0, ncol = 9))
  names(rez) <- c("Dependent", "Independent", "N", "Mean", "SD", "t", "df", "p", "effect")

  if (length(lhs) > 1) {
    nvars <- length(lhs)
    multi <- "dep"
    for (i in 1:length(lhs)) {
      form1 <- stats::formula(paste(lhs[i], "~", rhs))
      rez <- rbind(rez, small.t.flex(data = data, form = form1, groups = groups, effect = effect, type = type, deci = deci, conf.level = conf.level, multi = multi))
    }
  } else if (length(rhs) > 1) {
    nvars <- length(rhs)
    multi <- "ind"
    for (i in 1:length(rhs)) {
      form1 <- stats::formula(paste(lhs, "~", rhs[i]))
      rez <- rbind(rez, small.t.flex(data = data, form = form1, groups = groups, effect = effect, type = type, deci = deci, conf.level = conf.level, multi = multi))
    }
  } else {
    nvars <- 1
    multi <- "dep"
    rez <- small.t.flex(data = data, form = form, groups = groups, effect = effect, type = type, deci = deci, conf.level = conf.level, multi = multi)
  }
  tab <- t.to.flex(rez = rez, data = data, deci = deci, effect = effect, nvars = nvars, lang = lang)

  fin <- list(
    table = tab,
    df = rez,
    formula = form
  )

  return(fin)
}



#' single t-test. A helper function for ttest.flex
#'
#' @param data a parameter
#' @param form a parameter
#' @param multi a parameter
#' @param groups a parameter
#' @param effect a parameter
#' @param type a parameter
#' @param deci a parameter
#' @param conf.level a parameter
#'
#' @return single dataset for t-test
small.t.flex <- function(data, form, multi, groups, effect = "d", type = "two.sided", deci = 2, conf.level = 0.95) {
  # obrada nezavisne varijable po grupama
  # ako postoji cutoff, onda se koristi kao grupa
  if (length(groups) == 1) {
    data[[all.vars(form)[2]]] <- ifelse(data[[all.vars(form)[2]]] <= groups, 0, 1)
    data[[all.vars(form)[2]]] <- sjlabelled::set_labels(data[[all.vars(form)[2]]], labels = c("0" = paste0("<= ", groups), "1" = paste0("> ", groups)))
    levels <- c(0, 1)
  } else if (all(c(0, 0) == groups) == FALSE) { # ako su definirane grupe. Pazi na redoslijed.
    levels <- groups
  } else { # automatically see the levels
    levels <- stats::na.omit(unique(data[[all.vars(form)[2]]]))
    # check if the independent variable has exactly 2 levels
    if (length(stats::na.omit(unique(data[[all.vars(form)[2]]]))) != 2) {
      stop("The independent variable needs to have exactly 2 levels. You ned to define groups usung the groups argument or check the data for missing value")
    }

  }


  # add variable labels
  if (is.na(check.labs(data[[all.vars(form)[1]]])$var_lab) == TRUE) {
    data[[all.vars(form)[1]]] <- sjlabelled::set_label(data[[all.vars(form)[1]]], label = all.vars(form)[1])
  }
  if (is.na(check.labs(data[[all.vars(form)[2]]])$var_lab) == TRUE) {
    data[[all.vars(form)[2]]] <- sjlabelled::set_label(data[[all.vars(form)[2]]], label = all.vars(form)[2])
  }

  # add value labels. If there are no defined value labels, this will just add the levels as value labels
  if (check.labs(data[[all.vars(form)[2]]])$has_val_lab == FALSE) {
    labs_levels <- c(as.character(levels[1]), as.character(levels[2])) # ovo je vrlo glup ali ucinkovit nacin za dodati labele
    names(labs_levels) <- c(as.character(levels[1]), as.character(levels[2]))

    data[[all.vars(form)[2]]] <- sjlabelled::set_labels(data[[all.vars(form)[2]]], labels = labs_levels)
  }


  dep <- check.labs(data[[all.vars(form)[1]]])$var_lab
  ind <- check.labs(data[[all.vars(form)[2]]])$var_lab
  if (multi == "dep") {
    var <- dep
  } else {
    var <- ind
  }


  l1 <- check.labs(data[[all.vars(form)[2]]])$val_lab$label[check.labs(data[[all.vars(form)[2]]])$val_lab$value == levels[1]]
  l2 <- check.labs(data[[all.vars(form)[2]]])$val_lab$label[check.labs(data[[all.vars(form)[2]]])$val_lab$value == levels[2]]

  n1 <- sum(!is.na(data[[all.vars(form)[1]]][data[[all.vars(form)[2]]] == levels[1]]))
  n2 <- sum(!is.na(data[[all.vars(form)[1]]][data[[all.vars(form)[2]]] == levels[2]]))

  m1 <- mean(data[[all.vars(form)[1]]][data[[all.vars(form)[2]]] == levels[1]], na.rm = TRUE)
  m2 <- mean(data[[all.vars(form)[1]]][data[[all.vars(form)[2]]] == levels[2]], na.rm = TRUE)

  sd1 <- stats::sd(data[[all.vars(form)[1]]][data[[all.vars(form)[2]]] == levels[1]], na.rm = TRUE)
  sd2 <- stats::sd(data[[all.vars(form)[1]]][data[[all.vars(form)[2]]] == levels[2]], na.rm = TRUE)

  data=data %>% dplyr::filter(data[[all.vars(form)[2]]] %in% levels)

  q <- stats::t.test(formula = form, data = data, var.equal = FALSE, conf.level = conf.level, alternative = type)
  t <- q$statistic
  p <- q$p.value
  df <- q$parameter



  # calculate effect size
  cohen_d <- effectsize::cohens_d(x = form, data = data, pooled_sd = FALSE, verbose = FALSE)$Cohens_d
  prob_sup <- effectsize::p_superiority(x = form, data = data, pooled_sd = FALSE, verbose = FALSE)$p_superiority # koja je koja grupa!!!!!

  if (effect == "d") {
    effect_size <- cohen_d
  } else if (effect == "prob_sup") {
    effect_size <- prob_sup
  }




  # save to data frame
  ttest.data <- data.frame(Dependent = c(var, var), Independent = c(l1, l2), N = c(n1, n2), Mean = c(m1, m2), SD = c(sd1, sd2), t = c(t, t), df = c(df, df), p = c(p, p), effect = c(effect_size, effect_size))
  return(ttest.data)
}





#' Create flextable for t-test. A helper function for ttest.flex
#'
#' @param rez a parameter
#' @param data a parameter
#' @param deci a parameter
#' @param effect a parameter
#' @param nvars a parameter
#' @param lang a parameter
#'
#' @return flextable
t.to.flex <- function(rez, data, deci, effect, nvars, lang = "hr") {
  # prepare data.frame
  if (lang == "hr") {
    d.p <- ","
  } else {
    d.p <- "."
    names(rez)[1] <- "Variable"
    names(rez)[2] <- "Group"
  }
  rez$Mean <- format(round(rez$Mean, deci), nsmall = deci, decimal.mark = d.p)
  rez$SD <- format(round(rez$SD, deci + 1), nsmall = deci + 1, decimal.mark = d.p)
  rez$t <- format(round(rez$t, 2), nsmall = 2, decimal.mark = d.p)
  rez$p <- format(round(rez$p, 3), nsmall = 3, decimal.mark = d.p)
  rez$effect <- format(round(rez$effect, 3), nsmall = 3, decimal.mark = d.p)

  names(rez)[4] <- "M"

  if (lang == "hr") {
    names(rez)[1] <- "Varijabla"
    names(rez)[2] <- "Grupa"
  }

  if (effect == "d") {
    names(rez)[9] <- "D"
  } else if (effect == "prob_sup") {
    names(rez)[9] <- "p_sup"
  }

  rez <- rez %>% dplyr::select(-"df")



  tab <- rez %>%
    flextable::flextable() %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "body") %>%
    #flextable::merge_v(j=1, part = "body") %>%
    #flextable::merge_v(j=6:8, part = "body") %>%
    flextable::hline(i = 1, border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_top(part = "header")

    for (i in 1:nrow(rez)){
      if (i%%2==0) {
        tab <- tab %>% flextable::merge_at(i=(i-1):i, j=1, part="body")
        tab <- tab %>% flextable::merge_at(i=(i-1):i, j=6, part="body")
        tab <- tab %>% flextable::merge_at(i=(i-1):i, j=7, part="body")
        tab <- tab %>% flextable::merge_at(i=(i-1):i, j=8, part="body")
        tab <- tab %>% flextable::hline(i = i, border = officer::fp_border(color = "black", width = 1), part = "body")
      }
    }


  tab <- tab %>%
    flextable::autofit() %>%
    flextable::width(j = 1, width = 4, unit = "cm") %>%
    flextable::width(j = 2, width = 2.5, unit = "cm") %>%
    flextable::fix_border_issues()


  return(tab)
}
