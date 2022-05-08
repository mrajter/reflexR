#' Perform PCA
#'
#' Function is based on psych::principal function and in line with Field's Discovering statistics using R. The PCA is performed with listwise deletion and is based on correlation matrix.
#' Due to mystic differences in algorthms, loadings obtained by oblimin rotation are marginally different from those obtained in SPSS...I still don't know why
#' @param data data frame
#' @param form formula with variables on rhs. E.g. 1 ~ var1 + var2 + var3
#' @param n_factors number of components to extract. There are several methods
#' \itemize{
#'     \item exact number. Useful for forced single component and experienting
#'     \item "KG" - Kaiser-Guttman criteria (lambda>1)
#'     \item "MAP" - Velicer's Map test
#'     \item "VSS1" - very simple structure complexity 1
#'     \item "VSS2" - very simple structure complexity 2
#'     \item "Parallel" - parallel tests
#'     \item "ScreeAC" - Acceleration factor Scree test
#'     \item "ScreeOC" - Optimal coordinates Scree test
#' }
#' @param rotation Rotation to use. Options are: "none", "varimax", "oblimin". Default is "oblimin".
#' @param title Title of analysis. Will be determined if omitted
#' @param sing.solution Should forced single component solution also be presented (default=T)
#' @param sorted should the loadings be sorted in table (default=T)
#' @param supress suppress coefficients below threshold (default=0.3)
#' @param option inherited from r.flex.opts
#'
#' @return list with elements
#' \itemize{
#'     \item type - table type - used for inserting in word document
#'     \item title - used for table title. Can be set manually or automatically
#'     \item preqs - prerequisites - Bartlett and KMO
#'     \item df - results as data.frame
#'     \item table - flextable with results
#'     \item rotation - used rotation
#'     \item sing.solution - is there a forced single component solution
#'     \item extraction - how are components extracted (exact number or criteria) - see nfac.flex function for details
#' }
#' @export
pca.flex <- function(data, form, n_factors = "KG", rotation = "oblimin", title="", sing.solution = T, sorted = T, supress = 0.3, option = r.flex.opts) {
  # get vars
  if (missing(form)) {
    vars <- names(data)
  } else {
    vars <- all.vars(form)
  }

  #get title
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
  # create dataset (data)
  data <- data %>% dplyr::select(tidyselect::all_of(vars))
  data <- data %>% tidyr::drop_na()

  # get vector of variable labels (labs)
  labs <- c()
  for (i in vars) {
    l <- check.labs(data[[i]])
    if (is.na(l$var_lab)) {
      labs <- c(labs, i)
    } else {
      labs <- c(labs, l$var_lab)
    }
  }

  # determine number of components (ncomp)
  ncomp <- nfac.flex(data)
  if (is.numeric(n_factors)) {
    ncomp <- n_factors
  } else if (n_factors == "KG") {
    ncomp <- ncomp$df[1, 2]
  } else if (n_factors == "MAP") {
    ncomp <- ncomp$df[2, 2]
  } else if (n_factors == "VSSc1") {
    ncomp <- ncomp$df[3, 2]
  } else if (n_factors == "VSSc2") {
    ncomp <- ncomp$df[4, 2]
  } else if (n_factors == "Parallel") {
    ncomp <- ncomp$df[5, 2]
  } else if (n_factors == "ScreeAF") {
    ncomp <- ncomp$df[6, 2]
  } else if (n_factors == "ScreeOC") {
    ncomp <- ncomp$df[7, 2]
  } else {
    ncomp <- length(vars)
  }


  # preqs
  preqs <- pca.preqs(data, form)

  # PCA
  # find original lambdas (or_lambdas)
  or_lambdas <- list()
  or_lambdas$lambdas <- psych::principal(data, nfactors = ncomp, rotate = "none")$values[1:ncomp]
  or_lambdas$pct <- or_lambdas$lambdas * 100 / length(vars)


  # 1F PCA
  pca1F <- psych::principal(data, nfactors = 1, rotate = "none")

  # real PCA
  pca <- psych::principal(data, nfactors = ncomp, rotate = rotation)

  # lambdas etc
  lambdas <- as.data.frame(psych::print.psych(pca)$Vaccounted)
  loads <- cbind(labs, data.frame(unclass(pca$loadings)))
  if (sorted==T) {
    loads <- pca.sorting(loads)$df #sort lambdas per component
  }

  df_table <- loads[, 2:ncol(loads)]
  df_table <- as.data.frame(apply(as.matrix(df_table), c(1, 2), pval.apa))
  df_table <- df_table %>% dplyr::mutate_all(as.character) # convert df_table to data.frame
  df_table <- cbind(loads$labs, df_table) # for flextable

  df <- rbind(loads, append(list(paste0("\U03BB", " or")), or_lambdas$lambdas))
  df_table <- rbind(df_table, append(list(paste0("\U03BB", " or")), lapply(or_lambdas$lambdas, function(x) format.n(x, deci = 2))))

  df_table <- as.data.frame(df_table)


  df <- rbind(df, append(list("%V"), or_lambdas$pct))
  df_table <- rbind(df_table, append(list("%V"), lapply(or_lambdas$pct, function(x) format.n(x, deci = 2))))

  # add rotated lambdas
  if (rotation != "none" & ncomp != 1) {
    df <- rbind(df, append(list(paste0("\U03BB", " rot")), unname(lambdas[1, ])))
    df_table <- rbind(df_table, append(list(paste0("\U03BB", " rot")), lapply(unname(lambdas[1, ]), function(x) format.n(x, deci = 2))))
  }

  # add correlations to df_table
  # watch out for length

  if (rotation == "oblimin" & n_factors != 1) {
    if (option$lang == "hr") {
      txt_help <- "Korelacije"
    } else {
      txt_help <- "Correlations"
    }
    df_table <- rbind(df_table, c(txt_help, rep(NA, ncol(df_table) - 1)))

    cor_table <- as.data.frame(apply(data.frame(unclass(pca$r.scores)), c(1, 2), pval.apa))

    for (i in 1:nrow(cor_table)) {
      cor_table[i, i] <- "-"
    }
    cor_table <- cor_table[2:nrow(cor_table),] #you don't need first row
    if (option$lang == "hr") {
      txt_help <- "K"
    } else {
      txt_help <- "C"
    }
    for (i in 1:nrow(cor_table)) {
      df_table <- rbind(df_table, append(list(paste0(txt_help, i+1)), unname(cor_table[i, ])))
    }
  }

  colnames(df)[2:ncol(df)] <- unlist(lapply(seq(1, ncol(df)-1), function(x) paste0("C", x))) #add colnames to df, df_table will be solved later
  colnames(df)[1] <- "Variable"
  rownames(df) <- NULL

  #single component solution
  if (ncomp>1 & sing.solution == T) {
    load1F <- data.frame(unclass(pca1F$loadings))
    if (sorted==T) {
      load1F <- load1F[pca.sorting(loads)$row,]
      load1F_table <- unlist(lapply(load1F, pval.apa))

      load1F <- c(load1F, or_lambdas$lambdas[1], or_lambdas$pct[1])
      load1F_table <- c(load1F_table, format.n(or_lambdas$lambdas[1], deci = 2), format.n(or_lambdas$pct[1], deci = 2))


      df=cbind(df, c(load1F, rep(NA, nrow(df)-length(load1F))))
      df_table <- cbind(df_table, c(load1F_table, rep(NA, nrow(df_table)-length(load1F_table))))
      colnames(df)[ncol(df)] <- "Sing.comp"
    }
  }


  res <- list()
  res$type <- "pca"
  res$title <- title
  res$preqs <- preqs
  res$df <- df
  res_temp <- list(df=df, table=df_table)
  res$table <- pca_to_flex(res=res_temp, ncomp=ncomp, nvars=length(vars), rotation=rotation, sing.solution=sing.solution, supress=supress, option=option)
  res$rotation <- rotation
  res$sing.solution <- sing.solution
  res$extraction=n_factors

  return(res)
}


pca.sorting <- function(a) {
  # helper function for sorting component loadings
  # finds highest loading for each component and attributes this variable to a component with highest loading
  # then sorts by component and by value within component
  # returns the order of items
  sort.comp <- data.frame(row = seq(1, nrow(a)), comp = max.col(as.matrix(a[2:ncol(a)])))
  for (i in 1:nrow(a)){
    a[i,1] <- paste0(i, ". ", a[i,1])
  }

  for (i in 1:nrow(sort.comp)) {
    sort.comp$vals[i] <- a[i, sort.comp$comp[i] + 1]
  }
  sort.comp <- sort.comp %>% dplyr::arrange(.data$comp, dplyr::desc(.data$vals))
  res <- list(df=a[sort.comp$row,], row=sort.comp$row)
  return(res)
}



pca_to_flex=function(res, ncomp, nvars, rotation, sing.solution, supress, option=r.flex.opts){
  if (option$lang=="hr") {
    column.n=c("Varijabla") #column names
    for (i in 1:ncomp) {column.n=c(column.n, paste0("K",i))}
    if (sing.solution==T & ncomp>1) {column.n=c(column.n, "1K")}
  } else {
    column.n=c("Variable")
    for (i in 1:ncomp) {column.n=c(column.n, paste0("C",i))}
    if (sing.solution==T & ncomp>1) {column.n=c(column.n, "1C")}
  }
  colnames(res$table)=column.n
  ff=res$table %>%
    flextable::flextable() %>%
    flextable::hline_top(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "body") %>%
    flextable::hline(i=nvars, border = officer::fp_border(color = "black", width = 1), part = "body") %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(align = "center", part = "body") %>%
    flextable::align(j = 1, align = "left", part = "body") %>%
    flextable::align(i=(nvars+1):nrow(res$table), j = 1, align = "right", part = "body")
  if (sing.solution==T & ncomp!=1) {
    ff=ff %>% flextable::vline(j=ncol(res$df)-1, border = officer::fp_border(color = "black", width = 1), part = "all")
  }
  if (rotation=="oblimin" & ncomp>1) {
    ff = ff %>% flextable::hline(i=nvars+3, border = officer::fp_border(color = "black", width = 1), part = "body")
  }
  #ovo bi moglo biti bolno sporo
  if (supress!=0) {
    for (i in 1:nvars){
      for (j in 2:ncol(res$df)) {
        if (abs(res$df[i,j])<supress) {
          ff=ff %>% flextable::color(i=i, j=j, color="lightgray")
        }
      }
    }
  }

  #widths
  ff <- ff %>% flextable::width(j=1, width=5, unit="cm") %>%
    flextable::width(j=2:ncol(res$df), width=1.4, unit="cm") %>%
    flextable::font(fontname="Calibri", part="all") %>%
    flextable::padding(padding.top = 0, padding.bottom = 0, part="all")
  return(ff)
}


#' Determine how many components to extract in PCA
#'
#' This is a helper function for pca.flex function and it can be used to determine the number of components manually. It allows the determination of the number of components using various criteria from functions psych::VSS and nFactors::nScree functions
#'
#' @param data dataset - all variables are analyzed
#'
#' @return list with dataframe and flextable
#' @export
nfac.flex <- function(data) {
  pca <- psych::principal(data, nfactors = ncol(data), rotate = "none")
  q2 <- nFactors::nScree(x = pca$values, model = "components")
  KG <- as.integer(q2$Components$nkaiser) # KG
  paral <- q2$Components$nparallel # parallel
  af <- q2$Components$naf # Acceleration factor Scree test
  oc <- q2$Components$noc # Optimal coordinates Scree test

  q <- psych::VSS(data, n = ncol(data), rotate = "none", fm = "pc", plot = F)
  VSS1 <- which.max(q$cfit.1)
  VSS2 <- which.max(q$cfit.2)
  MAP <- which.min(q$map)

  res <- list()
  res$df <- data.frame(ind = c("KG", "MAP", "VSS c1", "VSS c2", "Parallel", "Scree AF", "Scree OC"), n_comp = c(KG, MAP, VSS1, VSS2, paral, af, oc))

  table <- res$df %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      ind = "Indice",
      n_comp = "Number of comp."
    ) %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(align = "center", part = "body") %>%
    flextable::align(j = 1, align = "left", part = "body") %>%
    flextable::hline_top(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1), part = "body") %>%
    flextable::font(fontname = "Calibri", part = "all") %>%
    flextable::padding(padding.top = 0, padding.bottom = 0, part = "all") %>%
    flextable::width(j = 1, width = 2, unit = "cm") %>%
    flextable::width(j = 2, width = 3, unit = "cm")

  res$table <- table
  return(res)
}

pca.preqs <- function(data, form, option = r.flex.opts) {
  if (missing(form)) {
    vars <- names(data)
  } else {
    vars <- all.vars(form)
  }

  data <- data %>% dplyr::select(tidyselect::all_of(vars))

  # bart
  bart <- psych::cortest.bartlett(data)
  bart$text <- paste0("\U03C7", "\U00B2", "=", format.n(bart$chisq, 2), "; df=", bart$df, "; p", pval.apa(bart$p.value, equal = T))

  # KMO
  kmo <- psych::KMO(data)

  res <- list()
  res$bart <- bart
  res$kmo <- kmo$MSA
  if (option$lang == "en") {
    res$text <- paste0("Bartlett's test of sphericity - ", bart$text, "; KMO MSA=", pval.apa(kmo))
  } else {
    res$text <- paste0("Bartlettov test sfericiteta - ", bart$text, "; KMO MSA - ", pval.apa(kmo$MSA))
  }
  return(res)
}

