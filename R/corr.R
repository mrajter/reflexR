
#' Correlation matrix from data.frame
#'
#' Creates correlation matrix from data.frame based on Hmisc::rcorr
#'
#' @param df  data.frame
#' @param na how will missing values be handled ("listwise" (default), "pairwise")
#' @param type type of correlation ("pearson" (default), "spearman")
#' @param labs type of table:
#' \itemize{
#'   \item "numbered" - numbered variable labels an numbers as column headings
#'   \item "asis" - variable labels in rows and column headings
#'   \item "names" - variable names in rows and column headings
#' }
#' @param diag table looks ("bottom" (default), "top", "all")
#' @param lang language ("hr" (default), "en")
#'
#' @return list with elements:
#' \itemize{
#'   \item $matrix - flextable object
#'   \item $N - number of participants (range if na="pairwise")
#'   \item $tab_cor - data frame with correlations
#'   \item $tab_p - data frame with p values
#'   \item $tab_cor - data frame with N
#' }
#' @export
#'

cor.flex=function(df, na="listwise", type="pearson", labs="numbered", diag="bottom", lang="hr"){
  if (na=="listwise") {df=df[stats::complete.cases(df),]}

  # q=Hmisc::rcorr(as.matrix(df), type=type)  ##stara varijanta - u njoj je problem da puca kad nema varijabliliteta
  rez_r=as.data.frame(matrix(nrow = ncol(df), ncol=ncol(df)))
  rez_p=as.data.frame(matrix(nrow = ncol(df), ncol=ncol(df)))
  rez_n=as.data.frame(matrix(nrow = ncol(df), ncol=ncol(df)))

  for (i in 1:ncol(df)) {
    for (j in 1:ncol(df)) {

      c=stats::cor.test(df[[i]], df[[j]], method=type)
      rez_r[i,j]=c$estimate
      rez_p[i,j]=c$p.value
      rez_n[i,j]=c$parameter+2
    }
  }

  q.c=rez_r
  q.c=format(round(q.c,3), nsmall=3)

  q.p=rez_p


  q.c[q.c == "1.000"] <- "-"

  #adding stars
  for (i in 1:ncol(q.c)){
    for (j in 1:nrow(q.c)) {
      if (j!=i) {
        if (q.p[j,i]<0.001) {q.c[j,i]=paste(q.c[j,i],"*", sep="")}
        if (q.p[j,i]<0.01) {q.c[j,i]=paste(q.c[j,i],"*", sep="")}
        if (q.p[j,i]<0.05) {q.c[j,i]=paste(q.c[j,i],"*", sep="")}

        #diagonals
        if (diag=="top") {if (j>i) {q.c[j,i]=""}}
        if (diag=="bottom") {if (j<i) {q.c[j,i]=""}}

      }
    }
  }

  #labels
  q.nam=data.frame(names(df))
  q.nam$labs=unlist(lapply(q.nam[,1], FUN=function(x){
    if (is.null(sjlabelled::get_label(df[[x]]))==TRUE){
      return(x)
    } else {
      return(sjlabelled::get_label(df[[x]]))
    }
  }))

  #labels variants
  if (labs=="asis"){
    q.c=cbind(q.nam[[2]],q.c)
    if (lang=="hr") {names(q.c)[1]="Varijabla"} else {names(q.c)[1]="Variable"}
  }

  if (labs=="names"){
    q.c=cbind(q.nam[[1]],q.c)
    if (lang=="hr") {names(q.c)[1]="Varijabla"} else {names(q.c)[1]="Variable"}
  }

  if (labs=="numbered"){
    q.c=cbind(seq(1,nrow(q.c)), q.nam[[2]], q.c)
    if (lang=="hr") {
      names(q.c)[1]="Br."
      names(q.c)[2]="Varijabla"
    } else {
      names(q.c)[1]="No."
      names(q.c)[2]="Variable"
    }
    names(q.c)[3:ncol(q.c)]=seq(1,nrow(q.c))
  }

  # if diagonal is not top then the last column can be removed
  if (diag!="top") {
    q.c=q.c[,1:(ncol(q.c)-1)]
  }

  #flextable

  q.c=q.c %>% flextable::flextable() %>%
    flextable::align(align="center", part="header") %>%
    flextable::align(align="center", part="body")
  if (labs=="numbered") {
    q.c = q.c %>% flextable::align(j=2,align="left", part="body")
  } else {
    q.c = q.c %>% flextable::align(j=1,align="left", part="body")
  }
  q.c = q.c %>%
    flextable::hline_top(border=officer::fp_border(color="black", width = 1), part="header") %>%
    flextable::hline_bottom(border=officer::fp_border(color="black", width = 1), part="header") %>%
    flextable::hline_bottom(border=officer::fp_border(color="black", width = 1), part="body")
  pretty_dims=flextable::dim_pretty(q.c)$widths
  for (i in 1:length(pretty_dims)){
    q.c=flextable::width(q.c, j=i,pretty_dims[i])
  }

  # N
  if (na=="listwise") {n=min(rez_n)} else {n=paste(min(rez_n), "-", max(rez_n))}


  #kako pišeš rezultate!!!!
  res=list(matrix=q.c, N=n, tab_cor=rez_r, tab_p=rez_p, tab_n=rez_n )

  return(res)

}



