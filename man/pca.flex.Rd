% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca.R
\name{pca.flex}
\alias{pca.flex}
\title{Perform PCA}
\usage{
pca.flex(
  data,
  form,
  n_factors = "KG",
  rotation = "oblimin",
  title = "",
  sing.solution = T,
  sorted = T,
  supress = 0.3,
  option = r.flex.opts
)
}
\arguments{
\item{data}{data frame}

\item{form}{formula with variables on rhs. E.g. 1 ~ var1 + var2 + var3}

\item{n_factors}{number of components to extract. There are several methods
\itemize{
\item exact number. Useful for forced single component and experienting
\item "KG" - Kaiser-Guttman criteria (lambda>1)
\item "MAP" - Velicer's Map test
\item "VSS1" - very simple structure complexity 1
\item "VSS2" - very simple structure complexity 2
\item "Parallel" - parallel tests
\item "ScreeAC" - Acceleration factor Scree test
\item "ScreeOC" - Optimal coordinates Scree test
}}

\item{rotation}{Rotation to use. Options are: "none", "varimax", "oblimin". Default is "oblimin".}

\item{title}{Title of analysis. Will be determined if omitted}

\item{sing.solution}{Should forced single component solution also be presented (default=T)}

\item{sorted}{should the loadings be sorted in table (default=T)}

\item{supress}{suppress coefficients below threshold (default=0.3)}

\item{option}{inherited from r.flex.opts}
}
\value{
list with elements
\itemize{
\item type - table type - used for inserting in word document
\item title - used for table title. Can be set manually or automatically
\item preqs - prerequisites - Bartlett and KMO
\item df - results as data.frame
\item table - flextable with results
\item rotation - used rotation
\item sing.solution - is there a forced single component solution
\item extraction - how are components extracted (exact number or criteria) - see nfac.flex function for details
}
}
\description{
Function is based on psych::principal function and in line with Field's Discovering statistics using R. The PCA is performed with listwise deletion and is based on correlation matrix.
Due to mystic differences in algorthms, loadings obtained by oblimin rotation are marginally different from those obtained in SPSS...I still don't know why
}
