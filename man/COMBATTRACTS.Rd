% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Zscore_function.R
\name{COMBATTRACTS}
\alias{COMBATTRACTS}
\title{COMBAT function}
\usage{
COMBATTRACTS(
  input_data,
  ID_column,
  tractrange,
  scanner_column,
  age_column,
  group_column
)
}
\arguments{
\item{input_data}{A dataframe containing unharmonised tract data.}

\item{ID_column}{The column index corresponding to participant ID}

\item{tractrange}{The column(s) corresponding to tract data}

\item{scanner_column}{The column corresponding to site or scanner ID}

\item{age_column}{The column corresponding to participant age}
}
\description{
This function allows you to perform neuroCombat on tract data, if data are
extracted from multiple sites and/or scanners and require harmonisation.
}
\examples{
COMBATTRACTS()
}
\keyword{neuroCombat}
\keyword{tracts}
