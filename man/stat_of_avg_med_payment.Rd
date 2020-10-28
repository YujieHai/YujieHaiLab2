\name{stat_of_avg_med_payment}
\alias{stat_of_avg_med_payment}
\title{Table of Statistics For Average Medicare Payments by DRG Code}
\description{
This function produces a table of \code{stat} for average medicare payments grouped by DRG code.
}
\usage{
stat_of_avg_med_payment(df, stat = c('mean', 'median', 'sd'))
}
\arguments{
  \item{df}{a dataframe (use DRG_data)}
  \item{stat}{a string name of statistic to compute in the dataframe \code{df}.
  Must be one of 'mean'(default), 'median', or 'sd'}
}
\value{A table of \code{stat} values of average medicare payments grouped by DRG code}

\author{Yujie Hai}

\examples{
data(DRG_data)

stat_of_avg_med_payment(DRG_data, 'median')
}
