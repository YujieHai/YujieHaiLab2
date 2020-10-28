\name{box_plot_by_DRG}
\alias{box_plot_by_DRG}
\title{Box Plot of Payment by DRG Code}
\description{
This function produces a boxplot of \code{vary} grouped by DRG code.
}
\usage{
box_plot_by_DRG(df, vary = c("Average.Medicare.Payments",
                "Average.Total.Payments", "Average.Covered.Charges"))
}
\arguments{
  \item{df}{a dataframe (use DRG_data)}
  \item{vary}{a string name for variable y in the dataframe \code{df}.
  Must be one of "Average.Medicare.Payments" (default),
  "Average.Total.Payments" or "Average.Covered.Charges"}
}
\value{A box plot of \code{vary} grouped by DRG code}

\author{Yujie Hai}

\examples{
data(DRG_data)

box_plot_by_DRG(DRG_data, 'Average.Medicare.Payments')
}
