% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DistributionWAgg.R
\name{DistributionWAgg}
\alias{DistributionWAgg}
\title{Aggregation Method: DistributionWAgg}
\usage{
DistributionWAgg(
  expert_judgements,
  type = "DistribArMean",
  name = NULL,
  placeholder = FALSE,
  percent_toggle = FALSE,
  round_2_filter = TRUE
)
}
\arguments{
\item{expert_judgements}{A dataframe in the format of \link{data_ratings}.}

\item{type}{One of \code{"DistribArMean"} or \code{"TriDistribArMean"}.}

\item{name}{Name for aggregation method. Defaults to \code{type} unless specified.}

\item{placeholder}{Toggle the output of the aggregation method to impute placeholder data.}

\item{percent_toggle}{Change the values to probabilities. Default is \code{FALSE}.}

\item{round_2_filter}{Note that the IDEA protocol results in both a Round 1
and Round 2 set of probabilities for each claim. Unless otherwise specified,
we will assume that the final Round 2 responses (after discussion) are being
referred to.}
}
\value{
A tibble of confidence scores \code{cs} for each \code{paper_id}.
}
\description{
Calculate the arithmetic mean of distributions created with expert judgements.
The aggregate is the median of the average distribution fitted on the individual estimates.
}
\details{
\loadmathjax

This method assumes that the elicited probabilities and bounds can be considered
to represent participants' subjective distributions associated with relative
frequencies (rather than unique events). That is to say that we considered that
the lower bound of the individual per claim corresponds to the 5th percentile
of their subjective distribution on the probability of replication, denoted
\mjeqn{q_{5,i}}{ascii}, the best estimate corresponds to the median, \mjeqn{q_{50,i}}{ascii}, and the upper
bound corresponds to the 95th percentile, \mjeqn{q_{95,i}}{ascii}. With these three
percentiles, we can fit parametric or non-parametric distributions and aggregate
them rather than the (point) best estimates.

\code{type} may be one of the following:

\strong{DistribArMean}: Applies a non-parametric distribution evenly across upper, lower and best estimates.

Using the three percentiles we can build the minimally
informative non-parametric distribution that spreads the mass uniformly between
the three percentiles.

\mjdeqn{F_{i}(x) = \begin{cases}
\displaystyle 0, \text{ for } x<0 \cr
\displaystyle \frac{0.05}{q_{5,i}}\cdot x, \text{ for } 0 \leq x< q_{5,i}\cr
\displaystyle \frac{0.45}{q_{50,i}-q_{5,i}}\cdot(x-q_{5,i})+0.05, \text{ for } q_{5,i}\leq x< q_{50,i}\cr
\displaystyle \frac{0.45}{q_{95,i}-q_{50,i}}\cdot(x-q_{50,i})+0.5, \text{ for } q_{50,i}\leq x< q_{95,i}\cr
\displaystyle \frac{0.05}{1 - q_{95,i}}\cdot(x-q_{95,i})+0.95, \text{ for } q_{95,i}\leq x< 1\cr
\displaystyle 1,  \text{ for } x\geq 1.
\end{cases}}{ascii}

Then take the average of all constructed distributions of participants for each claim:

\mjdeqn{AvDistribution = \frac{1}{N}\sum_{i=1}^N F_i(x),}{ascii}

and the aggregation is the median of the average distribution:

\mjdeqn{\hat{p}_c\left( DistribArMean \right) = AvDistribution^{-1}(0.5).}{ascii}

\strong{TriDistribArMean}: Applies a triangular distribution to the upper, lower and best estimates.

A more restrictive fit with different assumptions about the
elicited best estimates, upper and lower bounds. We can assume that the lower and upper
bounds form the support of the distribution, and the best estimate corresponds to the mode.

\mjdeqn{F_i(x)=
\begin{cases}
\displaystyle 0, \text{ for } x < L_{i} \cr
\displaystyle \frac{\left( x-L_{i}\right)^2}{\left( U_{i}-L_{i}\right)\left( B_{i}-L_{i}
\right)}, \text{ for } L_{i} \leq x < B_{i}\cr
\displaystyle 1 - \frac{\left( U_{i}-x\right)^2}{\left( U_{i}-L_{i}\right)\left
( U_{i}-B_{i}\right)}, \text{ for } B_{i} < x < U_{i}\cr
\displaystyle 1,  \text{ for } x \geq U_{i}.
\end{cases}}{ascii}

Then take the average of all constructed distributions of participants for each claim:

\mjdeqn{    AvDistribution = \frac{1}{N}\sum_{i=1}^N F_i(x),}{ascii}

and the aggregation is the median of the average distribution:

\mjdeqn{    \hat{p}_c\left(TriDistribArMean\right) = AvDistribution^{-1}(0.5).}{ascii}
}
\examples{
\donttest{DistributionWAgg(data_ratings)}

}
