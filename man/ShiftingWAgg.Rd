% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShiftingWAgg.R
\name{ShiftingWAgg}
\alias{ShiftingWAgg}
\title{Aggregation Method: ShiftingWAgg}
\usage{
ShiftingWAgg(
  expert_judgements,
  type = "ShiftWAgg",
  name = NULL,
  placeholder = FALSE,
  percent_toggle = FALSE
)
}
\arguments{
\item{expert_judgements}{A dataframe in the format of \link{data_ratings}.}

\item{type}{One of \code{"ShiftWAgg"}, \code{"BestShiftWAgg"}, \code{"IntShiftWAgg"}, \code{"DistShiftWAgg"}, or \code{"DistIntShiftWAgg"}.}

\item{name}{Name for aggregation method. Defaults to \code{type} unless specified.}

\item{placeholder}{Toggle the output of the aggregation method to impute placeholder data.}

\item{percent_toggle}{Change the values to probabilities. Default is \code{FALSE}.}
}
\value{
A tibble of confidence scores \code{cs} for each \code{paper_id}.
}
\description{
Weighted by judgements that shift the most after discussion
}
\details{
When judgements are elicited using the IDEA protocol (or any other protocol that allows
experts to revisit their original estimates), the second round of estimates may differ
from the original first set of estimates an expert provides. Greater changes between rounds
will be given greater weight.

\code{type} may be one of the following:

\loadmathjax
\strong{ShiftWAgg}: Takes into account the shift in all three estimates

Considers shifts across lower, \mjeqn{L_{i,c}}{ascii}, and upper,
\mjeqn{U_{i,c}}{ascii}, confidence limits, and the best estimate, \mjeqn{B_{i,c}}{ascii}.
More emphasis is placed on changes in the best estimate such that:

\mjdeqn{w\_Shift_{i,c} = |B1_{i,c} - B_{i,c}| + \frac{|L1_{i,c} - L_{i,c}|+|U1_{i,c} - U_{i,c}|}{2},}{ascii}

where \mjeqn{L1_{i,c}, B1_{i,c},U1_{i,c}}{ascii} are the first round lower, best and upper estimates (prior
to discussion) and \mjeqn{L_{i,c}, B_{i,c},U1_{i,c}}{ascii} are the individual’s revised second round estimates
(after discussion).

\mjdeqn{\hat{p}_c(ShiftWAgg) = \sum_{i=1}^N \tilde{w}\_Shift_{i,c}B_{i,c}}{ascii}

\strong{BestShiftWAgg}: Weighted according to shifts in best estimates alone

Taking into account the fact that the scales best estimates are measured on are bounded,
we can calculate shifts relative to the largest possible shift.

\mjdeqn{w\_BestShift_{i,c}=
\begin{cases}
\frac{|B1_{i,c} - B_{i,c}|}{B1_{i,c}},
\begin{aligned}
\displaystyle &\ for\ (B1_{i,c} > 0.5\ and\ B_{i,c} \leq 0.5) \cr
\displaystyle &\ or\ B_{i,c} < B1_{i,c} \leq 0.5\ or\ B1_{i,c} > B_{i,c} > 0.5
\end{aligned} \cr
\frac{|B1_{i,c} - B_{i,c}|}{1- B1_{i,c}},
\begin{aligned}
\displaystyle &\ for\ (B1_{i,c} < 0.5\ and\ B_{i,c} \geq 0.5) \cr
\displaystyle &\ or\ B1_{i,c} < B_{i,c} < 0.5\ or\ B_{i,c} > B1_{i,c} > 0.5.
\end{aligned}
\end{cases}}{ascii}

\mjdeqn{\hat{p}_c(BestShiftWAgg) = \sum_{i=1}^N \tilde{w}\_BestShift_{i,c}B_{i,c}}{ascii}

\strong{IntShiftWAgg}: Weighted by shifts in interval widths alone.

Individuals whose interval widths narrow between rounds are given more weight.

\mjdeqn{w\_IntShift_{i,c} = \frac{1}{(U_{i,c}-L_{i,c})-(U1_{i,c}-L1_{i,c})+1}}{ascii}

\mjdeqn{\hat{p}_c(IntShiftWAgg) = \sum_{i=1}^N \tilde{w}\_IntShift_{i,c}B_{i,c}}{ascii}

\strong{DistShiftWAgg}: Weighted by whether best estimates become more extreme (closer to 0 or 1) between rounds.

\mjdeqn{w\_DistShift_{i,c} = 1 - (\min (B_{i,c}, 1-B_{i,c}) - \min (B1_{i,c}, 1-B1_{i,c}))}{ascii}

\mjdeqn{\hat{p}_c(DistShiftWAgg) = \sum_{i=1}^N \tilde{w}\_DistShift_{i,c}B_{i,c}}{ascii}

\strong{DistIntShiftWAgg}: Rewards both narrowing of intervals and shifting towards the certainty limits between rounds.

We simply multiply the weights calculated in the "DistShiftWAgg" and "IntShiftWAgg" methods.

\mjdeqn{w\_DistIntShift_{i,c} = \tilde{w}\_IntShift_{i,c} \cdot \tilde{w}\_DistShift_{i,c}}{ascii}

\mjdeqn{\hat{p}_c(DistIntShiftWAgg) = \sum_{i=1}^N \tilde{w}\_DistIntShift_{i,c}B_{i,c}}{ascii}
}
\examples{
\donttest{ShiftingWAgg(data_ratings)}

}
