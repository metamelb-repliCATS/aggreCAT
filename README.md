
[![DOI](https://zenodo.org/badge/531484296.svg)](https://zenodo.org/badge/latestdoi/531484296)
[![R-CMD-check](https://github.com/metamelb-repliCATS/aggreCAT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/metamelb-repliCATS/aggreCAT/actions/workflows/R-CMD-check.yaml)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# `aggreCAT`: methods for mathematically aggregating expert judgements

## Problem Context

The use of structured elicitation to inform decision making has grown
dramatically across fields like ecology and environmental science over
the last decade, as researchers and managers are faced with the need to
act rapidly in the face of uncertainty and absent, uninformative data.
However, judgements from multiple experts must be aggregated into a
single estimate or distribution, and empirical evidence suggests that
mathematical aggregation provides more reliable estimates than
behavioural consensus models.

Unfortunately, there is a dearth of accessible tools for implementing
more complex aggregation methods other than linear averages, which are
arguably the most commonly used aggregation method, but may not be the
best approach for yielding robust estimates. The lack of readily
available aggregation methods severely limits users who may want to
utilise alternative aggregation methods more suitable for the task at
hand, but who do not have the time or technical capacity to implement.

The availability of methods implemented in R is even more limited, with
the most fully-fledged [package
`expert`](https://cran.r-project.org/package=expert) being archived from
CRAN in 2022, the [`SHELF`
package](https://cran.r-project.org/package=SHELF) implementing only a
single aggregation method (weighted linear pool / arithmetic mean), and
the [`opera` package](https://cran.r-project.org/package=opera)
aggregating non-point-estimate judgements only (time-series
predictions).

An archived version of `expert` is still available, however, the package
provides only three aggregation methods, and stipulates a structured
elicitation protocol that ‘calibrates’ experts through the use of seed
questions, which are required as additional input data to the
aggregation functions.

## The aggreCAT Package

The `aggreCAT` package implements state-of-the-art methods for
mathematically aggregating expert judgements, providing **XYZ** unique
methods.

Does not force calibration! Provides

## Installation

You can install:

-   the latest development version of `aggreCAT` package:

``` r
install.packages("devtools")
devtools::install_github("metamelb-repliCATS/aggreCAT")
```

-   the most recent official version of `aggreCAT` from CRAN:

    -   TBC! *We will upload to CRAN once our manuscript has been
        submitted.*

Then load the package:

``` r
library(aggreCAT)
```
