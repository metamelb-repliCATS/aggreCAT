#' @title
#' Aggregation Method: BayesianWAgg
#'
#' @description
#' Bayesian aggregation methods with either uninformative or informative prior distributions.
#'
#' @details
#'
#' `type` may be one of the following:
#'
#' **BayTriVar**: The Bayesian Triple-Variability Method, fit with JAGS.
#'
#' \loadmathjax
#' Three kinds of variability around best estimates are considered:
#' 1. generic claim variability: variation across individuals within a claim
#' 2. generic participant variability: variation within an individual across claims
#' 3. claim - participant specific uncertainty (operationalised by bounds): informed
#' by interval widths given by individual \mjeqn{i}{ascii} for claim \mjeqn{c}{ascii}.
#'
#' The model takes the log odds transformed individual best estimates as input (data),
#' uses a normal likelihood function and derives a posterior distribution for the
#' probability of replication.
#'
#' \mjdeqn{log( \frac{B_{i,c}}{1-B_{i,c}}) \sim N(\mu_c, \sigma_{i,c}),}{ascii}
#'
#' where \mjeqn{\mu_c}{ascii} denotes the mean estimated probability of replication for claim
#' \mjeqn{c}{ascii}, and  \mjeqn{\sigma_{i,c}}{ascii} denotes the standard deviation of the
#' estimated probability of replication for claim \mjeqn{c}{ascii} and individual
#' \mjeqn{i}{ascii} (on the logit scale).  Parameter \mjeqn{\sigma_{i,c}}{ascii} is calculated as:
#' \mjdeqn{\sigma_{i,c} = (U_{i,c} - L_{i,c} + 0.01) \times \sqrt{\sigma_i^2+\sigma_c^2}}{ascii}
#' with \mjeqn{\sigma_i}{ascii} denoting the standard deviation of estimated probabilities of
#' replication for individual \mjeqn{i}{ascii} and  \mjeqn{\sigma_c}{ascii} denoting the standard
#' deviation of the estimated probability of replication for claim \mjeqn{c}{ascii}.
#'
#' The uninformative priors for specifying this Bayesian model are
#' \mjeqn{\mu_c \sim N(0,\ 3)}{ascii}, \mjeqn{\sigma_i \sim U(0,\ 10)}{ascii} and
#' \mjeqn{\sigma_c \sim U(0,\ 10)}{ascii}. After obtaining the median of the posterior
#' distribution of \mjeqn{\mu_c}{ascii}, we can back transform to obtain
#' \mjeqn{\hat{p}_c}{ascii}:
#'
#' \mjdeqn{\hat{p}_c\left( BayTriVar \right) = \frac{e^{\mu_c}}{1+e^{\mu_c}}}{ascii}
#'
#' **BayPRIORsAgg**: Priors derived from predictive models, updated with best estimates.
#'
#' This method uses Bayesian updating to update a prior probability of
#' replication estimated from a predictive model with an aggregate of the individuals’ best
#' estimates for any given claim. Methodology is the same as `type` `"BayTriVar"` except an
#' informative prior is used for \mjeqn{\mu_c}{ascii}. Conceptually the parameters of the prior
#' distribution of \mjeqn{\mu_c}{ascii} are informed by the PRIORS model (Gould et al. 2021)
#' which is a multilevel logistic regression model that predicts the probability of
#' replication using attributes of the original study. However, any model providing predictions of
#' the probability of replication can be used to generate the required priors.
#'
#' # Warning
#'
#' Both `BayTriVar` and `BayPRIORsAgg` methods require a minimum of two claims for which judgements are supplied to `expert_judgements`. This is due to the mathematical definition of these aggregators: `BayesianWAgg` calculates the variance in best estimates across multiple claims as well as the variance in best estimates across claims per individual. Thus when only one claim is provided in `expert_judgements`, the variance is 0, hence more than one claim is required for the successful execution of both Bayesian methods.
#'
#' @param expert_judgements A dataframe in the format of [data_ratings].
#' @param type One of `"BayTriVar"`, or `"BayPRIORsAgg"`.
#' @param priors (Optional) A dataframe of priors in the format of [data_supp_priors], required for `type` `BayPRIORsAgg`.
#' @param name Name for aggregation method. Defaults to `type` unless specified.
#' @param placeholder Toggle the output of the aggregation method to impute placeholder data.
#' @param percent_toggle Change the values to probabilities. Default is `FALSE`.
#'
#' @return A tibble of confidence scores `cs` for each `paper_id`.
#'
#' @examples
#' \dontrun{BayesianWAgg(data_ratings)}
#'
#' @export
#' @md

BayesianWAgg <- function(expert_judgements,
                         type = "BayTriVar",
                         priors = NULL,
                         name = NULL,
                         placeholder = FALSE,
                         percent_toggle = FALSE) {

  if(!(type %in% c("BayTriVar",
                   "BayPRIORsAgg"))){

    stop('`type` must be one of "BayTriVar" or "BayPRIORsAgg"')

  }

  ## Check for n
  if(dplyr::n_distinct(expert_judgements$paper_id) < 2) {
    cli::cli_abort("Model requires n > 1 ids to successfully execute.")
  }

  ## Set name argument

  name <- ifelse(is.null(name),
                 type,
                 name)

  cli::cli_h1(sprintf("BayesianWAgg: %s",
                      name))

  if(isTRUE(placeholder)){

    method_placeholder(expert_judgements,
                       name)

  } else {

    df <- expert_judgements %>%
      preprocess_judgements(percent_toggle = {{percent_toggle}},
                            round_2_filter = TRUE,
                            three_point_filter = TRUE) %>%
      dplyr::group_by(paper_id,
                      user_name) %>%
      tidyr::pivot_wider(names_from = element)

    ## Standard code

    user <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(user_name) %>%
      dplyr::distinct() %>%
      dplyr::mutate(user = 1:dplyr::n())

    df <- df %>%
      dplyr::left_join(user) %>%
      dplyr::select(-user_name)

    paper_id <- dplyr::tibble(paper_id = unique(df$paper_id)) #store effectID's for after model fitting

    ## Create claim * participant matrices of best/lower/upper

    Best <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(user_name,
                    paper_id,
                    three_point_best) %>%
      tidyr::pivot_wider(names_from = user_name,
                         values_from = three_point_best) %>%
      dplyr::select(-paper_id) %>%
      as.matrix()

    Lower <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(user_name,
                    paper_id,
                    three_point_lower) %>%
      tidyr::pivot_wider(names_from = user_name,
                         values_from = three_point_lower) %>%
      dplyr::select(-paper_id) %>%
      as.matrix()

    Upper <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(user_name,
                    paper_id,
                    three_point_upper) %>%
      tidyr::pivot_wider(names_from = user_name,
                         values_from = three_point_upper) %>%
      dplyr::select(-paper_id) %>%
      as.matrix()

    ## Create a matrix with an index of non-missing values

    index_matrix <- Best >= 0

    n_claim <- nrow(Best)

    n_participant <- ncol(Best)

    ## Calculate claim and participant sd in advance

    claim_sd <- apply(Best, 1, sd, na.rm = TRUE)

    claim_sd <- ifelse(claim_sd %in% c(0, NA),
                       .Machine$double.eps,
                       claim_sd)

    participant_sd <- apply(Best, 2, sd, na.rm = TRUE)

    participant_sd <- ifelse(participant_sd %in% c(0, NA),
                             .Machine$double.eps,
                             participant_sd)

    ## Create sd matrices

    claim_sd_matrix <- matrix(claim_sd,
                              n_claim,
                              n_participant)

    claim_sd_matrix <- claim_sd_matrix * index_matrix

    participant_sd_matrix <- matrix(participant_sd,
                                    n_claim,
                                    n_participant,
                                    byrow = TRUE)

    participant_sd_matrix <- participant_sd_matrix * index_matrix

    ## Calculate number of participants per claim

    p_count <- df %>%
      dplyr::group_by(paper_id) %>%
      dplyr::summarise(p_count = dplyr::n()) %>%
      dplyr::arrange(match(paper_id, unique(df$paper_id))) %>%
      dplyr::pull(p_count)

    ## Build  matrices of generic participant * claim

    Generic_Best <- matrix(NA,
                           n_claim,
                           max(p_count))

    Generic_Lower <- Generic_Upper <- Generic_Claim_sd <- Generic_Participant_sd <- Generic_Best

    for(i in seq_len(nrow(Generic_Best))){

      val <- Best[i, ][!is.na(Best[i, ])]

      val <- c(val,
               rep(NA,
                   max(p_count) - p_count[i]))

      Generic_Best[i, ] <- val

    }

    for(i in seq_len(nrow(Generic_Lower))){

      val <- Lower[i, ][!is.na(Lower[i, ])]

      val <- c(val,
               rep(NA,
                   max(p_count) - p_count[i]))

      Generic_Lower[i, ] <- val

    }

    for(i in seq_len(nrow(Generic_Upper))){

      val <- Upper[i, ][!is.na(Upper[i, ])]

      val <- c(val,
               rep(NA,
                   max(p_count) - p_count[i]))

      Generic_Upper[i, ] <- val

    }

    for(i in seq_len(nrow(Generic_Claim_sd))){

      val <- claim_sd_matrix[i, ][!is.na(claim_sd_matrix[i, ])]

      val <- c(val,
               rep(NA,
                   max(p_count) - p_count[i]))

      Generic_Claim_sd[i, ] <- val

    }

    for(i in seq_len(nrow(Generic_Participant_sd))){

      val <- participant_sd_matrix[i, ][!is.na(participant_sd_matrix[i, ])]

      val <- c(val,
               rep(NA,
                   max(p_count) - p_count[i]))

      Generic_Participant_sd[i, ] <- val

    }

    ## Logit transform Best estimate

    Logit_Generic_Best <- log(.99 * Generic_Best / (1 - .99 * Generic_Best))

    switch(type,
           "BayTriVar" = {

             ## Create list of JAGS inputs

             JAGS_list <- list(Logit_Best = Logit_Generic_Best,
                               Lower = Generic_Lower,
                               Upper = Generic_Upper,
                               Claim_SD = Generic_Claim_sd,
                               Participant_SD = Generic_Participant_sd,
                               n_claim = n_claim,
                               n_assessments = p_count)

             ## Define Jags Model

             BayesModel <- "model{
  ## PRIORS

  ### Prior for claim location

  for (i in 1:n_claim)
  {

   mu[i] ~ dnorm(0,3) # claim location, weakly informative prior

  }

  ## DATA

  ### For each claim

  for (i in 1:n_claim) # For each claim
  {

    ### For each participant, up to the number of participants on that claim.
    ### No more problems with missing values due to varying number of assessors.

    for (j in 1:n_assessments[i])
    {

      Margin[i,j] <- (Upper[i,j] - Lower[i,j]) + .01

      sigma[i,j] <-  Margin[i,j] * sqrt(pow(Claim_SD[i,j], 2) + pow(Participant_SD[i,j], 2))

      lambda[i,j] <- pow(sigma[i,j],-2)

      Logit_Best[i,j] ~  dnorm(mu[i],lambda[i,j])

    }
  }
}"



           },
           "BayPRIORsAgg" = {

             ## adding prior values to the paper_ids

             prior_means <- paper_id %>%
               dplyr::left_join(priors,
                                by = "paper_id")

             ## Create list of JAGS inputs

             JAGS_list <- list(Logit_Best = Logit_Generic_Best,
                               Lower = Generic_Lower,
                               Upper = Generic_Upper,
                               Claim_SD = Generic_Claim_sd,
                               Participant_SD = Generic_Participant_sd,
                               Prior_means = prior_means$prior_means,
                               n_claim = n_claim,
                               n_assessments = p_count)

             ## Define Jags Model

             BayesModel <- "model{
  ## PRIORS

  ### Prior for claim location

  for (i in 1:n_claim)
  {

   mu[i] ~ dnorm(Prior_means[i],3) # claim location, weakly informative prior

  }

  ## DATA

  ### For each claim

  for (i in 1:n_claim) # For each claim
  {

    ### For each participant, up to the number of participants on that claim.
    ### No more problems with missing values due to varying number of assessors.

    for (j in 1:n_assessments[i])
    {

      Margin[i,j] <- (Upper[i,j] - Lower[i,j]) + .01

      sigma[i,j] <-  Margin[i,j] * sqrt(pow(Claim_SD[i,j], 2) + pow(Participant_SD[i,j], 2))

      lambda[i,j] <- pow(sigma[i,j],-2)

      Logit_Best[i,j] ~  dnorm(mu[i],lambda[i,j])

    }
  }
}"

           })

    # Fit Model

    JAGSparams <- c("mu", "sigma")

    JAGSinits <- function(){

      list ("mu" = rnorm (1),
            "sigma" = rgamma (1, .1, .1))

    }

    CSsamples <- R2jags::jags(
      # verbose = FALSE,
      data = JAGS_list,
      parameters.to.save = JAGSparams,
      n.thin = 1,
      n.iter = 10000,
      n.burnin = 5000,
      n.chains = 3,
      model.file = textConnection(BayesModel)
    )

    CSmcmc <- coda::as.mcmc(CSsamples)

    CSmu <- matrix ( , JAGS_list$n_claim, 5000) # 5000

    for(i in seq_len(JAGS_list$n_claim)){

      CSmu[i, ] <- CSmcmc[ , sprintf("mu[%s]", i)][[1]]

    }

    # Extract JAGS model output
    # Values in table

    Out <- CSsamples$BUGSoutput$summary
    Pars <- Out[grep ("mu", rownames(Out)), ]
    ProbPars <- exp(Pars) / (1 + exp(Pars))
    row.names (ProbPars) <- paper_id$paper_id #get the effectID's

    aggregation_analysis_output <-
      ProbPars %>%
      tibble::as_tibble() %>%
      dplyr::select(mean) %>%
      dplyr::mutate(method = name,
                    paper_id = row.names(ProbPars)) %>%
      dplyr::select(-mean,
                    dplyr::everything(),
                    mean) %>%
      dplyr::rename(aggregated_judgement = mean)

    df %>%
      dplyr::group_by(paper_id) %>%
      dplyr::summarise(
        n_experts = dplyr::n()
      ) %>%
      dplyr::right_join(aggregation_analysis_output) %>%
      postprocess_judgements()

  }
}
