# Bayesian Triple Variability

library(tidyverse)
library(R2jags)
library(tidybayes)

# ReShape Data for JAGS model fitting
expert_data <- input_data_example %>%
  preprocess_judgements() %>%
  # dplyr::filter(round == "round_2",
  #               element %in% c("three_point_best",
  #                              "three_point_lower",
  #                              "three_point_upper" ),
  #               str_detect(paper_id, "UOM")) %>%
  dplyr::mutate(effectID = str_remove_all(paper_id, "UOM(?!\\d$)|UOM(\\d$)"),
                effectID = as.numeric(effectID),
                groupID = str_sub(string = paper_id,start = -1, end = -1)) %>%
  dplyr::select(effectID, groupID, value, element, user_name) %>%
  group_by(effectID, groupID) %>%
  tidyr::spread(key = element, value = value)

user <- expert_data %>%
  dplyr::ungroup() %>%
  dplyr::select(user_name) %>%
  distinct() %>%
  mutate(user = 1:n())

expert_data <-
  expert_data %>%
  left_join(user) %>%
  dplyr::select(-user_name)

ParNames <- unique(expert_data$effectID) #store effectID's for after model fitting

JAGS_tidy <-
  expert_data %>%
  dplyr::ungroup() %>%
  dplyr::rename(claims = effectID,
                lower = "three_point_lower",
                best = "three_point_best",
                upper = "three_point_upper",
                participant = "user") %>%
  dplyr::arrange(claims, participant) %>%
  dplyr::mutate(lower = lower / 100,
                upper = upper / 100,
                best = best / 100,
                LogitEst = log(.99*best/(1-.99*best)),
                claims = factor(claims, levels = unique(claims)),
                participant = factor(participant, levels = unique(participant))) %>%
  dplyr::select(-best, -groupID) %>%  #drop groupID for Anca's analysis
  tidybayes::compose_data()

JAGS_tidy_list <- list(
  JAGS_tidy$LogitEst,
  JAGS_tidy$lower,
  JAGS_tidy$upper)

as_matrix_claims_participants <- . %>%
  matrix(data = ., nrow = JAGS_tidy$n_claims, ncol = JAGS_tidy$n_participant, byrow = TRUE)

JAGS_tidy_list <- lapply(JAGS_tidy_list, as_matrix_claims_participants)

JAGS_tidy_list <- append(JAGS_tidy_list, list(JAGS_tidy$n_claims, JAGS_tidy$n_participant))

names(JAGS_tidy_list) <- c("LogitEst", "lower", "upper", "n_claims","n_participant")

# Define Jags Model

BayTripleVar <- "model{
  # priors for claim location and variability
  for (i in 1:n_claims)
  {
   mu[i] ~ dnorm(0,3) #weakly informative prior

   claim_sigma[i] ~ dunif(0,10)

  }
  # prior for person variability
  for (j in 1:n_participant)
  {

   pp_sigma[j] ~ dunif(0,10)

  }

  # Data
  for (i in 1:n_claims)
  {
    for (j in 1:n_participant)
    {
      # Sd for claim*person combination of
      # 1. generic claim sd (claim_sigma)
      # 2. generic pp sd (pp_sigma)
      # 3. specific uncertainty of pp for claim (upper-lower)
      Margin[i,j] <- (upper[i,j]-lower[i,j])+.01
      sigma[i,j] <-  Margin[i,j]*sqrt(pow(claim_sigma[i], 2)+pow(pp_sigma[j], 2))
      lambda[i,j] <- pow(sigma[i,j],-2)
      LogitEst[i,j] ~  dnorm(mu[i],lambda[i,j])
    }
  }
}"

# Fit Model

JAGSparams <- c("mu", "sigma", "claim_sigma", "pp_sigma")

JAGSinits <- function() {list ("mu" = rnorm (1),
                              "sigma" = rgamma (1, .1, .1),
                              "claim_sigma" = rgamma (1, .1, .1),
                              "pp_sigma" = rgamma (1, .1, .1))}

CSsamples = jags (data = JAGS_tidy_list,
                    parameters.to.save = JAGSparams, n.thin = 1,
                    n.iter = 10000, n.burnin =5000, n.chains = 3,
                    model.file = textConnection(BayTripleVar))

CSmcmc <- as.mcmc(CSsamples)

CSmu <- matrix (, n_claims, 5000) # 5000

for (i in 1:n_claims)
{
  CSmu[i,] <- CSmcmc[,paste ("mu[", i, "]", sep = "")][[1]]
}

# Extract JAGS model output
# Values in table
Out <- CSsamples$BUGSoutput[[10]]
Pars <- Out[grep ("mu", rownames(Out)),]
ProbPars <- exp(Pars)/(1+exp(Pars))
row.names (ProbPars) <- ParNames #get the effectID's

aggregation_analysis_output <-
  ProbPars %>% as_tibble() %>%
  dplyr::select(mean) %>%
  dplyr::mutate(MethodID = "BayPRIORsAgg", Method_NumID = 11, QuestionID = ParNames) %>%
  dplyr::select(-mean, everything(), mean)

#readr::write_csv(aggregation_analysis_output, path = "./posterior_predictionsBayesTriVar.csv")

