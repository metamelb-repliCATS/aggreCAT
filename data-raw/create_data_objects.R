#######################################
### Create Data Objects For Package ###
#######################################

library(tidyverse)

## Connect to datacat

copycat::datacat_connect()

## Load objects

data_ratings <- pins::pin_get("anon_SIPS_ratings",
                              board = "github") %>%
  dplyr::select(-timestamp)

data_justifications <- pins::pin_get("anon_SIPS_justifications",
                                     board = "github") %>%
  dplyr::select(-timestamp)

data_comments <- pins::pin_get("anon_SIPS_comments",
                               board = "github") %>%
  dplyr::select(-timestamp)

data_outcomes <- pins::pin_get("anon_SIPS_outcomes",
                               board = "github")

data_supp_reasons <- pins::pin_get("anon_SIPS_reasons",
                                   board = "github")

data_supp_quiz <- pins::pin_get("anon_quiz_scores",
                                board = "github") %>%
  dplyr::filter(user_name %in% data_ratings$user_name,
                !is.na(quiz_score)) %>%
  dplyr::select(user_name,
                quiz_score)

data_supp_priors <- pins::pin_get("anon_SIPS_priors",
                                  board = "github")

data_confidence_scores <- pins::pin_get("SIPS-ConfidenceScores-LF",
                                        board = "github") %>%
  select(-first_expert_date,
         -last_expert_date)

## Write objects to file

usethis::use_data(data_ratings,
                  overwrite = TRUE)

usethis::use_data(data_justifications,
                  overwrite = TRUE)

usethis::use_data(data_comments,
                  overwrite = TRUE)

usethis::use_data(data_outcomes,
                  overwrite = TRUE)

usethis::use_data(data_supp_reasons,
                  overwrite = TRUE)

usethis::use_data(data_supp_quiz,
                  overwrite = TRUE)

usethis::use_data(data_supp_priors,
                  overwrite = TRUE)

usethis::use_data(data_confidence_scores,
                  overwrite = TRUE)

