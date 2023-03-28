#################
### Load Data ###
#################

copycat::datacat_connect()

data_ratings <- pins::pin_get("anon_SIPS_ratings",
                              board = "github")

data_justifications <- pins::pin_get("anon_SIPS_justifications",
                                     board = "github")

data_comments <- pins::pin_get("anon_SIPS_comments",
                               board = "github")

data_supp_priors <- pins::pin_get("anon_SIPS_priors",
                                  board = "github")

data_supp_reasons <- pins::pin_get("anon_SIPS_reasons",
                                   board = "github")

data_outcomes <- pins::pin_get("anon_SIPS_outcomes",
                               board = "github") %>%
  mutate(paper_id = as.character(paper_id))

data_supp_quiz <- pins::pin_get("anon_quiz_scores",
                                board = "github")

data_confidence_scores <- pins::pin_get("SIPS-ConfidenceScores-LF",
                                        board = "github")

######################
### Add To Package ###
######################

usethis::use_data(data_ratings,
                  overwrite = TRUE)

usethis::use_data(data_justifications,
                  overwrite = TRUE)

usethis::use_data(data_comments,
                  overwrite = TRUE)

usethis::use_data(data_supp_priors,
                  overwrite = TRUE)

usethis::use_data(data_supp_reasons,
                  overwrite = TRUE)

usethis::use_data(data_outcomes,
                  overwrite = TRUE)

usethis::use_data(data_supp_quiz,
                  overwrite = TRUE)

usethis::use_data(data_confidence_scores,
                  overwrite = TRUE)

