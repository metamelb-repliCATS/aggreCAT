#' P1_ratings
#'
#' Anonymized expert judgements of known-outcome
#' claims, assessed at the 2019 SIPS repliCATS workshop
#'
#' @format A table with 6880 rows and 7 columns:
#' \describe{
#'   \item{round}{character string identifying whether the round was 1 (pre-discussion) or 2 (post-discussion)}
#'   \item{paper_id}{character string of the claim ids (25 unique claims total)}
#'   \item{user_name}{character string of anonymized IDs for each participant (25 participants included in this dataset)}
#'   \item{question}{character string for the question type, with four options: direct_replication, involved_binary, belief_binary, or comprehension}
#'   \item{element}{character string for the type of response coded in the row, with five options: three_point_lower, three_point_best, three_point_upper, binary_question, or likert_binary}
#'   \item{value}{numeric value for the participant's response}
#'   \item{timestamp}{character string of response recorded time}
#'   \item{group}{character string of group IDs that contained the participants}
#' }
"data_ratings"

#' Free-text justifications for expert judgements
#'
#' @format A table with 5630 rows and 9 columns:
#' \describe{
#'   \item{round}{character string identifying whether the round was 1 (pre-discussion) or 2 (post-discussion)}
#'   \item{paper_id}{character string of the paper ids (25 papers total)}
#'   \item{user_name}{character string of anonymized IDs for each participant (25 participants included in this dataset)}
#'   \item{question}{character string for the question type, with five options: flushing_freetext, involved_binary, belief_binary, direct_replication, and comprehension}
#'   \item{justification}{character string with participant's free-text rationale for their responses}
#'   \item{justification_id}{character string with a unique ID for each row}
#'   \item{vote_count}{numeric of recorded votes (all 0 or 1)}
#'   \item{vote_sum}{numeric of summed vote counts(all 0 or 1)}
#'   \item{timestamp}{character string of response recorded time}
#'   \item{group}{character string of group IDs that contained the participants}
#' }
"data_justifications"

#' data_comments
#' @format A tibble with 2 rows and 10 columns
#' \describe{
#'   \item{round}{character string, both 'round_1' (before discussion)}
#'   \item{paper_id}{character string identifying 2 unique papers}
#'   \item{user_name}{factor for anonymized IDs for two participants}
#'   \item{question}{character string for the type of question, both 'comprehension'}
#'   \item{justification_id}{character string identifying 2 unique justifications}
#'   \item{comment_id}{character string identifying 2 unique comments}
#'   \item{commenter}{redundant column, same as user_name}
#'   \item{comment}{character string with free-text response for the user}
#'   \item{vote_count}{numeric, both 0}
#'   \item{vote_sum}{numeric, both 0}
#'   \item{timestamp}{character string of response recorded time}
#'   \item{group}{character string of group IDs that contained the participants}
#' }
#'
"data_comments"

#' A table of prior means, to be fed into the BayPRIORsAgg aggregation method
#'
#' @format A tibble of 25 rows and 2 columns
#' \describe{
#'   \item{paper_id}{character string with a unique id for each row corresponding to the assessed claim (from 125 papers total)}
#'   \item{prior_means}{numeric with the average prior probability for the claim corresponding to the paper_id}
#' }
#'
"data_supp_priors"

#' A table of scores on the quiz to assess prior knowledge, to be fed into the QuizWAgg aggregation method
#'
#' @format A tibble 19 rows and 2 columns
#' \describe{
#'   \item{user_name}{factor for anonymized IDs for each participant}
#'   \item{quiz_score}{numeric for the participant's score on the quiz (min of 0, max of 16, NA if no questions answered)}
#' }
#'
"data_supp_quiz"

#' Categories of reasons provided by participants for their expert judgements
#'
#' @format a tibble with 625 rows and 15 columns
#' \describe{
#'   \item{paper_id}{character string for the paper ID}
#'   \item{user_name}{character string for participant ID}
#'   \item{RW04 Date of publication}{numeric; references to the date of publication, for example in relation to something being published prior to the 'replication crisis' within the relevant discipline, or a study being difficult to re-run now because of changes in social expectations. }
#'   \item{RW15 Effect size}{numeric; any references to the effect size that indicate that the participant considered the size of the effect when assessing the claim. Don’t use if the term "effect size" is used in unrelated ways, but err on the side of considering statements as relevant to the participant’s assessment.}
#'   \item{RW16 Interaction effect}{numeric; references to when the effect was an interaction effect (rather than a direct effect).}
#'   \item{RW17 Interval or range measure for statistical uncertainty (CI, SD, etc )}{numeric; references to the inclusion, absence, or size of the uncertainty measure for a given effect.}
#'   \item{RW18 Outside participants areas of expertise}{numeric; references to the claim under assessment being outside the participant's areas of expertise.}
#'   \item{RW20 Plausibility}{numeric; references to the plausibility of the claim.}
#'   \item{RW21 Population or subject characteristics (sampling practices)}{numeric; references to the characteristics of the sample population or subjects used in a study that affect the participant’s assessment of the claim, including references to low response rate and any other questions or appreciation of the sampling practices.}
#'   \item{RW22 Power adequacy and or sample size}{numeric; combines 2 nodes for references to the adequacy (or not) of the statistical power of the study &/or sample size.}
#'   \item{RW32 Reputation}{numeric; references to the reputation of the journal/institute/author.}
#'   \item{RW37 Revision statements}{numeric; .}
#'   \item{RW42 Significance, statistical (p-value etc )}{numeric; references to a test of statistical significance for the claim as it impacts on the participant’s assessment. This explicitly includes p-values, t-values, critical alpha and p-rep.}
#' }
"data_supp_reasons"

#' Replication outcomes for the papers
#'
#' @format a tibble with 25 rows and 2 columns
#' \describe{
#'   \item{paper_id}{character string for the paper ID}
#'   \item{outcome}{numeric value of replication outcome. 1 = replication success, 0 = replication failure}
#' }
"data_outcomes"

#' Confidence Scores generated for 25 papers with 22 aggregation methods
#'
#' @format a tibble with 550 rows and 5 columns
#' \describe{
#'   \item{method}{character string of method name}
#'   \item{method_id}{integer of method IDs}
#'   \item{paper_id}{character string of paper IDs}
#'   \item{cs}{numeric of generated confidence scores}
#'   \item{n_experts}{numeric of the number of expert judgements aggregated in confidence score}
#'   \item{first_expert_date}{character string of first expert assessment timestamp}
#'   \item{last_expert_date}{character string of last expert assessment timestamp}
#' }
"data_confidence_scores"
