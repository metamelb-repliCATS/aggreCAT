library(tidyverse)
library(readxl)
library(flextable)
library(ggridges)
library(janitor)
library(tidytext)
library(aggreCAT)

# Descriptive analysis of the output from t&e is first explored then plots of the
# replicated outcome, aggregation methods and overall confidence scores are then
# displayed.

# Load Files --------------------------------------------------------------
copycat::datacat_connect()

# Temp locations till we approve location

# Latest te output file
replicated_outcomes <- pins::pin_get("Replicated-Outcomes", board = "github")
te_output <- replicated_outcomes

# Final confidence scores from phase one
TA2_CS <- pins::pin_get(name = "P1_confidence_scores_LF", board = "github")

# Last platform exports
latest_ratings <- pins::pin_get(name = "P1-ratings", board = "github")

# Count of replicated claims form t&e
te_output %>%
  group_by(paper_id, rr_type) %>%
  count(sort = TRUE) %>%
  distinct(paper_id) %>%
  nrow()
# There are 61 replicated claims that have come from T&E

library(gtsummary)
te_output %>%
  group_by(paper_id, rr_type) %>%
  count(sort = TRUE) %>%
  tbl_summary()



# Descriptive Exploration of TE Output Data -------------------------------

# Descriptive table of te output

# rr_repl_effect_direction_reference
# Whether the effect size parameter achieved in the replication/reproduction is
# in the same direction as the original effect. This is reported whether or not
# that effect is found to be significant, and refers ONLY to the
# positive/negative sign (so e.g. an Cohen’s F squared will always have ‘yes’
# value for this variable.) TRUE/FALSE

te_output %>%
  group_by(rr_repl_effect_direction_reference) %>%
  rename(Outcome = rr_repl_effect_direction_reference) %>%
  count(sort = TRUE, name = "Effect Direction") %>%
  janitor::adorn_totals()

# ---

# rr_repl_pattern_replicated_reported
# Whether the pattern of results found in this replication met the pattern
# criteria that were preregistered (i.e. was in the same or similar pattern as
# the original study. References repl_pattern_criteria and
# repl_pattern_description variables) TRUE/FALSE

te_output %>%
  filter(paper_id %in% TA2_CS$paper_id) %>%
  group_by(rr_repl_pattern_replicated_reported) %>%
  rename(Outcome = rr_repl_pattern_replicated_reported) %>%
  count(sort = TRUE, name = "Replicated Pattern") %>%
  janitor::adorn_totals()
  #flextable()

# ---

# rr_repl_exact_replicated_reference
# Calculated from the following variables: rr_p_value_value_reference (p < 0.05)
# & rr_final_repl_pattern_replicated_reported (yes), corresponding to the exact
# SCORE criteria  (a significant (p<0.05, two-tailed) result in the same or
# similar pattern as the original effect)

te_output %>%
  filter(paper_id %in% TA2_CS$paper_id) %>%
  group_by(rr_repl_exact_replicated_reference) %>%
  rename(Outcome = rr_repl_exact_replicated_reference) %>%
  count(sort = TRUE, name = "Exact Replication") %>%
  janitor::adorn_totals()


# ---
# Whether this empirical evaluation attempt is a replication (Direct
# Replication, with new data collection during SCORE, or Data Analytic
# Replication, with a second existing dataset identified and tested during
# SCORE) or reproduction (i.e. with the dataset used in the original study)

te_output %>%
  filter(paper_id %in% TA2_CS$paper_id) %>%
  group_by(rr_type) %>%
  count(sort = TRUE) %>%
  janitor::adorn_totals() %>%
  rename("Replication Type" = "rr_type")
  # flextable()

# ---
# Table to view the count of replicated claims across the replication type: Data
# Analytic versus Direct Replication vs Computational Reproduction

te_output %>%
  filter(paper_id %in% TA2_CS$paper_id) %>%
  select(rr_type, rr_repl_exact_replicated_reference) %>%
  janitor::tabyl(rr_type, rr_repl_exact_replicated_reference) %>%
  rename("Replication Type" = "rr_type") #%>%
  # tbl_summary(by = rr_repl_exact_replicated_reference) %>%
  # add_overall() %>%
  # add_n() %>%
  # bold_labels()


# Count of domains
te_output %>%
  filter(paper_id %in% TA2_CS$paper_id) %>%
  select(paper_id, rr_repl_exact_replicated_reference, rr_type) %>%
  left_join(TA2_CS, by = c("paper_id")) %>%
  group_by(paper_id, domain) %>%
  count() %>%
  group_by(domain) %>%
  mutate(domain = str_to_title(domain, locale = "en")) %>%
  count(sort = TRUE) %>%
  janitor::adorn_totals()


# Plots -------------------------------------------------------------------

# Using ridges and density  plot
density_plot <- te_output %>%
  filter(paper_id %in% TA2_CS$paper_id) %>%
  select(paper_id, rr_repl_exact_replicated_reference, rr_type) %>%
  left_join(., TA2_CS, by = c("paper_id"))

# Density Plot of Replication Type
# Removed computational reproduction as only n = 1
density_plot %>%
  filter(!rr_type == "Computational Reproduction") %>%
  ggplot() +
  aes(x = cs, y = "", fill = rr_type, group = rr_type) +
  labs(x = "Confidence Score",
       y = "",
       fill = "Replication Type",
       title = "Replication Type") +
  geom_density_ridges(scale = 1, alpha = .7) +
  geom_vline(xintercept = .5, linetype = "dashed", color = "red") +
  theme_bw() +
  theme(
        text = element_text((family = "FreeSans")),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
  ) +
  facet_wrap(~ rr_repl_exact_replicated_reference) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0., .1, .2, .3, .4, .5, .6, .7, .8, .9, 1))

# Density Plot of each Aggregation Method
ggplot(density_plot) +
  aes(x = cs,
      y = "" ,
      fill = rr_repl_exact_replicated_reference,
      group = rr_repl_exact_replicated_reference) +
  geom_density_ridges(scale = 1, alpha = .7) +
  geom_vline(xintercept = .5, linetype = "dashed", color = "red") +
  labs(x = "Confidence Score",
       y = " ",
       title = "Confidence Scores for Each Method",
       subtitle = "Density plot of each aggregation method and replicated outcome",
       fill = "TE Replicated \nOutcome",
       caption = "60 Claims",
       color = "") +
  theme_bw() +
  theme(text = element_text((family = "FreeSans"))) +
  facet_wrap(~method) +
  scale_y_discrete(expand = c(0, 0))

# Matrix Plot of Replication Outcome, CS and Methods ----------------------


cs_with_te_output <- te_output %>%
  filter(paper_id %in% TA2_CS$paper_id) %>%
  group_by(rr_repl_exact_replicated_reference) %>%
  #rename(Outcome = rr_repl_exact_replicated_reference)
  left_join(., TA2_CS, by = c("paper_id")) %>%
  select(paper_id, method, cs, rr_repl_exact_replicated_reference, rr_type)


# Set names for facet grid
labels_repl <- c("Replicated", "Did Not Replicate")
names(labels_repl) <- c("TRUE", "FALSE")

cs_with_te_output %>%
  #filter(rr_type == "Direct Replication") %>%
  ggplot() +
  aes(x = paper_id, y = factor(method, levels = rev(levels(factor(method)))), fill = cs, colour = cs) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  scale_color_distiller(palette = "RdBu", direction = 1) +
  labs(fill = "Confidence \nScore", color = "Confidence \nScore") +
  scale_y_reordered() +
  labs(x = "Claim ID",
       y = "Aggregation Method",
       title = "",
       subtitle = "Replicated Outcome Across Aggregation Method and Claim ID",
       caption = "61 Claims (16 Data Analytic Replication, 43 Direct Replication, 1 Computational Replication)") +
  theme_bw() +
  theme(
        text = element_text((family = "FreeSans")),
        axis.text.x = element_text(angle = 75, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        ) +
  facet_grid(vars(rr_repl_exact_replicated_reference),
             labeller = labeller(rr_repl_exact_replicated_reference = labels_repl))


# Pick out a focal claim --------------------------------------------------


# Forrest plot for best estimates with the confidence score

round2_estimates <- latest_ratings %>%
  filter(round == "round_2") %>%
  filter(question == "direct_replication") %>%
  select(-timestamp) %>%
  group_by(paper_id) %>%
  pivot_wider(names_from = element, values_from = value) %>%
  summarise(
    upper = median(three_point_upper)/100,
    best = median(three_point_best)/100,
    lower = median(three_point_lower)/100
    )

estimates_cs <- round2_estimates %>%
  filter(paper_id %in% te_output$paper_id) %>%
  left_join(., cs_with_te_output, by  = c("paper_id")) %>%
  select(paper_id, method, cs, upper, best, lower, rr_type, rr_repl_exact_replicated_reference)

# estimates_cs <- transform(estimates_cs, variables = reorder(paper_id, best))

# Function to plot estimates for each method
forest_plot_replicated <- function(method = "ArMean"){

  estimates_cs %>%
    filter(method == {{method}}) %>%
    mutate(paper_id = fct_reorder(paper_id, best)) %>%
    ggplot(aes(x = paper_id, y = best, color = rr_repl_exact_replicated_reference)) +
    geom_point(position = position_dodge(width = 2)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 2), width = .9) +
    geom_point(aes(y = cs), stat = "identity", position = "identity", shape = 17, color = "black") +
    geom_hline(yintercept = .5, color = "black", linetype = "dashed", alpha = .5) +
    ylab("Participants Estimates") +
    xlab("Claim ID") +
    labs(title = paste0({{method}}, ": Lower, Best, Upper and Overall Confidence Score"),
         fill = "",
         color = "",
         legend = "",
         caption = "Direct Replication; Bars are the median lower, best and upper estimates for the claim id. Overall Confidence Score is presented as a black marker") +
    theme_bw() +
    theme(
          text = element_text((family = "FreeSans")),
          axis.text.x = element_text(angle = 75, hjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = "white"),
          legend.position = "none"
    ) +
    coord_flip() +
    facet_grid(vars(rr_repl_exact_replicated_reference), scales = "free",
               labeller = labeller(rr_repl_exact_replicated_reference = labels_repl)) +
    scale_y_continuous(breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1))
}



forest_plot_replicated(method = "IntWAgg")


forest_plot_replicated(method = "Median")

