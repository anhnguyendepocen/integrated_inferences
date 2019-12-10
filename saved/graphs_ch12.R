library(tidyverse)

#Ch12_chain_model_regr <- readRDS("saved/Ch12_chain_model_regr.rds")
#Ch12_chain_model_suff <- readRDS("saved/Ch12_chain_model_suff.rds")
#Ch12_chain_model_conc <- readRDS("saved/Ch12_chain_model_conc.rds")
#Ch12_chain_model_flat <- readRDS("saved/Ch12_chain_model_flat.rds")
#Ch12_chain_model_necs <- readRDS("saved/Ch12_chain_model_necs.rds")

#givens <- list(Ch12_chain_model_regr,
#               Ch12_chain_model_suff,
#               Ch12_chain_model_conc,
#               Ch12_chain_model_flat,
#               Ch12_chain_model_necs)

#process <- function(given) {
#  given <<- given %>%
#    dplyr::mutate(Deep = as.numeric(sub(".*_d_", "", strategy))) %>%
#    dplyr::mutate(Wide = as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
#    dplyr::filter(!is.na(Deep)) %>%
#    dplyr::mutate(upper = post_var + post_var_sd) %>%
#    dplyr::mutate(lower = post_var - post_var_sd)
#}

#lapply(givens, process)

given1 <- readRDS("saved/Ch12_chain_model_regr.rds")$diagnoses_df %>%
  mutate(Deep= as.numeric(sub(".*_d_", "", strategy))) %>%
  mutate(Wide= as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
  filter(!is.na(Deep)) %>%
  mutate(upper = post_var + post_var_sd) %>%
  mutate(lower = post_var - post_var_sd) %>%
  mutate(given = "reg")

given2 <- readRDS("saved/Ch12_chain_model_suff.rds")$diagnoses_df %>%
  mutate(Deep= as.numeric(sub(".*_d_", "", strategy))) %>%
  mutate(Wide= as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
  filter(!is.na(Deep)) %>%
  mutate(upper = post_var + post_var_sd) %>%
  mutate(lower = post_var - post_var_sd) %>%
  mutate(given = "suff")

given3 <- readRDS("saved/Ch12_chain_model_conc.rds")$diagnoses_df %>%
  mutate(Deep= as.numeric(sub(".*_d_", "", strategy))) %>%
  mutate(Wide= as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
  filter(!is.na(Deep)) %>%
  mutate(upper = post_var + post_var_sd) %>%
  mutate(lower = post_var - post_var_sd) %>%
  mutate(given = "conc")

given4 <- readRDS("saved/Ch12_chain_model_flat.rds")$diagnoses_df %>%
  mutate(Deep= as.numeric(sub(".*_d_", "", strategy))) %>%
  mutate(Wide= as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
  filter(!is.na(Deep)) %>%
  mutate(upper = post_var + post_var_sd) %>%
  mutate(lower = post_var - post_var_sd) %>%
  mutate(given = "flat")

given5 <- readRDS("saved/Ch12_chain_model_necs.rds")$diagnoses_df %>%
  mutate(Deep= as.numeric(sub(".*_d_", "", strategy))) %>%
  mutate(Wide= as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
  filter(!is.na(Deep)) %>%
  mutate(upper = post_var + post_var_sd) %>%
  mutate(lower = post_var - post_var_sd) %>%
  mutate(given = "necs")

tograph <- bind_rows(given1, given2, given3, given4, given5)

ggplot(dplyr::filter(tograph, Query=="ATE"), aes(x= Deep, y=post_var)) +
  geom_point(shape = 15, size  = 2) +
  facet_grid(vars(given), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Chain Model. Query = ATE ")


ggplot(dplyr::filter(tograph, Query=="ProbPos"), aes(x= Deep, y=post_var)) +
  geom_point(shape = 15, size  = 2) +
  facet_grid(vars(given), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Chain Model. Query = ProbPos")


ggplot(dplyr::filter(tograph, Query=="via_M"), aes(x= Deep, y=post_var)) +
  geom_point(shape = 15, size  = 2) +
  facet_grid(vars(given), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Chain Model. Query = via_M")
