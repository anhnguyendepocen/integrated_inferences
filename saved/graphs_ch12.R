library(tidyverse)

givens <- c("regr", "suff")

process <- function(given) {

 readRDS(paste0("saved/Ch12_chain_model_", given, ".rds")) %>%
    dplyr::mutate(Deep = as.numeric(sub(".*_d_", "", strategy))) %>%
    dplyr::mutate(Wide = as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
    dplyr::filter(!is.na(Deep)) %>%
    dplyr::mutate(upper = post_var + 2*post_var_sd,
                   lower = post_var - 2*post_var_sd.
                   given = given)
}

df <- lapply(givens, process) %>% do_apply(rbind)

# Order: flat, regr, conc, necc, suff

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

ggplot(tograph, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(given), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Chain Model")

