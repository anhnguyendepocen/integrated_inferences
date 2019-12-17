library(tidyverse)

givens <- c("regr", "suff", "conc", "flat", "necs")

process <- function(given) {

 readRDS(paste0("saved/Ch12_chain_model_", given, ".rds"))$diagnoses_df %>%
    dplyr::mutate(Deep = as.numeric(sub(".*_d_", "", strategy))) %>%
    dplyr::mutate(Wide = as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
    dplyr::filter(!is.na(Deep)) %>%
    dplyr::mutate(upper = post_var + 2*post_var_sd,
                   lower = post_var - 2*post_var_sd,
                   given = given)
}

df <- lapply(givens, process) %>%
  bind_rows()

df$given<- factor(df$given,levels=c("flat","regr","conc", "necs", "suff"))

ggplot(df, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(given), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Chain Model")

