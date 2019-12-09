library(dplyr)
library(ggplot2)
<<<<<<< Updated upstream

tograph <- Ch12_chain_model_regr$diagnoses_df %>%
=======
library(tidyverse)


given <-  Ch12_chain_model_regr$diagnoses_df %>%
>>>>>>> Stashed changes
  mutate(Wide=substr(strategy, 3, 3)) %>%
  mutate(Deep= as.numeric(substr(strategy, 7, 7))) %>%
  filter(!is.na(Deep)) %>%
  mutate(upper= post_var + post_var_sd) %>%
<<<<<<< Updated upstream
  mutate(lower= post_var - post_var_sd)

wide.labs <- c("Deep = 0", "Deep = 2", "Deep = 4")
names(wide.labs) <- c("0", "2", "4")

ggplot(dplyr::filter(tograph, Query=="ATE"), aes(x= Deep, y=post_var)) + 
  geom_point(shape = 15, size  = 2) + 
  facet_wrap(~Wide) +
=======
  mutate(lower= post_var - post_var_sd) 

given1 <- given%>%
  mutate(given="reg")

given2 <- given %>%
  mutate(given="flat")

given3 <- given %>%
  mutate(given="suff")

given4 <- given %>%
  mutate(given="necs")

given5 <- given %>%
  mutate(given="conc")

tograph <- bind_rows(given1, given2, given3, given4, given5)

ggplot(dplyr::filter(tograph, Query=="ATE"), aes(x= Deep, y=post_var)) + 
  geom_point(shape = 15, size  = 2) + 
  facet_grid(vars(given), vars(Wide)) +
>>>>>>> Stashed changes
  scale_x_continuous(breaks=seq(0, 4, 2)) + 
  expand_limits(y=.025) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
<<<<<<< Updated upstream
  ggtitle("Wide and Deep Stategies fo a Chain Model. Given = Reg; Query = ATE ")
=======
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Chain Model. Query = ATE ")
>>>>>>> Stashed changes


ggplot(dplyr::filter(tograph, Query=="ProbPos"), aes(x= Deep, y=post_var)) + 
  geom_point(shape = 15, size  = 2) + 
<<<<<<< Updated upstream
  facet_wrap(~Wide) +
=======
  facet_grid(vars(given), vars(Wide)) +
>>>>>>> Stashed changes
  scale_x_continuous(breaks=seq(0, 4, 2)) + 
  expand_limits(y=c(.035, .055)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
<<<<<<< Updated upstream
  ggtitle("Wide and Deep Stategies fo a Chain Model. Given = Reg; Query = ProbPos ")

ggplot(dplyr::filter(tograph, Query=="via_M"), aes(x= Deep, y=post_var)) + 
  geom_point(shape = 15, size  = 2) + 
  facet_wrap(~Wide) +
=======
  ggtitle("Wide and Deep Stategies for a Chain Model. Query = ProbPos ")

ggplot(dplyr::filter(tograph, Query=="via_M"), aes(x= Deep, y=post_var)) + 
  geom_point(shape = 15, size  = 2) + 
  facet_grid(vars(given), vars(Wide)) +
  ylab("Expected Posterior Variance") +
>>>>>>> Stashed changes
  scale_x_continuous(breaks=seq(0, 4, 2)) +
  scale_y_continuous(breaks=seq(.0, .08, .015))  +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
<<<<<<< Updated upstream
  ggtitle("Wide and Deep Stategies fo a Chain Model. Given = Reg; Query = via_M ")
=======
  ggtitle("Wide and Deep Stategies for a Chain Model. Query = via_M ")
>>>>>>> Stashed changes


  