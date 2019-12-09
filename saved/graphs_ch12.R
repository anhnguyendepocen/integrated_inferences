library(dplyr)
library(ggplot2)

tograph <- Ch12_chain_model_regr$diagnoses_df %>%
  mutate(Wide=substr(strategy, 3, 3)) %>%
  mutate(Deep= as.numeric(substr(strategy, 7, 7))) %>%
  filter(!is.na(Deep)) %>%
  mutate(upper= post_var + post_var_sd) %>%
  mutate(lower= post_var - post_var_sd)

wide.labs <- c("Deep = 0", "Deep = 2", "Deep = 4")
names(wide.labs) <- c("0", "2", "4")

ggplot(dplyr::filter(tograph, Query=="ATE"), aes(x= Deep, y=post_var)) + 
  geom_point(shape = 15, size  = 2) + 
  facet_wrap(~Wide) +
  scale_x_continuous(breaks=seq(0, 4, 2)) + 
  expand_limits(y=.025) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ggtitle("Wide and Deep Stategies fo a Chain Model. Given = Reg; Query = ATE ")


ggplot(dplyr::filter(tograph, Query=="ProbPos"), aes(x= Deep, y=post_var)) + 
  geom_point(shape = 15, size  = 2) + 
  facet_wrap(~Wide) +
  scale_x_continuous(breaks=seq(0, 4, 2)) + 
  expand_limits(y=c(.035, .055)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies fo a Chain Model. Given = Reg; Query = ProbPos ")

ggplot(dplyr::filter(tograph, Query=="via_M"), aes(x= Deep, y=post_var)) + 
  geom_point(shape = 15, size  = 2) + 
  facet_wrap(~Wide) +
  scale_x_continuous(breaks=seq(0, 4, 2)) +
  scale_y_continuous(breaks=seq(.0, .08, .015))  +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies fo a Chain Model. Given = Reg; Query = via_M ")


  