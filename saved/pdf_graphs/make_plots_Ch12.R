library(tidyverse)

observed <- c("regr", "suff", "flat", "necs")

process <- function(observed) {

  readRDS(paste0("saved/Ch12_base_model_", observed, ".rds"))$diagnoses_df %>%
    dplyr::mutate(Wide = as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
    dplyr::mutate(Deep = as.numeric(sub(".*_d_", "", strategy))) %>%
    dplyr::filter(!is.na(Deep)) %>%
    dplyr::mutate(upper = post_var + 2*post_var_sd,
                  lower = post_var - 2*post_var_sd,
                  observed = observed) %>%
    dplyr::mutate(Wide=paste0("Wide N=", Wide))
}

df <- lapply(observed, process) %>%
  bind_rows()


df$observed<- factor(df$observed,levels=c("flat","regr", "necs", "suff"))

ggplot(df, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a 2-path model- All Queries")

ate <- filter(df, Query=="ATE")
ggplot(ate, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a 2-path model- ATE")

probpos <- filter(df, Query=="ProbPos")
ggplot(probpos, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a 2-path model- ProbPos")

via_M <- filter(df, Query=="via_M")
ggplot(via_M, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a 2-path model- via_M")

################
observed <- c("regr", "suff", "flat", "necs")

process <- function(observed) {

  readRDS(paste0("saved/Ch12_chain_model_", observed, ".rds"))$diagnoses_df %>%
    dplyr::mutate(Wide = as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
    dplyr::mutate(Deep = as.numeric(sub(".*_d_", "", strategy))) %>%
    dplyr::filter(!is.na(Deep)) %>%
    dplyr::mutate(upper = post_var + 2*post_var_sd,
                  lower = post_var - 2*post_var_sd,
                  observed = observed) %>%
    dplyr::mutate(Wide=paste0("Wide N=", Wide))
}

df <- lapply(observed, process) %>%
  bind_rows()


df$observed<- factor(df$observed,levels=c("flat","regr", "necs", "suff"))

ggplot(df, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a 1-path model- All Queries")

ate <- filter(df, Query=="ATE")
ggplot(ate, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a 1-path model- ATE")

probpos <- filter(df, Query=="ProbPos")
ggplot(probpos, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a 1-path model- ProbPos")

via_M <- filter(df, Query=="via_M")
ggplot(via_M, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a 1-path model- via_M")

######################
observed <- c("regr", "suff", "flat", "necs")

process <- function(observed) {

  readRDS(paste0("saved/Ch12_restricted_model_", observed, ".rds"))$diagnoses_df %>%
    dplyr::mutate(Wide = as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
    dplyr::mutate(Deep = as.numeric(sub(".*_d_", "", strategy))) %>%
    dplyr::filter(!is.na(Deep)) %>%
    dplyr::mutate(upper = post_var + 2*post_var_sd,
                  lower = post_var - 2*post_var_sd,
                  observed = observed) %>%
    dplyr::mutate(Wide=paste0("Wide N=", Wide))

}


df <- lapply(observed, process) %>%
  bind_rows()


df$observed<- factor(df$observed,levels=c("flat","regr", "necs", "suff"))

ggplot(df, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Restricted Model- All Queries")

ate <- filter(df, Query=="ATE")
ggplot(ate, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Restricted model- ATE")

probpos <- filter(df, Query=="ProbPos")
ggplot(probpos, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Restricted model- ProbPos")

via_M <- filter(df, Query=="via_M")
ggplot(via_M, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  theme_bw() +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Restricted model- via_M")

###
observed <- c("regr", "suff", "flat", "necs")

process <- function(observed) {

  readRDS(paste0("saved/Ch12_obs_confound_", observed, ".rds"))$diagnoses_df %>%
    dplyr::mutate(Wide = as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
    dplyr::mutate(Deep = as.numeric(sub(".*_d_", "", strategy))) %>%
    dplyr::filter(!is.na(Deep)) %>%
    dplyr::mutate(upper = post_var + 2*post_var_sd,
                  lower = post_var - 2*post_var_sd,
                  observed = observed) %>%
    dplyr::mutate(Wide=paste0("Wide N=", Wide))

}


df <- lapply(observed, process) %>%
  bind_rows()


df$observed<- factor(df$observed,levels=c("flat","regr", "necs", "suff"))

labs <- c("estimate | flat
ATE= ##
Prob Pos= ##
Via M = ##",
          "estimate | regr
ATE= ##
Prob Pos= ##
Via M = ##",
          "estimate | necs
ATE= ##
Prob Pos= ##
Via M = ##",
          "estimate | suff
ATE= ##
Prob Pos= ##
Via M = ##"
)
names(labs) <- c("flat", "regr", "necs", "suff")

ggplot(df, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide),
             labeller = labeller(observed = labs)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  ylim(0,.025) +
  scale_x_continuous(breaks=c(0, 2, 4), limits=c(0, 4))  +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Model with Observed Confound")

ate <- filter(df, Query=="ATE")
ggplot(ate, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide),
             labeller = labeller(observed = labs)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  ylim(0,.025) +
  scale_x_continuous(breaks=c(0, 2, 4), limits=c(0, 4))  +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Model with Obs Confound- ATE")

probpos <- filter(df, Query=="ProbPos")
ggplot(probpos, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide),
             labeller = labeller(observed = labs)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  ylim(0,.025) +
  scale_x_continuous(breaks=c(0, 2, 4), limits=c(0, 4))  +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Model with Obs Confound- ProbPos")

via_M <- filter(df, Query=="via_M")
ggplot(via_M, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide),
             labeller = labeller(observed = labs)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  ylim(0,.025) +
  scale_x_continuous(breaks=c(0, 2, 4), limits=c(0, 4))  +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Model with Obs Confound- via_M")

###
observed <- c("regr", "suff", "flat", "necs")

process <- function(observed) {

  readRDS(paste0("saved/Ch12_unobs_confound_", observed, ".rds"))$diagnoses_df %>%
    dplyr::mutate(Wide = as.numeric(sub(".*w[_]([^.]+)[_]d.*", "\\1", strategy))) %>%
    dplyr::mutate(Deep = as.numeric(sub(".*_d_", "", strategy))) %>%
    dplyr::filter(!is.na(Deep)) %>%
    dplyr::mutate(upper = post_var + 2*post_var_sd,
                  lower = post_var - 2*post_var_sd,
                  observed = observed) %>%
    dplyr::mutate(Wide=paste0("Wide N=", Wide))

}


df <- lapply(observed, process) %>%
  bind_rows()


df$observed<- factor(df$observed,levels=c("flat","regr", "necs", "suff"))

labs <- c("estimate | flat
ATE= ##
Prob Pos= ##
Via M = ##",
          "estimate | regr
ATE= ##
Prob Pos= ##
Via M = ##",
          "estimate | necs
ATE= ##
Prob Pos= ##
Via M = ##",
          "estimate | suff
ATE= ##
Prob Pos= ##
Via M = ##"
)
names(labs) <- c("flat", "regr", "necs", "suff")

ggplot(df, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide),
             labeller = labeller(observed = labs)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  ylim(0,.025) +
  scale_x_continuous(breaks=c(0, 2, 4), limits=c(0, 4))  +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Model with Unbserved Confound")

ate <- filter(df, Query=="ATE")
ggplot(ate, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide),
             labeller = labeller(observed = labs)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  ylim(0,.025) +
  scale_x_continuous(breaks=c(0, 2, 4), limits=c(0, 4))  +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Model with Unobserved Confound- ATE")

probpos <- filter(df, Query=="ProbPos")
ggplot(probpos, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide),
             labeller = labeller(observed = labs)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  ylim(0,.025) +
  scale_x_continuous(breaks=c(0, 2, 4), limits=c(0, 4))  +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Model with Unobserved Confound- ProbPos")

via_M <- filter(df, Query=="via_M")
ggplot(via_M, aes(x= Deep, y=post_var)) +
  geom_point(aes(shape=Query), size=2) +
  geom_line(aes(linetype=Query)) +
  facet_grid(vars(observed), vars(Wide),
             labeller = labeller(observed = labs)) +
  geom_errorbar(aes(ymin  = lower, ymax  = upper, width = 0.15))+
  ylim(0,.025) +
  scale_x_continuous(breaks=c(0, 2, 4), limits=c(0, 4))  +
  theme_bw() +
  theme(strip.text.y = element_text(angle=0)) +
  ylab("Expected Posterior Variance") +
  ggtitle("Wide and Deep Stategies for a Model with Unobserved Confound- via_M")

