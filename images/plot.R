
# cover image

frame()
library(DeclareDesign)
library(tidyverse)
data <- fabricate(N = 5000,
                  Y = complete_ra(N, prob = .6),
                  LR =   block_ra(blocks = Y, block_prob = c(.3, .7)) + rnorm(N)/10,
                  Z =   block_ra(blocks = Y, block_prob = c(.2, .8)) + rnorm(N)/10,
                  C = .1 + runif(N)/9,
                  Z = C*Z + (1-C)*.5,
                  LR = C*LR + (1-C)*.5

)

bayesplot <- data %>% ggplot(aes(LR, Z, color = factor(Y))) + geom_point(alpha = .5) + theme_void()+ theme(legend.position = "none")

png("plot.png")
bayesplot
dev.off()
