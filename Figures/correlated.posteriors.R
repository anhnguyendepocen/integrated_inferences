rm(list=ls())

setwd("D:/Dropbox/ProcessTracing/3 paper/2 APSR Revision/Figures")


# X=1 Y=1 case chose. Is it b or d. Say phi_d = .1 and phi_b~ uniform[0,1]. Say prior on b is .5.
k = 10000
phi_b = seq(0.001,.999, length = k)

theta = data.frame(type = c(rep(0,k), rep(1,k)), phi_b = c(phi_b, phi_b), phi_d = rep(.1,2*k)) 

posterior_k_seen = (theta$type*theta$phi_b + (1-theta$type)*theta$phi_d)
posterior_k_seen=posterior_k_seen/sum(posterior_k_seen)

posterior_k_not_seen = (theta$type*(1-theta$phi_b) + (1-theta$type)*(1-theta$phi_d))
posterior_k_not_seen = posterior_k_not_seen/sum(posterior_k_not_seen)

select1 = sample(1:(2*k), k, replace = FALSE, prob = posterior_k_seen)
select2 = sample(1:(2*k), k, replace = FALSE, prob = posterior_k_not_seen)

pdf("correlatedposteriors.pdf")
  par(mfrow=c(1,2))
  plot(theta$type[select2]+(rnorm(2*k)/10)[select2], theta$phi_b[select2], col = rgb(.8,.1,.2,.4), pch =16, cex=.4, xlab = "Is it a b?", ylab = expression(phi[b]), main="Beliefs | K not seen", axes=FALSE) 
  axis(1, at=c(0,1), labels=c("d", "b"));   axis(2)
  box()
  
  plot(theta$type[select1]+(rnorm(2*k)/10)[select1], theta$phi_b[select1], col = rgb(.8,.1,.2,.4), pch =16, cex=.4, xlab = "Is it a b?", ylab = expression(phi[b]), main="Beliefs | K seen", axes=FALSE) 
  axis(1, at=c(0,1), labels=c("d", "b"));   axis(2)
  box()
dev.off()
