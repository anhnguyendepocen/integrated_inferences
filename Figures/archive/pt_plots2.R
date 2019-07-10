
##  Smoking gun and friends
setwd("d:/Dropbox/ProcessTracing/3 paper/")



## How does the probitative value of test depen on priors?
# Assume no uncertainty about Pr(k|A)
# Assume doubly decisive test



gt.slope.text = function(text, f, xl=0, xh=1, vshift=.05, col="black", fixer=1){
	TX = c(" ", strsplit(text, "")[[1]]," ")
	k = length(TX)
	z = xl+ ((0:(k-1))/k)*(xh-xl)
	angles = 	c(0,(atan2(f(z)[3:k]  - f(z)[1:(k-2)],fixer*2*(xh-xl)/(k))*180/pi),0)
	for(i in 1:k){text(z[i], f(z[i])+vshift, TX[i], col = col, srt=angles[i])}
	}
		
k0 = .1
k1 = .9




# posterior = Prob(data|A)Prior(A)/Prob(Data)
posterior = function(prior, observed, k0, k1) observed*k1*prior/(k1*prior+k0*(1-prior)) +  (1-observed)*(1-k1)*prior/(1-(k1*prior+k0*(1-prior)))
plotit = function(k0, k1, main="", xl=.25, xh=.75){
	prior = seq(0,1,.02)
	plot(prior, posterior(prior, 1, k0, k1), type="l", main=(paste(main, "\n (q0 = ", k0,  ", q1 = ", k1,")",  sep="")), ylab="Posterior")
	lines(prior, posterior(prior, 0, k0, k1), type="l")
	abline(a=0, b=1)
	gt.slope.text("posterior if clue present", f= function(x) posterior(x, 1, k0, k1), xl = xl, xh=xh) 
	gt.slope.text("posterior if clue absent", f= function(x) posterior(x, 0, k0, k1), xl = xl, xh=xh) 
	gt.slope.text("prior", f= function(x) x, xl = xl, xh=xh) 
	}

	plotit(.4, .6, main="Straw in the Wind")
		

plot(1:10,main=expression(paste("q"[0],"A")))


pdf(file="PTtests1.pdf", width=9, height=9) 
plot(c(0,1), c(0,1), type="l", col="grey", xlab=expression(paste("q"[d], " (Probability of observing ", italic(k), " given d)")), 
				ylab=expression(paste("q"[b], " (Probability of observing ", italic(k), " given b)")), 
				main="Classification of tests")
text(.1,.95, "k present: \n doubly decisive for b  ")
text(.1,.875, "k absent: \n doubly decisive for d  ")
text(.08,.25, "k present: \n smoking gun for b \n k absent \n hoop test for d")
text(.7,.9, "k present: \n hoop test for b \n k absent: \n smoking gun for d")
text(.4,.6, "k present: \n straw in the wind for b \n k absent: \n straw in the wind for d")

text(.9,.125, "k present: \n doubly decisive for d  ")
text(.9,.05, "k absent: \n doubly decisive for b")
text(.9,.7, "k present: \n hoop test for d \n k absent: \n smoking gun for b")
text(.25,.05, "k present: \n smoking gun test for d \n k absent: \n hoop test for b")
text(.6,.4, "k present: \n straw in the wind for d \n k absent: \n straw in the wind for b ")

arrows(.25,.7, .25, .85, col="red")
arrows(.25,.7, .1, .7, col="red")

text(.35, .775, "More sensitive \n for b", col="red")
text(.1775, .65, "More specific \n for b", col="red")


arrows(.75,.3, .9, .3, col="red")
arrows(.75,.3, .75, .15, col="red")

text(.65, .225, "More specific \n for d", col="red")
text(.825, .35, "More sensitive \n for d", col="red")

dev.off()


par(mfrow=c(1,1))

pdf(file="PTtests2.pdf", width=9, height=9) 
par(mfrow = c(2,2))
	plotit(.4, .6, main="Straw in the Wind")
	plotit(.6, .95, main="Hoop")
	plotit(.05, .4, main="Smoking Gun")
	plotit(.05, .95, main="Doubly Decisive")
dev.off()
