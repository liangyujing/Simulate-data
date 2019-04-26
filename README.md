pwr.2p.test    two proportions (equal n) 
pwr.2p2n.test  two proportions (unequal n) 
pwr.anova.test  balanced one way ANOVA 
pwr.chisq.test  chi-square test
pwr.f2.test  general linear model 
pwr.p.test  proportion (one sample) 
pwr.r.test  correlation
pwr.t.test  t-tests (one sample, 2 sample, paired) 
pwr.t2n.test  t-test (two samples with unequal n) 





# 1. ANOVA
# F2,87=5.49, p<0.01.


# combined standard deviation
sd1=0.32
sd2=0.27
sd3=0.33

n1=30
n2=35
n3=28

pooledSD<-sqrt(sum(sd1^2*(n1-1)+sd2^2*(n2-1)+sd3^2*(n3-1))/sum(n1+n2+n3-3))
                                  

## 规定power = 0.8， 求sample size
# power analysis of ANOVA
### x轴：sample size； Y轴：power
groupmeans <- c(2.75, 3.2, 3.54)
p1 <- power.anova.test(groups = length(groupmeans), 
                      between.var = var(groupmeans), within.var = pooledSD, 
                      sig.level=0.01,n=c(10,15,20,25,30,35))

plot(p1$n,p1$power,type="b",xlab="sample size",ylab="power")

## calculate the sample per group to get 0.80 power.
p2=power.anova.test(groups=3, between.var = var(groupmeans), within.var = pooledSD, 
                    sig.level=0.01, power=.80) 
p2$n




# 2.main effect of 2D:4D
pwr.t.test




# 3. correlation between 2D : 4D and the minimum acceptance level. 
## elderly women condition: (rs=-0.30, p>0.05, n=28),


## elderly condition   x轴：sample size； Y轴：power
library("pwr")
p3=pwr.r.test(n =c(10,20,30,40,50,60,70,80,90,100), r =-0.3 , sig.level = .05, power = NULL) 
plot(p3$n,p3$power,type="b",xlab="sample size",ylab="power")


## 规定power = 0.8， 求sample size
## calculate the sample per group to get 0.80 power for 3 conditions.
##### I' not sure whether for correlation, we should also expect a 0.80 power ### 
p4=pwr.r.test(n = NULL, r =-0.3 , sig.level = .05, power = 0.80)
p5=pwr.r.test(n = NULL, r =0.4 , sig.level = .05, power = 0.80) 
p6=pwr.r.test(n = NULL, r =-0.36 , sig.level = .05, power = 0.80) 

p4$n
p5$n
p6$n
