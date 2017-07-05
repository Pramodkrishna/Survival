""
Survival Analysis 
http://www.sthda.com/english/wiki/survival-analysis
http://www.sthda.com/english/wiki/survival-analysis-basics

""

setwd("/home/pramod/Desktop/BU/")
install.packages("survival")
install.packages("survminer")


library(survival)
library(survMisc)
library(ggplot2)

cor(d.lung$time,d.lung$age)
cor(d.lung$status,d.lung$age,method = "pearson")
cor(d.lung$status,y = d.lung$sex)
cor(d.lung$time,y = d.lung$sex)
"""
Finding Correlation
"""

data("lung")
d.lung <- as.data.frame(lung)
names(d.lung)

"""
Kaplan-Meier survival estimate
"""
sex_lung <- survfit(Surv(d.lung$time,d.lung$status) ~ d.lung$sex,data = d.lung)
print(sex_lung)
summary(sex_lung)$table


age_lung <- survfit(Surv(d.lung$time,d.lung$status) ~ d.lung$age)
summary(age_lung)$table

plot(age_lung$strata)

plot(x = age_lung$n.risk,y = age_lung$time)

"""
Uni-variate Analysis
"""

res.cox <- coxph(Surv(time, status) ~ sex, data = lung)
res.cox

summary(res.cox)


"""
Multivariate Analysis
"""
res.cox_multi <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data =  d.lung)
summary(res.cox_multi)

"The p-value for all three overall tests (likelihood, Wald, and score) are significant, 
indicating that the model is significant. These tests evaluate the omnibus null hypothesis that all 
of the betas () are 0. In the above example, the test statistics are in close agreement, 
and the omnibus null hypothesis is soundly rejected."



