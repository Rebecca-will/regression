COVID_data <- Assignment_regression_dataset_3_
attach(COVID_data)
names(COVID_data)

model <- lm(age ~ meal.cal, data = COVID_data)
summary(model)

model_test<-lm(age ~ Survival_days)
summary(model_test)
model_test<-lm(age ~ as.factor(status))
summary(model_test)                     # Significant
model_test<-lm(age ~ sex)
summary(model_test)
model_test<-lm(age ~ as.factor(ph.ecog))
summary(model_test)                     # Significant
model_test<-lm(age ~ meal.cal)
summary(model_test)                     # Significant
model_test<-lm(age ~ wt.loss)
summary(model_test)

COVID_Data_multireg=lm(age ~ as.factor(status) + as.factor(ph.ecog) + meal.cal+ sex)
summary(COVID_Data_multireg)

COVID_Data_multireg=glm(age ~ as.factor(status) + as.factor(ph.ecog) + meal.cal + sex,family=gaussian)
summary(COVID_Data_multireg)

library(epiDisplay)

regress.display(COVID_Data_multireg)

par(mfrow=c(2,2))
plot(COVID_Data_multireg)


# Question 2

sex[which(sex=="M")]<-1
sex[which(sex=="F")]<-0
sex<-as.numeric(sex)
sex

model_test<-glm(sex ~ Survival_days,family=binomial)    #
summary(model_test)
model_test<-glm(sex ~ as.factor(status),family=binomial)  #
summary(model_test)
model_test<-glm(sex ~ age,family=binomial)
summary(model_test)
model_test<-glm(sex ~ as.factor(ph.ecog),family=binomial)
summary(model_test)
model_test<-glm(sex ~ meal.cal,family=binomial) #
summary(model_test)
model_test<-glm(sex ~ wt.loss,family=binomial)
summary(model_test)

COVID_sex_reg=glm(sex ~ Survival_days+as.factor(status)+meal.cal,family=binomial)
summary(COVID_sex_reg)

COVID_sex_reg=glm(sex ~ as.factor(status)+meal.cal,family=binomial)
summary(COVID_sex_reg)

logistic.display(COVID_sex_reg)





######3
#does a persons survival differ by sex? What covariates have a significant impact on sex.

metabric=read.csv(file.choose())
attach(metabric)
names(metabric)


library(survival)

model_fit_surv2<-survfit(Surv(Survival_days)~age)
model_fit_surv

plot(model_fit_surv2,col=1:2,lwd = 2, xlab="Days",ylab = "Survival")
legend(265,.95,c("Female","Male"),col=1:2,lwd=2,bty='n')

table(COVID$sex)

survdiff(Surv(Survival_days,status)~sex)
#there is not a significant difference in deaths between male and female


#this is a univariate analysis for one covariate lets do it for multiple

model_cox<-coxph(Surv(Survival_days,status)~sex) #
summary(model_cox)
model_cox<-coxph(Surv(Survival_days,status)~age) #
summary(model_cox)
model_cox<-coxph(Surv(Survival_days,status)~as.factor(ph.ecog)) #
summary(model_cox)
model_cox<-coxph(Surv(Survival_days,status)~meal.cal)
summary(model_cox)
model_cox<-coxph(Surv(Survival_days,status)~wt.loss)
summary(model_cox)



#is there something I need to convert to as.factor?

model_cox<-coxph(Surv(Survival_days,status)~sex+age+as.factor(ph.ecog))
summary(model_cox)
# was age the only other significant factor?


model_cox<-coxph(Surv(Survival_days)~ sex+as.factor(ph.ecog))
summary(model_cox)
#its not significant on its own?


library(epiDisplay)
cox.display(model_cox)
#????

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("survminer")
library(survminer)

model_cox.zph<-cox.zph(model_cox)
print(model_cox.zph)
ggcoxzph(model_cox.zph)

#so were happy that the odds ratio stays the same the entire way along the graph

model_cox.zph<-cox.zph(model_cox)
print(model_cox.zph)
