library(aod)
library (readxl)
library(ggplot2)
library(multcomp)
library(lme4)
library(nlme)
library(emmeans)

setwd("")
data<- read_excel("M3_YJ_Thesis.xlsx")
data$success<- factor(data$success)
data$landmark<- factor(data$landmark, levels = c("N", "P", "D", "PD"))
data$trial<-as.integer(data$trial)
data$subject <- factor(data$subject)

data$sex<-factor(data$sex)
data$Game<-as.numeric(data$Game)
data$Computer<-as.numeric(data$Computer)
data$drive<-as.integer(data$drive)
data$Age<-as.integer(data$Age)


data$CST<-as.integer(data$CST)
data$PTSOT<-as.numeric(data$PTSOT)
data$MRT<-as.integer(data$MRT)
data$DST<-as.integer(data$DST)

data$SBSOD<-as.numeric(data$SBSOD)
data$egocentric<-as.numeric(data$egocentric)
data$procedural<-as.numeric(data$procedural)
data$NSQT<-as.numeric(data$NSQT)
data$survey<-as.numeric(data$survey)

data$RR<-as.integer(data$RR)



###demographic effect#######
demographic<-glmer(success ~ sex + Game  + Age + drive + trial+ (0+trial|subject),data = data, family = "binomial",control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(demographic)

######VSWM MEASUERS#########

MRTfit<-glmer(success ~ landmark*MRT  + trial + (0+trial|subject), data = data, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(MRTfit)

ggplot(data,aes(y=success,x=MRT,color=landmark))+geom_point()+stat_smooth(method="lm",se=TRUE)

emmip(MRTfit, landmark~MMRT , at = list(MRT = 0 : 30), CIs= FALSE, CIarg = list(lwd = 2, alpha = 0.5), cov.reduce = FALSE, type="response", xlab = "MRT", ylab = "Success Rate Log-odd Scale")


PTSOTfit<-glmer(success ~ landmark*PTSOT  + trial + (0+trial|subject) , data = data, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(PTSOTfit)

emmip(PTSOTfit, landmark~PTSOT , at = list(PPTSOT = 0 : 100), CIs= FALSE, CIarg = list(lwd = 2, alpha = 0.5), cov.reduce = FALSE, type="response", xlab = "MRT", ylab = "Success Rate Log-odd Scale")


CSTfit<-glmer(success ~ landmark*CST + trial  + (0+trial|subject), data = data, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(CSTfit)
CSTfitlsmeans <- lsmeans(CSTfit, pairwise~landmark|CST,adjust="tukey")
CSTfitlsmeans

DSTfit <- glmer(success ~ landmark*DST + trial   + (0+trial|subject), data = data, family = "binomial")
summary(DSTfit)

################## Questionnairs ######
egofit<-glmer(success ~ landmark*egocentric + trial + (0+trial|subject) , data = data, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(egofit)
egofitlsmeans <- lsmeans(egofit, pairwise ~ landmark|egocentric,adjust="tukey")
egofitlsmeans

surveyfit<-glmer(success ~ landmark* survey + trial+   (0+trial|subject) , data = data, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(surveyfit)
surveyfitlsmeans <- lsmeans(surveyfit, pairwise~landmark|survey,adjust="tukey")


profit<-glmer(success ~ landmark* procedural + trial  + (0+trial|subject), data = data, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(profit)
profitlsmeans <- lsmeans(profit, pairwise~landmark|procedural,adjust="tukey")
profitlsmeans


SBSODfit<-glmer(success ~ landmark*SBSOD   + trial + (0+trial|subject), data = data, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(SBSODfit)
profitlsmeans <- lsmeans(SBSODfit, pairwise~landmark|SBSOD,adjust="tukey")


 ########


exp(confint.merMod(MRTfit,method="Wald"))
exp(confint.merMod(PTSOTfit,method="Wald"))
exp(confint.merMod(CSTfit,method="Wald"))
exp(confint.merMod(egofit,method="Wald"))
exp(confint.merMod(SBSODfit,method="Wald"))
exp(confint.merMod(profit,method="Wald"))
exp(fixef(MRTfit))## odds ratios
exp(fixef(PTSOTfit))## odds ratios
exp(fixef(CSTfit))## odds ratios
exp(fixef(egofit))## odds ratios
exp(fixef(SBSODfit))## odds ratios
exp(fixef(profit))## odds ratios

########################################



