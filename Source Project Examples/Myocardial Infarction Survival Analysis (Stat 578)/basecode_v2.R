# Setup
library(readxl)
library(knitr)
library(tidyverse)
library(kableExtra)
library(survival)
library(survminer)
library(VIM)
library(missForest)
library(writexl)

df = data.frame(read_excel("df.xlsx"))    #importing the dataset
df[df=="?"] = " "    #removing all "?'s" with blanks. This prepares the dataset for imputation.
df.new = data.frame(read_excel("df.new.xlsx")) 

# Dataset: Variable Summary (Appendix)
df.sum <- data.frame(read_excel("df.sum.xlsx"))
df.sum

# Table of the data
df <- data.frame(read_excel("df.xlsx"))
df

# Dataset Imputation
set.seed(7522) 
df.i = missForest(df, maxiter = 30, ntree = 1000)
df.i$ximp #quick check of imputed values
df.i$OOBerror #this is the normalized mean squared error. We will compared this with the next step

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x}

df.impute = round_df(df.i$ximp,2) #imputed values table
df.new = df.impute[,c(-5,-12)] #remove incomplete strata from original data
Age.s = ifelse(df.impute$Age < 63,0,1) #new age strata based on imputed data #new age strata based on imputed data
WMS.s = ifelse(df.impute$WMS < 14,0,1) #new WMS strata based on imputed data
Fshort.s = ifelse(df.impute$F.Shortening < 0.2,0,1) #new fshort strata based on imputed data
LVDD.s = ifelse(df.impute$LVDD < 4.75,0,1) #new lvdd strata based on imputed data
EPSS.s = ifelse(df.impute$EPSS < 11.1,0,1)#new epss strata based on imputed data


df.new$Age.s = Age.s
df.new$WMS.s = WMS.s
df.new$F.Short.s = Fshort.s
df.new$LVDD.s = LVDD.s
df.new$EPSS.s = EPSS.s

s.df = Surv(df.new$Survival,df.new$Status)

write_xlsx(df.new,"df.new.xlsx")
write.csv(df.new,"df.new.csv")

s.df = Surv(df.new$Survival,df.new$Status)

# Kaplan-Meier Plots
km.plots = list()

km.all = survfit(s.df~1,type="kaplan-meier", data=df.new)
km.plots[[1]] = ggsurvplot(km.all, 
                           palette = "#2E9FDF", 
                           conf.int = TRUE, 
                           title="Post-Myocardial Infarction Survival", 
                           subtitle="Survival Among All Groups",
                           font.title=c(14,"bold.italic"),
                           font.subtitle = c(10,"italic"),
                           font.x = c(9, "bold.italic"),
                           font.y = c(9, "bold.italic"),
                           ylab="Surival Proportion", 
                           xlab="Time to Death (Months)",
                           surv.median.line = "hv",
                           legend.title = "Groups",
                           legend.labs = "All")


km.age = survfit(s.df~Age.s, type="kaplan-meier", data = df.new)
km.plots[[2]] = ggsurvplot(km.age, 
                           palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
                           subtitle="Survival, Stratified by Age Group",
                           font.subtitle = c(10,"italic"),
                           font.x = c(9, "bold.italic"),
                           font.y = c(9, "bold.italic"),
                           ylab="Surival Proportion", 
                           xlab="Time to Death (Months)",
                           surv.median.line = "hv",
                           legend.title = "Groups",
                           legend.labs = c("< 63 Years","\u2265 63 Years"))

km.effusion = survfit(s.df~P.Effusion, type="kaplan-meier", data = df.new)
km.plots[[3]] = ggsurvplot(km.effusion, 
                           palette = c("darkcyan","darkgoldenrod3"), 
                           subtitle="Survival, Stratified by Presence of Pericardial Effusion",
                           font.subtitle = c(10,"italic"),
                           font.x = c(9, "bold.italic"),
                           font.y = c(9, "bold.italic"),
                           ylab="Surival Proportion", 
                           xlab="Time to Death (Months)",
                           surv.median.line = "hv",
                           legend.title = "Groups",
                           legend.labs = c("Present","Absent"))

km.wms = survfit(s.df~WMS.s, type="kaplan-meier", data = df.new)
km.plots[[4]] = ggsurvplot(km.wms, 
                           palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
                           subtitle="Survival, Stratified by Wall Motion Score",
                           font.subtitle = c(10,"italic"),
                           font.x = c(9, "bold.italic"),
                           font.y = c(9, "bold.italic"),
                           ylab="Surival Proportion", 
                           xlab="Time to Death (Months)",
                           surv.median.line = "hv",
                           legend.title = "Groups",
                           legend.labs = c("< 14","\u2265 14"))

km.fshort = survfit(s.df~Fshort.s, type="kaplan-meier", data = df.new)
km.plots[[5]] = ggsurvplot(km.fshort, 
                           palette = c("darkcyan","darkgoldenrod3"), 
                           subtitle="Survival, Stratified by Fractal Shortening",
                           font.subtitle = c(10,"italic"),
                           font.x = c(9, "bold.italic"),
                           font.y = c(9, "bold.italic"),
                           ylab="Surival Proportion", 
                           xlab="Time to Death (Months)",
                           surv.median.line = "hv",
                           legend.title = "Groups",
                           legend.labs = c("< 0.2","\u2265 0.2"))

km.lvdd = survfit(s.df~LVDD.s, type="kaplan-meier", data = df.new)
km.plots[[6]] = ggsurvplot(km.lvdd, 
                           palette = c("darkcyan","darkgoldenrod3"), 
                           subtitle="Survival, Stratified by LVDD",
                           font.subtitle = c(10,"italic"),
                           font.x = c(9, "bold.italic"),
                           font.y = c(9, "bold.italic"),
                           ylab="Surival Proportion", 
                           xlab="Time to Death (Months)",
                           surv.median.line = "hv",
                           legend.title = "Groups",
                           legend.labs = c("< 4.75","\u2265 4.75"))

km.epss = survfit(s.df~EPSS.s, type="kaplan-meier", data = df.new)
km.plots[[7]] = ggsurvplot(km.epss, 
                           palette = c("darkcyan","darkgoldenrod3"), 
                           subtitle="Survival, Stratified by EPSS",
                           font.subtitle = c(10,"italic"),
                           font.x = c(9, "bold.italic"),
                           font.y = c(9, "bold.italic"),
                           ylab="Surival Proportion", 
                           xlab="Time to Death (Months)",
                           surv.median.line = "hv",
                           legend.title = "Groups",
                           legend.labs = c("< 11","\u2265 11"))


arrange_ggsurvplots(km.plots, print=TRUE, ncol=2, nrow=2)

# Kaplan-Meier Survival Summary
ks1 = data.frame(t(summary(km.all)$table))
ks2 = data.frame(summary(km.age)$table)
ks3 = data.frame(summary(km.effusion)$table)
ks4 = data.frame(summary(km.wms)$table)
ks5 = data.frame(summary(km.fshort)$table)

ksall = rbind(ks1, ks2, ks3, ks4,ks5)
km.as = ksall[,c(1,4,5,7,8,9)]
colnames(km.as) = c("Records","Events","Mean","Median","Median 0.95 LCL","Median 0.95 UCL")
rownames(km.as) = c("All","Age:<55", "Age: 55-65", "Age: >65", "P.Eff: Absent","P.Eff: Present", "WMS: < 14", "WMS: \u2265 14","Fractal Shortening:", "Fractal Shortening: ")

kable(km.as, caption="Kaplan-Meier Results",align="c", digits=2) %>%
  kable_styling(position = "center", latex_options="hold_position")

# Kaplan-Meier Cumulative Hazard Estimators
haz.plot = list()

haz.plots[[1]] = ggsurvplot(km.all, 
                            fun = "cumhaz",
                            palette = "#2E9FDF", 
                            conf.int = TRUE, 
                            title="Post-Myocardial Infarction Hazard", 
                            subtitle="Hazard Among All Groups",
                            font.title=c(14,"bold.italic"),
                            font.subtitle = c(10,"italic"),
                            font.x = c(9, "bold.italic"),
                            font.y = c(9, "bold.italic"),
                            ylab="Surival Proportion", 
                            xlab="Time to Death (Months)",
                            legend.title = "Groups",
                            legend.labs = "All")

haz.plots[[2]] = ggsurvplot(km.age, 
                            fun = "cumhaz",
                            palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
                            subtitle="Hazard, Stratified by Age Group",
                            font.subtitle = c(10,"italic"),
                            font.x = c(9, "bold.italic"),
                            font.y = c(9, "bold.italic"),
                            ylab="Surival Proportion", 
                            xlab="Time to Death (Months)",
                            legend.title = "Groups",
                            legend.labs = c("< 63 Years","\u2265 63 Years"))

haz.plots[[3]] = ggsurvplot(km.effusion, 
                            fun = "cumhaz",
                            palette = c("darkcyan","darkgoldenrod3"), 
                            subtitle="Hazard, Stratified by Pericardial Effusion Presence",
                            font.subtitle = c(10,"italic"),
                            font.x = c(9, "bold.italic"),
                            font.y = c(9, "bold.italic"),
                            ylab="Surival Proportion", 
                            xlab="Time to Death (Months)",
                            legend.title = "Groups",
                            legend.labs = c("Present","Absent"))

haz.plots[[4]] = ggsurvplot(km.wms, 
                            fun = "cumhaz",
                            palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
                            subtitle="Hazard, Stratified by Wall Motion Score",
                            font.subtitle = c(10,"italic"),
                            font.x = c(9, "bold.italic"),
                            font.y = c(9, "bold.italic"),
                            ylab="Surival Proportion", 
                            xlab="Time to Death (Months)",
                            legend.title = "Groups",
                            legend.labs = c("< 14","\u2265 14"))

haz.plots[[5]] = ggsurvplot(km.fshort, 
                            fun = "cumhaz",
                            palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
                            subtitle="Hazard, Stratified by Fractal Shortening",
                            font.subtitle = c(10,"italic"),
                            font.x = c(9, "bold.italic"),
                            font.y = c(9, "bold.italic"),
                            ylab="Surival Proportion", 
                            xlab="Time to Death (Months)",
                            legend.title = "Groups",
                            legend.labs = c("< 0.2","\u2265 0.2"))

haz.plots[[6]] = ggsurvplot(km.epss, 
                            fun = "cumhaz",
                            palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
                            subtitle="Hazard, Stratified by EPSS",
                            font.subtitle = c(10,"italic"),
                            font.x = c(9, "bold.italic"),
                            font.y = c(9, "bold.italic"),
                            ylab="Surival Proportion", 
                            xlab="Time to Death (Months)",
                            legend.title = "Groups",
                            legend.labs = c("< 4.75","\u2265 4.75"))

haz.plots[[7]] = ggsurvplot(km.lvdd, 
                            fun = "cumhaz",
                            palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
                            subtitle="Hazard, Stratified by LVDD",
                            font.subtitle = c(10,"italic"),
                            font.x = c(9, "bold.italic"),
                            font.y = c(9, "bold.italic"),
                            ylab="Surival Proportion", 
                            xlab="Time to Death (Months)",
                            legend.title = "Groups",
                            legend.labs = c("< 11","\u2265 11"))


arrange_ggsurvplots(haz.plots, print=TRUE, ncol=2, nrow=2)

## Parametric Modeling ##

#Load libraries

library(tidyverse)
library(dplyr)
library(kableExtra)
library(survival)
library(survminer)

#Data preparation for model fits

months=df.new$Survival
status=df.new$Status
months.u=months[status == 1]
months.u = sort(months.u)
nu = length(months.u)

#Weibull model fit

weib.fit=survreg(Surv(months,status)~1,dist="weib")
alphahat=1/weib.fit$scale
scalehat=exp(weib.fit$coefficients)

#Point and 95% CI estimates for Weibull

medhat25.w = predict(weib.fit,type="uquantile",p=0.25,se.fit=T)
medhat25.1.w = medhat25.w$fit[1]
medhat25.1.se.w = medhat25.w$se.fit[1]
C.I.median25.w = c(exp(medhat25.1.w),exp(medhat25.1.w-1.96*medhat25.1.se.w),exp(medhat25.1.w+1.96*medhat25.1.se.w))
names(C.I.median25.w) = c("median_25_w","LCL","UCL")

medhat5.w = predict(weib.fit,type="uquantile",p=0.5,se.fit=T)
medhat5.1.w = medhat5.w$fit[1]
medhat5.1.se.w = medhat5.w$se.fit[1]
C.I.median5.w = c(exp(medhat5.1.w),exp(medhat5.1.w-1.96*medhat5.1.se.w),exp(medhat5.1.w+1.96*medhat5.1.se.w))
names(C.I.median5.w) = c("median_50_w","LCL","UCL")

medhat75.w = predict(weib.fit,type="uquantile",p=0.75,se.fit=T)
medhat75.1.w = medhat75.w$fit[1]
medhat75.1.se.w = medhat75.w$se.fit[1]
C.I.median75.w = c(exp(medhat75.1.w),exp(medhat75.1.w-1.96*medhat75.1.se.w),exp(medhat75.1.w+1.96*medhat75.1.se.w))
names(C.I.median75.w) = c("median_75_w","LCL","UCL")

C.I.median25.w
C.I.median5.w
C.I.median75.w

#Point estimates S(t) for Weibull

Shat.w = 1- pweibull(months.u,alphahat,scalehat)
C.I.Shat.w = data.frame(months.u,Shat.w)
round(C.I.Shat.w,5)

#Overlay K-M and Weibull survival curves

plot(km.all,conf.int=F,xlab="time until death (in months)",
     ylab="proportion survived",
     main= "Survival Curves - Weibull and Kaplan-Meier",
     lwd=2,
     col = "darkcyan")
lines(months.u, Shat.w, col="darkgoldenrod3",lwd=2)
legend(40, 0.8, legend=c("Kaplan-Meier", "Weibull"),
       col=c("darkcyan","darkgoldenrod3"), lty=1:1, cex=0.8,lwd=2)
abline(h=0)

#Log-normal model fit

lognorm.fit=survreg(Surv(months,status)~1,dist="lognormal")
muhat=lognorm.fit$coefficients
sigmahat=lognorm.fit$scale

#Point and 95% CI estimates for quantiles

medhat25.l = predict(lognorm.fit,type="uquantile",p=0.25,se.fit=T)
medhat25.1.l = medhat25.l$fit[1]
medhat25.1.se.l = medhat25.l$se.fit[1]
C.I.median25.l = c(exp(medhat25.1.l),exp(medhat25.1.l-1.96*medhat25.1.se.l),exp(medhat25.1.l+1.96*medhat25.1.se.l))
names(C.I.median25.l) = c("median_25_l","LCL","UCL")

medhat5.l = predict(lognorm.fit,type="uquantile",p=0.5,se.fit=T)
medhat5.1.l = medhat5.l$fit[1]
medhat5.1.se.l = medhat5.l$se.fit[1]
C.I.median5.l = c(exp(medhat5.1.l),exp(medhat5.1.l-1.96*medhat5.1.se.l),exp(medhat5.1.l+1.96*medhat5.1.se.l))
names(C.I.median5.l) = c("median_50_l","LCL","UCL")

medhat75.l = predict(lognorm.fit,type="uquantile",p=0.75,se.fit=T)
medhat75.1.l = medhat75.l$fit[1]
medhat75.1.se.l = medhat75.l$se.fit[1]
C.I.median75.l = c(exp(medhat75.1.l),exp(medhat75.1.l-1.96*medhat75.1.se.l),exp(medhat75.1.l+1.96*medhat75.1.se.l))
names(C.I.median75.l) = c("median_75_l","LCL","UCL")

C.I.median25.l
C.I.median5.l
C.I.median75.l

#Point estimates S(t) log-normal fit

Shat.l = 1- pnorm(log(months.u),muhat,sigmahat)
C.I.Shat.l = data.frame(months.u,Shat.l)
round(C.I.Shat.l,5)

#K-M and log-normal survival curve overlay

plot(km.all,conf.int=F,xlab="time until death (in months)",
     ylab="proportion survived",
     main="Survival Curves - Log-normal and Kaplan-Meier",
     lwd=2,
     col="darkcyan")
lines(months.u, Shat.l, col="darkgoldenrod3",lwd=2)
legend(40, 0.8, legend=c("Kaplain", "Weibull"), lwd=2,
       col=c("darkcyan","darkgoldenrod3"), lty=1:1, cex=0.8)
abline(h=0)

#Log-logistic Model Fit

#Log-logistical model 

loglog.fit=survreg(Surv(months,status)~1,dist="loglogistic")
muhat=loglog.fit$coefficients
sigmahat=loglog.fit$scale

#Point and 95% CI estimates for quantiles - Log-logistic fit

medhat25.ll = predict(loglog.fit,type="uquantile",p=0.25,se.fit=T)
medhat25.1.ll = medhat25.ll$fit[1]
medhat25.1.se.ll = medhat25.ll$se.fit[1]
C.I.median25.ll = c(exp(medhat25.1.ll),exp(medhat25.1.ll-1.96*medhat25.1.se.ll),exp(medhat25.1.ll+1.96*medhat25.1.se.ll))
names(C.I.median25.ll) = c("median_25_ll","LCL","UCL")

medhat5.ll = predict(loglog.fit,type="uquantile",p=0.5,se.fit=T)
medhat5.1.ll = medhat5.ll$fit[1]
medhat5.1.se.ll = medhat5.ll$se.fit[1]
C.I.median5.ll = c(exp(medhat5.1.ll),exp(medhat5.1.ll-1.96*medhat5.1.se.ll),exp(medhat5.1.ll+1.96*medhat5.1.se.ll))
names(C.I.median5.ll) = c("median_50_ll","LCL","UCL")

medhat75.ll = predict(loglog.fit,type="uquantile",p=0.75,se.fit=T)
medhat75.1.ll = medhat75.ll$fit[1]
medhat75.1.se.ll = medhat75.ll$se.fit[1]
C.I.median75.ll = c(exp(medhat75.1.ll),exp(medhat75.1.ll-1.96*medhat75.1.se.ll),exp(medhat75.1.ll+1.96*medhat75.1.se.ll))
names(C.I.median75.ll) = c("median_75_ll","LCL","UCL")

C.I.median25.ll
C.I.median5.ll
C.I.median75.ll

#Point estimates S(t) - log-logistic

Shat.ll = 1- plogis(log(months.u),muhat,sigmahat)
C.I.Shat.ll = data.frame(months.u,Shat.ll)
round(C.I.Shat.ll,5)

#K-M and log-logistic survival curves overlaid

plot(km.all,conf.int=F,xlab="time until death (in months)",
     ylab="proportion survived",
     main="Survival Curves - Log-logistic and Kaplan-Meier",
     lwd=2,
     col="darkcyan")
lines(months.u, Shat.ll, col="darkgoldenrod3",lwd=2,)
legend(40, 0.8, legend=c("Kaplan", "Weibull"), lwd=2,
       col=c("darkcyan","darkgoldenrod3"), lty=1:1, cex=0.8)
abline(h=0)

#Q-Q Plots - Weibull, Log-lognormal, Log-logistic
#qq.surv function: 
#Author: Jong Sung Kim, Date: 8/10/2004
# Edited by D. Leif Rustvold, Date: 6/7/2006

qq.surv <- function(time, status, pdgy = 0, distribution = "weibull", scale = 0, adjpb = 
                      0.025, ...)
{
  ## Purpose: qqplot for distributions that satisfy a log-linear form
  ## for one sample.  It fits each sample with own intercept and slope 
  ## (location and scale).
  ##-------------------------------------------------------------------
  ## Arguments
  ## =========
  ## time:   observed time
  ## status: censoring indicator
  ##
  ## Options
  ## =======
  ## pdgy:   Flag to generate for pedagogical purposes additional lines
  ##         incorporating the effect of how we treat censored
  ##         observations on the MLE's (equivalently estimated line).
  ##         pdgy=0 is the default, for no additional lines. 
  ##         pdgy=1 generates additional lines.
  ## distribution:  Distribution for fit.
  ##         May take values "weibull", "loglogistic", or "lognormal".
  ##         The default is "weibull" distribution (exponential model with
  ##         scale=1).  Enter "loglogistic" to fit loglogistic distribution;
  ##         Enter "lognormal" to fit lognormal distribution. 
  ## scale:  Scale parameter.  scale=0 is the default. This estimates 
  ##         the scale. With distribution "weibull", scale=1 fits the 
  ##         exponential model. 
  ## adjpb:  Replaces the zero survival probability when the max is exact.
  ##         Or when the min is censored, it replaces the survival 
  ##         probability by 1 - adjpb.  Default is 0.025. 
  ##         This has nothing to do with the MLE line, but is solely for 
  ##         plotting the point on the graph.
  ##-------------------------------------------------------------------
  ## Author: Jong Sung Kim, Date: 8/10/2004
  ## Edited by D. Leif Rustvold, Date: 6/7/2006
  d <- data.frame(time, status)
  # data frame 
  d <- na.exclude(d)
  # Missing observations excluded
  d <- d[order(d$time),  ]
  # Rearranging the observed times into a nondecreasing order
  # Unordered times sometimes mess up QQ-plots. 
  time <- d$time
  # sorted time
  status <- d$status
  # status corresponding to sorted time
  data <- Surv(time, status)
  # Surv object
  t.c <- class(data)
  if((!is.null(t.c)) && t.c == "Surv")
    data <- list(data)
  t.s <- summary(survfit(Surv(time, status)~1, type = "kaplan-meier",
                         na.action = na.exclude))
  survp <- t.s$surv
  survtime <- t.s$time
  rare <- F
  # rare = T indicates that the smallest observation is censored
  if(time[1] < survtime[1]) {
    print("Smallest observation is censored!")
    survp <- c(1 - adjpb, survp)
    survtime <- c(time[1], survtime)
    rare <- T
  }
  ############
  ############
  xlabs <- ifelse(distribution == "weibull", 
                  "Standard Extreme Value Quantiles", ifelse(distribution == 
                                                               "loglogistic", "Standard Log-logistic Quantiles", ifelse(
                                                                 distribution == "lognormal", "Standard Lognormal Quantiles",
                                                                 "")))
  if(pdgy == 1) {
    ###############
    t.s.exactall <- summary(survfit(Surv(time, status >= 0)~1, type
                                    = "kaplan-meier", na.action = na.exclude))
    exactall.survp <- t.s.exactall$surv
    exactall.survtime <- t.s.exactall$time
    exactall.length <- length(exactall.survtime)
    exactall.survp[exactall.length] <- adjpb
    t.ss.exactall <- exactall.survp
    #quant.exactall <- qweibull(1 - t.ss.exactall, 1)
    quant.exactall <- switch(distribution,
                             weibull = qweibull(1 - t.ss.exactall, 1),
                             lognormal = qlnorm(1 - t.ss.exactall),
                             loglogistic = exp(logis((1 - t.ss.exactall))))
    exactall.sevq <- log(quant.exactall)
    # standard extreme value quantile
    exactall.logtime <- log(exactall.survtime)
    print(data.frame(exactall.logtime, exactall.sevq))
    ############### 
    ok <- status == 1
    t.s.exact <- summary(survfit(Surv(time[ok], status[ok])~1, type
                                 = "kaplan-meier", na.action = na.exclude))
    exact.survp <- t.s.exact$surv
    exact.survtime <- t.s.exact$time
    exact.length <- length(exact.survtime)
    exact.survp[exact.length] <- adjpb
    t.ss.exact <- exact.survp
    #quant.exact <- qweibull(1 - t.ss.exact, 1)
    quant.exact <- switch(distribution,
                          weibull = qweibull(1 - t.ss.exact, 1),
                          lognormal = qlnorm(1 - t.ss.exact),
                          loglogistic = exp(qlogis(1 - t.ss.exact)))
    exact.sevq <- log(quant.exact)
    # standard extreme value quantile
    exact.logtime <- log(exact.survtime)
    print(data.frame(exact.logtime, exact.sevq))
    ###############
    n <- length(time)
    t.ss <- rep(0, n)
    for(i in 1:n) {
      # This loop assigns probabilities to censored time points, 
      # and takes care of tied observations as well
      idx <- time[i] >= survtime
      t.ss[i] <- min(survp[idx], na.rm = T)
    }
    #sevq <- log(qweibull(1 - t.ss, 1))
    sevq <- log(switch(distribution,
                       weibull = qweibull(1 - t.ss, 1),
                       lognormal = qlnorm(1 - t.ss),
                       loglogistic = exp(qlogis(1 - t.ss))))
    # standard extreme value quantile
    logtime <- log(time)
    print(data.frame(logtime, sevq))
    ######## Multiple Plot starts ##########
    xrange <- range(c(exactall.sevq, exact.sevq, sevq))
    yrange <- range(c(exactall.logtime, exact.logtime, logtime))
    par(mar = c(5, 5, 2, 2))
    plot(sevq, logtime, type = "n", lty = 1, xlim = xrange, ylim
         = yrange, xlab = xlabs, ylab = "Ordered Log Time",
         ...)
    points(sevq[ok], logtime[ok], pch = 1)
    # exact points portion
    points(sevq[!ok], logtime[!ok], pch = "\255", font = 8)
    # censored points portion
    points(exactall.sevq, exactall.logtime, pch = 3, col = 6)
    # exactall
    exactallfit <- survreg(Surv(time, status >= 0) ~ 1, dist = 
                             distribution, scale = scale)
    # treating censored as exac
    t
    abline(exactallfit$coef, exactallfit$scale, lty = 3, col = 6)
    points(exact.sevq, exact.logtime, pch = 5, col = 5)
    # exact points only
    exactonlyfit <- survreg(Surv(time[ok], status[ok]) ~ 1, dist
                            = distribution, scale = scale)
    # deleting censored
    abline(exactonlyfit$coef, exactonlyfit$scale, lty = 2, col = 5
    )
    fit <- survreg(Surv(time, status) ~ 1, dist = "weibull", scale
                   = scale)
    # censoring taken into account
    abline(fit$coef, fit$scale, lty = 1, col = 1)
  }
  else {
    n <- length(time)
    t.ss <- rep(0, n)
    for(i in 1:n) {
      # This loop assigns probabilities to censored time points, 
      # and takes care of tied observations as well
      idx <- time[i] >= survtime
      t.ss[i] <- min(survp[idx], na.rm = T)
    }
    #sevq <- log(qweibull(1 - t.ss, 1))
    sevq <- log(switch(distribution,
                       weibull = qweibull(1 - t.ss, 1),
                       lognormal = qlnorm(1 - t.ss),
                       loglogistic = exp(qlogis(1 - t.ss))))
    # standard extreme value quantile
    logtime <- log(time)
    print(data.frame(logtime, sevq))
    par(mar = c(5, 5, 2, 2))
    plot(sevq, logtime, type = "n", xlab = xlabs, ylab = 
           "Ordered Log Time", ...)
    ok <- status == 1
    # exact status only 
    points(sevq[ok], logtime[ok], pch = 1)
    # exact points only
    points(sevq[!ok], logtime[!ok], pch = "\255", font = 8)
    # censored points only
    fit <- survreg(Surv(time, status) ~ 1, dist = distribution,
                   scale = scale)
    # censoring taken into account
    abline(fit$coef, fit$scale, lty = 1, col = 1)
  }
  ymax <- max(logtime)
  yrange <- diff(range(logtime))
  yn <- ymax - yrange * seq(0, by = 0.05, length = 5)
  if(pdgy == 1) {
    xmin <- min(c(sevq, exact.sevq, exactall.sevq))
    xrange <- diff(range(c(sevq, exact.sevq, exactall.sevq)))
  }
  else {
    xmin <- min(sevq)
    xrange <- diff(range(sevq))
  }
  x1 <- xmin + 0.05 * xrange
  x2 <- xmin + 0.1 * xrange
  x3 <- xmin + 0.15 * xrange
  points(x1, yn[1], pch = "\255", font = 8)
  text(x3, yn[1], "censored", adj = 0)
  points(x1, yn[2], pch = 1)
  text(x3, yn[2], "exact", adj = 0)
  if(pdgy == 1) {
    lines(c(x1, x2), rep(yn[3], 2), lty = 1, col = 1, lwd = 3)
    text(x3, yn[3], "censoring taken into account", adj = 0)
    lines(c(x1, x2), rep(yn[4], 2), lty = 3, col = 6, lwd = 3)
    text(x3, yn[4], "treating censored as exact", adj = 0)
    lines(c(x1, x2), rep(yn[5], 2), lty = 2, col = 5, lwd = 3)
    text(x3, yn[5], "deleting censored", adj = 0)
  }
  on.exit()
  paste("Q-Q plot for", distribution, "done")
}

qq.surv(months, status, distribution = "weibull",
        main="Q-Q plot - Weibull fit")
qq.surv(months, status, distribution = "lognormal",
        main="Q-Q plot - Log-normal fit")
qq.surv(months, status, distribution = "loglogistic",
        main="Q-Q plot - Log-logistic fit")

## Semi-Parametric Modeling (CoxPH) ##

#Load libraries

library(survminer)
library(MASS)
library(survival)
library(SurvCorr)

#Initial data evaluation; inspect for multicollinearity in covariates

df=data.frame(df.new$P.Effusion,df.new$Age,df.new$F.Shortening,df.new$EPSS,df.new$LVDD,df.new$WMS)
pairs(df) 
#We exclude EPSS and LVDD as a result of this inspection. 

#Hazard plots for regression covariates with survival time subsets identified

p= ggsurvplot(km.age, 
              fun = "cumhaz",
              palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
              subtitle="Hazard, Stratified by Age Group",
              font.subtitle = c(10,"italic"),
              font.x = c(9, "bold.italic"),
              font.y = c(9, "bold.italic"),
              ylab="Surival Proportion", 
              xlab="Time to Death (Months)",
              legend.title = "Groups",
              legend.labs = c("Age < 63","Age => 63")
)
p$plot + geom_vline(xintercept=26)+
  geom_vline(xintercept=46)

p = ggsurvplot(km.effusion, 
               fun = "cumhaz",
               palette = c("darkcyan","darkgoldenrod3"), 
               subtitle="Hazard, Stratified by Pericardial Effusion Presence",
               font.subtitle = c(10,"italic"),
               font.x = c(9, "bold.italic"),
               font.y = c(9, "bold.italic"),
               ylab="Surival Proportion", 
               xlab="Time to Death (Months)",
               legend.title = "Groups",
               legend.labs = c("Present","Absent"))
p$plot + geom_vline(xintercept=26)+
  geom_vline(xintercept=46)

p = ggsurvplot(km.wms, 
               fun = "cumhaz",
               palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
               subtitle="Hazard, Stratified by Wall Motion Index",
               font.subtitle = c(10,"italic"),
               font.x = c(9, "bold.italic"),
               font.y = c(9, "bold.italic"),
               ylab="Surival Proportion", 
               xlab="Time to Death (Months)")
p$plot + geom_vline(xintercept=26)+
  geom_vline(xintercept=46)


p = ggsurvplot(km.fshort, 
               fun = "cumhaz",
               palette = c("darkcyan","darkgoldenrod3","darkorange3"), 
               subtitle="Hazard, Stratified by Fractional Shortening",
               font.subtitle = c(10,"italic"),
               font.x = c(9, "bold.italic"),
               font.y = c(9, "bold.italic"),
               ylab="Surival Proportion", 
               xlab="Time to Death (Months)"
)
p$plot + geom_vline(xintercept=26)+
  geom_vline(xintercept=46)


#Model for first survival time subset

#Set time region for model

LL=0.0
UL=26.0

#Subset the data based on the time region 

months=df.new$Survival[df.new$Survival>=LL & df.new$Survival<=UL]
status=df.new$Status[df.new$Survival>=LL & df.new$Survival<=UL]
Age=df.new$Age[df.new$Survival>=LL & df.new$Survival<=UL]
P.Eff=df.new$P.Effusion[df.new$Survival>=LL & df.new$Survival<=UL]
W.MS=df.new$WMS[df.new$Survival>=LL & df.new$Survival<=UL]
F.Short=df.new$F.Shortening[df.new$Survival>=LL & df.new$Survival<=UL]

#Create initial model fit

cph.fit1=coxph(Surv(months,status)~Age+P.Eff+W.MS+F.Short,x=T)
summary(cph.fit1)

#Reduce with StepAIC procedure

cph.fit2=stepAIC(cph.fit1,~.^2,direction="both")
summary(cph.fit2)
mod1=cph.fit2

#Perform Likelihood Ratio Test

lrt=-2*cph.fit2$loglik[2]+2*cph.fit1$loglik[2]
varstartcount=10
varendcount=3
vars=varstartcount - varendcount
1-pchisq(lrt,vars)

#Note: selects the reduced model (p-val>0.05)

#Cox-Snell residual analysis for overall model fit

rc=abs(status - cph.fit2$residuals)
km.rc = survfit(Surv(rc,status)~1)
summary.km.rc=summary(km.rc)
rcu=summary.km.rc$time
surv.rc = summary.km.rc$surv
plot(rcu,-log(surv.rc),type="p",
     xlab="Cox-Snell residual rc",ylab="Cumulative hazard on rc",
     main="Cox-Snell residual model fit evaluation")
abline(a=0,b=1); abline(v=0); abline(h=0)

#Schoenfeld residuals; test for constant coefficients 

par(mfrow=c(1,1))
test.ph <- cox.zph(cph.fit2)
ggcoxzph(test.ph,font.y=8)

#Dfbeta detection of influential observations

ggcoxdiagnostics(cph.fit2, type = "dfbetas",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#Model for second survival time subset

#Set time region for model

LL=26
UL=46

#Subset the data based on the time region 

months=df.new$Survival[df.new$Survival>LL & df.new$Survival<=UL]
status=df.new$Status[df.new$Survival>LL & df.new$Survival<=UL]
Age=df.new$Age[df.new$Survival>LL & df.new$Survival<=UL]
P.Eff=df.new$P.Effusion[df.new$Survival>LL & df.new$Survival<=UL]
W.MS=df.new$WMS[df.new$Survival>LL & df.new$Survival<=UL]
F.Short=df.new$F.Shortening[df.new$Survival>LL & df.new$Survival<=UL]

#Create initial model fit

cph.fit1=coxph(Surv(months,status)~Age+P.Eff+W.MS+F.Short,x=T)
summary(cph.fit1)

#Reduce with StepAIC procedure

cph.fit2=stepAIC(cph.fit1,~.^2,direction="both")
summary(cph.fit2)
mod2=cph.fit2

#Perform Likelihood Ratio Test

lrt=-2*cph.fit2$loglik[2]+2*cph.fit1$loglik[2]
varstartcount=10
varendcount=1
vars=varstartcount - varendcount
1-pchisq(lrt,vars)

#Note: selects the reduced model (p-val>0.05)

#Cox-Snell residual analysis for overall model fit

rc=abs(status - cph.fit2$residuals)
km.rc = survfit(Surv(rc,status)~1)
summary.km.rc=summary(km.rc)
rcu=summary.km.rc$time
surv.rc = summary.km.rc$surv
plot(rcu,-log(surv.rc),type="p",
     xlab="Cox-Snell residual rc",ylab="Cumulative hazard on rc",
     main="Cox-Snell residual model fit evaluation")
abline(a=0,b=1); abline(v=0); abline(h=0)

#Schoenfeld residuals; test for constant coefficients 

test.ph <- cox.zph(cph.fit2)
print(test.ph)
ggcoxzph(test.ph)

#slight pattern with time but p>0.05. The assumption of proportional hazards appears to be supported for the covariates sex (which is, recall, a two-level factor, accounting for the two bands in the graph), wt.loss and age.

#Dfbeta detection of influential observations

ggcoxdiagnostics(cph.fit2, type = "dfbetas",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#Look up potentially influential observation 

residuals(cph.fit2,type="dfbetas")
cph.fit2$x 
#obs 8 x1=80 (AGE) is a potential influential observation. 

#Model for third survival time subset

#Subset the data based on the time region 

months=df.new$Survival[df.new$Survival>UL]
status=df.new$Status[df.new$Survival>UL]
Age=df.new$Age[df.new$Survival>UL]
P.Eff=df.new$P.Effusion[df.new$Survival>UL]
W.MS=df.new$WMS[df.new$Survival>UL]
F.Short=df.new$F.Shortening[df.new$Survival>UL]

#Create initial model fit

cph.fit1=coxph(Surv(months,status)~Age+P.Eff+W.MS+F.Short,x=T)
summary(cph.fit1)

#Reduce with StepAIC procedure

cph.fit2=stepAIC(cph.fit1,~.^2,direction="backward")
summary(cph.fit2)

mod3=cph.fit2

#Perform Likelihood Ratio Test

lrt=-2*cph.fit2$loglik[2]+2*cph.fit1$loglik[2]
varstartcount=10
varendcount=1
vars=varstartcount - varendcount
pval=1-pchisq(lrt,vars)

#Note: selects the reduced model (p-val>0.05)

#Cox-Snell residual analysis for overall model fit

rc=abs(status - cph.fit2$residuals)
km.rc = survfit(Surv(rc,status)~1)
summary.km.rc=summary(km.rc)
rcu=summary.km.rc$time
surv.rc = summary.km.rc$surv
plot(rcu,-log(surv.rc),type="p",
     xlab="Cox-Snell residual rc",ylab="Cumulative hazard on rc",
     main="Cox-Snell residual model fit evaluation")
abline(a=0,b=1); abline(v=0); abline(h=0)

#Schoenfeld residuals; test for constant coefficients 

test.ph <- cox.zph(cph.fit2)
ggcoxzph(test.ph)

#Dfbeta detection of influential observations

ggcoxdiagnostics(cph.fit2, type = "dfbetas",
                 linear.predictions = FALSE, ggtheme = theme_bw())

