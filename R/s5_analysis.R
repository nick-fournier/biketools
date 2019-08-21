library(data.table)
library(lme4)
library(glmnet)
library(MASS)
library(lmerTest)

#Load data
load('./data/ADBdata.RData')
load('./data/censusdata.RData')

#dataset
mergeddata <- merge(ADB.station,stations, by = "station_id")

#checking for NA
mergeddata <- mergeddata[ , lapply(.SD, function(x) ifelse(is.na(x),0,x))]
mergeddata <- mergeddata[alpha<1,]
#mergeddata[ , alpha := log(alpha)]
mergeddata[ , alpha := -alpha]
#Columns to analyze
allcols <- c("alpha",colnames(stations)[-grepl("station_id",colnames(stations))])

#### LASSO analysis
#y~x
y.vec <- mergeddata$alpha
x.mat <- as.matrix(mergeddata[ , allcols[-grepl("alpha",allcols)], with=F])

#LASSO
fit.lasso <- glmnet(x.mat, y.vec)
fit.cv <- cv.glmnet(x.mat,y.vec)
plot(fit.cv)

#selecting variables based on largest value of lambda such that error is within 1 standard error of the minimum.
#Or just the minimum lambda
varcols <- coef(fit.lasso, s = fit.cv$lambda.min)
#varcols <- coef(fit.lasso, s = fit.cv$lambda.1se)
varcols <- as.matrix(varcols)[-1,1]

#Keeping the names only
varcols <- names(varcols[abs(varcols)>0])

#manually
varcols <- varcols[!(varcols %in% c("LU_Open","LU_Recreation"))]

#### Stepwise regression
fit.lm <- lm(alpha~., data=mergeddata[,c("alpha",varcols), with=F])
step <- stepAIC(fit.lm, direction="both")
#step$anova # display results

fit.lm <- lm(alpha~. , data=step$model)
fit.glm <- glm(alpha~. , data=step$model)
#fit.mle <- lmer(paste("alpha~",paste(c(colnames(step$model)[-1],"(1|municipality)"),collapse = "+"),sep=""),data=mergeddata, REML=F)


summary(fit.glm)
summary(fit.lm)
#summary(fit.mle)

#anova(fit.glm)
#anova(fit.mle)

rm(list=ls())




