install.packages("readxl")
library("readxl")
library(car)
library(perturb)
library(leaps)
library(faraway)
d2p <- read_excel('/Users/meganduff/Desktop/D2P.xlsx')

pctcom <- ((d2p$COMMUTE_60_PLUS+d2p$COMMUTE_45_TO_60+d2p$COMMUTE_30_TO_45)/d2p$TOTAL_COMMUTERS)*100

plot(density(pctcom), main = "Percentage of Citizens Commuting more than 30 mins a Day")
summary(d2p)

pctLHS <- (d2p$LESS_THAN_HS_DIPLOMA_EDU/d2p$TTLPOP_25PLUS_EDU)*100
pctHS <- (d2p$HSGRAD_OR_EQUIV_EDU/d2p$TTLPOP_25PLUS_EDU)*100
pctAA <- (d2p$SOMECOLLEGE_OR_AA_EDU/d2p$TTLPOP_25PLUS_EDU)*100
pctBA <- (d2p$BACHELORS_OR_HIGHER_EDU/d2p$TTLPOP_25PLUS_EDU)*100
pctrent <- (d2p$RENTER_OCCUPIED_HU/d2p$TTL_HOUSING_UNITS)*100
pcthome <- (d2p$OWNER_OCCUPIED_HU/d2p$TTL_HOUSING_UNITS)*100
pctfamHH <- (d2p$FAMILY_HOUSEHOLDS/d2p$TTL_HOUSEHOLDS)*100
pctmale <- (d2p$MALE/d2p$TTL_POPULATION_ALL)*100
pctfem <- (d2p$FEMALE/d2p$TTL_POPULATION_ALL)*100
medHHI <-d2p$MED_HH_INCOME
medFamI <- d2p$MED_FAMILY_INCOME
medrent <- d2p$MED_GROSS_RENT
medHV <- d2p$MEDIAN_HOME_VALUE
pctpov <-  d2p$PCT_POVERTY
pctfampov <- d2p$PCT_FAM_POVERTY
pctW <- d2p$PCT_WHITE
pctB <- d2p$PCT_BLACK
pctA <- d2p$PCT_ASIAN
pctNA <- d2p$PCT_NATIVEAM
pctHA <- d2p$PCT_HAWAIIANPI
pctHI <- d2p$PCT_HISPANIC
pctOR <- d2p$PCT_OTHERRACE
pct2M <- d2p$PCT_TWOORMORE_RACES
pctC <- d2p$PCT_CRIME*100
neigh <- d2p$NBHD_NAME



data <- data.frame(pctcom, pctLHS, pctHS, pctAA, pctBA, pctrent, pcthome,pctfamHH, pctmale, pctfem,medHHI,medFamI, medrent, medHV, pctpov, pctfampov, pctW,pctB,pctA, pctNA, pctHI, pctHA, pctOR, pct2M, pctC)
data.rowNamesDF <- neigh
data.rowNamesDF


data[1,]


##EXPLORATORY DATA ANALYSIS##
summary(data)
even<-seq(from=2, to=26, by=2)
odd<-seq(from=3,to=26,by=2)
scatterplotMatrix(data[even])
pairs(data[even])
pairs(data[odd])
data$medFamI[data$medFamI==0] <- NA
data$medrent[data$medrent==0] <- NA
data$medHV[data$medHV == 0] <- NA
data$medHV
plot(density(pctcom), main = "Density of Percentage of Residents Commuting an Hour or More a Day")
abline(v = mean(pctcom))
mean(pctcom)
max(pctcom)
##ASSESING FOR COLLINEARITY##
lmod <- lm(pctcom~.,data=data)
lmod
lmod <- update(lmod,.~.-pctfem-pctBA-pcthome)
#removed pctfem, pctBA, pcthome since coefficients gave NA
lmod
vif(lmod)
#VIF suggests pctLHS, pctW, pctB, pctHI could be removed 
colldiag(lmod)
lmod2 <- update(lmod,.~.-pctW)
vif(lmod2)
lmod3 <- update(lmod2,.~.-pctLHS)
vif(lmod3)
lmod4 <- update(lmod3,.~.-pctfampov)
vif(lmod4)
lmod5 <- update(lmod4,.~.-medHHI)
vif(lmod5)
colldiag(lmod5)

##VARIABLE SELECTION## 
b <- regsubsets(pctcom~.-pctfem-pctBA-pcthome-pctLHS-pctW-pctB-pctHI,data=data, nvmax = 17)
rs <- summary(b) 
rs
##AIC##
p = 2:18
aic = rs$bic + p * (2 - log(97))
plot(aic ~ p)
min(aic)
aic[7]
##Based on AIC regressors (6 regressor model):pctHS, pctrent, medFamI, medHV, medrent, pctA


##BIC## 
bic = rs$bic
plot(bic~p)
bic[4]
min(bic)
##Based on BIC regressors (4 regressor model):

##R^2##
adjr = rs$adjr
plot(adjr ~ p, ylab = expression({R^2}[a]))
subsets(b, statistic = "adjr2", legend = FALSE)
##Based on Adjusted R-Sqaured (10 regressor model): pctHS, pctrent, medFamI,
## medrent, medHV, pctpov, pctfampov, pctA, pctHA, pctC

##Mallows CP##
cp = rs$cp
plot(cp ~ p, ylab = expression(paste(C[p], " statistic")))
abline(0, 1)
subsets(b, statistic = "cp", legend = FALSE)
##16 regressor model (-pctOR)

#Comparing 4 models using Cross Validation
library(caret)
cv_10fold = trainControl(method = "cv", number = 10) 
cv_loo = trainControl(method = "LOOCV") 
cv_loo_slow = trainControl(method = "cv", number = 50) 

f1 = pctcom~.-pctfem-pctBA-pcthome-pctW-pctLHS-pctfampov-medHHI
f2BIC = pctcom~pctHS+pctrent+medHV+pctA
f3AIC = pctcom ~ pctHS+pctrent+medFamI+medrent+pctA
f4R2 = pctcom ~ pctHS+pctrent+medFamI+medrent+medHV+pctpov+pctfampov+pctA+pctHA+pctC
f5CP = pctcom ~.-pctfem-pctBA-pcthome-pctW-pctLHS-pctfampov-medHHI-pctOR


modela = train(f1, data = data, trControl = cv_10fold, 
               method = "lm")
modelb = train(f2BIC, data = data, trControl = cv_10fold, 
               method = "lm")
modelc = train(f3AIC, data = data, trControl = cv_10fold, 
               method = "lm")
modeld = train(f4R2, data = data, trControl = cv_10fold, 
               method = "lm")
modele = train(f5CP, data = data, trControl = cv_10fold, 
               method = "lm")

print(modela) 
print(modelb) 
print(modelc)
print(modeld)
print(modele)

##Based on 10-fold -- model from BIC selection is prefered

model1 = train(f1, data = data, trControl = cv_loo, 
               method = "lm")
model2 = train(f2BIC, data = data, trControl = cv_loo, 
               method = "lm")
model3 = train(f3AIC, data = data, trControl = cv_loo, 
               method = "lm")
model4 = train(f4R2, data = data, trControl = cv_loo, 
               method = "lm")
model5 = train(f5CP, data = data, trControl = cv_loo, 
               method = "lm")
# compare mse (rmse) for the two models using loo cv
print(model1) 
print(model2) 
print(model3) 
print(model4) 
print(model5) 

##Based on LOO CV -- model from BIC selection is prefered

model1b = train(f1, data = data, trControl = cv_loo_slow, 
               method = "lm")
model2b = train(f2BIC, data = data, trControl = cv_loo_slow, 
               method = "lm")
model3b = train(f3AIC, data = data, trControl = cv_loo_slow, 
               method = "lm")
model4b = train(f4R2, data = data, trControl = cv_loo_slow, 
               method = "lm")
model5b = train(f5CP, data = data, trControl = cv_loo_slow, 
               method = "lm")

# compare mse (rmse) for the two models using loo cv
print(model1b) 
print(model2b) 
print(model3b) 
print(model4b) 
print(model5b) 
##Based on LOO SLOW CV -- model from BIC selection is prefered

lmodd <- lm(pctcom~pctHS+pctrent+medHV+pctA, data=data)
summary(lmodd)
residualPlots(lmodd)
marginalModelPlots(lmodd)
avPlots(lmodd, id = FALSE)
crPlots(lmodd)

lm1 <- lm(pctcom~pctHS+I(pctrent^2)+medHV+pctA+pctrent, data=data)
lm2 <- lm(pctcom~pctHS+pctrent+medHV+pctA, data=data)
crPlots(lm1)
residualPlots(lm1)
marginalModelPlots(lm1)

##influencial observations##
h <- hatvalues(lm1)
halfnorm(h, nlab = 3, ylab= "leverage")
infIndexPlot(lm1, vars = "hat")
neigh[61]
neigh[40]
neigh[2]
##Kennedy, Auraria, and Country club could be leverage values 

##outlier observations##
outlierTest(lm1)
infIndexPlot(lm1, vars = c("Studentized", "Bonf"))
##No outiers##

##Influencial Observations##
cook <- cooks.distance(lm1)
halfnorm(cook, n = 6,ylab = "Cook's distances")

##Potential Influencial: 
##"College View - South Platte", "Goldsmith","Auraria", "Chaffee Park", "DIA"
infIndexPlot(lm1, var = "Cook",id = list(n = 3))
neigh[2]
neigh[19]
neigh[40]
plot(lm1, which = 4)
influencePlot(lm1)

lmodd2 <- lm(pctcom~pctHS+I(pctrent^2)+medHV+pctA+pctrent, data=data, subset = (data.rowNamesDF !="Auraria"))
compareCoefs(lm1, lmodd2)

lmodd3 <- lm(pctcom~pctHS+I(pctrent^2)+medHV+pctA+pctrent, data=data, subset = (data.rowNamesDF !="College View - South Platte"))
compareCoefs(lm1, lmodd3)

lmodd4 <- lm(pctcom~pctHS+I(pctrent^2)+medHV+pctA+pctrent, data=data, subset = (data.rowNamesDF != "Goldsmith"))
compareCoefs(lm1, lmodd3)

lmodd5 <- lm(pctcom~pctHS+I(pctrent^2)+medHV+pctA+pctrent, data=data, subset = (data.rowNamesDF != "Chaffee Park"))
compareCoefs(lm1, lmodd5)

lmodd6 <- lm(pctcom~pctHS+I(pctrent^2)+medHV+pctA+pctrent, data=data, subset = (data.rowNamesDF != "DIA"))
compareCoefs(lm1, lmodd6)

lmodd7 <- lm(pctcom~pctHS+I(pctrent^2)+medHV+pctA+pctrent, data=data, subset = (data.rowNamesDF != "Kennedy"))
compareCoefs(lm1, lmodd7)


##No influencial observations## 

##Checking Error Assumptions## 
plot(lm1, which = 1)
residualPlot(lm1)
plot(lm1, which = 3)
##Mean 0 satisfied but not constant variance (??) or yes constant variance just outliers?
neigh[19]
neigh[23]
neigh[11]
qqPlot(lm1)
shapiro.test(residuals(lm1))
##Since only 1-2 points are outside the confidence bands, we can conclude the erros are normal

##F Test for a Regression Relationship 

nullmod = lm(pctcom~1 , data=data)
summary(nullmod)
summary(lm1)
anova(nullmod, lm1)
#Since p value is small, there exists a regression relationship 
##Individual Test:
sumary(lm1)
confint(lm1, level=0.95)
##^confidence intervals
min(data$medHV,na.rm = TRUE)
max(data$medHV,na.rm = TRUE)
# Check if I(pctrent^2) and pctrent should be dropped together from model 
data2 <- na.omit(data)
lmods <- lm(pctcom~pctHS+medHV+pctA, data = data2) 
lmm <- lm(pctcom~pctHS+medHV+pctA+I(pctrent^2)+pctrent, data = data2)
anova(lmods, lmm)
lmods2 <- lm(pctcom~pctHS+medHV+pctA+I(pctrent^2), data = data2) 
lmm2 <- lm(pctcom~pctHS+medHV+pctA+I(pctrent^2)+pctrent, data = data2)
anova(lmods2, lmm2)
lmods3 <- lm(pctcom~pctHS+medHV+pctA+pctrent, data = data2) 
lmm3 <- lm(pctcom~pctHS+medHV+pctA+I(pctrent^2)+pctrent, data = data2)
anova(lmods3, lmm3)
