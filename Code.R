library(R.utils)
library(crayon)
library(car)
library("lmtest")
library(ggplot2)
library(GGally)
library(caret)
library(olsrr)


# reading file
canser_dt = read.csv2("cancer_reg.csv", sep = ",", dec = ".")
str(canser_dt)

summary(canser_dt)


# analyze death rate spread and distribution
# distribution plot
hist(canser_dt$TARGET_deathRate, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "DEATH",
     main = "Moyen Death")
lines(density(canser_dt$TARGET_deathRate), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

#box plot of spread
boxplot(canser_dt$TARGET_deathRate, main = "Moyen Death",
        ylab = "Death",
        col = "blue3",
        border = "black",
        notch = TRUE)


# eliminating categorical variables

canser_dt <- subset(canser_dt, select = c('avgAnnCount', 'avgDeathsPerYear', 'AvgHouseholdSize', 'BirthRate',
                                          'incidenceRate', 'MedianAge', 'MedianAgeFemale', 'MedianAgeMale', 'medIncome', 'PctAsian',
                                            'PctBachDeg18_24', 'PctBachDeg25_Over', 'PctBlack', 'PctEmployed16_Over', 'PctEmpPrivCoverage',
                                            'PctHS18_24', 'PctHS25_Over', 'PctMarriedHouseholds', 'PctNoHS18_24', 'PctOtherRace',
                                            'PctPrivateCoverage', 'PctPrivateCoverageAlone', 'PctPublicCoverage', 'PctPublicCoverageAlone',
                                            'PctSomeCol18_24', 'PctUnemployed16_Over','PctWhite', 'PercentMarried', 'popEst2015', 'povertyPercent',
                                            'studyPerCap', 'TARGET_deathRate'))

# split to test and training dataset
set.seed(123)
samplesize <- round(0.8 * nrow(canser_dt), 0)
index <- sample(seq_len(nrow(canser_dt)), size = samplesize)

data_train <- canser_dt[index, ]
data_test <- canser_dt[-index, ]

# calculer le modele de regression
reg <- lm(TARGET_deathRate ~ ., data = data_train)
summary(reg)


# supprimer la redendance
vif(reg)

# remove PctSomeCol18_24
canser_dt1 <- subset(canser_dt, select = c('avgAnnCount', 'avgDeathsPerYear', 'AvgHouseholdSize', 'BirthRate',
                                           'incidenceRate', 'MedianAge', 'MedianAgeFemale', 'MedianAgeMale', 'medIncome', 'PctAsian',
                                           'PctBachDeg18_24', 'PctBachDeg25_Over', 'PctBlack', 'PctEmployed16_Over', 'PctEmpPrivCoverage',
                                           'PctHS18_24', 'PctHS25_Over', 'PctMarriedHouseholds', 'PctNoHS18_24', 'PctOtherRace',
                                           'PctPrivateCoverage', 'PctPrivateCoverageAlone', 'PctPublicCoverage', 'PctPublicCoverageAlone',
                                           'PctUnemployed16_Over','PctWhite', 'PercentMarried', 'popEst2015', 'povertyPercent',
                                           'studyPerCap', 'TARGET_deathRate'))
set.seed(123)
samplesize <- round(0.8 * nrow(canser_dt1), 0)
index <- sample(seq_len(nrow(canser_dt1)), size = samplesize)

data_train1 <- canser_dt1[index, ]
data_test1 <- canser_dt1[-index, ]

reg1 = lm(TARGET_deathRate~ . , data=data_train1)
summary(reg1)

vif(reg1)

# remove PctPrivateCoverageAlone
canser_dt2 <- subset(canser_dt, select = c('avgAnnCount', 'avgDeathsPerYear', 'AvgHouseholdSize', 'BirthRate',
                                           'incidenceRate', 'MedianAge', 'MedianAgeFemale', 'MedianAgeMale', 'medIncome', 'PctAsian',
                                           'PctBachDeg18_24', 'PctBachDeg25_Over', 'PctBlack', 'PctEmployed16_Over', 'PctEmpPrivCoverage',
                                           'PctHS18_24', 'PctHS25_Over', 'PctMarriedHouseholds', 'PctNoHS18_24', 'PctOtherRace',
                                           'PctPrivateCoverage','PctPublicCoverage', 'PctPublicCoverageAlone',
                                           'PctUnemployed16_Over','PctWhite', 'PercentMarried', 'popEst2015', 'povertyPercent',
                                           'studyPerCap', 'TARGET_deathRate'))
set.seed(123)
samplesize <- round(0.8 * nrow(canser_dt2), 0)
index <- sample(seq_len(nrow(canser_dt2)), size = samplesize)

data_train2 <- canser_dt2[index, ]
data_test2 <- canser_dt2[-index, ]

reg2 = lm(TARGET_deathRate~ . , data=data_train2)
summary(reg2)

vif(reg2)

# remove avgDeathsPerYear
canser_dt3 <- subset(canser_dt, select = c('avgAnnCount','AvgHouseholdSize', 'BirthRate',
                                           'incidenceRate', 'MedianAge', 'MedianAgeFemale', 'MedianAgeMale', 'medIncome', 'PctAsian',
                                           'PctBachDeg18_24', 'PctBachDeg25_Over', 'PctBlack', 'PctEmployed16_Over', 'PctEmpPrivCoverage',
                                           'PctHS18_24', 'PctHS25_Over', 'PctMarriedHouseholds', 'PctNoHS18_24', 'PctOtherRace',
                                           'PctPrivateCoverage','PctPublicCoverage', 'PctPublicCoverageAlone',
                                           'PctUnemployed16_Over','PctWhite', 'PercentMarried', 'popEst2015', 'povertyPercent',
                                           'studyPerCap', 'TARGET_deathRate'))
set.seed(123)
samplesize <- round(0.8 * nrow(canser_dt3), 0)
index <- sample(seq_len(nrow(canser_dt3)), size = samplesize)

data_train3 <- canser_dt3[index, ]
data_test3 <- canser_dt3[-index, ]

reg3 = lm(TARGET_deathRate~ . , data=data_train3)
summary(reg3)

vif(reg3)

#remove PctPublicCoverage
canser_dt4 <- subset(canser_dt, select = c('avgAnnCount','AvgHouseholdSize', 'BirthRate',
                                           'incidenceRate', 'MedianAge', 'MedianAgeFemale', 'MedianAgeMale', 'medIncome', 'PctAsian',
                                           'PctBachDeg18_24', 'PctBachDeg25_Over', 'PctBlack', 'PctEmployed16_Over', 'PctEmpPrivCoverage',
                                           'PctHS18_24', 'PctHS25_Over', 'PctMarriedHouseholds', 'PctNoHS18_24', 'PctOtherRace',
                                           'PctPrivateCoverage','PctPublicCoverageAlone',
                                           'PctUnemployed16_Over','PctWhite', 'PercentMarried', 'popEst2015', 'povertyPercent',
                                           'studyPerCap', 'TARGET_deathRate'))
set.seed(123)
samplesize <- round(0.8 * nrow(canser_dt4), 0)
index <- sample(seq_len(nrow(canser_dt4)), size = samplesize)

data_train4 <- canser_dt4[index, ]
data_test4 <- canser_dt4[-index, ]

reg4 = lm(TARGET_deathRate~ . , data=data_train4)
summary(reg4)

vif(reg4)

#remove PctPrivateCoverage
canser_dt5 <- subset(canser_dt, select = c('avgAnnCount','AvgHouseholdSize', 'BirthRate',
                                           'incidenceRate', 'MedianAge', 'MedianAgeFemale', 'MedianAgeMale', 'medIncome', 'PctAsian',
                                           'PctBachDeg18_24', 'PctBachDeg25_Over', 'PctBlack', 'PctEmployed16_Over', 'PctEmpPrivCoverage',
                                          'PctHS18_24', 'PctHS25_Over', 'PctMarriedHouseholds', 'PctNoHS18_24', 'PctOtherRace',
                                        'PctPublicCoverageAlone','PctUnemployed16_Over','PctWhite', 'PercentMarried', 'popEst2015', 'povertyPercent',
                                           'studyPerCap', 'TARGET_deathRate'))
set.seed(123)
samplesize <- round(0.8 * nrow(canser_dt5), 0)
index <- sample(seq_len(nrow(canser_dt5)), size = samplesize)

data_train5 <- canser_dt5[index, ]
data_test5 <- canser_dt5[-index, ]

reg5 = lm(TARGET_deathRate~ . , data=data_train5)
summary(reg5)

vif(reg5)

#remove PercentMarried
canser_dt6 <- subset(canser_dt, select = c('avgAnnCount','AvgHouseholdSize', 'BirthRate',
                                           'incidenceRate', 'MedianAge', 'MedianAgeFemale', 'MedianAgeMale', 'medIncome', 'PctAsian',
                                           'PctBachDeg18_24', 'PctBachDeg25_Over', 'PctBlack', 'PctEmployed16_Over', 'PctEmpPrivCoverage',
                                           'PctHS18_24', 'PctHS25_Over', 'PctMarriedHouseholds', 'PctNoHS18_24', 'PctOtherRace',
                                           'PctPublicCoverageAlone','PctUnemployed16_Over','PctWhite','popEst2015', 'povertyPercent',
                                           'studyPerCap', 'TARGET_deathRate'))
canser_dt6<-na.omit(canser_dt6)
set.seed(123)
samplesize <- round(0.8 * nrow(canser_dt6), 0)
index <- sample(seq_len(nrow(canser_dt6)), size = samplesize)

data_train6 <- canser_dt6[index, ]
data_test6 <- canser_dt6[-index, ]

reg6 = lm(TARGET_deathRate~ . , data=data_train6)
summary(reg6)

vif(reg6)

# Backward tracing
step(reg6, trace = TRUE, direction = "backward")

reg7 = lm(TARGET_deathRate ~ avgAnnCount + BirthRate + incidenceRate + 
            MedianAgeFemale + PctBachDeg25_Over + PctEmployed16_Over + 
            PctEmpPrivCoverage + PctHS18_24 + PctMarriedHouseholds + 
            PctOtherRace + PctPublicCoverageAlone + popEst2015 + povertyPercent, 
          data = data_train6)
summary(reg7)

# Forward Tracing With constant regression
step(lm(TARGET_deathRate~1, data=data_train6), scope=~.+avgAnnCount + AvgHouseholdSize + BirthRate + incidenceRate + MedianAge + MedianAgeFemale +
     MedianAgeMale + medIncome + PctAsian + PctBachDeg18_24 + PctBachDeg25_Over + PctBlack +
     PctEmployed16_Over + PctEmpPrivCoverage + PctHS18_24 + PctHS25_Over + PctMarriedHouseholds +
     PctNoHS18_24 + PctOtherRace + PctPublicCoverageAlone + PctUnemployed16_Over + PctWhite +
     popEst2015 + povertyPercent + studyPerCap, direction="forward", trace = TRUE)

reg8 = lm(TARGET_deathRate ~ PctBachDeg25_Over + incidenceRate + 
            povertyPercent + PctHS18_24 + PctOtherRace + PctMarriedHouseholds + 
            MedianAgeFemale + BirthRate + PctPublicCoverageAlone + PctHS25_Over + 
            PctEmployed16_Over + PctEmpPrivCoverage + PctWhite + avgAnnCount + 
            popEst2015, data = data_train6)
summary(reg8)



# removing the non significant variables "PctWhite"

reg9 = lm(TARGET_deathRate ~ PctBachDeg25_Over + incidenceRate + 
            povertyPercent + PctHS18_24 + PctOtherRace + PctMarriedHouseholds + 
            MedianAgeFemale + BirthRate + PctPublicCoverageAlone + PctHS25_Over + 
            PctEmployed16_Over + PctEmpPrivCoverage + avgAnnCount + 
            popEst2015, data = data_train6)
summary(reg9)

# verification of outliers
plot(reg9$fitted.values, rstudent(reg9), xlab="Valeurs Pr�dites", ylab="R�sidus studentis�es")
abline(h=c(4,-4), col="red")
par(mfrow=c(1,2))
plot(cooks.distance(reg9), type="h", ylab="Distance de cook")

max(cooks.distance(reg9))
#removing outliers
data_train6 = data_train6[-c(1875),]

reg10 = lm(TARGET_deathRate ~ PctBachDeg25_Over + incidenceRate + 
            povertyPercent + PctHS18_24 + PctOtherRace + PctMarriedHouseholds + 
            MedianAgeFemale + BirthRate + PctPublicCoverageAlone + PctHS25_Over + 
            PctEmployed16_Over + PctEmpPrivCoverage + avgAnnCount + 
            popEst2015, data = data_train6)
summary(reg10)

###############################TEST#####################
#model performance
lm_pred <- predict(reg10, newdata = data_test6 %>% select(-TARGET_deathRate))
data_train6$predict = fitted(reg10)
# RMSE of train dataset
RMSE(pred = reg10$fitted.values, obs = data_train6$TARGET_deathRate)

# RMSE of test dataset
RMSE(pred = lm_pred, obs = data_test6$TARGET_deathRate)

#linearty test
resact <- data.frame(residual = reg10$residuals, fitted = reg10$fitted.values)
resact %>% ggplot(aes(fitted, residual)) + geom_point() + geom_smooth(method = 'gam') + geom_hline(aes(yintercept = 0)) + 
  theme(panel.grid = element_blank(), panel.background = element_blank())


# normality test
hist(rstudent(reg10),prob=TRUE,main="Distribution des r�sidus",xlab="residuals")
lines(density(rstudent(reg10)),col="red")

qqnorm(rstudent(reg10),datax=TRUE,main="Q-Q plot des r�sidus")
qqline(rstudent(reg10),datax=TRUE,col="red")

shapiro.test(rstudent(reg10))


# Heteroscedasticity Test
plot(reg10$fitted.values,rstudent(reg10),main = "Heteroscedasticity",xlab="Valeurs pr�dites",ylab = "Valeurs r�siduelles") 
abline(h=0,col="red")
bptest(reg10)
# autocorrelation test
dwtest(reg10)

#### transformation ####
library(estimatr)

reg11<-lm_robust(TARGET_deathRate ~ PctBachDeg25_Over + incidenceRate + 
            povertyPercent + PctHS18_24 + PctOtherRace + PctMarriedHouseholds + 
            MedianAgeFemale + BirthRate + PctPublicCoverageAlone + PctHS25_Over + 
            PctEmployed16_Over + PctEmpPrivCoverage + avgAnnCount + 
            popEst2015, data = data_train6)

summary(reg11)
bptest(reg11)

reg12<-lm(log(TARGET_deathRate) ~ PctBachDeg25_Over + incidenceRate + 
                   povertyPercent + PctHS18_24 + PctOtherRace + PctMarriedHouseholds + 
                   MedianAgeFemale + BirthRate + PctPublicCoverageAlone + PctHS25_Over + 
                   PctEmployed16_Over + PctEmpPrivCoverage + avgAnnCount + 
                   popEst2015, data = data_train6)
############


