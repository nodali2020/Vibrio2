setwd("C:/Users/Nodal/OneDrive - email.ntou.edu.tw/Dissertation/Data")

################# INTRODUCTION ############################################################
# Code written by Nodali Ndraha, National Taiwan Ocean University 

library(mc2d)
library(actuar)
library(tidyverse)
library(dplyr)
library(plyr)
library(caret)
library(missForest)
library(fitdistrplus)
library(LaplacesDemon)
library(truncdist)
library(data.table)
library(mltools)

######### RANDOM FOREST ###################################################################
rfDataOyster1 <- allData %>% filter(Type == "Oyster")
rfDataOyster1$Conc[is.na(rfDataOyster1$Conc)] <- 1.5
rfDataOyster1$Conc <- log10(rfDataOyster1$Conc)
rfDataOyster1 <- select(rfDataOyster1, -c("Year", "Type", "Date", "Season", "Loc", "Month"))
# imputing data
rfDataOyster1 <- as.data.frame(rfDataOyster1)
imprfDataOyster1 <- missForest(rfDataOyster1)

rfDataOyster1 <- imprfDataOyster1$ximp
anyNA(rfDataOyster1)
#dependent variable columns
dep.rfDataOyster1 <- c("Conc")
#remove variable columns
rfDataOyster2 <- rfDataOyster1[!names(rfDataOyster1) %in% dep.rfDataOyster1]
#run correlations
cor_mat.rfDataOyster2 <- cor(rfDataOyster2, method = "spearman")

#the index of the columns to be removed because they have a high correlation
index.rfDataOyster1 <- findCorrelation(cor_mat.rfDataOyster2, .99)

#the name of the columns chosen above
to_be_removed.rfDataOyster1 <- colnames(cor_mat.rfDataOyster2)[index.rfDataOyster1]

#now go back to df and use to_be_removed to subset the original df
rfDataOyster1 <- rfDataOyster1[!names(rfDataOyster1) %in% to_be_removed.rfDataOyster1]
glimpse(rfDataOyster1)

# splitting data
splitDataOysterQMRA <- createDataPartition(rfDataOyster1$Conc, p = 0.75, list = FALSE)
trainDataOysterQMRA <- rfDataOyster1[splitDataOysterQMRA,]
testDataOysterQMRA <- rfDataOyster1[-splitDataOysterQMRA,]

# Control the process and validation of the models
fitControl <- trainControl(
  method = "repeatedcv",            # repeated k-fold cross-validation
  repeats = 10,                     # the number of repeating cross-validation
  number = 10,                      # number of folds
  search = "grid",                  # search the grid to find the best hyperparameters 
  savePredictions = 'final',        # saves predictions for optimal tuning parameter
  summaryFunction=defaultSummary    # results summary function
)
# tune the grid search for hyperparameters
tuneGrid1 <- expand.grid(.mtry = seq(2, 50, by = 3),
                         .splitrule = "variance", 
                         .min.node.size = 5)

# train model based on provided tuning hyperparameters
set.seed(111)
rf.Oyster_QMRA <- train(Conc ~ ., 
                        data = trainDataOysterQMRA, 
                        method = "ranger", 
                        metric = "RMSE", 
                        trControl = fitControl,
                        importance = "permutation",
                        tuneGrid = tuneGrid1,
                        num.trees = 2000)
print(rf.Oyster_QMRA)
saveRDS(rf.Oyster_QMRA,"./Result/rf.Oyster_QMRA.rds")
rf.Oyster_QMRA <- readRDS("./Result/rf.Oyster_QMRA.rds")
plot(rf.Oyster_QMRA)
best_mtry.QMRA <- rf.Oyster_QMRA$bestTune$mtry 
best_splitrule.QMRA <- rf.Oyster_QMRA$bestTune$splitrule
best_nodesize.QMRA <- rf.Oyster_QMRA$bestTune$min.node.size
# tune the grids
tuneGrid2 <- expand.grid(.mtry = best_mtry.QMRA,
                         .splitrule = best_splitrule.QMRA, 
                         .min.node.size = best_nodesize.QMRA)

set.seed(222)
fit_rf.QMRA <- train(Conc ~ ., 
                     data=trainDataOysterQMRA, 
                     method="ranger", 
                     metric="RMSE", 
                     tuneGrid = tuneGrid2,
                     trControl=fitControl,
                     importance = "permutation",
                     num.trees = 20000)
fit_rf.QMRA
saveRDS(fit_rf.QMRA,"./Result/fit_rf.QMRA.rds")
fit_rf.QMRA <- readRDS("./Result/fit_rf.QMRA.rds")

# prediction 
rf_predict.QMRA <- predict(fit_rf.QMRA, testDataOysterQMRA)

#performance
rFperformance.QMRA <- postResample(pred = rf_predict.QMRA, obs = testDataOysterQMRA$Conc)
rFperformance.QMRA

# Selecting the top 10 predictors
varImp(fit_rf.QMRA)

set.seed(222)
final_rf.QMRA <- train(Conc ~ SST_avgX_3 + SST_imX_10 + SST_maxX_0 + SST_xmX_15 +
                         SST_xmX_30 + SST_maxX_3 + SST_imX_20 + SST_xmX_25 + WSX_2, 
                       data=trainDataOysterQMRA, 
                       method="ranger", 
                       metric="RMSE", 
                       tuneGrid = tuneGrid2,
                       trControl=fitControl,
                       importance = "permutation",
                       num.trees = 20000)
final_rf.QMRA
saveRDS(final_rf.QMRA,"./Result/final_rf.QMRA.rds")
final_rf.QMRA <- readRDS("./Result/final_rf.QMRA.rds")
# prediction 
final_predict.QMRA <- predict(final_rf.QMRA, testDataOysterQMRA)
#performance
final_rFperformance.QMRA <- postResample(pred = final_predict.QMRA, obs = testDataOysterQMRA$Conc)
final_rFperformance.QMRA

####### USER INPUTS #######################################################################

#_____Monte Carlo iterations
mcTrials= 1e5
ndvar(mcTrials)
ndunc(mcTrials)

# Distribution of increasing temperature based on RCPs ################
#____________RCP 2.6
#___________________ 2016 - 2035
RCP2.6_20162035 <- rpert(mcTrials, min=-1, mode=0, max=1, shape=4)
#___________________ 2046 - 2065
RCP2.6_20462065 <- rpert(mcTrials, min=-1, mode=0, max=1, shape=4)
#___________________ 2081 - 2100
RCP2.6_20852100 <- rpert(mcTrials, min=-1, mode=0, max=1, shape=4)

#____________RCP 4.5
#___________________ 2016 - 2035
RCP4.5_20162035 <- rpert(mcTrials, min=-1, mode=0, max=1, shape=4)
#___________________ 2046 - 2065
RCP4.5_20462065 <- rpert(mcTrials, min=-1, mode=0, max=1, shape=4)
#___________________ 2081 - 2100
RCP4.5_20852100 <- rpert(mcTrials, min=-1, mode=0, max=1, shape=4)

#____________RCP 6.0
#___________________ 2016 - 2035
RCP6.0_20162035 <- rpert(mcTrials, min=-1, mode=0, max=1, shape=4)
#___________________ 2046 - 2065
RCP6.0_20462065 <- rpert(mcTrials, min=-1, mode=0, max=1, shape=4)
#___________________ 2081 - 2100
RCP6.0_20852100 <- rpert(mcTrials, min=-1, mode=0, max=1, shape=4)

#____________RCP 8.5
#___________________ 2016 - 2035
RCP8.5_20162035 <- rpert(mcTrials, min=-1, mode=0, max=1, shape=4)
#___________________ 2046 - 2065
RCP8.5_20462065 <- rpert(mcTrials, min=-1, mode=0, max=1, shape=4)
#___________________ 2081 - 2100
RCP8.5_20852100 <- rpert(mcTrials, min=-1, mode=0, max=1, shape=4)

# Seasonal distribution of temperature change based on RCPs ##############
#______ Baseline ###########
climData <- dplyr::select(allData, c(Type, Season, SST_avgX_3, SST_imX_10, SST_maxX_0, 
                                     SST_xmX_15, SST_xmX_30, SST_maxX_3, SST_imX_20, SST_xmX_25, WSX_2)) %>% 
  dplyr::filter(Type == "Oyster")
# imputing data
Type_Season <- climData %>%  dplyr::select(Type, Season)
climData <- as.data.frame(climData)
climData <- climData %>%  dplyr::select(-Type, -Season)
impclimData <- missForest(climData)
climData <- impclimData$ximp
climData$Type <- Type_Season$Type
climData$Season <- Type_Season$Season
anyNA(climData)

par(mfrow = c(1,1))
#_________________Winter
dataWinter <- climData %>% dplyr::select("Season", "SST_avgX_3", "SST_imX_10", "SST_maxX_0", 
                                         "SST_xmX_15", "SST_xmX_30", "SST_maxX_3", "SST_imX_20", "SST_xmX_25", "WSX_2") %>% 
  dplyr::filter(Season == "Winter")

# dist_SST_avgX_3.Winter
fit_w.SST_avgX_3.Winter  <- fitdist(dataWinter$SST_avgX_3, "weibull")
fit_g.SST_avgX_3.Winter  <- fitdist(dataWinter$SST_avgX_3, "gamma")
fit_ln.SST_avgX_3.Winter <- fitdist(dataWinter$SST_avgX_3, "lnorm")
fit_n.SST_avgX_3.Winter <- fitdist(dataWinter$SST_avgX_3, "norm")
fit_lg.SST_avgX_3.Winter <- fitdist(dataWinter$SST_avgX_3, "logis")
plot.legend <- c("Weibull", "gamma", "lognormal", "normal", "logistic")
denscomp(list(fit_w.SST_avgX_3.Winter, fit_g.SST_avgX_3.Winter, fit_ln.SST_avgX_3.Winter, fit_n.SST_avgX_3.Winter, fit_lg.SST_avgX_3.Winter), legendtext = plot.legend)
qqcomp(list(fit_w.SST_avgX_3.Winter, fit_g.SST_avgX_3.Winter, fit_ln.SST_avgX_3.Winter, fit_n.SST_avgX_3.Winter, fit_lg.SST_avgX_3.Winter), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_avgX_3.Winter, fit_g.SST_avgX_3.Winter, fit_ln.SST_avgX_3.Winter, fit_n.SST_avgX_3.Winter, fit_lg.SST_avgX_3.Winter), legendtext = plot.legend)
gofstat(list(fit_w.SST_avgX_3.Winter, fit_g.SST_avgX_3.Winter, fit_ln.SST_avgX_3.Winter, fit_n.SST_avgX_3.Winter, fit_lg.SST_avgX_3.Winter), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_avgX_3.Winter <- bootdist(fit_lg.SST_avgX_3.Winter, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
summary(dataWinter$SST_avgX_3)
dist_SST_avgX_3.Winter <- rtrunc(mcTrials, spec = "logis", location = 20.5698874, scale = 0.8112341, a = min(dataWinter$SST_avgX_3), b = max(dataWinter$SST_avgX_3))
hist(dist_SST_avgX_3.Winter)


# dist_SST_imX_10.Winter
fit_w.SST_imX_10.Winter  <- fitdist(dataWinter$SST_imX_10, "weibull")
fit_g.SST_imX_10.Winter  <- fitdist(dataWinter$SST_imX_10, "gamma")
fit_ln.SST_imX_10.Winter <- fitdist(dataWinter$SST_imX_10, "lnorm")
fit_n.SST_imX_10.Winter <- fitdist(dataWinter$SST_imX_10, "norm")
fit_lg.SST_imX_10.Winter <- fitdist(dataWinter$SST_imX_10, "logis")
plot.legend <- c("Weibull", "gamma", "lognormal", "normal", "logistic")
denscomp(list(fit_w.SST_imX_10.Winter, fit_g.SST_imX_10.Winter, fit_ln.SST_imX_10.Winter, fit_n.SST_imX_10.Winter, fit_lg.SST_imX_10.Winter), legendtext = plot.legend)
qqcomp(list(fit_w.SST_imX_10.Winter, fit_g.SST_imX_10.Winter, fit_ln.SST_imX_10.Winter, fit_n.SST_imX_10.Winter, fit_lg.SST_imX_10.Winter), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_imX_10.Winter, fit_g.SST_imX_10.Winter, fit_ln.SST_imX_10.Winter, fit_n.SST_imX_10.Winter, fit_lg.SST_imX_10.Winter), legendtext = plot.legend)
gofstat(list(fit_w.SST_imX_10.Winter, fit_g.SST_imX_10.Winter, fit_ln.SST_imX_10.Winter, fit_n.SST_imX_10.Winter, fit_lg.SST_imX_10.Winter), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_imX_10.Winter <- bootdist(fit_lg.SST_imX_10.Winter, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
summary(dataWinter$SST_imX_10)
dist_SST_imX_10.Winter <- rtrunc(mcTrials, spec = "logis", location = 20.322435, scale = 0.914653, a = 17.50, b = 22.30)
hist(dist_SST_imX_10.Winter)

# dist_SST_maxX_0.Winter
fit_w.SST_maxX_0.Winter  <- fitdist(dataWinter$SST_maxX_0, "weibull")
fit_g.SST_maxX_0.Winter  <- fitdist(dataWinter$SST_maxX_0, "gamma")
fit_ln.SST_maxX_0.Winter <- fitdist(dataWinter$SST_maxX_0, "lnorm")
fit_n.SST_maxX_0.Winter <- fitdist(dataWinter$SST_maxX_0, "norm")
fit_lg.SST_maxX_0.Winter <- fitdist(dataWinter$SST_maxX_0, "logis")
plot.legend <- c("Weibull", "gamma", "lognormal", "normal", "logistic")
denscomp(list(fit_w.SST_maxX_0.Winter, fit_g.SST_maxX_0.Winter, fit_ln.SST_maxX_0.Winter, fit_n.SST_maxX_0.Winter, fit_lg.SST_maxX_0.Winter), legendtext = plot.legend)
qqcomp(list(fit_w.SST_maxX_0.Winter, fit_g.SST_maxX_0.Winter, fit_ln.SST_maxX_0.Winter, fit_n.SST_maxX_0.Winter, fit_lg.SST_maxX_0.Winter), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_maxX_0.Winter, fit_g.SST_maxX_0.Winter, fit_ln.SST_maxX_0.Winter, fit_n.SST_maxX_0.Winter, fit_lg.SST_maxX_0.Winter), legendtext = plot.legend)
gofstat(list(fit_w.SST_maxX_0.Winter, fit_g.SST_maxX_0.Winter, fit_ln.SST_maxX_0.Winter, fit_n.SST_maxX_0.Winter, fit_lg.SST_maxX_0.Winter), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_maxX_0.Winter <- bootdist(fit_lg.SST_maxX_0.Winter, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
summary(dataWinter$SST_maxX_0)
dist_SST_maxX_0.Winter <- rtrunc(mcTrials, spec = "logis", location = 20.857604, scale = 1.181004, a = 17.20, b = 24.50)
hist(dist_SST_maxX_0.Winter)

# dist_SST_xmX_15.Winter
fit_w.SST_xmX_15.Winter  <- fitdist(dataWinter$SST_xmX_15, "weibull")
fit_g.SST_xmX_15.Winter  <- fitdist(dataWinter$SST_xmX_15, "gamma")
fit_ln.SST_xmX_15.Winter <- fitdist(dataWinter$SST_xmX_15, "lnorm")
fit_n.SST_xmX_15.Winter <- fitdist(dataWinter$SST_xmX_15, "norm")
fit_lg.SST_xmX_15.Winter <- fitdist(dataWinter$SST_xmX_15, "logis")
plot.legend <- c("Weibull", "gamma", "lognormal", "normal", "logistic")
denscomp(list(fit_w.SST_xmX_15.Winter, fit_g.SST_xmX_15.Winter, fit_ln.SST_xmX_15.Winter, fit_n.SST_xmX_15.Winter, fit_lg.SST_xmX_15.Winter), legendtext = plot.legend)
qqcomp(list(fit_w.SST_xmX_15.Winter, fit_g.SST_xmX_15.Winter, fit_ln.SST_xmX_15.Winter, fit_n.SST_xmX_15.Winter, fit_lg.SST_xmX_15.Winter), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_xmX_15.Winter, fit_g.SST_xmX_15.Winter, fit_ln.SST_xmX_15.Winter, fit_n.SST_xmX_15.Winter, fit_lg.SST_xmX_15.Winter), legendtext = plot.legend)
gofstat(list(fit_w.SST_xmX_15.Winter, fit_g.SST_xmX_15.Winter, fit_ln.SST_xmX_15.Winter, fit_n.SST_xmX_15.Winter, fit_lg.SST_xmX_15.Winter), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_xmX_15.Winter <- bootdist(fit_lg.SST_xmX_15.Winter, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_xmX_15.Winter <- rtrunc(mcTrials, spec = "logis", location = 20.5698874, scale = 0.8112341, a = 20, b = 24)
hist(dist_SST_xmX_15.Winter)

# dist_SST_xmX_30.Winter
fit_w.SST_xmX_30.Winter  <- fitdist(dataWinter$SST_xmX_30, "weibull")
fit_g.SST_xmX_30.Winter  <- fitdist(dataWinter$SST_xmX_30, "gamma")
fit_ln.SST_xmX_30.Winter <- fitdist(dataWinter$SST_xmX_30, "lnorm")
fit_n.SST_xmX_30.Winter <- fitdist(dataWinter$SST_xmX_30, "norm")
fit_lg.SST_xmX_30.Winter <- fitdist(dataWinter$SST_xmX_30, "logis")
plot.legend <- c("Weibull", "gamma", "lognormal", "normal", "logistic")
denscomp(list(fit_w.SST_xmX_30.Winter, fit_g.SST_xmX_30.Winter, fit_ln.SST_xmX_30.Winter, fit_n.SST_xmX_30.Winter, fit_lg.SST_xmX_30.Winter), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_xmX_30.Winter, fit_g.SST_xmX_30.Winter, fit_ln.SST_xmX_30.Winter, fit_n.SST_xmX_30.Winter, fit_lg.SST_xmX_30.Winter), legendtext = plot.legend)
gofstat(list(fit_w.SST_xmX_30.Winter, fit_g.SST_xmX_30.Winter, fit_ln.SST_xmX_30.Winter, fit_n.SST_xmX_30.Winter, fit_lg.SST_xmX_30.Winter), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_xmX_30.Winter <- bootdist(fit_w.SST_xmX_30.Winter, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_xmX_30.Winter <- rtrunc(mcTrials, spec = "weibull", shape = 18.9708, scale = 20.8716, a = 17.20, b = 22.20)
summary(dataWinter$SST_xmX_30)
hist(dist_SST_xmX_30.Winter)

# dist_SST_maxX_3.Winter
fit_w.SST_maxX_3.Winter  <- fitdist(dataWinter$SST_maxX_3, "weibull")
fit_g.SST_maxX_3.Winter  <- fitdist(dataWinter$SST_maxX_3, "gamma")
fit_ln.SST_maxX_3.Winter <- fitdist(dataWinter$SST_maxX_3, "lnorm")
fit_n.SST_maxX_3.Winter <- fitdist(dataWinter$SST_maxX_3, "norm")
fit_lg.SST_maxX_3.Winter <- fitdist(dataWinter$SST_maxX_3, "logis")
plot.legend <- c("Weibull", "gamma", "lognormal", "normal", "logistic")
denscomp(list(fit_w.SST_maxX_3.Winter, fit_g.SST_maxX_3.Winter, fit_ln.SST_maxX_3.Winter, fit_n.SST_maxX_3.Winter, fit_lg.SST_maxX_3.Winter), legendtext = plot.legend)
qqcomp(list(fit_w.SST_maxX_3.Winter, fit_g.SST_maxX_3.Winter, fit_ln.SST_maxX_3.Winter, fit_n.SST_maxX_3.Winter, fit_lg.SST_maxX_3.Winter), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_maxX_3.Winter, fit_g.SST_maxX_3.Winter, fit_ln.SST_maxX_3.Winter, fit_n.SST_maxX_3.Winter, fit_lg.SST_maxX_3.Winter), legendtext = plot.legend)
gofstat(list(fit_w.SST_maxX_3.Winter, fit_g.SST_maxX_3.Winter, fit_ln.SST_maxX_3.Winter, fit_n.SST_maxX_3.Winter, fit_lg.SST_maxX_3.Winter), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_maxX_3.Winter <- bootdist(fit_lg.SST_maxX_3.Winter, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_maxX_3.Winter <- rtrunc(mcTrials, spec = "logis", location = 20.94568, scale = 0.84402, a = 20.0, b = 25.5)
summary(dataWinter$SST_maxX_3)
hist(dist_SST_maxX_3.Winter)

# dist_SST_imX_20.Winter
fit_w.SST_imX_20.Winter  <- fitdist(dataWinter$SST_imX_20, "weibull")
fit_g.SST_imX_20.Winter  <- fitdist(dataWinter$SST_imX_20, "gamma")
fit_ln.SST_imX_20.Winter <- fitdist(dataWinter$SST_imX_20, "lnorm")
fit_n.SST_imX_20.Winter <- fitdist(dataWinter$SST_imX_20, "norm")
fit_lg.SST_imX_20.Winter <- fitdist(dataWinter$SST_imX_20, "logis")
plot.legend <- c("Weibull", "gamma", "lognormal", "normal", "logistic")
denscomp(list(fit_w.SST_imX_20.Winter, fit_g.SST_imX_20.Winter, fit_ln.SST_imX_20.Winter, fit_n.SST_imX_20.Winter, fit_lg.SST_imX_20.Winter), legendtext = plot.legend)
qqcomp(list(fit_w.SST_imX_20.Winter, fit_g.SST_imX_20.Winter, fit_ln.SST_imX_20.Winter, fit_n.SST_imX_20.Winter, fit_lg.SST_imX_20.Winter), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_imX_20.Winter, fit_g.SST_imX_20.Winter, fit_ln.SST_imX_20.Winter, fit_n.SST_imX_20.Winter, fit_lg.SST_imX_20.Winter), legendtext = plot.legend)
gofstat(list(fit_w.SST_imX_20.Winter, fit_g.SST_imX_20.Winter, fit_ln.SST_imX_20.Winter, fit_n.SST_imX_20.Winter, fit_lg.SST_imX_20.Winter), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_imX_20.Winter <- bootdist(fit_lg.SST_imX_20.Winter, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_imX_20.Winter <- rtrunc(mcTrials, spec = "logis", location = 19.553808, scale = 1.002325, a = 16.00, b = 22.20)
summary(dataWinter$SST_imX_20)
hist(dist_SST_imX_20.Winter)

# dist_SST_xmX_25.Winter
fit_w.SST_xmX_25.Winter  <- fitdist(dataWinter$SST_xmX_25, "weibull")
fit_g.SST_xmX_25.Winter  <- fitdist(dataWinter$SST_xmX_25, "gamma")
fit_ln.SST_xmX_25.Winter <- fitdist(dataWinter$SST_xmX_25, "lnorm")
fit_n.SST_xmX_25.Winter <- fitdist(dataWinter$SST_xmX_25, "norm")
fit_lg.SST_xmX_25.Winter <- fitdist(dataWinter$SST_xmX_25, "logis")
plot.legend <- c("Weibull", "gamma", "lognormal", "normal", "logistic")
denscomp(list(fit_w.SST_xmX_25.Winter, fit_g.SST_xmX_25.Winter, fit_ln.SST_xmX_25.Winter, fit_n.SST_xmX_25.Winter, fit_lg.SST_xmX_25.Winter), legendtext = plot.legend)
qqcomp(list(fit_w.SST_xmX_25.Winter, fit_g.SST_xmX_25.Winter, fit_ln.SST_xmX_25.Winter, fit_n.SST_xmX_25.Winter, fit_lg.SST_xmX_25.Winter), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_xmX_25.Winter, fit_g.SST_xmX_25.Winter, fit_ln.SST_xmX_25.Winter, fit_n.SST_xmX_25.Winter, fit_lg.SST_xmX_25.Winter), legendtext = plot.legend)
gofstat(list(fit_w.SST_xmX_25.Winter, fit_g.SST_xmX_25.Winter, fit_ln.SST_xmX_25.Winter, fit_n.SST_xmX_25.Winter, fit_lg.SST_xmX_25.Winter), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_xmX_25.Winter <- bootdist(fit_lg.SST_xmX_25.Winter, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_xmX_25.Winter <- rtrunc(mcTrials, spec = "logis", location = 20.32826, scale = 0.40878, a = 19.70, b = 22.05)
summary(dataWinter$SST_xmX_25)
hist(dist_SST_xmX_25.Winter)

# dist_WSX_2.Winter
fit_w.WSX_2.Winter  <- fitdist(dataWinter$WSX_2, "weibull")
fit_g.WSX_2.Winter  <- fitdist(dataWinter$WSX_2, "gamma")
fit_ln.WSX_2.Winter <- fitdist(dataWinter$WSX_2, "lnorm")
fit_n.WSX_2.Winter <- fitdist(dataWinter$WSX_2, "norm")
fit_lg.WSX_2.Winter <- fitdist(dataWinter$WSX_2, "logis")
plot.legend <- c("Weibull", "gamma", "lognormal", "normal", "logistic")
denscomp(list(fit_w.WSX_2.Winter, fit_g.WSX_2.Winter, fit_ln.WSX_2.Winter, fit_n.WSX_2.Winter, fit_lg.WSX_2.Winter), legendtext = plot.legend)
qqcomp(list(fit_w.WSX_2.Winter, fit_g.WSX_2.Winter, fit_ln.WSX_2.Winter, fit_n.WSX_2.Winter, fit_lg.WSX_2.Winter), legendtext = plot.legend)
cdfcomp(list(fit_w.WSX_2.Winter, fit_g.WSX_2.Winter, fit_ln.WSX_2.Winter, fit_n.WSX_2.Winter, fit_lg.WSX_2.Winter), legendtext = plot.legend)
gofstat(list(fit_w.WSX_2.Winter, fit_g.WSX_2.Winter, fit_ln.WSX_2.Winter, fit_n.WSX_2.Winter, fit_lg.WSX_2.Winter), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.WSX_2.Winter <- bootdist(fit_ln.WSX_2.Winter, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_WSX_2.Winter <- rtrunc(mcTrials, spec = "lnorm", meanlog = 1.6014648, sdlog = 0.2785947, a = 3.500, b = 8.200)
summary(dataWinter$WSX_2)
hist(dist_WSX_2.Winter)

# ___________________Spring
dataSpring <- climData %>% dplyr::select("Season", "SST_avgX_3", "SST_imX_10", "SST_maxX_0", 
                                         "SST_xmX_15", "SST_xmX_30", "SST_maxX_3", "SST_imX_20", "SST_xmX_25", "WSX_2") %>% 
  dplyr::filter(Season == "Spring")

# dist_SST_avgX_3.Spring
fit_w.SST_avgX_3.Spring  <- fitdist(dataSpring$SST_avgX_3, "weibull")
fit_g.SST_avgX_3.Spring  <- fitdist(dataSpring$SST_avgX_3, "gamma")
fit_ln.SST_avgX_3.Spring <- fitdist(dataSpring$SST_avgX_3, "lnorm")
fit_n.SST_avgX_3.Spring <- fitdist(dataSpring$SST_avgX_3, "norm")
fit_lg.SST_avgX_3.Spring <- fitdist(dataSpring$SST_avgX_3, "logis")
denscomp(list(fit_w.SST_avgX_3.Spring, fit_g.SST_avgX_3.Spring, fit_ln.SST_avgX_3.Spring, fit_n.SST_avgX_3.Spring, fit_lg.SST_avgX_3.Spring), legendtext = plot.legend)
qqcomp(list(fit_w.SST_avgX_3.Spring, fit_g.SST_avgX_3.Spring, fit_ln.SST_avgX_3.Spring, fit_n.SST_avgX_3.Spring, fit_lg.SST_avgX_3.Spring), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_avgX_3.Spring, fit_g.SST_avgX_3.Spring, fit_ln.SST_avgX_3.Spring, fit_n.SST_avgX_3.Spring, fit_lg.SST_avgX_3.Spring), legendtext = plot.legend)
gofstat(list(fit_w.SST_avgX_3.Spring, fit_g.SST_avgX_3.Spring, fit_ln.SST_avgX_3.Spring, fit_n.SST_avgX_3.Spring, fit_lg.SST_avgX_3.Spring), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_avgX_3.Spring <- bootdist(fit_w.SST_avgX_3.Spring, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_avgX_3.Spring <- rtrunc(mcTrials, spec = "weibull", shape = 27.68241, scale = 27.60143, a = 25.01, b = 28.53)
summary(dataSpring$SST_avgX_3)
hist(dist_SST_avgX_3.Spring)

# dist_SST_imX_10.Spring
fit_w.SST_imX_10.Spring  <- fitdist(dataSpring$SST_imX_10, "weibull")
fit_g.SST_imX_10.Spring  <- fitdist(dataSpring$SST_imX_10, "gamma")
fit_ln.SST_imX_10.Spring <- fitdist(dataSpring$SST_imX_10, "lnorm")
fit_n.SST_imX_10.Spring <- fitdist(dataSpring$SST_imX_10, "norm")
fit_lg.SST_imX_10.Spring <- fitdist(dataSpring$SST_imX_10, "logis")
denscomp(list(fit_w.SST_imX_10.Spring, fit_g.SST_imX_10.Spring, fit_ln.SST_imX_10.Spring, fit_n.SST_imX_10.Spring, fit_lg.SST_imX_10.Spring), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_imX_10.Spring, fit_g.SST_imX_10.Spring, fit_ln.SST_imX_10.Spring, fit_n.SST_imX_10.Spring, fit_lg.SST_imX_10.Spring), legendtext = plot.legend)
gofstat(list(fit_w.SST_imX_10.Spring, fit_g.SST_imX_10.Spring, fit_ln.SST_imX_10.Spring, fit_n.SST_imX_10.Spring, fit_lg.SST_imX_10.Spring), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_imX_10.Spring <- bootdist(fit_lg.SST_imX_10.Spring, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_imX_10.Spring <- rtrunc(mcTrials, spec = "logis", location = 26.3415988, scale = 0.8982935, a = 23.40, b = 28.10)
summary(dataSpring$SST_imX_10)
hist(dist_SST_imX_10.Spring)

# dist_SST_maxX_0.Spring
fit_w.SST_maxX_0.Spring  <- fitdist(dataSpring$SST_maxX_0, "weibull")
fit_g.SST_maxX_0.Spring  <- fitdist(dataSpring$SST_maxX_0, "gamma")
fit_ln.SST_maxX_0.Spring <- fitdist(dataSpring$SST_maxX_0, "lnorm")
fit_n.SST_maxX_0.Spring <- fitdist(dataSpring$SST_maxX_0, "norm")
fit_lg.SST_maxX_0.Spring <- fitdist(dataSpring$SST_maxX_0, "logis")
denscomp(list(fit_w.SST_maxX_0.Spring, fit_g.SST_maxX_0.Spring, fit_ln.SST_maxX_0.Spring, fit_n.SST_maxX_0.Spring, fit_lg.SST_maxX_0.Spring), legendtext = plot.legend)
qqcomp(list(fit_w.SST_maxX_0.Spring, fit_g.SST_maxX_0.Spring, fit_ln.SST_maxX_0.Spring, fit_n.SST_maxX_0.Spring, fit_lg.SST_maxX_0.Spring), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_maxX_0.Spring, fit_g.SST_maxX_0.Spring, fit_ln.SST_maxX_0.Spring, fit_n.SST_maxX_0.Spring, fit_lg.SST_maxX_0.Spring), legendtext = plot.legend)
gofstat(list(fit_w.SST_maxX_0.Spring, fit_g.SST_maxX_0.Spring, fit_ln.SST_maxX_0.Spring, fit_n.SST_maxX_0.Spring, fit_lg.SST_maxX_0.Spring), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_maxX_0.Spring <- bootdist(fit_w.SST_maxX_0.Spring, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_maxX_0.Spring <- rtrunc(mcTrials, spec = "weibull", shape = 17.86553, scale = 29.14437, a = 24.40, b = 30.60)
summary(dataSpring$SST_maxX_0)
hist(dist_SST_maxX_0.Spring)

# dist_SST_xmX_15.Spring
fit_w.SST_xmX_15.Spring  <- fitdist(dataSpring$SST_xmX_15, "weibull")
fit_g.SST_xmX_15.Spring  <- fitdist(dataSpring$SST_xmX_15, "gamma")
fit_ln.SST_xmX_15.Spring <- fitdist(dataSpring$SST_xmX_15, "lnorm")
fit_n.SST_xmX_15.Spring <- fitdist(dataSpring$SST_xmX_15, "norm")
fit_lg.SST_xmX_15.Spring <- fitdist(dataSpring$SST_xmX_15, "logis")
plot.legend <- c("Weibull", "gamma", "lognormal", "normal", "logistic")
denscomp(list(fit_w.SST_xmX_15.Spring, fit_g.SST_xmX_15.Spring, fit_ln.SST_xmX_15.Spring, fit_n.SST_xmX_15.Spring, fit_lg.SST_xmX_15.Spring), legendtext = plot.legend)
qqcomp(list(fit_w.SST_xmX_15.Spring, fit_g.SST_xmX_15.Spring, fit_ln.SST_xmX_15.Spring, fit_n.SST_xmX_15.Spring, fit_lg.SST_xmX_15.Spring), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_xmX_15.Spring, fit_g.SST_xmX_15.Spring, fit_ln.SST_xmX_15.Spring, fit_n.SST_xmX_15.Spring, fit_lg.SST_xmX_15.Spring), legendtext = plot.legend)
gofstat(list(fit_w.SST_xmX_15.Spring, fit_g.SST_xmX_15.Spring, fit_ln.SST_xmX_15.Spring, fit_n.SST_xmX_15.Spring, fit_lg.SST_xmX_15.Spring), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_xmX_15.Spring <- bootdist(fit_w.SST_xmX_15.Spring, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_xmX_15.Spring <- rtrunc(mcTrials, spec = "weibull", shape = 12.37745, scale = 28.91535, a = 23.55, b = 31.70)
summary(dataSpring$SST_xmX_15)
hist(dist_SST_xmX_15.Spring)

# dist_SST_xmX_30.Spring
fit_w.SST_xmX_30.Spring  <- fitdist(dataSpring$SST_xmX_30, "weibull")
fit_g.SST_xmX_30.Spring  <- fitdist(dataSpring$SST_xmX_30, "gamma")
fit_ln.SST_xmX_30.Spring <- fitdist(dataSpring$SST_xmX_30, "lnorm")
fit_n.SST_xmX_30.Spring <- fitdist(dataSpring$SST_xmX_30, "norm")
fit_lg.SST_xmX_30.Spring <- fitdist(dataSpring$SST_xmX_30, "logis")
denscomp(list(fit_w.SST_xmX_30.Spring, fit_g.SST_xmX_30.Spring, fit_ln.SST_xmX_30.Spring, fit_n.SST_xmX_30.Spring, fit_lg.SST_xmX_30.Spring), legendtext = plot.legend)
qqcomp(list(fit_w.SST_xmX_30.Spring, fit_g.SST_xmX_30.Spring, fit_ln.SST_xmX_30.Spring, fit_n.SST_xmX_30.Spring, fit_lg.SST_xmX_30.Spring), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_xmX_30.Spring, fit_g.SST_xmX_30.Spring, fit_ln.SST_xmX_30.Spring, fit_n.SST_xmX_30.Spring, fit_lg.SST_xmX_30.Spring), legendtext = plot.legend)
gofstat(list(fit_w.SST_xmX_30.Spring, fit_g.SST_xmX_30.Spring, fit_ln.SST_xmX_30.Spring, fit_n.SST_xmX_30.Spring, fit_lg.SST_xmX_30.Spring), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_xmX_30.Spring <- bootdist(fit_lg.SST_xmX_30.Spring, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_xmX_30.Spring <- rtrunc(mcTrials, spec = "logis", location = 26.529308, scale = 1.032354, a = 23.95, b = 28.90)
summary(dataSpring$SST_xmX_30)
hist(dist_SST_xmX_30.Spring)

# dist_SST_maxX_3.Spring
fit_w.SST_maxX_3.Spring  <- fitdist(dataSpring$SST_maxX_3, "weibull")
fit_g.SST_maxX_3.Spring  <- fitdist(dataSpring$SST_maxX_3, "gamma")
fit_ln.SST_maxX_3.Spring <- fitdist(dataSpring$SST_maxX_3, "lnorm")
fit_n.SST_maxX_3.Spring <- fitdist(dataSpring$SST_maxX_3, "norm")
fit_lg.SST_maxX_3.Spring <- fitdist(dataSpring$SST_maxX_3, "logis")
denscomp(list(fit_w.SST_maxX_3.Spring, fit_g.SST_maxX_3.Spring, fit_ln.SST_maxX_3.Spring, fit_n.SST_maxX_3.Spring, fit_lg.SST_maxX_3.Spring), legendtext = plot.legend)
qqcomp(list(fit_w.SST_maxX_3.Spring, fit_g.SST_maxX_3.Spring, fit_ln.SST_maxX_3.Spring, fit_n.SST_maxX_3.Spring, fit_lg.SST_maxX_3.Spring), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_maxX_3.Spring, fit_g.SST_maxX_3.Spring, fit_ln.SST_maxX_3.Spring, fit_n.SST_maxX_3.Spring, fit_lg.SST_maxX_3.Spring), legendtext = plot.legend)
gofstat(list(fit_w.SST_maxX_3.Spring, fit_g.SST_maxX_3.Spring, fit_ln.SST_maxX_3.Spring, fit_n.SST_maxX_3.Spring, fit_lg.SST_maxX_3.Spring), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_maxX_3.Spring <- bootdist(fit_w.SST_maxX_3.Spring, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_maxX_3.Spring <- rtrunc(mcTrials, spec = "weibull", shape = 27.53831, scale = 28.28751, a = 25.50, b = 29.21)
summary(dataSpring$SST_maxX_3)
hist(dist_SST_maxX_3.Spring)

# dist_SST_imX_20.Spring
fit_w.SST_imX_20.Spring  <- fitdist(dataSpring$SST_imX_20, "weibull")
fit_g.SST_imX_20.Spring  <- fitdist(dataSpring$SST_imX_20, "gamma")
fit_ln.SST_imX_20.Spring <- fitdist(dataSpring$SST_imX_20, "lnorm")
fit_n.SST_imX_20.Spring <- fitdist(dataSpring$SST_imX_20, "norm")
fit_lg.SST_imX_20.Spring <- fitdist(dataSpring$SST_imX_20, "logis")
denscomp(list(fit_w.SST_imX_20.Spring, fit_g.SST_imX_20.Spring, fit_ln.SST_imX_20.Spring, fit_n.SST_imX_20.Spring, fit_lg.SST_imX_20.Spring), legendtext = plot.legend)
qqcomp(list(fit_w.SST_imX_20.Spring, fit_g.SST_imX_20.Spring, fit_ln.SST_imX_20.Spring, fit_n.SST_imX_20.Spring, fit_lg.SST_imX_20.Spring), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_imX_20.Spring, fit_g.SST_imX_20.Spring, fit_ln.SST_imX_20.Spring, fit_n.SST_imX_20.Spring, fit_lg.SST_imX_20.Spring), legendtext = plot.legend)
gofstat(list(fit_w.SST_imX_20.Spring, fit_g.SST_imX_20.Spring, fit_ln.SST_imX_20.Spring, fit_n.SST_imX_20.Spring, fit_lg.SST_imX_20.Spring), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_imX_20.Spring <- bootdist(fit_ln.SST_imX_20.Spring, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_imX_20.Spring <- rtrunc(mcTrials, spec = "lnorm", meanlog = 3.25944665, sdlog = 0.07923708, a = 23.60, b = 29.90)
hist(dist_SST_imX_20.Spring)
summary(dataSpring$SST_imX_20)

# dist_SST_xmX_25.Spring
fit_w.SST_xmX_25.Spring  <- fitdist(dataSpring$SST_xmX_25, "weibull")
fit_g.SST_xmX_25.Spring  <- fitdist(dataSpring$SST_xmX_25, "gamma")
fit_ln.SST_xmX_25.Spring <- fitdist(dataSpring$SST_xmX_25, "lnorm")
fit_n.SST_xmX_25.Spring <- fitdist(dataSpring$SST_xmX_25, "norm")
fit_lg.SST_xmX_25.Spring <- fitdist(dataSpring$SST_xmX_25, "logis")
denscomp(list(fit_w.SST_xmX_25.Spring, fit_g.SST_xmX_25.Spring, fit_ln.SST_xmX_25.Spring, fit_n.SST_xmX_25.Spring, fit_lg.SST_xmX_25.Spring), legendtext = plot.legend)
qqcomp(list(fit_w.SST_xmX_25.Spring, fit_g.SST_xmX_25.Spring, fit_ln.SST_xmX_25.Spring, fit_n.SST_xmX_25.Spring, fit_lg.SST_xmX_25.Spring), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_xmX_25.Spring, fit_g.SST_xmX_25.Spring, fit_ln.SST_xmX_25.Spring, fit_n.SST_xmX_25.Spring, fit_lg.SST_xmX_25.Spring), legendtext = plot.legend)
gofstat(list(fit_w.SST_xmX_25.Spring, fit_g.SST_xmX_25.Spring, fit_ln.SST_xmX_25.Spring, fit_n.SST_xmX_25.Spring, fit_lg.SST_xmX_25.Spring), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_xmX_25.Spring <- bootdist(fit_ln.SST_xmX_25.Spring, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_xmX_25.Spring <- rtrunc(mcTrials, spec = "lnorm", meanlog = 3.29024360, sdlog = 0.09166077, a = 23.75, b = 31.30)
hist(dist_SST_xmX_25.Spring)
summary(dataSpring$SST_xmX_25)

# dist_WSX_2.Spring
fit_w.WSX_2.Spring  <- fitdist(dataSpring$WSX_2, "weibull")
fit_g.WSX_2.Spring  <- fitdist(dataSpring$WSX_2, "gamma")
fit_ln.WSX_2.Spring <- fitdist(dataSpring$WSX_2, "lnorm")
fit_n.WSX_2.Spring <- fitdist(dataSpring$WSX_2, "norm")
fit_lg.WSX_2.Spring <- fitdist(dataSpring$WSX_2, "logis")
denscomp(list(fit_w.WSX_2.Spring, fit_g.WSX_2.Spring, fit_ln.WSX_2.Spring, fit_n.WSX_2.Spring, fit_lg.WSX_2.Spring), legendtext = plot.legend)
qqcomp(list(fit_w.WSX_2.Spring, fit_g.WSX_2.Spring, fit_ln.WSX_2.Spring, fit_n.WSX_2.Spring, fit_lg.WSX_2.Spring), legendtext = plot.legend)
cdfcomp(list(fit_w.WSX_2.Spring, fit_g.WSX_2.Spring, fit_ln.WSX_2.Spring, fit_n.WSX_2.Spring, fit_lg.WSX_2.Spring), legendtext = plot.legend)
gofstat(list(fit_w.WSX_2.Spring, fit_g.WSX_2.Spring, fit_ln.WSX_2.Spring, fit_n.WSX_2.Spring, fit_lg.WSX_2.Spring), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.WSX_2.Spring <- bootdist(fit_ln.WSX_2.Spring, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_WSX_2.Spring <- rtrunc(mcTrials, spec = "lnorm", meanlog = 1.166699, sdlog = 0.233399, a = 2.600, b = 5.200)
hist(dist_WSX_2.Spring)
summary(dataSpring$WSX_2)

# _______________Summer
dataSummer <- climData %>% dplyr::select("Season", "SST_avgX_3", "SST_imX_10", "SST_maxX_0", 
                                         "SST_xmX_15", "SST_xmX_30", "SST_maxX_3", "SST_imX_20", "SST_xmX_25", "WSX_2") %>% 
  dplyr::filter(Season == "Summer")

# dist_SST_avgX_3.Summer
fit_w.SST_avgX_3.Summer  <- fitdist(dataSummer$SST_avgX_3, "weibull")
fit_g.SST_avgX_3.Summer  <- fitdist(dataSummer$SST_avgX_3, "gamma")
fit_ln.SST_avgX_3.Summer <- fitdist(dataSummer$SST_avgX_3, "lnorm")
fit_n.SST_avgX_3.Summer <- fitdist(dataSummer$SST_avgX_3, "norm")
fit_lg.SST_avgX_3.Summer <- fitdist(dataSummer$SST_avgX_3, "logis")
denscomp(list(fit_w.SST_avgX_3.Summer, fit_g.SST_avgX_3.Summer, fit_ln.SST_avgX_3.Summer, fit_n.SST_avgX_3.Summer, fit_lg.SST_avgX_3.Summer), legendtext = plot.legend)
qqcomp(list(fit_w.SST_avgX_3.Summer, fit_g.SST_avgX_3.Summer, fit_ln.SST_avgX_3.Summer, fit_n.SST_avgX_3.Summer, fit_lg.SST_avgX_3.Summer), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_avgX_3.Summer, fit_g.SST_avgX_3.Summer, fit_ln.SST_avgX_3.Summer, fit_n.SST_avgX_3.Summer, fit_lg.SST_avgX_3.Summer), legendtext = plot.legend)
gofstat(list(fit_w.SST_avgX_3.Summer, fit_g.SST_avgX_3.Summer, fit_ln.SST_avgX_3.Summer, fit_n.SST_avgX_3.Summer, fit_lg.SST_avgX_3.Summer), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_avgX_3.Summer <- bootdist(fit_ln.SST_avgX_3.Summer, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_avgX_3.Summer <- rtrunc(mcTrials, spec = "lnorm", meanlog = 3.39102448, sdlog = 0.01609745, a = 28.93, b = 30.55)
hist(dist_SST_avgX_3.Summer)
summary(dataSummer$SST_avgX_3)

# dist_SST_imX_10.Summer
fit_w.SST_imX_10.Summer  <- fitdist(dataSummer$SST_imX_10, "weibull")
fit_g.SST_imX_10.Summer  <- fitdist(dataSummer$SST_imX_10, "gamma")
fit_ln.SST_imX_10.Summer <- fitdist(dataSummer$SST_imX_10, "lnorm")
fit_n.SST_imX_10.Summer <- fitdist(dataSummer$SST_imX_10, "norm")
fit_lg.SST_imX_10.Summer <- fitdist(dataSummer$SST_imX_10, "logis")
denscomp(list(fit_w.SST_imX_10.Summer, fit_g.SST_imX_10.Summer, fit_ln.SST_imX_10.Summer, fit_n.SST_imX_10.Summer, fit_lg.SST_imX_10.Summer), legendtext = plot.legend)
qqcomp(list(fit_w.SST_imX_10.Summer, fit_g.SST_imX_10.Summer, fit_ln.SST_imX_10.Summer, fit_n.SST_imX_10.Summer, fit_lg.SST_imX_10.Summer), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_imX_10.Summer, fit_g.SST_imX_10.Summer, fit_ln.SST_imX_10.Summer, fit_n.SST_imX_10.Summer, fit_lg.SST_imX_10.Summer), legendtext = plot.legend)
gofstat(list(fit_w.SST_imX_10.Summer, fit_g.SST_imX_10.Summer, fit_ln.SST_imX_10.Summer, fit_n.SST_imX_10.Summer, fit_lg.SST_imX_10.Summer), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_imX_10.Summer <- bootdist(fit_lg.SST_imX_10.Summer, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_imX_10.Summer <- rtrunc(mcTrials, spec = "logis", location = 29.2929398, scale = 0.3293526, a = 28.45, b = 30.15)
hist(dist_SST_imX_10.Summer)
summary(dataSummer$SST_imX_10)

# dist_SST_maxX_0.Summer
fit_w.SST_maxX_0.Summer  <- fitdist(dataSummer$SST_maxX_0, "weibull")
fit_g.SST_maxX_0.Summer  <- fitdist(dataSummer$SST_maxX_0, "gamma")
fit_ln.SST_maxX_0.Summer <- fitdist(dataSummer$SST_maxX_0, "lnorm")
fit_n.SST_maxX_0.Summer <- fitdist(dataSummer$SST_maxX_0, "norm")
fit_lg.SST_maxX_0.Summer <- fitdist(dataSummer$SST_maxX_0, "logis")
denscomp(list(fit_w.SST_maxX_0.Summer, fit_g.SST_maxX_0.Summer, fit_ln.SST_maxX_0.Summer, fit_n.SST_maxX_0.Summer, fit_lg.SST_maxX_0.Summer), legendtext = plot.legend)
qqcomp(list(fit_w.SST_maxX_0.Summer, fit_g.SST_maxX_0.Summer, fit_ln.SST_maxX_0.Summer, fit_n.SST_maxX_0.Summer, fit_lg.SST_maxX_0.Summer), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_maxX_0.Summer, fit_g.SST_maxX_0.Summer, fit_ln.SST_maxX_0.Summer, fit_n.SST_maxX_0.Summer, fit_lg.SST_maxX_0.Summer), legendtext = plot.legend)
gofstat(list(fit_w.SST_maxX_0.Summer, fit_g.SST_maxX_0.Summer, fit_ln.SST_maxX_0.Summer, fit_n.SST_maxX_0.Summer, fit_lg.SST_maxX_0.Summer), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_maxX_0.Summer <- bootdist(fit_lg.SST_maxX_0.Summer, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_maxX_0.Summer <- rtrunc(mcTrials, spec = "logis", location = 29.8585992, scale = 0.3429955, a = 29.00, b = 30.90)
hist(dist_SST_maxX_0.Summer)
summary(dataSummer$SST_maxX_0)

# dist_SST_xmX_15.Summer
fit_w.SST_xmX_15.Summer  <- fitdist(dataSummer$SST_xmX_15, "weibull")
fit_g.SST_xmX_15.Summer  <- fitdist(dataSummer$SST_xmX_15, "gamma")
fit_ln.SST_xmX_15.Summer <- fitdist(dataSummer$SST_xmX_15, "lnorm")
fit_n.SST_xmX_15.Summer <- fitdist(dataSummer$SST_xmX_15, "norm")
fit_lg.SST_xmX_15.Summer <- fitdist(dataSummer$SST_xmX_15, "logis")
denscomp(list(fit_w.SST_xmX_15.Summer, fit_g.SST_xmX_15.Summer, fit_ln.SST_xmX_15.Summer, fit_n.SST_xmX_15.Summer, fit_lg.SST_xmX_15.Summer), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_xmX_15.Summer, fit_g.SST_xmX_15.Summer, fit_ln.SST_xmX_15.Summer, fit_n.SST_xmX_15.Summer, fit_lg.SST_xmX_15.Summer), legendtext = plot.legend)
gofstat(list(fit_w.SST_xmX_15.Summer, fit_g.SST_xmX_15.Summer, fit_ln.SST_xmX_15.Summer, fit_n.SST_xmX_15.Summer, fit_lg.SST_xmX_15.Summer), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_xmX_15.Summer <- bootdist(fit_lg.SST_xmX_15.Summer, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_xmX_15.Summer <- rtrunc(mcTrials, spec = "logis", location = 29.894880, scale = 0.332719, a = 29.10, b = 30.45)
hist(dist_SST_xmX_15.Summer)
summary(dataSummer$SST_xmX_15)

# dist_SST_xmX_30.Summer
fit_w.SST_xmX_30.Summer  <- fitdist(dataSummer$SST_xmX_30, "weibull")
fit_g.SST_xmX_30.Summer  <- fitdist(dataSummer$SST_xmX_30, "gamma")
fit_ln.SST_xmX_30.Summer <- fitdist(dataSummer$SST_xmX_30, "lnorm")
fit_n.SST_xmX_30.Summer <- fitdist(dataSummer$SST_xmX_30, "norm")
fit_lg.SST_xmX_30.Summer <- fitdist(dataSummer$SST_xmX_30, "logis")
denscomp(list(fit_w.SST_xmX_30.Summer, fit_g.SST_xmX_30.Summer, fit_ln.SST_xmX_30.Summer, fit_n.SST_xmX_30.Summer, fit_lg.SST_xmX_30.Summer), legendtext = plot.legend)
qqcomp(list(fit_w.SST_xmX_30.Summer, fit_g.SST_xmX_30.Summer, fit_ln.SST_xmX_30.Summer, fit_n.SST_xmX_30.Summer, fit_lg.SST_xmX_30.Summer), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_xmX_30.Summer, fit_g.SST_xmX_30.Summer, fit_ln.SST_xmX_30.Summer, fit_n.SST_xmX_30.Summer, fit_lg.SST_xmX_30.Summer), legendtext = plot.legend)
gofstat(list(fit_w.SST_xmX_30.Summer, fit_g.SST_xmX_30.Summer, fit_ln.SST_xmX_30.Summer, fit_n.SST_xmX_30.Summer, fit_lg.SST_xmX_30.Summer), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_xmX_30.Summer <- bootdist(fit_w.SST_xmX_30.Summer, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_xmX_30.Summer <- rtrunc(mcTrials, spec = "weibull", shape = 54.07186, scale = 30.23875, a = 28.30, b = 30.70)
hist(dist_SST_xmX_30.Summer)
summary(dataSummer$SST_xmX_30)

# dist_SST_maxX_3.Summer
fit_w.SST_maxX_3.Summer  <- fitdist(dataSummer$SST_maxX_3, "weibull")
fit_g.SST_maxX_3.Summer  <- fitdist(dataSummer$SST_maxX_3, "gamma")
fit_ln.SST_maxX_3.Summer <- fitdist(dataSummer$SST_maxX_3, "lnorm")
fit_n.SST_maxX_3.Summer <- fitdist(dataSummer$SST_maxX_3, "norm")
fit_lg.SST_maxX_3.Summer <- fitdist(dataSummer$SST_maxX_3, "logis")
denscomp(list(fit_w.SST_maxX_3.Summer, fit_g.SST_maxX_3.Summer, fit_ln.SST_maxX_3.Summer, fit_n.SST_maxX_3.Summer, fit_lg.SST_maxX_3.Summer), legendtext = plot.legend)
qqcomp(list(fit_w.SST_maxX_3.Summer, fit_g.SST_maxX_3.Summer, fit_ln.SST_maxX_3.Summer, fit_n.SST_maxX_3.Summer, fit_lg.SST_maxX_3.Summer), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_maxX_3.Summer, fit_g.SST_maxX_3.Summer, fit_ln.SST_maxX_3.Summer, fit_n.SST_maxX_3.Summer, fit_lg.SST_maxX_3.Summer), legendtext = plot.legend)
gofstat(list(fit_w.SST_maxX_3.Summer, fit_g.SST_maxX_3.Summer, fit_ln.SST_maxX_3.Summer, fit_n.SST_maxX_3.Summer, fit_lg.SST_maxX_3.Summer), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_maxX_3.Summer <- bootdist(fit_lg.SST_maxX_3.Summer, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_maxX_3.Summer <- rtrunc(mcTrials, spec = "logis", location = 29.9404352, scale = 0.2969591, a = 29.50 , b = 31.10 )
hist(dist_SST_maxX_3.Summer)
summary(dataSummer$SST_maxX_3)

# dist_SST_imX_20.Summer
fit_w.SST_imX_20.Summer  <- fitdist(dataSummer$SST_imX_20, "weibull")
fit_g.SST_imX_20.Summer  <- fitdist(dataSummer$SST_imX_20, "gamma")
fit_ln.SST_imX_20.Summer <- fitdist(dataSummer$SST_imX_20, "lnorm")
fit_n.SST_imX_20.Summer <- fitdist(dataSummer$SST_imX_20, "norm")
fit_lg.SST_imX_20.Summer <- fitdist(dataSummer$SST_imX_20, "logis")
denscomp(list(fit_w.SST_imX_20.Summer, fit_g.SST_imX_20.Summer, fit_ln.SST_imX_20.Summer, fit_n.SST_imX_20.Summer, fit_lg.SST_imX_20.Summer), legendtext = plot.legend)
qqcomp(list(fit_w.SST_imX_20.Summer, fit_g.SST_imX_20.Summer, fit_ln.SST_imX_20.Summer, fit_n.SST_imX_20.Summer, fit_lg.SST_imX_20.Summer), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_imX_20.Summer, fit_g.SST_imX_20.Summer, fit_ln.SST_imX_20.Summer, fit_n.SST_imX_20.Summer, fit_lg.SST_imX_20.Summer), legendtext = plot.legend)
gofstat(list(fit_w.SST_imX_20.Summer, fit_g.SST_imX_20.Summer, fit_ln.SST_imX_20.Summer, fit_n.SST_imX_20.Summer, fit_lg.SST_imX_20.Summer), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_imX_20.Summer <- bootdist(fit_lg.SST_imX_20.Summer, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_imX_20.Summer <- rtrunc(mcTrials, spec = "logis", location = 29.2239943, scale = 0.2691444 , a = 28.65, b = 30.00)
hist(dist_SST_imX_20.Summer)
summary(dataSummer$SST_imX_20)

# dist_SST_xmX_25.Summer
fit_w.SST_xmX_25.Summer  <- fitdist(dataSummer$SST_xmX_25, "weibull")
fit_g.SST_xmX_25.Summer  <- fitdist(dataSummer$SST_xmX_25, "gamma")
fit_ln.SST_xmX_25.Summer <- fitdist(dataSummer$SST_xmX_25, "lnorm")
fit_n.SST_xmX_25.Summer <- fitdist(dataSummer$SST_xmX_25, "norm")
fit_lg.SST_xmX_25.Summer <- fitdist(dataSummer$SST_xmX_25, "logis")
denscomp(list(fit_w.SST_xmX_25.Summer, fit_g.SST_xmX_25.Summer, fit_ln.SST_xmX_25.Summer, fit_n.SST_xmX_25.Summer, fit_lg.SST_xmX_25.Summer), legendtext = plot.legend)
qqcomp(list(fit_w.SST_xmX_25.Summer, fit_g.SST_xmX_25.Summer, fit_ln.SST_xmX_25.Summer, fit_n.SST_xmX_25.Summer, fit_lg.SST_xmX_25.Summer), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_xmX_25.Summer, fit_g.SST_xmX_25.Summer, fit_ln.SST_xmX_25.Summer, fit_n.SST_xmX_25.Summer, fit_lg.SST_xmX_25.Summer), legendtext = plot.legend)
gofstat(list(fit_w.SST_xmX_25.Summer, fit_g.SST_xmX_25.Summer, fit_ln.SST_xmX_25.Summer, fit_n.SST_xmX_25.Summer, fit_lg.SST_xmX_25.Summer), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_xmX_25.Summer <- bootdist(fit_w.SST_xmX_25.Summer, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_xmX_25.Summer <- rtrunc(mcTrials, spec = "weibull", shape = 99.98268, scale = 30.32606, a = 29.00, b = 30.55)
hist(dist_SST_xmX_25.Summer)
summary(dataSummer$SST_xmX_25)

# dist_WSX_2.Summer
fit_w.WSX_2.Summer  <- fitdist(dataSummer$WSX_2, "weibull")
fit_g.WSX_2.Summer  <- fitdist(dataSummer$WSX_2, "gamma")
fit_ln.WSX_2.Summer <- fitdist(dataSummer$WSX_2, "lnorm")
fit_n.WSX_2.Summer <- fitdist(dataSummer$WSX_2, "norm")
fit_lg.WSX_2.Summer <- fitdist(dataSummer$WSX_2, "logis")
denscomp(list(fit_w.WSX_2.Summer, fit_g.WSX_2.Summer, fit_ln.WSX_2.Summer, fit_n.WSX_2.Summer, fit_lg.WSX_2.Summer), legendtext = plot.legend)
qqcomp(list(fit_w.WSX_2.Summer, fit_g.WSX_2.Summer, fit_ln.WSX_2.Summer, fit_n.WSX_2.Summer, fit_lg.WSX_2.Summer), legendtext = plot.legend)
cdfcomp(list(fit_w.WSX_2.Summer, fit_g.WSX_2.Summer, fit_ln.WSX_2.Summer, fit_n.WSX_2.Summer, fit_lg.WSX_2.Summer), legendtext = plot.legend)
gofstat(list(fit_w.WSX_2.Summer, fit_g.WSX_2.Summer, fit_ln.WSX_2.Summer, fit_n.WSX_2.Summer, fit_lg.WSX_2.Summer), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.WSX_2.Summer <- bootdist(fit_ln.WSX_2.Summer, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_WSX_2.Summer <- rtrunc(mcTrials, spec = "lnorm", meanlog = 1.005910, sdlog = 0.440703, a = 1.900, b = 5.700)
hist(dist_WSX_2.Summer)
summary(dataSummer$WSX_2)

# ________________Fall
dataFall <- climData %>% dplyr::select("Season", "SST_avgX_3", "SST_imX_10", "SST_maxX_0", 
                                       "SST_xmX_15", "SST_xmX_30", "SST_maxX_3", "SST_imX_20", "SST_xmX_25", "WSX_2") %>% 
  dplyr::filter(Season == "Fall")

# dist_SST_avgX_3.Fall
fit_w.SST_avgX_3.Fall  <- fitdist(dataFall$SST_avgX_3, "weibull")
fit_g.SST_avgX_3.Fall  <- fitdist(dataFall$SST_avgX_3, "gamma")
fit_ln.SST_avgX_3.Fall <- fitdist(dataFall$SST_avgX_3, "lnorm")
fit_n.SST_avgX_3.Fall <- fitdist(dataFall$SST_avgX_3, "norm")
fit_lg.SST_avgX_3.Fall <- fitdist(dataFall$SST_avgX_3, "logis")
denscomp(list(fit_w.SST_avgX_3.Fall, fit_g.SST_avgX_3.Fall, fit_ln.SST_avgX_3.Fall, fit_n.SST_avgX_3.Fall, fit_lg.SST_avgX_3.Fall), legendtext = plot.legend)
qqcomp(list(fit_w.SST_avgX_3.Fall, fit_g.SST_avgX_3.Fall, fit_ln.SST_avgX_3.Fall, fit_n.SST_avgX_3.Fall, fit_lg.SST_avgX_3.Fall), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_avgX_3.Fall, fit_g.SST_avgX_3.Fall, fit_ln.SST_avgX_3.Fall, fit_n.SST_avgX_3.Fall, fit_lg.SST_avgX_3.Fall), legendtext = plot.legend)
gofstat(list(fit_w.SST_avgX_3.Fall, fit_g.SST_avgX_3.Fall, fit_ln.SST_avgX_3.Fall, fit_n.SST_avgX_3.Fall, fit_lg.SST_avgX_3.Fall), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_avgX_3.Fall <- bootdist(fit_lg.SST_avgX_3.Fall, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_avgX_3.Fall <- rtrunc(mcTrials, spec = "logis", location = 25.026993, scale = 1.109556, a = 21.91, b = 27.90)
hist(dist_SST_avgX_3.Fall)
summary(dataFall$SST_avgX_3)

# dist_SST_imX_10.Fall
fit_w.SST_imX_10.Fall  <- fitdist(dataFall$SST_imX_10, "weibull")
fit_g.SST_imX_10.Fall  <- fitdist(dataFall$SST_imX_10, "gamma")
fit_ln.SST_imX_10.Fall <- fitdist(dataFall$SST_imX_10, "lnorm")
fit_n.SST_imX_10.Fall <- fitdist(dataFall$SST_imX_10, "norm")
fit_lg.SST_imX_10.Fall <- fitdist(dataFall$SST_imX_10, "logis")
denscomp(list(fit_w.SST_imX_10.Fall, fit_g.SST_imX_10.Fall, fit_ln.SST_imX_10.Fall, fit_n.SST_imX_10.Fall, fit_lg.SST_imX_10.Fall), legendtext = plot.legend)
qqcomp(list(fit_w.SST_imX_10.Fall, fit_g.SST_imX_10.Fall, fit_ln.SST_imX_10.Fall, fit_n.SST_imX_10.Fall, fit_lg.SST_imX_10.Fall), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_imX_10.Fall, fit_g.SST_imX_10.Fall, fit_ln.SST_imX_10.Fall, fit_n.SST_imX_10.Fall, fit_lg.SST_imX_10.Fall), legendtext = plot.legend)
gofstat(list(fit_w.SST_imX_10.Fall, fit_g.SST_imX_10.Fall, fit_ln.SST_imX_10.Fall, fit_n.SST_imX_10.Fall, fit_lg.SST_imX_10.Fall), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_imX_10.Fall <- bootdist(fit_w.SST_imX_10.Fall, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_imX_10.Fall <- rtrunc(mcTrials, spec = "weibull", shape = 13.81255, scale = 25.71948, a = 21.55, b = 27.90)
hist(dist_SST_imX_10.Fall)
summary(dataFall$SST_imX_10)

# dist_SST_maxX_0.Fall
fit_w.SST_maxX_0.Fall  <- fitdist(dataFall$SST_maxX_0, "weibull")
fit_g.SST_maxX_0.Fall  <- fitdist(dataFall$SST_maxX_0, "gamma")
fit_ln.SST_maxX_0.Fall <- fitdist(dataFall$SST_maxX_0, "lnorm")
fit_n.SST_maxX_0.Fall <- fitdist(dataFall$SST_maxX_0, "norm")
fit_lg.SST_maxX_0.Fall <- fitdist(dataFall$SST_maxX_0, "logis")
denscomp(list(fit_w.SST_maxX_0.Fall, fit_g.SST_maxX_0.Fall, fit_ln.SST_maxX_0.Fall, fit_n.SST_maxX_0.Fall, fit_lg.SST_maxX_0.Fall), legendtext = plot.legend)
qqcomp(list(fit_w.SST_maxX_0.Fall, fit_g.SST_maxX_0.Fall, fit_ln.SST_maxX_0.Fall, fit_n.SST_maxX_0.Fall, fit_lg.SST_maxX_0.Fall), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_maxX_0.Fall, fit_g.SST_maxX_0.Fall, fit_ln.SST_maxX_0.Fall, fit_n.SST_maxX_0.Fall, fit_lg.SST_maxX_0.Fall), legendtext = plot.legend)
gofstat(list(fit_w.SST_maxX_0.Fall, fit_g.SST_maxX_0.Fall, fit_ln.SST_maxX_0.Fall, fit_n.SST_maxX_0.Fall, fit_lg.SST_maxX_0.Fall), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_maxX_0.Fall <- bootdist(fit_lg.SST_maxX_0.Fall, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_maxX_0.Fall <- rtrunc(mcTrials, spec = "logis", location = 24.997610, scale = 1.348541, a = 21.60, b = 28.80)
hist(dist_SST_maxX_0.Fall)
summary(dataFall$SST_maxX_0)

# dist_SST_xmX_15.Fall
fit_w.SST_xmX_15.Fall  <- fitdist(dataFall$SST_xmX_15, "weibull")
fit_g.SST_xmX_15.Fall  <- fitdist(dataFall$SST_xmX_15, "gamma")
fit_ln.SST_xmX_15.Fall <- fitdist(dataFall$SST_xmX_15, "lnorm")
fit_n.SST_xmX_15.Fall <- fitdist(dataFall$SST_xmX_15, "norm")
fit_lg.SST_xmX_15.Fall <- fitdist(dataFall$SST_xmX_15, "logis")
denscomp(list(fit_w.SST_xmX_15.Fall, fit_g.SST_xmX_15.Fall, fit_ln.SST_xmX_15.Fall, fit_n.SST_xmX_15.Fall, fit_lg.SST_xmX_15.Fall), legendtext = plot.legend)
qqcomp(list(fit_w.SST_xmX_15.Fall, fit_g.SST_xmX_15.Fall, fit_ln.SST_xmX_15.Fall, fit_n.SST_xmX_15.Fall, fit_lg.SST_xmX_15.Fall), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_xmX_15.Fall, fit_g.SST_xmX_15.Fall, fit_ln.SST_xmX_15.Fall, fit_n.SST_xmX_15.Fall, fit_lg.SST_xmX_15.Fall), legendtext = plot.legend)
gofstat(list(fit_w.SST_xmX_15.Fall, fit_g.SST_xmX_15.Fall, fit_ln.SST_xmX_15.Fall, fit_n.SST_xmX_15.Fall, fit_lg.SST_xmX_15.Fall), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_xmX_15.Fall <- bootdist(fit_lg.SST_xmX_15.Fall, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_xmX_15.Fall <- rtrunc(mcTrials, spec = "logis", location = 25.92957, scale = 1.44491, a = 22.20, b = 29.90)
hist(dist_SST_xmX_15.Fall)
summary(dataFall$SST_xmX_15)

# dist_SST_xmX_30.Fall
fit_w.SST_xmX_30.Fall  <- fitdist(dataFall$SST_xmX_30, "weibull")
fit_g.SST_xmX_30.Fall  <- fitdist(dataFall$SST_xmX_30, "gamma")
fit_ln.SST_xmX_30.Fall <- fitdist(dataFall$SST_xmX_30, "lnorm")
fit_n.SST_xmX_30.Fall <- fitdist(dataFall$SST_xmX_30, "norm")
fit_lg.SST_xmX_30.Fall <- fitdist(dataFall$SST_xmX_30, "logis")
denscomp(list(fit_w.SST_xmX_30.Fall, fit_g.SST_xmX_30.Fall, fit_ln.SST_xmX_30.Fall, fit_n.SST_xmX_30.Fall, fit_lg.SST_xmX_30.Fall), legendtext = plot.legend)
qqcomp(list(fit_w.SST_xmX_30.Fall, fit_g.SST_xmX_30.Fall, fit_ln.SST_xmX_30.Fall, fit_n.SST_xmX_30.Fall, fit_lg.SST_xmX_30.Fall), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_xmX_30.Fall, fit_g.SST_xmX_30.Fall, fit_ln.SST_xmX_30.Fall, fit_n.SST_xmX_30.Fall, fit_lg.SST_xmX_30.Fall), legendtext = plot.legend)
gofstat(list(fit_w.SST_xmX_30.Fall, fit_g.SST_xmX_30.Fall, fit_ln.SST_xmX_30.Fall, fit_n.SST_xmX_30.Fall, fit_lg.SST_xmX_30.Fall), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_xmX_30.Fall <- bootdist(fit_w.SST_xmX_30.Fall, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_xmX_30.Fall <- rtrunc(mcTrials, spec = "weibull", shape = 13.98767, scale = 27.44430, a = 22.85, b = 29.75)
hist(dist_SST_xmX_30.Fall)
summary(dataFall$SST_xmX_30)

# dist_SST_maxX_3.Fall
fit_w.SST_maxX_3.Fall  <- fitdist(dataFall$SST_maxX_3, "weibull")
fit_g.SST_maxX_3.Fall  <- fitdist(dataFall$SST_maxX_3, "gamma")
fit_ln.SST_maxX_3.Fall <- fitdist(dataFall$SST_maxX_3, "lnorm")
fit_n.SST_maxX_3.Fall <- fitdist(dataFall$SST_maxX_3, "norm")
fit_lg.SST_maxX_3.Fall <- fitdist(dataFall$SST_maxX_3, "logis")
denscomp(list(fit_w.SST_maxX_3.Fall, fit_g.SST_maxX_3.Fall, fit_ln.SST_maxX_3.Fall, fit_n.SST_maxX_3.Fall, fit_lg.SST_maxX_3.Fall), legendtext = plot.legend)
qqcomp(list(fit_w.SST_maxX_3.Fall, fit_g.SST_maxX_3.Fall, fit_ln.SST_maxX_3.Fall, fit_n.SST_maxX_3.Fall, fit_lg.SST_maxX_3.Fall), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_maxX_3.Fall, fit_g.SST_maxX_3.Fall, fit_ln.SST_maxX_3.Fall, fit_n.SST_maxX_3.Fall, fit_lg.SST_maxX_3.Fall), legendtext = plot.legend)
gofstat(list(fit_w.SST_maxX_3.Fall, fit_g.SST_maxX_3.Fall, fit_ln.SST_maxX_3.Fall, fit_n.SST_maxX_3.Fall, fit_lg.SST_maxX_3.Fall), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_maxX_3.Fall <- bootdist(fit_lg.SST_maxX_3.Fall, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_maxX_3.Fall <- rtrunc(mcTrials, spec = "logis", location = 25.428854, scale = 1.223208, a = 22.10, b = 28.30)
hist(dist_SST_maxX_3.Fall)
summary(dataFall$SST_maxX_3)

# dist_SST_imX_20.Fall
fit_w.SST_imX_20.Fall  <- fitdist(dataFall$SST_imX_20, "weibull")
fit_g.SST_imX_20.Fall  <- fitdist(dataFall$SST_imX_20, "gamma")
fit_ln.SST_imX_20.Fall <- fitdist(dataFall$SST_imX_20, "lnorm")
fit_n.SST_imX_20.Fall <- fitdist(dataFall$SST_imX_20, "norm")
fit_lg.SST_imX_20.Fall <- fitdist(dataFall$SST_imX_20, "logis")
denscomp(list(fit_w.SST_imX_20.Fall, fit_g.SST_imX_20.Fall, fit_ln.SST_imX_20.Fall, fit_n.SST_imX_20.Fall, fit_lg.SST_imX_20.Fall), legendtext = plot.legend)
qqcomp(list(fit_w.SST_imX_20.Fall, fit_g.SST_imX_20.Fall, fit_ln.SST_imX_20.Fall, fit_n.SST_imX_20.Fall, fit_lg.SST_imX_20.Fall), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_imX_20.Fall, fit_g.SST_imX_20.Fall, fit_ln.SST_imX_20.Fall, fit_n.SST_imX_20.Fall, fit_lg.SST_imX_20.Fall), legendtext = plot.legend)
gofstat(list(fit_w.SST_imX_20.Fall, fit_g.SST_imX_20.Fall, fit_ln.SST_imX_20.Fall, fit_n.SST_imX_20.Fall, fit_lg.SST_imX_20.Fall), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_imX_20.Fall <- bootdist(fit_w.SST_imX_20.Fall, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_imX_20.Fall <- rtrunc(mcTrials, spec = "weibull", shape = 12.14105, scale = 26.74106, a = 21.75, b = 29.00)
hist(dist_SST_imX_20.Fall)
summary(dataFall$SST_imX_20)

# dist_SST_xmX_25.Fall
fit_w.SST_xmX_25.Fall  <- fitdist(dataFall$SST_xmX_25, "weibull")
fit_g.SST_xmX_25.Fall  <- fitdist(dataFall$SST_xmX_25, "gamma")
fit_ln.SST_xmX_25.Fall <- fitdist(dataFall$SST_xmX_25, "lnorm")
fit_n.SST_xmX_25.Fall <- fitdist(dataFall$SST_xmX_25, "norm")
fit_lg.SST_xmX_25.Fall <- fitdist(dataFall$SST_xmX_25, "logis")
denscomp(list(fit_w.SST_xmX_25.Fall, fit_g.SST_xmX_25.Fall, fit_ln.SST_xmX_25.Fall, fit_n.SST_xmX_25.Fall, fit_lg.SST_xmX_25.Fall), legendtext = plot.legend)
qqcomp(list(fit_w.SST_xmX_25.Fall, fit_g.SST_xmX_25.Fall, fit_ln.SST_xmX_25.Fall, fit_n.SST_xmX_25.Fall, fit_lg.SST_xmX_25.Fall), legendtext = plot.legend)
cdfcomp(list(fit_w.SST_xmX_25.Fall, fit_g.SST_xmX_25.Fall, fit_ln.SST_xmX_25.Fall, fit_n.SST_xmX_25.Fall, fit_lg.SST_xmX_25.Fall), legendtext = plot.legend)
gofstat(list(fit_w.SST_xmX_25.Fall, fit_g.SST_xmX_25.Fall, fit_ln.SST_xmX_25.Fall, fit_n.SST_xmX_25.Fall, fit_lg.SST_xmX_25.Fall), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.SST_xmX_25.Fall <- bootdist(fit_w.SST_xmX_25.Fall, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_SST_xmX_25.Fall <- rtrunc(mcTrials, spec = "weibull", shape = 12.73996, scale = 27.26218, a = 22.60, b = 29.60)
hist(dist_SST_xmX_25.Fall)
summary(dataFall$SST_xmX_25)

# dist_WSX_2.Fall
fit_w.WSX_2.Fall  <- fitdist(dataFall$WSX_2, "weibull")
fit_g.WSX_2.Fall  <- fitdist(dataFall$WSX_2, "gamma")
fit_ln.WSX_2.Fall <- fitdist(dataFall$WSX_2, "lnorm")
fit_n.WSX_2.Fall <- fitdist(dataFall$WSX_2, "norm")
fit_lg.WSX_2.Fall <- fitdist(dataFall$WSX_2, "logis")
denscomp(list(fit_w.WSX_2.Fall, fit_g.WSX_2.Fall, fit_ln.WSX_2.Fall, fit_n.WSX_2.Fall, fit_lg.WSX_2.Fall), legendtext = plot.legend)
qqcomp(list(fit_w.WSX_2.Fall, fit_g.WSX_2.Fall, fit_ln.WSX_2.Fall, fit_n.WSX_2.Fall, fit_lg.WSX_2.Fall), legendtext = plot.legend)
cdfcomp(list(fit_w.WSX_2.Fall, fit_g.WSX_2.Fall, fit_ln.WSX_2.Fall, fit_n.WSX_2.Fall, fit_lg.WSX_2.Fall), legendtext = plot.legend)
gofstat(list(fit_w.WSX_2.Fall, fit_g.WSX_2.Fall, fit_ln.WSX_2.Fall, fit_n.WSX_2.Fall, fit_lg.WSX_2.Fall), fitnames = c("Weibull", "gamma", "lognormal", "normal", "logistic"))
summary(bootstrap.WSX_2.Fall <- bootdist(fit_w.WSX_2.Fall, niter = mcTrials)) # choose the best fit dist based on AIC and QQ plot
dist_WSX_2.Fall <- rtrunc(mcTrials, spec = "weibull", shape = 6.900943, scale = 5.689968, a = 3.800, b = 6.500)
hist(dist_WSX_2.Fall)
summary(dataFall$WSX_2)

#______ 2016 - 2035 #############
climData_20162035_Oyster <- dplyr::select(climData, c(Type, Season, SST_avgX_3, SST_imX_10, SST_maxX_0, 
                                                      SST_xmX_15, SST_xmX_30, SST_maxX_3, SST_imX_20, SST_xmX_25, WSX_2)) %>% 
  dplyr::filter(Type == "Oyster")


# Concentration of Vp in oysters right after the harvest ##################
# _____ VpCon Baseline ###################
par(mfrow = c(2,2))
#1 VpConcHarvest_Baseline_Winter
dataWinterBaseline <- dplyr::select(allData, c(Conc, Season)) %>% dplyr::filter(Season == "Winter")
dataWinterBaseline$Conc[is.na(dataWinterBaseline$Conc)] <- 1.5
dataWinterBaseline$Conc <- log10(dataWinterBaseline$Conc)
dataWinter_Baseline <- sample(dataWinterBaseline$Conc, size=mcTrials, replace=TRUE)
VpConcHarvest_Baseline_Winter <- mcdata(dataWinter_Baseline, type = "V")
hist(VpConcHarvest_Baseline_Winter)

#2 VpConcHarvest_Baseline_Spring
dataSpringBaseline <- dplyr::select(allData, c(Conc, Season)) %>% dplyr::filter(Season == "Spring")
dataSpringBaseline$Conc[is.na(dataSpringBaseline$Conc)] <- 1.5
dataSpringBaseline$Conc <- log10(dataSpringBaseline$Conc)
dataSpring_Baseline <- sample(dataSpringBaseline$Conc, size=mcTrials, replace=TRUE)
VpConcHarvest_Baseline_Spring <- mcdata(dataSpring_Baseline, type = "V")
hist(VpConcHarvest_Baseline_Spring)

#3 VpConcHarvest_Baseline_Summer
dataSummerBaseline <- dplyr::select(allData, c(Conc, Season)) %>% dplyr::filter(Season == "Summer")
dataSummerBaseline$Conc[is.na(dataSummerBaseline$Conc)] <- 1.5
dataSummerBaseline$Conc <- log10(dataSummerBaseline$Conc)
dataSummer_Baseline <- sample(dataSummerBaseline$Conc, size=mcTrials, replace=TRUE)
VpConcHarvest_Baseline_Summer <- mcdata(dataSummer_Baseline, type = "V")
hist(VpConcHarvest_Baseline_Summer)

#4 VpConcHarvest_Baseline_Fall
dataFallBaseline <- dplyr::select(allData, c(Conc, Season)) %>% dplyr::filter(Season == "Fall")
dataFallBaseline$Conc[is.na(dataFallBaseline$Conc)] <- 1.5
dataFallBaseline$Conc <- log10(dataFallBaseline$Conc)
dataFall_Baseline <- sample(dataFallBaseline$Conc, size=mcTrials, replace=TRUE)
VpConcHarvest_Baseline_Fall <- mcdata(dataFall_Baseline, type = "V")
hist(VpConcHarvest_Baseline_Fall)

# _____ VpConc 2016 - 2035 ################
# ____________ RCP2.6 ################
dataWinter <- climData %>% 
  dplyr::select("Season", "SST_avgX_3", "SST_imX_10", "SST_maxX_0", 
                "SST_xmX_15", "SST_xmX_30", "SST_maxX_3", "SST_imX_20", "SST_xmX_25", "WSX_2") %>% 
  dplyr::filter(Season == "Winter")
dataWinter <- dataWinter %>%  dplyr::select(-c(Season))
dataWinter2016_2036.RCP2.6 <- data.frame(SST_avgX_3 = sample(dataWinter$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4),
                          SST_imX_10 = sample(dataWinter$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                          SST_maxX_0 = sample(dataWinter$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                          SST_xmX_15 = sample(dataWinter$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                          SST_xmX_30 = sample(dataWinter$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                          SST_maxX_3 = sample(dataWinter$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                          SST_imX_20 = sample(dataWinter$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                          SST_xmX_25 = sample(dataWinter$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                          WSX_2 = sample(dataWinter$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP2.6_Winter <- predict(final_rf.QMRA, dataWinter2016_2036.RCP2.6)
(VpConcHarvest_2016_2036.RCP2.6_Winter <- mcdata(predict_VpConcHarvest_2016_2036.RCP2.6_Winter, type = "V"))

dataSpring <- climData %>% 
  dplyr::select("Season", "SST_avgX_3", "SST_imX_10", "SST_maxX_0", 
                "SST_xmX_15", "SST_xmX_30", "SST_maxX_3", "SST_imX_20", "SST_xmX_25", "WSX_2") %>% 
  dplyr::filter(Season == "Spring")
dataSpring <- dataSpring %>%  dplyr::select(-c(Season))
dataSpring2016_2036.RCP2.6 <- data.frame(SST_avgX_3 = sample(dataSpring$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4),
                                         SST_imX_10 = sample(dataSpring$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         SST_maxX_0 = sample(dataSpring$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         SST_xmX_15 = sample(dataSpring$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         SST_xmX_30 = sample(dataSpring$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         SST_maxX_3 = sample(dataSpring$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         SST_imX_20 = sample(dataSpring$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         SST_xmX_25 = sample(dataSpring$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         WSX_2 = sample(dataSpring$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP2.6_Spring <- predict(final_rf.QMRA, dataSpring2016_2036.RCP2.6)
(VpConcHarvest_2016_2036.RCP2.6_Spring <- mcdata(predict_VpConcHarvest_2016_2036.RCP2.6_Spring, type = "V"))

dataSummer <- climData %>% 
  dplyr::select("Season", "SST_avgX_3", "SST_imX_10", "SST_maxX_0", 
                "SST_xmX_15", "SST_xmX_30", "SST_maxX_3", "SST_imX_20", "SST_xmX_25", "WSX_2") %>% 
  dplyr::filter(Season == "Summer")
dataSummer <- dataSummer %>%  dplyr::select(-c(Season))
dataSummer2016_2036.RCP2.6 <- data.frame(SST_avgX_3 = sample(dataSummer$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4),
                                         SST_imX_10 = sample(dataSummer$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         SST_maxX_0 = sample(dataSummer$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         SST_xmX_15 = sample(dataSummer$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         SST_xmX_30 = sample(dataSummer$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         SST_maxX_3 = sample(dataSummer$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         SST_imX_20 = sample(dataSummer$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         SST_xmX_25 = sample(dataSummer$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                         WSX_2 = sample(dataSummer$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP2.6_Summer <- predict(final_rf.QMRA, dataSummer2016_2036.RCP2.6)
(VpConcHarvest_2016_2036.RCP2.6_Summer <- mcdata(predict_VpConcHarvest_2016_2036.RCP2.6_Summer, type = "V"))

dataFall <- climData %>% 
  dplyr::select("Season", "SST_avgX_3", "SST_imX_10", "SST_maxX_0", 
                "SST_xmX_15", "SST_xmX_30", "SST_maxX_3", "SST_imX_20", "SST_xmX_25", "WSX_2") %>% 
  dplyr::filter(Season == "Fall")
dataFall <- dataFall %>%  dplyr::select(-c(Season))
dataFall2016_2036.RCP2.6 <- data.frame(SST_avgX_3 = sample(dataFall$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4),
                                       SST_imX_10 = sample(dataFall$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                       SST_maxX_0 = sample(dataFall$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                       SST_xmX_15 = sample(dataFall$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                       SST_xmX_30 = sample(dataFall$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                       SST_maxX_3 = sample(dataFall$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                       SST_imX_20 = sample(dataFall$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                       SST_xmX_25 = sample(dataFall$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.42, mode=0.65, max=1.2, shape=4), 
                                       WSX_2 = sample(dataFall$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP2.6_Fall <- predict(final_rf.QMRA, dataFall2016_2036.RCP2.6)
(VpConcHarvest_2016_2036.RCP2.6_Fall <- mcdata(predict_VpConcHarvest_2016_2036.RCP2.6_Fall, type = "V"))

# ____________ RCP4.5 ################
dataWinter2016_2036.RCP4.5 <- data.frame(SST_avgX_3 = sample(dataWinter$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4),
                                         SST_imX_10 = sample(dataWinter$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_maxX_0 = sample(dataWinter$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_xmX_15 = sample(dataWinter$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_xmX_30 = sample(dataWinter$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_maxX_3 = sample(dataWinter$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_imX_20 = sample(dataWinter$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_xmX_25 = sample(dataWinter$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         WSX_2 = sample(dataWinter$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP4.5_Winter <- predict(final_rf.QMRA, dataWinter2016_2036.RCP4.5)
(VpConcHarvest_2016_2036.RCP4.5_Winter <- mcdata(predict_VpConcHarvest_2016_2036.RCP4.5_Winter, type = "V"))

dataSpring2016_2036.RCP4.5 <- data.frame(SST_avgX_3 = sample(dataSpring$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4),
                                         SST_imX_10 = sample(dataSpring$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_maxX_0 = sample(dataSpring$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_xmX_15 = sample(dataSpring$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_xmX_30 = sample(dataSpring$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_maxX_3 = sample(dataSpring$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_imX_20 = sample(dataSpring$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_xmX_25 = sample(dataSpring$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         WSX_2 = sample(dataSpring$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP4.5_Spring <- predict(final_rf.QMRA, dataSpring2016_2036.RCP4.5)
(VpConcHarvest_2016_2036.RCP4.5_Spring <- mcdata(predict_VpConcHarvest_2016_2036.RCP4.5_Spring, type = "V"))

dataSummer2016_2036.RCP4.5 <- data.frame(SST_avgX_3 = sample(dataSummer$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4),
                                         SST_imX_10 = sample(dataSummer$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_maxX_0 = sample(dataSummer$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_xmX_15 = sample(dataSummer$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_xmX_30 = sample(dataSummer$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_maxX_3 = sample(dataSummer$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_imX_20 = sample(dataSummer$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         SST_xmX_25 = sample(dataSummer$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                         WSX_2 = sample(dataSummer$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP4.5_Summer <- predict(final_rf.QMRA, dataSummer2016_2036.RCP4.5)
(VpConcHarvest_2016_2036.RCP4.5_Summer <- mcdata(predict_VpConcHarvest_2016_2036.RCP4.5_Summer, type = "V"))

dataFall2016_2036.RCP4.5 <- data.frame(SST_avgX_3 = sample(dataFall$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4),
                                       SST_imX_10 = sample(dataFall$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                       SST_maxX_0 = sample(dataFall$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                       SST_xmX_15 = sample(dataFall$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                       SST_xmX_30 = sample(dataFall$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                       SST_maxX_3 = sample(dataFall$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                       SST_imX_20 = sample(dataFall$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                       SST_xmX_25 = sample(dataFall$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.48, mode=0.7, max=1.0, shape=4), 
                                       WSX_2 = sample(dataFall$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP4.5_Fall <- predict(final_rf.QMRA, dataFall2016_2036.RCP4.5)
(VpConcHarvest_2016_2036.RCP4.5_Fall <- mcdata(predict_VpConcHarvest_2016_2036.RCP4.5_Fall, type = "V"))

# ____________ RCP6.0 ################
dataWinter2016_2036.RCP6.0 <- data.frame(SST_avgX_3 = sample(dataWinter$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4),
                                         SST_imX_10 = sample(dataWinter$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_maxX_0 = sample(dataWinter$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_xmX_15 = sample(dataWinter$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_xmX_30 = sample(dataWinter$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_maxX_3 = sample(dataWinter$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_imX_20 = sample(dataWinter$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_xmX_25 = sample(dataWinter$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         WSX_2 = sample(dataWinter$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP6.0_Winter <- predict(final_rf.QMRA, dataWinter2016_2036.RCP6.0)
(VpConcHarvest_2016_2036.RCP6.0_Winter <- mcdata(predict_VpConcHarvest_2016_2036.RCP6.0_Winter, type = "V"))

dataSpring2016_2036.RCP6.0 <- data.frame(SST_avgX_3 = sample(dataSpring$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4),
                                         SST_imX_10 = sample(dataSpring$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_maxX_0 = sample(dataSpring$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_xmX_15 = sample(dataSpring$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_xmX_30 = sample(dataSpring$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_maxX_3 = sample(dataSpring$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_imX_20 = sample(dataSpring$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_xmX_25 = sample(dataSpring$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         WSX_2 = sample(dataSpring$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP6.0_Spring <- predict(final_rf.QMRA, dataSpring2016_2036.RCP6.0)
(VpConcHarvest_2016_2036.RCP6.0_Spring <- mcdata(predict_VpConcHarvest_2016_2036.RCP6.0_Spring, type = "V"))

dataSummer2016_2036.RCP6.0 <- data.frame(SST_avgX_3 = sample(dataSummer$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4),
                                         SST_imX_10 = sample(dataSummer$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_maxX_0 = sample(dataSummer$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_xmX_15 = sample(dataSummer$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_xmX_30 = sample(dataSummer$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_maxX_3 = sample(dataSummer$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_imX_20 = sample(dataSummer$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         SST_xmX_25 = sample(dataSummer$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                         WSX_2 = sample(dataSummer$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP6.0_Summer <- predict(final_rf.QMRA, dataSummer2016_2036.RCP6.0)
(VpConcHarvest_2016_2036.RCP6.0_Summer <- mcdata(predict_VpConcHarvest_2016_2036.RCP6.0_Summer, type = "V"))

dataFall2016_2036.RCP6.0 <- data.frame(SST_avgX_3 = sample(dataFall$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4),
                                       SST_imX_10 = sample(dataFall$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                       SST_maxX_0 = sample(dataFall$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                       SST_xmX_15 = sample(dataFall$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                       SST_xmX_30 = sample(dataFall$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                       SST_maxX_3 = sample(dataFall$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                       SST_imX_20 = sample(dataFall$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                       SST_xmX_25 = sample(dataFall$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=0.6, max=1.0, shape=4), 
                                       WSX_2 = sample(dataFall$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP6.0_Fall <- predict(final_rf.QMRA, dataFall2016_2036.RCP6.0)
(VpConcHarvest_2016_2036.RCP6.0_Fall <- mcdata(predict_VpConcHarvest_2016_2036.RCP6.0_Fall, type = "V"))

# ____________ RCP8.5 ################
dataWinter2016_2036.RCP8.5 <- data.frame(SST_avgX_3 = sample(dataWinter$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4),
                                         SST_imX_10 = sample(dataWinter$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_maxX_0 = sample(dataWinter$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_xmX_15 = sample(dataWinter$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_xmX_30 = sample(dataWinter$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_maxX_3 = sample(dataWinter$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_imX_20 = sample(dataWinter$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_xmX_25 = sample(dataWinter$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         WSX_2 = sample(dataWinter$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP8.5_Winter <- predict(final_rf.QMRA, dataWinter2016_2036.RCP8.5)
(VpConcHarvest_2016_2036.RCP8.5_Winter <- mcdata(predict_VpConcHarvest_2016_2036.RCP8.5_Winter, type = "V"))

dataSpring2016_2036.RCP8.5 <- data.frame(SST_avgX_3 = sample(dataSpring$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4),
                                         SST_imX_10 = sample(dataSpring$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_maxX_0 = sample(dataSpring$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_xmX_15 = sample(dataSpring$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_xmX_30 = sample(dataSpring$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_maxX_3 = sample(dataSpring$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_imX_20 = sample(dataSpring$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_xmX_25 = sample(dataSpring$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         WSX_2 = sample(dataSpring$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP8.5_Spring <- predict(final_rf.QMRA, dataSpring2016_2036.RCP8.5)
(VpConcHarvest_2016_2036.RCP8.5_Spring <- mcdata(predict_VpConcHarvest_2016_2036.RCP8.5_Spring, type = "V"))

dataSummer2016_2036.RCP8.5 <- data.frame(SST_avgX_3 = sample(dataSummer$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4),
                                         SST_imX_10 = sample(dataSummer$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_maxX_0 = sample(dataSummer$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_xmX_15 = sample(dataSummer$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_xmX_30 = sample(dataSummer$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_maxX_3 = sample(dataSummer$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_imX_20 = sample(dataSummer$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         SST_xmX_25 = sample(dataSummer$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                         WSX_2 = sample(dataSummer$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP8.5_Summer <- predict(final_rf.QMRA, dataSummer2016_2036.RCP8.5)
(VpConcHarvest_2016_2036.RCP8.5_Summer <- mcdata(predict_VpConcHarvest_2016_2036.RCP8.5_Summer, type = "V"))

dataFall2016_2036.RCP8.5 <- data.frame(SST_avgX_3 = sample(dataFall$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4),
                                       SST_imX_10 = sample(dataFall$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                       SST_maxX_0 = sample(dataFall$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                       SST_xmX_15 = sample(dataFall$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                       SST_xmX_30 = sample(dataFall$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                       SST_maxX_3 = sample(dataFall$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                       SST_imX_20 = sample(dataFall$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                       SST_xmX_25 = sample(dataFall$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.50, mode=0.8, max=1.2, shape=4), 
                                       WSX_2 = sample(dataFall$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2016_2036.RCP8.5_Fall <- predict(final_rf.QMRA, dataFall2016_2036.RCP8.5)
(VpConcHarvest_2016_2036.RCP8.5_Fall <- mcdata(predict_VpConcHarvest_2016_2036.RCP8.5_Fall, type = "V"))

# _____ VpConc 2046 - 2065 ################
# ____________ RCP2.6 ################
# ____________________ Winter #################
dataWinter2046_2065.RCP2.6 <- data.frame(SST_avgX_3 = sample(dataWinter$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4),
                                         SST_imX_10 = sample(dataWinter$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_maxX_0 = sample(dataWinter$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_xmX_15 = sample(dataWinter$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_xmX_30 = sample(dataWinter$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_maxX_3 = sample(dataWinter$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_imX_20 = sample(dataWinter$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_xmX_25 = sample(dataWinter$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         WSX_2 = sample(dataWinter$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP2.6_Winter <- predict(final_rf.QMRA, dataWinter2046_2065.RCP2.6)
(VpConcHarvest_2046_2065.RCP2.6_Winter <- mcdata(predict_VpConcHarvest_2046_2065.RCP2.6_Winter, type = "V"))

# ____________________ Spring #################
dataSpring2046_2065.RCP2.6 <- data.frame(SST_avgX_3 = sample(dataSpring$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4),
                                         SST_imX_10 = sample(dataSpring$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_maxX_0 = sample(dataSpring$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_xmX_15 = sample(dataSpring$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_xmX_30 = sample(dataSpring$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_maxX_3 = sample(dataSpring$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_imX_20 = sample(dataSpring$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_xmX_25 = sample(dataSpring$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         WSX_2 = sample(dataSpring$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP2.6_Spring <- predict(final_rf.QMRA, dataSpring2046_2065.RCP2.6)
(VpConcHarvest_2046_2065.RCP2.6_Spring <- mcdata(predict_VpConcHarvest_2046_2065.RCP2.6_Spring, type = "V"))

# ____________________ Summer #################
dataSummer2046_2065.RCP2.6 <- data.frame(SST_avgX_3 = sample(dataSummer$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4),
                                         SST_imX_10 = sample(dataSummer$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_maxX_0 = sample(dataSummer$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_xmX_15 = sample(dataSummer$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_xmX_30 = sample(dataSummer$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_maxX_3 = sample(dataSummer$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_imX_20 = sample(dataSummer$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         SST_xmX_25 = sample(dataSummer$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                         WSX_2 = sample(dataSummer$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP2.6_Summer <- predict(final_rf.QMRA, dataSummer2046_2065.RCP2.6)
(VpConcHarvest_2046_2065.RCP2.6_Summer <- mcdata(predict_VpConcHarvest_2046_2065.RCP2.6_Summer, type = "V"))

# ____________________ Fall #################
dataFall2046_2065.RCP2.6 <- data.frame(SST_avgX_3 = sample(dataFall$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4),
                                       SST_imX_10 = sample(dataFall$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                       SST_maxX_0 = sample(dataFall$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                       SST_xmX_15 = sample(dataFall$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                       SST_xmX_30 = sample(dataFall$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                       SST_maxX_3 = sample(dataFall$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                       SST_imX_20 = sample(dataFall$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                       SST_xmX_25 = sample(dataFall$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.40, mode=1.0, max=1.6, shape=4), 
                                       WSX_2 = sample(dataFall$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP2.6_Fall <- predict(final_rf.QMRA, dataFall2046_2065.RCP2.6)
(VpConcHarvest_2046_2065.RCP2.6_Fall <- mcdata(predict_VpConcHarvest_2046_2065.RCP2.6_Fall, type = "V"))

# ____________ RCP4.5 ################
# ____________________ Winter #################
dataWinter2046_2065.RCP4.5 <- data.frame(SST_avgX_3 = sample(dataWinter$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4),
                                         SST_imX_10 = sample(dataWinter$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_maxX_0 = sample(dataWinter$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_xmX_15 = sample(dataWinter$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_xmX_30 = sample(dataWinter$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_maxX_3 = sample(dataWinter$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_imX_20 = sample(dataWinter$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_xmX_25 = sample(dataWinter$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         WSX_2 = sample(dataWinter$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP4.5_Winter <- predict(final_rf.QMRA, dataWinter2046_2065.RCP4.5)
(VpConcHarvest_2046_2065.RCP4.5_Winter <- mcdata(predict_VpConcHarvest_2046_2065.RCP4.5_Winter, type = "V"))

# ____________________ Spring #################
dataSpring2046_2065.RCP4.5 <- data.frame(SST_avgX_3 = sample(dataSpring$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4),
                                         SST_imX_10 = sample(dataSpring$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_maxX_0 = sample(dataSpring$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_xmX_15 = sample(dataSpring$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_xmX_30 = sample(dataSpring$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_maxX_3 = sample(dataSpring$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_imX_20 = sample(dataSpring$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_xmX_25 = sample(dataSpring$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         WSX_2 = sample(dataSpring$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP4.5_Spring <- predict(final_rf.QMRA, dataSpring2046_2065.RCP4.5)
(VpConcHarvest_2046_2065.RCP4.5_Spring <- mcdata(predict_VpConcHarvest_2046_2065.RCP4.5_Spring, type = "V"))

# ____________________ Summer #################
dataSummer2046_2065.RCP4.5 <- data.frame(SST_avgX_3 = sample(dataSummer$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4),
                                         SST_imX_10 = sample(dataSummer$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_maxX_0 = sample(dataSummer$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_xmX_15 = sample(dataSummer$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_xmX_30 = sample(dataSummer$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_maxX_3 = sample(dataSummer$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_imX_20 = sample(dataSummer$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         SST_xmX_25 = sample(dataSummer$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                         WSX_2 = sample(dataSummer$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP4.5_Summer <- predict(final_rf.QMRA, dataSummer2046_2065.RCP4.5)
(VpConcHarvest_2046_2065.RCP4.5_Summer <- mcdata(predict_VpConcHarvest_2046_2065.RCP4.5_Summer, type = "V"))

# ____________________ Fall #################
dataFall2046_2065.RCP4.5 <- data.frame(SST_avgX_3 = sample(dataFall$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4),
                                       SST_imX_10 = sample(dataFall$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                       SST_maxX_0 = sample(dataFall$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                       SST_xmX_15 = sample(dataFall$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                       SST_xmX_30 = sample(dataFall$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                       SST_maxX_3 = sample(dataFall$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                       SST_imX_20 = sample(dataFall$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                       SST_xmX_25 = sample(dataFall$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.90, mode=1.4, max=2.0, shape=4), 
                                       WSX_2 = sample(dataFall$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP4.5_Fall <- predict(final_rf.QMRA, dataFall2046_2065.RCP4.5)
(VpConcHarvest_2046_2065.RCP4.5_Fall <- mcdata(predict_VpConcHarvest_2046_2065.RCP4.5_Fall, type = "V"))

# ____________ RCP6.0 ################
# ____________________ Winter #################
dataWinter2046_2065.RCP6.0 <- data.frame(SST_avgX_3 = sample(dataWinter$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4),
                                         SST_imX_10 = sample(dataWinter$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_maxX_0 = sample(dataWinter$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_xmX_15 = sample(dataWinter$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_xmX_30 = sample(dataWinter$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_maxX_3 = sample(dataWinter$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_imX_20 = sample(dataWinter$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_xmX_25 = sample(dataWinter$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         WSX_2 = sample(dataWinter$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP6.0_Winter <- predict(final_rf.QMRA, dataWinter2046_2065.RCP6.0)
(VpConcHarvest_2046_2065.RCP6.0_Winter <- mcdata(predict_VpConcHarvest_2046_2065.RCP6.0_Winter, type = "V"))

# ____________________ Spring #################
dataSpring2046_2065.RCP6.0 <- data.frame(SST_avgX_3 = sample(dataSpring$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4),
                                         SST_imX_10 = sample(dataSpring$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_maxX_0 = sample(dataSpring$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_xmX_15 = sample(dataSpring$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_xmX_30 = sample(dataSpring$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_maxX_3 = sample(dataSpring$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_imX_20 = sample(dataSpring$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_xmX_25 = sample(dataSpring$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         WSX_2 = sample(dataSpring$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP6.0_Spring <- predict(final_rf.QMRA, dataSpring2046_2065.RCP6.0)
(VpConcHarvest_2046_2065.RCP6.0_Spring <- mcdata(predict_VpConcHarvest_2046_2065.RCP6.0_Spring, type = "V"))

# ____________________ Summer #################
dataSummer2046_2065.RCP6.0 <- data.frame(SST_avgX_3 = sample(dataSummer$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4),
                                         SST_imX_10 = sample(dataSummer$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_maxX_0 = sample(dataSummer$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_xmX_15 = sample(dataSummer$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_xmX_30 = sample(dataSummer$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_maxX_3 = sample(dataSummer$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_imX_20 = sample(dataSummer$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         SST_xmX_25 = sample(dataSummer$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                         WSX_2 = sample(dataSummer$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP6.0_Summer <- predict(final_rf.QMRA, dataSummer2046_2065.RCP6.0)
(VpConcHarvest_2046_2065.RCP6.0_Summer <- mcdata(predict_VpConcHarvest_2046_2065.RCP6.0_Summer, type = "V"))

# ____________________ Fall #################
dataFall2046_2065.RCP6.0 <- data.frame(SST_avgX_3 = sample(dataFall$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4),
                                       SST_imX_10 = sample(dataFall$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                       SST_maxX_0 = sample(dataFall$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                       SST_xmX_15 = sample(dataFall$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                       SST_xmX_30 = sample(dataFall$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                       SST_maxX_3 = sample(dataFall$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                       SST_imX_20 = sample(dataFall$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                       SST_xmX_25 = sample(dataFall$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.80, mode=1.3, max=1.8, shape=4), 
                                       WSX_2 = sample(dataFall$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP6.0_Fall <- predict(final_rf.QMRA, dataFall2046_2065.RCP6.0)
(VpConcHarvest_2046_2065.RCP6.0_Fall <- mcdata(predict_VpConcHarvest_2046_2065.RCP6.0_Fall, type = "V"))

# ____________ RCP8.5 ################
# ____________________ Winter #################
dataWinter2046_2065.RCP8.5 <- data.frame(SST_avgX_3 = sample(dataWinter$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4),
                                         SST_imX_10 = sample(dataWinter$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_maxX_0 = sample(dataWinter$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_xmX_15 = sample(dataWinter$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_xmX_30 = sample(dataWinter$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_maxX_3 = sample(dataWinter$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_imX_20 = sample(dataWinter$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_xmX_25 = sample(dataWinter$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         WSX_2 = sample(dataWinter$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP8.5_Winter <- predict(final_rf.QMRA, dataWinter2046_2065.RCP8.5)
(VpConcHarvest_2046_2065.RCP8.5_Winter <- mcdata(predict_VpConcHarvest_2046_2065.RCP8.5_Winter, type = "V"))

# ____________________ Spring #################
dataSpring2046_2065.RCP8.5 <- data.frame(SST_avgX_3 = sample(dataSpring$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4),
                                         SST_imX_10 = sample(dataSpring$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_maxX_0 = sample(dataSpring$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_xmX_15 = sample(dataSpring$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_xmX_30 = sample(dataSpring$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_maxX_3 = sample(dataSpring$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_imX_20 = sample(dataSpring$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_xmX_25 = sample(dataSpring$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         WSX_2 = sample(dataSpring$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP8.5_Spring <- predict(final_rf.QMRA, dataSpring2046_2065.RCP8.5)
(VpConcHarvest_2046_2065.RCP8.5_Spring <- mcdata(predict_VpConcHarvest_2046_2065.RCP8.5_Spring, type = "V"))

# ____________________ Summer #################
dataSummer2046_2065.RCP8.5 <- data.frame(SST_avgX_3 = sample(dataSummer$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4),
                                         SST_imX_10 = sample(dataSummer$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_maxX_0 = sample(dataSummer$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_xmX_15 = sample(dataSummer$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_xmX_30 = sample(dataSummer$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_maxX_3 = sample(dataSummer$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_imX_20 = sample(dataSummer$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         SST_xmX_25 = sample(dataSummer$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                         WSX_2 = sample(dataSummer$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP8.5_Summer <- predict(final_rf.QMRA, dataSummer2046_2065.RCP8.5)
(VpConcHarvest_2046_2065.RCP8.5_Summer <- mcdata(predict_VpConcHarvest_2046_2065.RCP8.5_Summer, type = "V"))

# ____________________ Fall #################
dataFall2046_2065.RCP8.5 <- data.frame(SST_avgX_3 = sample(dataFall$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4),
                                       SST_imX_10 = sample(dataFall$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                       SST_maxX_0 = sample(dataFall$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                       SST_xmX_15 = sample(dataFall$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                       SST_xmX_30 = sample(dataFall$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                       SST_maxX_3 = sample(dataFall$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                       SST_imX_20 = sample(dataFall$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                       SST_xmX_25 = sample(dataFall$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.0, max=2.6, shape=4), 
                                       WSX_2 = sample(dataFall$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2046_2065.RCP8.5_Fall <- predict(final_rf.QMRA, dataFall2046_2065.RCP8.5)
(VpConcHarvest_2046_2065.RCP8.5_Fall <- mcdata(predict_VpConcHarvest_2046_2065.RCP8.5_Fall, type = "V"))

# _____ VpConc 2081 - 2100 ################
# ____________ RCP2.6 ################
# ____________________ Winter #################
dataWinter2081_2100.RCP2.6 <- data.frame(SST_avgX_3 = sample(dataWinter$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4),
                                         SST_imX_10 = sample(dataWinter$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_maxX_0 = sample(dataWinter$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_xmX_15 = sample(dataWinter$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_xmX_30 = sample(dataWinter$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_maxX_3 = sample(dataWinter$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_imX_20 = sample(dataWinter$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_xmX_25 = sample(dataWinter$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         WSX_2 = sample(dataWinter$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP2.6_Winter <- predict(final_rf.QMRA, dataWinter2081_2100.RCP2.6)
(VpConcHarvest_2081_2100.RCP2.6_Winter <- mcdata(predict_VpConcHarvest_2081_2100.RCP2.6_Winter, type = "V"))

# ____________________ Spring #################
dataSpring2081_2100.RCP2.6 <- data.frame(SST_avgX_3 = sample(dataSpring$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4),
                                         SST_imX_10 = sample(dataSpring$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_maxX_0 = sample(dataSpring$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_xmX_15 = sample(dataSpring$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_xmX_30 = sample(dataSpring$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_maxX_3 = sample(dataSpring$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_imX_20 = sample(dataSpring$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_xmX_25 = sample(dataSpring$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         WSX_2 = sample(dataSpring$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP2.6_Spring <- predict(final_rf.QMRA, dataSpring2081_2100.RCP2.6)
(VpConcHarvest_2081_2100.RCP2.6_Spring <- mcdata(predict_VpConcHarvest_2081_2100.RCP2.6_Spring, type = "V"))

# ____________________ Summer #################
dataSummer2081_2100.RCP2.6 <- data.frame(SST_avgX_3 = sample(dataSummer$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4),
                                         SST_imX_10 = sample(dataSummer$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_maxX_0 = sample(dataSummer$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_xmX_15 = sample(dataSummer$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_xmX_30 = sample(dataSummer$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_maxX_3 = sample(dataSummer$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_imX_20 = sample(dataSummer$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         SST_xmX_25 = sample(dataSummer$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                         WSX_2 = sample(dataSummer$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP2.6_Summer <- predict(final_rf.QMRA, dataSummer2081_2100.RCP2.6)
(VpConcHarvest_2081_2100.RCP2.6_Summer <- mcdata(predict_VpConcHarvest_2081_2100.RCP2.6_Summer, type = "V"))

# ____________________ Fall #################
dataFall2081_2100.RCP2.6 <- data.frame(SST_avgX_3 = sample(dataFall$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4),
                                       SST_imX_10 = sample(dataFall$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                       SST_maxX_0 = sample(dataFall$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                       SST_xmX_15 = sample(dataFall$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                       SST_xmX_30 = sample(dataFall$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                       SST_maxX_3 = sample(dataFall$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                       SST_imX_20 = sample(dataFall$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                       SST_xmX_25 = sample(dataFall$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=0.30, mode=1.0, max=1.7, shape=4), 
                                       WSX_2 = sample(dataFall$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP2.6_Fall <- predict(final_rf.QMRA, dataFall2081_2100.RCP2.6)
(VpConcHarvest_2081_2100.RCP2.6_Fall <- mcdata(predict_VpConcHarvest_2081_2100.RCP2.6_Fall, type = "V"))

# ____________ RCP4.5 ################
# ____________________ Winter #################
dataWinter2081_2100.RCP4.5 <- data.frame(SST_avgX_3 = sample(dataWinter$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4),
                                         SST_imX_10 = sample(dataWinter$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_maxX_0 = sample(dataWinter$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_xmX_15 = sample(dataWinter$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_xmX_30 = sample(dataWinter$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_maxX_3 = sample(dataWinter$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_imX_20 = sample(dataWinter$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_xmX_25 = sample(dataWinter$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         WSX_2 = sample(dataWinter$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP4.5_Winter <- predict(final_rf.QMRA, dataWinter2081_2100.RCP4.5)
(VpConcHarvest_2081_2100.RCP4.5_Winter <- mcdata(predict_VpConcHarvest_2081_2100.RCP4.5_Winter, type = "V"))

# ____________________ Spring #################
dataSpring2081_2100.RCP4.5 <- data.frame(SST_avgX_3 = sample(dataSpring$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4),
                                         SST_imX_10 = sample(dataSpring$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_maxX_0 = sample(dataSpring$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_xmX_15 = sample(dataSpring$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_xmX_30 = sample(dataSpring$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_maxX_3 = sample(dataSpring$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_imX_20 = sample(dataSpring$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_xmX_25 = sample(dataSpring$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         WSX_2 = sample(dataSpring$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP4.5_Spring <- predict(final_rf.QMRA, dataSpring2081_2100.RCP4.5)
(VpConcHarvest_2081_2100.RCP4.5_Spring <- mcdata(predict_VpConcHarvest_2081_2100.RCP4.5_Spring, type = "V"))

# ____________________ Summer #################
dataSummer2081_2100.RCP4.5 <- data.frame(SST_avgX_3 = sample(dataSummer$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4),
                                         SST_imX_10 = sample(dataSummer$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_maxX_0 = sample(dataSummer$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_xmX_15 = sample(dataSummer$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_xmX_30 = sample(dataSummer$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_maxX_3 = sample(dataSummer$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_imX_20 = sample(dataSummer$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         SST_xmX_25 = sample(dataSummer$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                         WSX_2 = sample(dataSummer$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP4.5_Summer <- predict(final_rf.QMRA, dataSummer2081_2100.RCP4.5)
(VpConcHarvest_2081_2100.RCP4.5_Summer <- mcdata(predict_VpConcHarvest_2081_2100.RCP4.5_Summer, type = "V"))

# ____________________ Fall #################
dataFall2081_2100.RCP4.5 <- data.frame(SST_avgX_3 = sample(dataFall$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4),
                                       SST_imX_10 = sample(dataFall$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                       SST_maxX_0 = sample(dataFall$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                       SST_xmX_15 = sample(dataFall$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                       SST_xmX_30 = sample(dataFall$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                       SST_maxX_3 = sample(dataFall$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                       SST_imX_20 = sample(dataFall$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                       SST_xmX_25 = sample(dataFall$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.1, mode=1.8, max=2.6, shape=4), 
                                       WSX_2 = sample(dataFall$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP4.5_Fall <- predict(final_rf.QMRA, dataFall2081_2100.RCP4.5)
(VpConcHarvest_2081_2100.RCP4.5_Fall <- mcdata(predict_VpConcHarvest_2081_2100.RCP4.5_Fall, type = "V"))

# ____________ RCP6.0 ################
# ____________________ Winter #################
dataWinter2081_2100.RCP6.0 <- data.frame(SST_avgX_3 = sample(dataWinter$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4),
                                         SST_imX_10 = sample(dataWinter$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_maxX_0 = sample(dataWinter$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_xmX_15 = sample(dataWinter$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_xmX_30 = sample(dataWinter$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_maxX_3 = sample(dataWinter$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_imX_20 = sample(dataWinter$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_xmX_25 = sample(dataWinter$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         WSX_2 = sample(dataWinter$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP6.0_Winter <- predict(final_rf.QMRA, dataWinter2081_2100.RCP6.0)
(VpConcHarvest_2081_2100.RCP6.0_Winter <- mcdata(predict_VpConcHarvest_2081_2100.RCP6.0_Winter, type = "V"))

# ____________________ Spring #################
dataSpring2081_2100.RCP6.0 <- data.frame(SST_avgX_3 = sample(dataSpring$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4),
                                         SST_imX_10 = sample(dataSpring$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_maxX_0 = sample(dataSpring$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_xmX_15 = sample(dataSpring$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_xmX_30 = sample(dataSpring$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_maxX_3 = sample(dataSpring$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_imX_20 = sample(dataSpring$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_xmX_25 = sample(dataSpring$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         WSX_2 = sample(dataSpring$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP6.0_Spring <- predict(final_rf.QMRA, dataSpring2081_2100.RCP6.0)
(VpConcHarvest_2081_2100.RCP6.0_Spring <- mcdata(predict_VpConcHarvest_2081_2100.RCP6.0_Spring, type = "V"))

# ____________________ Summer #################
dataSummer2081_2100.RCP6.0 <- data.frame(SST_avgX_3 = sample(dataSummer$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4),
                                         SST_imX_10 = sample(dataSummer$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_maxX_0 = sample(dataSummer$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_xmX_15 = sample(dataSummer$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_xmX_30 = sample(dataSummer$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_maxX_3 = sample(dataSummer$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_imX_20 = sample(dataSummer$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         SST_xmX_25 = sample(dataSummer$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                         WSX_2 = sample(dataSummer$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP6.0_Summer <- predict(final_rf.QMRA, dataSummer2081_2100.RCP6.0)
(VpConcHarvest_2081_2100.RCP6.0_Summer <- mcdata(predict_VpConcHarvest_2081_2100.RCP6.0_Summer, type = "V"))

# ____________________ Fall #################
dataFall2081_2100.RCP6.0 <- data.frame(SST_avgX_3 = sample(dataFall$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4),
                                       SST_imX_10 = sample(dataFall$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                       SST_maxX_0 = sample(dataFall$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                       SST_xmX_15 = sample(dataFall$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                       SST_xmX_30 = sample(dataFall$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                       SST_maxX_3 = sample(dataFall$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                       SST_imX_20 = sample(dataFall$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                       SST_xmX_25 = sample(dataFall$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=1.4, mode=2.2, max=3.1, shape=4), 
                                       WSX_2 = sample(dataFall$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP6.0_Fall <- predict(final_rf.QMRA, dataFall2081_2100.RCP6.0)
(VpConcHarvest_2081_2100.RCP6.0_Fall <- mcdata(predict_VpConcHarvest_2081_2100.RCP6.0_Fall, type = "V"))

# ____________ RCP8.5 ################
# ____________________ Winter #################
dataWinter2081_2100.RCP8.5 <- data.frame(SST_avgX_3 = sample(dataWinter$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4),
                                         SST_imX_10 = sample(dataWinter$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_maxX_0 = sample(dataWinter$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_xmX_15 = sample(dataWinter$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_xmX_30 = sample(dataWinter$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_maxX_3 = sample(dataWinter$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_imX_20 = sample(dataWinter$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_xmX_25 = sample(dataWinter$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         WSX_2 = sample(dataWinter$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP8.5_Winter <- predict(final_rf.QMRA, dataWinter2081_2100.RCP8.5)
(VpConcHarvest_2081_2100.RCP8.5_Winter <- mcdata(predict_VpConcHarvest_2081_2100.RCP8.5_Winter, type = "V"))

# ____________________ Spring #################
dataSpring2081_2100.RCP8.5 <- data.frame(SST_avgX_3 = sample(dataSpring$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4),
                                         SST_imX_10 = sample(dataSpring$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_maxX_0 = sample(dataSpring$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_xmX_15 = sample(dataSpring$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_xmX_30 = sample(dataSpring$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_maxX_3 = sample(dataSpring$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_imX_20 = sample(dataSpring$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_xmX_25 = sample(dataSpring$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         WSX_2 = sample(dataSpring$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP8.5_Spring <- predict(final_rf.QMRA, dataSpring2081_2100.RCP8.5)
(VpConcHarvest_2081_2100.RCP8.5_Spring <- mcdata(predict_VpConcHarvest_2081_2100.RCP8.5_Spring, type = "V"))

# ____________________ Summer #################
dataSummer2081_2100.RCP8.5 <- data.frame(SST_avgX_3 = sample(dataSummer$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4),
                                         SST_imX_10 = sample(dataSummer$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_maxX_0 = sample(dataSummer$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_xmX_15 = sample(dataSummer$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_xmX_30 = sample(dataSummer$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_maxX_3 = sample(dataSummer$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_imX_20 = sample(dataSummer$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         SST_xmX_25 = sample(dataSummer$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                         WSX_2 = sample(dataSummer$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP8.5_Summer <- predict(final_rf.QMRA, dataSummer2081_2100.RCP8.5)
(VpConcHarvest_2081_2100.RCP8.5_Summer <- mcdata(predict_VpConcHarvest_2081_2100.RCP8.5_Summer, type = "V"))

# ____________________ Fall #################
dataFall2081_2100.RCP8.5 <- data.frame(SST_avgX_3 = sample(dataFall$SST_avgX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4),
                                       SST_imX_10 = sample(dataFall$SST_imX_10, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                       SST_maxX_0 = sample(dataFall$SST_maxX_0, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                       SST_xmX_15 = sample(dataFall$SST_xmX_15, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                       SST_xmX_30 = sample(dataFall$SST_xmX_30, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                       SST_maxX_3 = sample(dataFall$SST_maxX_3, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                       SST_imX_20 = sample(dataFall$SST_imX_20, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                       SST_xmX_25 = sample(dataFall$SST_xmX_25, size=mcTrials, replace=TRUE) + rpert(mcTrials, min=2.6, mode=3.7, max=4.8, shape=4), 
                                       WSX_2 = sample(dataFall$WSX_2, size=mcTrials, replace=TRUE))
predict_VpConcHarvest_2081_2100.RCP8.5_Fall <- predict(final_rf.QMRA, dataFall2081_2100.RCP8.5)
(VpConcHarvest_2081_2100.RCP8.5_Fall <- mcdata(predict_VpConcHarvest_2081_2100.RCP8.5_Fall, type = "V"))
VpConcAll_Baseline <- data.frame(
  Conc_Baseline_Winter = dataWinter_Baseline,
  Conc_Baseline_Spring = dataSpring_Baseline,
  Conc_Baseline_Summer = dataSummer_Baseline,
  Conc_Baseline_Fall = dataFall_Baseline)
VpConcAll_2016_2036 <- data.frame(
  Conc_2016_2036.RCP2.6_Winter = predict_VpConcHarvest_2016_2036.RCP2.6_Winter,
  Conc_2016_2036.RCP2.6_Spring = predict_VpConcHarvest_2016_2036.RCP2.6_Spring,
  Conc_2016_2036.RCP2.6_Summer = predict_VpConcHarvest_2016_2036.RCP2.6_Summer,
  Conc_2016_2036.RCP2.6_Fall = predict_VpConcHarvest_2016_2036.RCP2.6_Fall,
  Conc_2016_2036.RCP4.5_Winter = predict_VpConcHarvest_2016_2036.RCP4.5_Winter,
  Conc_2016_2036.RCP4.5_Spring = predict_VpConcHarvest_2016_2036.RCP4.5_Spring,
  Conc_2016_2036.RCP4.5_Summer = predict_VpConcHarvest_2016_2036.RCP4.5_Summer,
  Conc_2016_2036.RCP4.5_Fall = predict_VpConcHarvest_2016_2036.RCP4.5_Fall,
  Conc_2016_2036.RCP6.0_Winter = predict_VpConcHarvest_2016_2036.RCP6.0_Winter,
  Conc_2016_2036.RCP6.0_Spring = predict_VpConcHarvest_2016_2036.RCP6.0_Spring,
  Conc_2016_2036.RCP6.0_Summer = predict_VpConcHarvest_2016_2036.RCP6.0_Summer,
  Conc_2016_2036.RCP6.0_Fall = predict_VpConcHarvest_2016_2036.RCP6.0_Fall,
  Conc_2016_2036.RCP6.0_Winter = predict_VpConcHarvest_2016_2036.RCP6.0_Winter,
  Conc_2016_2036.RCP6.0_Spring = predict_VpConcHarvest_2016_2036.RCP6.0_Spring,
  Conc_2016_2036.RCP6.0_Summer = predict_VpConcHarvest_2016_2036.RCP6.0_Summer,
  Conc_2016_2036.RCP6.0_Fall = predict_VpConcHarvest_2016_2036.RCP6.0_Fall)

VpConcAll_2046_2065 <- data.frame(
  Conc_2046_2065.RCP2.6_Winter = predict_VpConcHarvest_2046_2065.RCP2.6_Winter,
  Conc_2046_2065.RCP2.6_Spring = predict_VpConcHarvest_2046_2065.RCP2.6_Spring,
  Conc_2046_2065.RCP2.6_Summer = predict_VpConcHarvest_2046_2065.RCP2.6_Summer,
  Conc_2046_2065.RCP2.6_Fall = predict_VpConcHarvest_2046_2065.RCP2.6_Fall,
  Conc_2046_2065.RCP4.5_Winter = predict_VpConcHarvest_2046_2065.RCP4.5_Winter,
  Conc_2046_2065.RCP4.5_Spring = predict_VpConcHarvest_2046_2065.RCP4.5_Spring,
  Conc_2046_2065.RCP4.5_Summer = predict_VpConcHarvest_2046_2065.RCP4.5_Summer,
  Conc_2046_2065.RCP4.5_Fall = predict_VpConcHarvest_2046_2065.RCP4.5_Fall,
  Conc_2046_2065.RCP6.0_Winter = predict_VpConcHarvest_2046_2065.RCP6.0_Winter,
  Conc_2046_2065.RCP6.0_Spring = predict_VpConcHarvest_2046_2065.RCP6.0_Spring,
  Conc_2046_2065.RCP6.0_Summer = predict_VpConcHarvest_2046_2065.RCP6.0_Summer,
  Conc_2046_2065.RCP6.0_Fall = predict_VpConcHarvest_2046_2065.RCP6.0_Fall,
  Conc_2046_2065.RCP6.0_Winter = predict_VpConcHarvest_2046_2065.RCP6.0_Winter,
  Conc_2046_2065.RCP6.0_Spring = predict_VpConcHarvest_2046_2065.RCP6.0_Spring,
  Conc_2046_2065.RCP6.0_Summer = predict_VpConcHarvest_2046_2065.RCP6.0_Summer,
  Conc_2046_2065.RCP6.0_Fall = predict_VpConcHarvest_2046_2065.RCP6.0_Fall)

VpConcAll_2081_2100 <- data.frame(
  Conc_2081_2100.RCP2.6_Winter = predict_VpConcHarvest_2081_2100.RCP2.6_Winter,
  Conc_2081_2100.RCP2.6_Spring = predict_VpConcHarvest_2081_2100.RCP2.6_Spring,
  Conc_2081_2100.RCP2.6_Summer = predict_VpConcHarvest_2081_2100.RCP2.6_Summer,
  Conc_2081_2100.RCP2.6_Fall = predict_VpConcHarvest_2081_2100.RCP2.6_Fall,
  Conc_2081_2100.RCP4.5_Winter = predict_VpConcHarvest_2081_2100.RCP4.5_Winter,
  Conc_2081_2100.RCP4.5_Spring = predict_VpConcHarvest_2081_2100.RCP4.5_Spring,
  Conc_2081_2100.RCP4.5_Summer = predict_VpConcHarvest_2081_2100.RCP4.5_Summer,
  Conc_2081_2100.RCP4.5_Fall = predict_VpConcHarvest_2081_2100.RCP4.5_Fall,
  Conc_2081_2100.RCP6.0_Winter = predict_VpConcHarvest_2081_2100.RCP6.0_Winter,
  Conc_2081_2100.RCP6.0_Spring = predict_VpConcHarvest_2081_2100.RCP6.0_Spring,
  Conc_2081_2100.RCP6.0_Summer = predict_VpConcHarvest_2081_2100.RCP6.0_Summer,
  Conc_2081_2100.RCP6.0_Fall = predict_VpConcHarvest_2081_2100.RCP6.0_Fall,
  Conc_2081_2100.RCP6.0_Winter = predict_VpConcHarvest_2081_2100.RCP6.0_Winter,
  Conc_2081_2100.RCP6.0_Spring = predict_VpConcHarvest_2081_2100.RCP6.0_Spring,
  Conc_2081_2100.RCP6.0_Summer = predict_VpConcHarvest_2081_2100.RCP6.0_Summer,
  Conc_2081_2100.RCP6.0_Fall = predict_VpConcHarvest_2081_2100.RCP6.0_Fall)

VpConcAll <- data.frame(
  Conc_Baseline_Winter = dataWinter_Baseline,
  Conc_Baseline_Spring = dataSpring_Baseline,
  Conc_Baseline_Summer = dataSummer_Baseline,
  Conc_Baseline_Fall = dataFall_Baseline,
  Conc_2016_2036.RCP2.6_Winter = predict_VpConcHarvest_2016_2036.RCP2.6_Winter,
  Conc_2016_2036.RCP2.6_Spring = predict_VpConcHarvest_2016_2036.RCP2.6_Spring,
  Conc_2016_2036.RCP2.6_Summer = predict_VpConcHarvest_2016_2036.RCP2.6_Summer,
  Conc_2016_2036.RCP2.6_Fall = predict_VpConcHarvest_2016_2036.RCP2.6_Fall,
  Conc_2016_2036.RCP4.5_Winter = predict_VpConcHarvest_2016_2036.RCP4.5_Winter,
  Conc_2016_2036.RCP4.5_Spring = predict_VpConcHarvest_2016_2036.RCP4.5_Spring,
  Conc_2016_2036.RCP4.5_Summer = predict_VpConcHarvest_2016_2036.RCP4.5_Summer,
  Conc_2016_2036.RCP4.5_Fall = predict_VpConcHarvest_2016_2036.RCP4.5_Fall,
  Conc_2016_2036.RCP6.0_Winter = predict_VpConcHarvest_2016_2036.RCP6.0_Winter,
  Conc_2016_2036.RCP6.0_Spring = predict_VpConcHarvest_2016_2036.RCP6.0_Spring,
  Conc_2016_2036.RCP6.0_Summer = predict_VpConcHarvest_2016_2036.RCP6.0_Summer,
  Conc_2016_2036.RCP6.0_Fall = predict_VpConcHarvest_2016_2036.RCP6.0_Fall,
  Conc_2016_2036.RCP6.0_Winter = predict_VpConcHarvest_2016_2036.RCP6.0_Winter,
  Conc_2016_2036.RCP6.0_Spring = predict_VpConcHarvest_2016_2036.RCP6.0_Spring,
  Conc_2016_2036.RCP6.0_Summer = predict_VpConcHarvest_2016_2036.RCP6.0_Summer,
  Conc_2016_2036.RCP6.0_Fall = predict_VpConcHarvest_2016_2036.RCP6.0_Fall,
  Conc_2046_2065.RCP2.6_Winter = predict_VpConcHarvest_2046_2065.RCP2.6_Winter,
  Conc_2046_2065.RCP2.6_Spring = predict_VpConcHarvest_2046_2065.RCP2.6_Spring,
  Conc_2046_2065.RCP2.6_Summer = predict_VpConcHarvest_2046_2065.RCP2.6_Summer,
  Conc_2046_2065.RCP2.6_Fall = predict_VpConcHarvest_2046_2065.RCP2.6_Fall,
  Conc_2046_2065.RCP4.5_Winter = predict_VpConcHarvest_2046_2065.RCP4.5_Winter,
  Conc_2046_2065.RCP4.5_Spring = predict_VpConcHarvest_2046_2065.RCP4.5_Spring,
  Conc_2046_2065.RCP4.5_Summer = predict_VpConcHarvest_2046_2065.RCP4.5_Summer,
  Conc_2046_2065.RCP4.5_Fall = predict_VpConcHarvest_2046_2065.RCP4.5_Fall,
  Conc_2046_2065.RCP6.0_Winter = predict_VpConcHarvest_2046_2065.RCP6.0_Winter,
  Conc_2046_2065.RCP6.0_Spring = predict_VpConcHarvest_2046_2065.RCP6.0_Spring,
  Conc_2046_2065.RCP6.0_Summer = predict_VpConcHarvest_2046_2065.RCP6.0_Summer,
  Conc_2046_2065.RCP6.0_Fall = predict_VpConcHarvest_2046_2065.RCP6.0_Fall,
  Conc_2046_2065.RCP6.0_Winter = predict_VpConcHarvest_2046_2065.RCP6.0_Winter,
  Conc_2046_2065.RCP6.0_Spring = predict_VpConcHarvest_2046_2065.RCP6.0_Spring,
  Conc_2046_2065.RCP6.0_Summer = predict_VpConcHarvest_2046_2065.RCP6.0_Summer,
  Conc_2046_2065.RCP6.0_Fall = predict_VpConcHarvest_2046_2065.RCP6.0_Fall,
  Conc_2081_2100.RCP2.6_Winter = predict_VpConcHarvest_2081_2100.RCP2.6_Winter,
  Conc_2081_2100.RCP2.6_Spring = predict_VpConcHarvest_2081_2100.RCP2.6_Spring,
  Conc_2081_2100.RCP2.6_Summer = predict_VpConcHarvest_2081_2100.RCP2.6_Summer,
  Conc_2081_2100.RCP2.6_Fall = predict_VpConcHarvest_2081_2100.RCP2.6_Fall,
  Conc_2081_2100.RCP4.5_Winter = predict_VpConcHarvest_2081_2100.RCP4.5_Winter,
  Conc_2081_2100.RCP4.5_Spring = predict_VpConcHarvest_2081_2100.RCP4.5_Spring,
  Conc_2081_2100.RCP4.5_Summer = predict_VpConcHarvest_2081_2100.RCP4.5_Summer,
  Conc_2081_2100.RCP4.5_Fall = predict_VpConcHarvest_2081_2100.RCP4.5_Fall,
  Conc_2081_2100.RCP6.0_Winter = predict_VpConcHarvest_2081_2100.RCP6.0_Winter,
  Conc_2081_2100.RCP6.0_Spring = predict_VpConcHarvest_2081_2100.RCP6.0_Spring,
  Conc_2081_2100.RCP6.0_Summer = predict_VpConcHarvest_2081_2100.RCP6.0_Summer,
  Conc_2081_2100.RCP6.0_Fall = predict_VpConcHarvest_2081_2100.RCP6.0_Fall,
  Conc_2081_2100.RCP6.0_Winter = predict_VpConcHarvest_2081_2100.RCP6.0_Winter,
  Conc_2081_2100.RCP6.0_Spring = predict_VpConcHarvest_2081_2100.RCP6.0_Spring,
  Conc_2081_2100.RCP6.0_Summer = predict_VpConcHarvest_2081_2100.RCP6.0_Summer,
  Conc_2081_2100.RCP6.0_Fall = predict_VpConcHarvest_2081_2100.RCP6.0_Fall)

hist(VpConcAll$Conc_Baseline_Summer)
hist(VpConcAll$Conc_2081_2100.RCP6.0_Summer)
summary(VpConcAll$Conc_Baseline_Summer)
summary(VpConcAll$Conc_2081_2100.RCP6.0_Summer)

#_____Choose analysis
#1 Baseline_Winter
#2 Baseline_Spring
#3 Baseline_Summer
#4 Baseline_Fall
#5 20162035_RCP2.6_Winter
#6 20162035_RCP2.6_Spring
#7 20162035_RCP2.6_Summer
#8 20162035_RCP2.6_Fall
#9 20162035_RCP4.5_Winter
#10 20162035_RCP4.5_Spring
#11 20162035_RCP4.5_Summer
#12 20162035_RCP4.5_Fall
#13 20162035_RCP6.0_Winter
#14 20162035_RCP6.0_Spring
#15 20162035_RCP6.0_Summer
#16 20162035_RCP6.0_Fall
#17 20162035_RCP8.5_Winter
#18 20162035_RCP8.5_Spring
#19 20162035_RCP8.5_Summer
#20 20162035_RCP8.5_Fall

#21 20462065_RCP2.6_Winter
#22 20462065_RCP2.6_Spring
#23 20462065_RCP2.6_Summer
#24 20462065_RCP2.6_Fall
#25 20462065_RCP4.5_Winter
#26 20462065_RCP4.5_Spring
#27 20462065_RCP4.5_Summer
#28 20462065_RCP4.5_Fall
#29 20462065_RCP6.0_Winter
#30 20462065_RCP6.0_Spring
#31 20462065_RCP6.0_Summer
#32 20462065_RCP6.0_Fall
#33 20462065_RCP8.5_Winter
#34 20462065_RCP8.5_Spring
#35 20462065_RCP8.5_Summer
#36 20462065_RCP8.5_Fall

#37 20812100_RCP2.6_Winter
#38 20812100_RCP2.6_Spring
#39 20812100_RCP2.6_Summer
#40 20812100_RCP2.6_Fall
#41 20812100_RCP4.5_Winter
#41 20812100_RCP4.5_Spring
#43 20812100_RCP4.5_Summer
#44 20812100_RCP4.5_Fall
#45 20812100_RCP6.0_Winter
#46 20812100_RCP6.0_Spring
#47 20812100_RCP6.0_Summer
#48 20812100_RCP6.0_Fall
#49 20812100_RCP8.5_Winter
#50 20812100_RCP8.5_Spring
#51 20812100_RCP8.5_Summer
#52 20812100_RCP8.5_Fall

#2 

#1 gamma irr, peeled + devein, cooked properly
#2 gamma irr, peeled + devein, undercook
#3 gamma irr, peel only, cooked properly
#4 gamma irr, peel only, undercook
#5 peeled + devein, cooked properly
#6 peeled + devein, undercook
#7 peel only, cooked properly
#8 peel only, undercook

# estimate the Vp concentration on each scenarios

analysis <- 1

######## MONTE CARLO NODES #########################################################################

#all lognormal parameters are transformed appropriately

#____Wastewater concentration and removal 
#rem_treat <- mcstoc(func = runif, type="V", min=1, max =3.6) #in log removals
#conc_inf <- mcstoc(rlnorm, type = "V", meanlog = 7.171, sdlog = 2.985) #concentration in raw ww 
#conc_ww_treat <- (10^(-1*rem_treat))*conc_inf #conc of bug in ww entering pond = conc in treated ww effluent
#dil <-mcstoc(runif, type = "V", min=0.2, max = 0.5) # dilution factor
#accum_ratio <- mcstoc(rweibull, type="V", shape = 0.386, scale = 0.031, rtrunc=TRUE, linf=3.32e-5, lsup=1.75) #gives #/g shrimp
#conc_shrimp_harvest <- dil*conc_ww_treat

#ind_ratio <- mcstoc(runif, type="V", min=1e-4, max=1e-6) #indicator ratio
ind_ratio <-1e-4
conc_ec <-mcstoc(runif, type="V", min=1e4, max=1e5)
conc_shrimp_harvest <-ind_ratio*conc_ec

#conc_shrimp_harvest <-1 # Reverse QMRA

####DOSE CALCULATION################

frac_vein <-mcstoc(rweibull, type="V", shape = 0.255, scale = 3.941)
plot(frac_flesh)
mass_vein <- 0.4/100
frac_flesh <- mcstoc(runif, type="V", min= 4.62e-5, max=0.78)
mass_flesh <-.5272
peel_only <- frac_vein*mass_vein + frac_flesh*mass_flesh
peel_and_devein <- frac_flesh*mass_flesh
plot(peel_and_devein)

#____growth & inactivation during food processing
growthexp_ice_ship <- mcstoc(runif, type="V", min = 0.18, max=1.59) # units d^-1, exponential
time_ice_ship <- 2

growthlog_controlled_thaw <- mcstoc(runif, type="V", min=0, max=0.76) #NEW- log10 base

growthexp_consumer_defrost <- 0.142 #Erdilal
time_consumer_defrost <- 1/24

removallog_dip <- mcstoc(runif, type="V", min=1.67, max=3.25) #additive dip

removallog_contact_freeze <- mcstoc(rnorm, type="V", 1.26, 0.10)

processing1 <- exp(growthexp_ice_ship*time_ice_ship +
                     growthexp_consumer_defrost*time_consumer_defrost)*
  10^(-removallog_dip+ -removallog_contact_freeze+growthlog_controlled_thaw)

#___variables that change in each scenario

removallog_gamma_irr <- mcstoc(runif, type="V", min=4, max=5.5)

removalexp_cook <- mcstoc(runif, type="V", 0.074, 0.097)
undercook <- 15
proper_cook <- mcstoc(rnorm, type="V", 96, 8)


#____Intake rate
consumption_per_serving <- 85 # g/serving
num_per_year <- mcstoc(rlnorm, type = "V", meanlog=2.164, sdlog = 0.766) #servings per year

#____dose response
alpha <- 0.3126
beta <- 2884 
morb_ratio <-mcstoc(rnorm, type="V", 0.41, 0.26, rtrunc=TRUE, linf=0, lsup=1) #NOTE LIM USED 1 here
daly_per_illness_case <- 6.14e-3


###################### CALCULATED VALUES #######################################################################
if (analysis==1){
  dose_daily_sa <- consumption_per_serving*peel_and_devein*conc_shrimp_harvest*processing1*10^(-removallog_gamma_irr)*exp(-removalexp_cook*proper_cook)
}else if (analysis==2){
  dose_daily_sa <- consumption_per_serving*peel_and_devein*conc_shrimp_harvest*processing1*10^(-removallog_gamma_irr)*exp(-removalexp_cook*undercook)
}else if (analysis==3){
  dose_daily_sa <- consumption_per_serving*peel_only*conc_shrimp_harvest*processing1*10^(-removallog_gamma_irr)*exp(-removalexp_cook*proper_cook)
}else if (analysis==4){
  dose_daily_sa <- consumption_per_serving*peel_only*conc_shrimp_harvest*processing1*10^(-removallog_gamma_irr)*exp(-removalexp_cook*undercook)
}else if (analysis==5){
  dose_daily_sa <- consumption_per_serving*peel_and_devein*conc_shrimp_harvest*processing1*exp(-removalexp_cook*proper_cook)
}else if (analysis==6){
  dose_daily_sa <- consumption_per_serving*peel_and_devein*conc_shrimp_harvest*processing1*exp(-removalexp_cook*undercook)
}else if (analysis==7){
  dose_daily_sa <- consumption_per_serving*peel_only*conc_shrimp_harvest*processing1*exp(-removalexp_cook*proper_cook)
}else if (analysis==8){
  dose_daily_sa <- consumption_per_serving*peel_only*conc_shrimp_harvest*processing1*exp(-removalexp_cook*undercook)
}else{
  stop("Specify analysis type in line 12")
}

p_inf_daily_sa <- 1-(1+ dose_daily_sa/beta)^(-1*alpha)
annual_inf_risks<-1-(1-p_inf_daily_sa)^num_per_year
annual_ill_risks <-annual_inf_risks*morb_ratio
daly_metric <- daly_per_illness_case*annual_ill_risks


############################## MONTE CARLO OBJECT ####################################################
# create the monte carlo object, include all nodes and calculated values that include mc nodes
# need different object for each scenario
# note in the sim object that the outcome variable for sensitivity analysis must be last

if (analysis==1){
  sim_inf<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, frac_vein, peel_and_devein, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removallog_gamma_irr, removalexp_cook, proper_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks) 
  sim_ill<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, peel_and_devein, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removallog_gamma_irr, removalexp_cook, proper_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks)
  sim_daly<-mc(conc_ec, conc_shrimp_harvest, frac_flesh, peel_and_devein, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removallog_gamma_irr, removalexp_cook, proper_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks, daly_metric)
}else if (analysis==2){
  sim_inf<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, peel_and_devein, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removallog_gamma_irr, removalexp_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks)  
  sim_ill<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, peel_and_devein, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removallog_gamma_irr, removalexp_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks) 
  sim_daly<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, peel_and_devein, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removallog_gamma_irr, removalexp_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks, daly_metric) 
}else if (analysis==3){
  sim_inf<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, frac_vein, peel_only, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removallog_gamma_irr, removalexp_cook, proper_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks)
  sim_ill<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, frac_vein, peel_only, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removallog_gamma_irr, removalexp_cook, proper_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks) 
  sim_daly<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, frac_vein, peel_only, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removallog_gamma_irr, removalexp_cook, proper_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks, daly_metric) 
}else if (analysis==4){
  sim_inf<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, frac_vein, peel_only, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removallog_gamma_irr, removalexp_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks)  
  sim_ill<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, frac_vein, peel_only, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removallog_gamma_irr, removalexp_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks)  
  sim_daly<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, frac_vein, peel_only, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removallog_gamma_irr, removalexp_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks, daly_metric)  
}else if (analysis==5){
  sim_inf<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, peel_and_devein, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw ,num_per_year, removalexp_cook, proper_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks) 
  sim_ill<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, peel_and_devein, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw ,num_per_year, removalexp_cook, proper_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks) 
  sim_daly<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, peel_and_devein, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw ,num_per_year, removalexp_cook, proper_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks, daly_metric) 
}else if (analysis==6){
  sim_inf<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, peel_and_devein, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removalexp_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks) 
  sim_ill<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, peel_and_devein, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removalexp_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks) 
  sim_daly<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, peel_and_devein, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removalexp_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks, daly_metric) 
}else if (analysis==7){
  sim_inf<- mc(conc_ec, conc_shrimp_harvest,  frac_flesh, frac_vein, peel_only, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removalexp_cook, proper_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks)
  sim_ill<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, frac_vein, peel_only, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removalexp_cook, proper_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks) 
  sim_daly<- mc(conc_ec, conc_shrimp_harvest, frac_flesh, frac_vein, peel_only, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removalexp_cook, proper_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks,morb_ratio, annual_ill_risks, daly_metric) 
}else if (analysis==8){ 
  sim_inf<- mc(conc_ec, conc_shrimp_harvest,frac_flesh, frac_vein, peel_only, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removalexp_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks) 
  sim_ill<- mc(conc_ec, conc_shrimp_harvest,frac_flesh, frac_vein, peel_only, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removalexp_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks) 
  sim_daly<- mc(conc_ec, conc_shrimp_harvest,frac_flesh, frac_vein, peel_only, growthexp_ice_ship, removallog_dip, removallog_contact_freeze, processing1, growthlog_controlled_thaw, num_per_year, removalexp_cook, dose_daily_sa, p_inf_daily_sa, annual_inf_risks, morb_ratio, annual_ill_risks, daly_metric) 
}else{
  stop("Specify analysis type in line 12")
}

##### PRINT RESULTS ###########################################################################

qs = c(0.05,.25,.50,0.75,0.95)
print("Salmonella annual infection risks")
print(quantile(annual_inf_risks,qs))
print(mean(annual_inf_risks))
print(sd(annual_inf_risks))
print("Sensitivity analysis")
print(tornado(sim_inf))
#change filename below, save annual risk computations
#write.table(annualRisks, file="test.txt", quote=F, row.names=F, col.names=F)

print("Salmonella annual illness risks")
print(quantile(annual_ill_risks,qs))
print(mean(annual_ill_risks))
print(sd(annual_ill_risks))
print("Sensitivity analysis")
print(tornado(sim_ill))
#change filename below, save annual risk computations
#write.table(annualRisks, file="test.txt", quote=F, row.names=F, col.names=F)

print("Salmonella daly metric")
print(quantile(daly_metric,qs))
print(mean(daly_metric))
print(sd(daly_metric))
print("Sensitivity analysis")
print(tornado(sim_daly))
#change filename below, save annual risk computations
#write.table(annualRisks, file="test.txt", quote=F, row.names=F, col.names=F)

#FIX THIS
write.table(sim_inf[["annual_inf_risks"]][1:mcTrials], file="shrimpannualinfectionrisks.txt", quote=F, row.names=F, col.names=F)
write.table(sim_ill[["annual_ill_risks"]][1:mcTrials], file="shrimpannualillnessrisks.txt", quote=F, row.names=F, col.names=F)
write.table(sim_daly[["daly_metric"]][1:mcTrials], file="shrimpdalymetric.txt", quote=F, row.names=F, col.names=F)
write.table(sim_daly[["dose_daily_sa"]][1:mcTrials], file="shrimpdose.txt", quote=F, row.names=F, col.names=F)


