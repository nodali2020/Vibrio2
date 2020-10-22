# PREDICTING THE NUMBER OF VP IN OYSTERS
# This code is prepared to perform random forest for Nodali Ndraha
# First, check the respond language
# Sys.setlocale("LC_ALL","English")
# set working directory
# new dir in Github
setwd("C:/Users/Nodal/OneDrive - email.ntou.edu.tw/Dissertation/Data")
# loading libraries
library(tidyverse)
library(ggpubr)
library(rstatix)
library(FSA)
# Load and create the training and test datasets

allData <- select(myData, c(Year, Month, Season, Loc, Repl, Date, Oy, Sw, Sed, pH, Sal, SST_avgX_3, SST_avgX_2, 
                               SST_avgX_1, SST_avgX_0, SST_maxX_3, SST_maxX_2, SST_maxX_1, SST_maxX_0, SST_minX_3, 
                               SST_minX_2, SST_minX_1, SST_minX_0, WSX_3, WSX_2, WSX_1, WSX_0, WS_gustX_3, WS_gustX_2, 
                               WS_gustX_1, WS_gustX_0, PrecpX_3, PrecpX_2, PrecpX_1, PrecpX_0, SST_amX_30, SST_amX_25, 
                               SST_amX_20, SST_amX_15, SST_amX_10, SST_amX_9, SST_amX_8, SST_amX_7, SST_amX_6, 
                               SST_amX_5, SST_amX_4, SST_amX_3, SST_amX_2, SST_amX_1, SST_amX_0, SST_xmX_30, SST_xmX_25, 
                               SST_xmX_20, SST_xmX_15, SST_xmX_10, SST_xmX_9, SST_xmX_8, SST_xmX_7, SST_xmX_6, SST_xmX_5, 
                               SST_xmX_4, SST_xmX_3, SST_xmX_2, SST_xmX_1, SST_xmX_0, SST_imX_30, SST_imX_25, SST_imX_20, 
                               SST_imX_15, SST_imX_10, SST_imX_9, SST_imX_8, SST_imX_7, SST_imX_6, SST_imX_5, SST_imX_4, 
                               SST_imX_3, SST_imX_2, SST_imX_1, SST_imX_0, Precp_mX_30, Precp_mX_25, Precp_mX_20, 
                               Precp_mX_15, Precp_mX_10, Precp_mX_9, Precp_mX_8, Precp_mX_7, Precp_mX_6, Precp_mX_5, 
                               Precp_mX_4, Precp_mX_3, Precp_mX_2, Precp_mX_1, Precp_mX_0, WS_amX_30, WS_amX_25, 
                               WS_amX_20, WS_amX_15, WS_amX_10, WS_amX_9, WS_amX_8, WS_amX_7, WS_amX_6, WS_amX_5, 
                               WS_amX_4, WS_amX_3, WS_amX_2, WS_amX_1, WS_amX_0, WS_gustamX_30, WS_gustamX_25, 
                               WS_gustamX_20, WS_gustamX_15, WS_gustamX_10, WS_gustamX_9, WS_gustamX_8, WS_gustamX_7, 
                               WS_gustamX_6, WS_gustamX_5, WS_gustamX_4, WS_gustamX_3, WS_gustamX_2, WS_gustamX_1, 
                               WS_gustamX_0, Sal_avg_30, Sal_avg_25, Sal_avg_20, Sal_avg_15, Sal_avg_10, Sal_avg_9, 
                               Sal_avg_8, Sal_avg_7, Sal_avg_6, Sal_avg_5, Sal_avg_4, Sal_avg_3, Sal_avg_2, Sal_avg_1, 
                               Sal_avg_0, Sal_Max_30, Sal_Max_25, Sal_Max_20, Sal_Max_15, Sal_Max_10, Sal_Max_9, 
                               Sal_Max_8, Sal_Max_7, Sal_Max_6, Sal_Max_5, Sal_Max_4, Sal_Max_3, Sal_Max_2, Sal_Max_1, 
                               Sal_Max_0, Sal_Min_30, Sal_Min_25, Sal_Min_20, Sal_Min_15, Sal_Min_10, Sal_Min_9, 
                               Sal_Min_8, Sal_Min_7, Sal_Min_6, Sal_Min_5, Sal_Min_4, Sal_Min_3, Sal_Min_2, Sal_Min_1, 
                               Sal_Min_0, Sal2_amX_30, Sal2_amX_25, Sal2_amX_20, Sal2_amX_15, Sal2_amX_10, Sal2_amX_9, 
                               Sal2_amX_8, Sal2_amX_7, Sal2_amX_6, Sal2_amX_5, Sal2_amX_4, Sal2_amX_3, Sal2_amX_2, 
                               Sal2_amX_1, Sal2_amX_0, Sal2_xmX_30, Sal2_xmX_25, Sal2_xmX_20, Sal2_xmX_15, Sal2_xmX_10, 
                               Sal2_xmX_9, Sal2_xmX_8, Sal2_xmX_7, Sal2_xmX_6, Sal2_xmX_5, Sal2_xmX_4, Sal2_xmX_3, 
                               Sal2_xmX_2, Sal2_xmX_1, Sal2_xmX_0, Sal2_imX_30, Sal2_imX_25, Sal2_imX_20, Sal2_imX_15, 
                               Sal2_imX_10, Sal2_imX_9, Sal2_imX_8, Sal2_imX_7, Sal2_imX_6, Sal2_imX_5, Sal2_imX_4, 
                               Sal2_imX_3, Sal2_imX_2, Sal2_imX_1, Sal2_imX_0))
oysterData <- select(myData, c(Year, Month, Season, Loc, Repl, Date, Oy, pH, Sal, SST_avgX_3, SST_avgX_2, 
                            SST_avgX_1, SST_avgX_0, SST_maxX_3, SST_maxX_2, SST_maxX_1, SST_maxX_0, SST_minX_3, 
                            SST_minX_2, SST_minX_1, SST_minX_0, WSX_3, WSX_2, WSX_1, WSX_0, WS_gustX_3, WS_gustX_2, 
                            WS_gustX_1, WS_gustX_0, PrecpX_3, PrecpX_2, PrecpX_1, PrecpX_0, SST_amX_30, SST_amX_25, 
                            SST_amX_20, SST_amX_15, SST_amX_10, SST_amX_9, SST_amX_8, SST_amX_7, SST_amX_6, 
                            SST_amX_5, SST_amX_4, SST_amX_3, SST_amX_2, SST_amX_1, SST_amX_0, SST_xmX_30, SST_xmX_25, 
                            SST_xmX_20, SST_xmX_15, SST_xmX_10, SST_xmX_9, SST_xmX_8, SST_xmX_7, SST_xmX_6, SST_xmX_5, 
                            SST_xmX_4, SST_xmX_3, SST_xmX_2, SST_xmX_1, SST_xmX_0, SST_imX_30, SST_imX_25, SST_imX_20, 
                            SST_imX_15, SST_imX_10, SST_imX_9, SST_imX_8, SST_imX_7, SST_imX_6, SST_imX_5, SST_imX_4, 
                            SST_imX_3, SST_imX_2, SST_imX_1, SST_imX_0, Precp_mX_30, Precp_mX_25, Precp_mX_20, 
                            Precp_mX_15, Precp_mX_10, Precp_mX_9, Precp_mX_8, Precp_mX_7, Precp_mX_6, Precp_mX_5, 
                            Precp_mX_4, Precp_mX_3, Precp_mX_2, Precp_mX_1, Precp_mX_0, WS_amX_30, WS_amX_25, 
                            WS_amX_20, WS_amX_15, WS_amX_10, WS_amX_9, WS_amX_8, WS_amX_7, WS_amX_6, WS_amX_5, 
                            WS_amX_4, WS_amX_3, WS_amX_2, WS_amX_1, WS_amX_0, WS_gustamX_30, WS_gustamX_25, 
                            WS_gustamX_20, WS_gustamX_15, WS_gustamX_10, WS_gustamX_9, WS_gustamX_8, WS_gustamX_7, 
                            WS_gustamX_6, WS_gustamX_5, WS_gustamX_4, WS_gustamX_3, WS_gustamX_2, WS_gustamX_1, 
                            WS_gustamX_0, Sal_avg_30, Sal_avg_25, Sal_avg_20, Sal_avg_15, Sal_avg_10, Sal_avg_9, 
                            Sal_avg_8, Sal_avg_7, Sal_avg_6, Sal_avg_5, Sal_avg_4, Sal_avg_3, Sal_avg_2, Sal_avg_1, 
                            Sal_avg_0, Sal_Max_30, Sal_Max_25, Sal_Max_20, Sal_Max_15, Sal_Max_10, Sal_Max_9, 
                            Sal_Max_8, Sal_Max_7, Sal_Max_6, Sal_Max_5, Sal_Max_4, Sal_Max_3, Sal_Max_2, Sal_Max_1, 
                            Sal_Max_0, Sal_Min_30, Sal_Min_25, Sal_Min_20, Sal_Min_15, Sal_Min_10, Sal_Min_9, 
                            Sal_Min_8, Sal_Min_7, Sal_Min_6, Sal_Min_5, Sal_Min_4, Sal_Min_3, Sal_Min_2, Sal_Min_1, 
                            Sal_Min_0, Sal2_amX_30, Sal2_amX_25, Sal2_amX_20, Sal2_amX_15, Sal2_amX_10, Sal2_amX_9, 
                            Sal2_amX_8, Sal2_amX_7, Sal2_amX_6, Sal2_amX_5, Sal2_amX_4, Sal2_amX_3, Sal2_amX_2, 
                            Sal2_amX_1, Sal2_amX_0, Sal2_xmX_30, Sal2_xmX_25, Sal2_xmX_20, Sal2_xmX_15, Sal2_xmX_10, 
                            Sal2_xmX_9, Sal2_xmX_8, Sal2_xmX_7, Sal2_xmX_6, Sal2_xmX_5, Sal2_xmX_4, Sal2_xmX_3, 
                            Sal2_xmX_2, Sal2_xmX_1, Sal2_xmX_0, Sal2_imX_30, Sal2_imX_25, Sal2_imX_20, Sal2_imX_15, 
                            Sal2_imX_10, Sal2_imX_9, Sal2_imX_8, Sal2_imX_7, Sal2_imX_6, Sal2_imX_5, Sal2_imX_4, 
                            Sal2_imX_3, Sal2_imX_2, Sal2_imX_1, Sal2_imX_0))
oysterData <- oysterData %>% dplyr::rename(Conc = Oy)
SwData <- select(myData, c(Year, Month, Season, Loc, Repl, Date, Sw, pH, Sal, SST_avgX_3, SST_avgX_2, 
                            SST_avgX_1, SST_avgX_0, SST_maxX_3, SST_maxX_2, SST_maxX_1, SST_maxX_0, SST_minX_3, 
                            SST_minX_2, SST_minX_1, SST_minX_0, WSX_3, WSX_2, WSX_1, WSX_0, WS_gustX_3, WS_gustX_2, 
                            WS_gustX_1, WS_gustX_0, PrecpX_3, PrecpX_2, PrecpX_1, PrecpX_0, SST_amX_30, SST_amX_25, 
                            SST_amX_20, SST_amX_15, SST_amX_10, SST_amX_9, SST_amX_8, SST_amX_7, SST_amX_6, 
                            SST_amX_5, SST_amX_4, SST_amX_3, SST_amX_2, SST_amX_1, SST_amX_0, SST_xmX_30, SST_xmX_25, 
                            SST_xmX_20, SST_xmX_15, SST_xmX_10, SST_xmX_9, SST_xmX_8, SST_xmX_7, SST_xmX_6, SST_xmX_5, 
                            SST_xmX_4, SST_xmX_3, SST_xmX_2, SST_xmX_1, SST_xmX_0, SST_imX_30, SST_imX_25, SST_imX_20, 
                            SST_imX_15, SST_imX_10, SST_imX_9, SST_imX_8, SST_imX_7, SST_imX_6, SST_imX_5, SST_imX_4, 
                            SST_imX_3, SST_imX_2, SST_imX_1, SST_imX_0, Precp_mX_30, Precp_mX_25, Precp_mX_20, 
                            Precp_mX_15, Precp_mX_10, Precp_mX_9, Precp_mX_8, Precp_mX_7, Precp_mX_6, Precp_mX_5, 
                            Precp_mX_4, Precp_mX_3, Precp_mX_2, Precp_mX_1, Precp_mX_0, WS_amX_30, WS_amX_25, 
                            WS_amX_20, WS_amX_15, WS_amX_10, WS_amX_9, WS_amX_8, WS_amX_7, WS_amX_6, WS_amX_5, 
                            WS_amX_4, WS_amX_3, WS_amX_2, WS_amX_1, WS_amX_0, WS_gustamX_30, WS_gustamX_25, 
                            WS_gustamX_20, WS_gustamX_15, WS_gustamX_10, WS_gustamX_9, WS_gustamX_8, WS_gustamX_7, 
                            WS_gustamX_6, WS_gustamX_5, WS_gustamX_4, WS_gustamX_3, WS_gustamX_2, WS_gustamX_1, 
                            WS_gustamX_0, Sal_avg_30, Sal_avg_25, Sal_avg_20, Sal_avg_15, Sal_avg_10, Sal_avg_9, 
                            Sal_avg_8, Sal_avg_7, Sal_avg_6, Sal_avg_5, Sal_avg_4, Sal_avg_3, Sal_avg_2, Sal_avg_1, 
                            Sal_avg_0, Sal_Max_30, Sal_Max_25, Sal_Max_20, Sal_Max_15, Sal_Max_10, Sal_Max_9, 
                            Sal_Max_8, Sal_Max_7, Sal_Max_6, Sal_Max_5, Sal_Max_4, Sal_Max_3, Sal_Max_2, Sal_Max_1, 
                            Sal_Max_0, Sal_Min_30, Sal_Min_25, Sal_Min_20, Sal_Min_15, Sal_Min_10, Sal_Min_9, 
                            Sal_Min_8, Sal_Min_7, Sal_Min_6, Sal_Min_5, Sal_Min_4, Sal_Min_3, Sal_Min_2, Sal_Min_1, 
                            Sal_Min_0, Sal2_amX_30, Sal2_amX_25, Sal2_amX_20, Sal2_amX_15, Sal2_amX_10, Sal2_amX_9, 
                            Sal2_amX_8, Sal2_amX_7, Sal2_amX_6, Sal2_amX_5, Sal2_amX_4, Sal2_amX_3, Sal2_amX_2, 
                            Sal2_amX_1, Sal2_amX_0, Sal2_xmX_30, Sal2_xmX_25, Sal2_xmX_20, Sal2_xmX_15, Sal2_xmX_10, 
                            Sal2_xmX_9, Sal2_xmX_8, Sal2_xmX_7, Sal2_xmX_6, Sal2_xmX_5, Sal2_xmX_4, Sal2_xmX_3, 
                            Sal2_xmX_2, Sal2_xmX_1, Sal2_xmX_0, Sal2_imX_30, Sal2_imX_25, Sal2_imX_20, Sal2_imX_15, 
                            Sal2_imX_10, Sal2_imX_9, Sal2_imX_8, Sal2_imX_7, Sal2_imX_6, Sal2_imX_5, Sal2_imX_4, 
                            Sal2_imX_3, Sal2_imX_2, Sal2_imX_1, Sal2_imX_0))
SwData <- SwData %>% dplyr::rename(Conc = Sw)
SedData <- select(myData, c(Year, Month, Season, Loc, Repl, Date, Sed, pH, Sal, SST_avgX_3, SST_avgX_2, 
                            SST_avgX_1, SST_avgX_0, SST_maxX_3, SST_maxX_2, SST_maxX_1, SST_maxX_0, SST_minX_3, 
                            SST_minX_2, SST_minX_1, SST_minX_0, WSX_3, WSX_2, WSX_1, WSX_0, WS_gustX_3, WS_gustX_2, 
                            WS_gustX_1, WS_gustX_0, PrecpX_3, PrecpX_2, PrecpX_1, PrecpX_0, SST_amX_30, SST_amX_25, 
                            SST_amX_20, SST_amX_15, SST_amX_10, SST_amX_9, SST_amX_8, SST_amX_7, SST_amX_6, 
                            SST_amX_5, SST_amX_4, SST_amX_3, SST_amX_2, SST_amX_1, SST_amX_0, SST_xmX_30, SST_xmX_25, 
                            SST_xmX_20, SST_xmX_15, SST_xmX_10, SST_xmX_9, SST_xmX_8, SST_xmX_7, SST_xmX_6, SST_xmX_5, 
                            SST_xmX_4, SST_xmX_3, SST_xmX_2, SST_xmX_1, SST_xmX_0, SST_imX_30, SST_imX_25, SST_imX_20, 
                            SST_imX_15, SST_imX_10, SST_imX_9, SST_imX_8, SST_imX_7, SST_imX_6, SST_imX_5, SST_imX_4, 
                            SST_imX_3, SST_imX_2, SST_imX_1, SST_imX_0, Precp_mX_30, Precp_mX_25, Precp_mX_20, 
                            Precp_mX_15, Precp_mX_10, Precp_mX_9, Precp_mX_8, Precp_mX_7, Precp_mX_6, Precp_mX_5, 
                            Precp_mX_4, Precp_mX_3, Precp_mX_2, Precp_mX_1, Precp_mX_0, WS_amX_30, WS_amX_25, 
                            WS_amX_20, WS_amX_15, WS_amX_10, WS_amX_9, WS_amX_8, WS_amX_7, WS_amX_6, WS_amX_5, 
                            WS_amX_4, WS_amX_3, WS_amX_2, WS_amX_1, WS_amX_0, WS_gustamX_30, WS_gustamX_25, 
                            WS_gustamX_20, WS_gustamX_15, WS_gustamX_10, WS_gustamX_9, WS_gustamX_8, WS_gustamX_7, 
                            WS_gustamX_6, WS_gustamX_5, WS_gustamX_4, WS_gustamX_3, WS_gustamX_2, WS_gustamX_1, 
                            WS_gustamX_0, Sal_avg_30, Sal_avg_25, Sal_avg_20, Sal_avg_15, Sal_avg_10, Sal_avg_9, 
                            Sal_avg_8, Sal_avg_7, Sal_avg_6, Sal_avg_5, Sal_avg_4, Sal_avg_3, Sal_avg_2, Sal_avg_1, 
                            Sal_avg_0, Sal_Max_30, Sal_Max_25, Sal_Max_20, Sal_Max_15, Sal_Max_10, Sal_Max_9, 
                            Sal_Max_8, Sal_Max_7, Sal_Max_6, Sal_Max_5, Sal_Max_4, Sal_Max_3, Sal_Max_2, Sal_Max_1, 
                            Sal_Max_0, Sal_Min_30, Sal_Min_25, Sal_Min_20, Sal_Min_15, Sal_Min_10, Sal_Min_9, 
                            Sal_Min_8, Sal_Min_7, Sal_Min_6, Sal_Min_5, Sal_Min_4, Sal_Min_3, Sal_Min_2, Sal_Min_1, 
                            Sal_Min_0, Sal2_amX_30, Sal2_amX_25, Sal2_amX_20, Sal2_amX_15, Sal2_amX_10, Sal2_amX_9, 
                            Sal2_amX_8, Sal2_amX_7, Sal2_amX_6, Sal2_amX_5, Sal2_amX_4, Sal2_amX_3, Sal2_amX_2, 
                            Sal2_amX_1, Sal2_amX_0, Sal2_xmX_30, Sal2_xmX_25, Sal2_xmX_20, Sal2_xmX_15, Sal2_xmX_10, 
                            Sal2_xmX_9, Sal2_xmX_8, Sal2_xmX_7, Sal2_xmX_6, Sal2_xmX_5, Sal2_xmX_4, Sal2_xmX_3, 
                            Sal2_xmX_2, Sal2_xmX_1, Sal2_xmX_0, Sal2_imX_30, Sal2_imX_25, Sal2_imX_20, Sal2_imX_15, 
                            Sal2_imX_10, Sal2_imX_9, Sal2_imX_8, Sal2_imX_7, Sal2_imX_6, Sal2_imX_5, Sal2_imX_4, 
                            Sal2_imX_3, Sal2_imX_2, Sal2_imX_1, Sal2_imX_0))
SedData <- SedData %>% dplyr::rename(Conc = Sed)
allData2 <- rbind(oysterData, SwData, SedData)


data1 <- data.frame(oysterData)

# univariate analysis
str(data1)
summary(log10(data1$Oy))
summary(log10(data1$Sw))
summary(log10(data1$Sed))

# data preparation, normality test, and mean comparison
data2 <- select(oysterData, c(Oy, Sw, Sed))
data2[is.na(data2)] <- 1.5
data2 <- log10(data2)
data2$Season <- oysterData$Season

# checking the correlation in the concentration between samples
corrData <- select(data2, -(Season))
corValues <- cor(corrData, method = "pearson")
print(corValues)

library("ggpubr")
(CorrOySw <- ggscatter(corrData, x = "Oy", y = "Sw", 
                       add = "reg.line", conf.int = F,
                       cor.coef = TRUE, cor.method = "pearson",
                       xlab = "Conc. of Vp in oysters \n(log MPN/g)", ylab = "Conc. of Vp in seawater (log MPN/mL)") +
    ggtitle("A"))
(CorrOySed <- ggscatter(corrData, x = "Oy", y = "Sed", 
                        add = "reg.line", conf.int = F, 
                        cor.coef = TRUE, cor.method = "pearson",
                        xlab = "Conc. of Vp in oysters \n(log MPN/g)", ylab = "Conc. of Vp in sediment (log MPN/g)") +
    ggtitle("B"))
(CorrSwSed <- ggscatter(corrData, x = "Sw", y = "Sed", 
                        add = "reg.line", conf.int = F, 
                        cor.coef = TRUE, cor.method = "pearson",
                        xlab = "Conc. of Vp in seawater \n(log MPN/mL)", ylab = "Conc. of Vp in sediment (log MPN/g)") +
    ggtitle("C"))
(CorrAll <- ggarrange(CorrOySw,
                      CorrOySed,
                      CorrSwSed,
                      ncol = 3, nrow = 1))
ggsave(filename = file.path("Figures/Kruskal","CorrAll.jpeg"))
cor.test(corrData$Oy, corrData$Sw, method = "pearson")
cor.test(corrData$Oy, corrData$Sed, method = "pearson")
cor.test(corrData$Sw, corrData$Sed, method = "pearson")


CorrOy_SSTd3 <- ggscatter(rand_rfData, x = "SST_avgX_3", y = "Oy", 
                          add = "reg.line", conf.int = F, 
                          cor.coef = TRUE, cor.method = "pearson",
                          ylab = "Conc. of Vp in oysters \n(log MPN/g)", xlab = "Average SST d-3 (°C)\n") +
  ggtitle("A")
CorrOy_SSTd3
CorrSw_SSTd3 <- ggscatter(rand_rfData, x = "SST_avgX_3", y = "Sw", 
                          add = "reg.line", conf.int = F, 
                          cor.coef = TRUE, cor.method = "pearson",
                          ylab = "Conc. of Vp in seawater \n(log MPN/mL)", xlab = "Average SST d-3 (°C)\n") +
  ggtitle("B")
CorrSw_SSTd3
CorrSed_SSTd3 <- ggscatter(rand_rfData, x = "SST_avgX_3", y = "Sed", 
                           add = "reg.line", conf.int = F, 
                           cor.coef = TRUE, cor.method = "pearson",
                           ylab = "Conc. of Vp in sediment \n(log MPN/g)", xlab = "Average SST d-3 (°C)\n") +
  ggtitle("C")
CorrSed_SSTd3
CorrAll_SSTd3 <- ggscatter(data5, x = "SST_avgX_3", y = "Conc", 
                           add = "reg.line", conf.int = F, 
                           cor.coef = TRUE, cor.method = "pearson",
                           ylab = "Conc. of Vp in all sample types \n(log MPN/g or log MPN/mL)", xlab = "Average SST d-3 (°C)\n") +
  ggtitle("D")
CorrAll_SSTd3
CorrAll <- ggarrange(CorrOy_SSTd3,
                     CorrSw_SSTd3,
                     CorrSed_SSTd3,
                     CorrAll_SSTd3,
                     ncol = 2, nrow = 2)
CorrAll
ggsave(filename = file.path("Figures/Kruskal","CorrAll_SSTd3.jpeg"))
cor.test(rand_rfData$Oy, rand_rfData$SST_avgX_3, method = "pearson")
cor.test(rand_rfData$Sw, rand_rfData$SST_avgX_3, method = "pearson")
cor.test(rand_rfData$Sed, rand_rfData$SST_avgX_3, method = "pearson")
cor.test(data5$Conc, data5$SST_avgX_3, method = "pearson")
# seasonal distribution of SST
seasonData <- allData %>% select(Season, SST_avgX_0, SST_maxX_0, SST_minX_0)
summerTemp <- seasonData %>% group_by(Season) %>% filter(Season == "Summer")
summary(summerTemp)
fallTemp<- seasonData %>% group_by(Season) %>% filter(Season == "Fall")
summary(fallTemp)
winterTemp <- seasonData %>% group_by(Season) %>% filter(Season == "Winter")
summary(winterTemp)
springTemp <- seasonData %>% group_by(Season) %>% filter(Season == "Spring")
summary(springTemp)

# seasonal distribution of Vp
concData <- allData %>% select(Season, Conc, Type)
concData$Conc <- log10(concData$Conc)
summary(summerConcOyster <- concData %>% group_by(Season) %>% filter(Season == "Summer") %>% filter(Type == "Oyster"))
summary(summerConcSeawater <- concData %>% group_by(Season) %>% filter(Season == "Summer") %>% filter(Type == "Seawater"))
summary(summerConcSediment <- concData %>% group_by(Season) %>% filter(Season == "Summer") %>% filter(Type == "Sediment"))

summary(FallConcOyster <- concData %>% group_by(Season) %>% filter(Season == "Fall") %>% filter(Type == "Oyster"))
summary(FallConcSeawater <- concData %>% group_by(Season) %>% filter(Season == "Fall") %>% filter(Type == "Seawater"))
summary(FallConcSediment <- concData %>% group_by(Season) %>% filter(Season == "Fall") %>% filter(Type == "Sediment"))

summary(WinterConcOyster <- concData %>% group_by(Season) %>% filter(Season == "Winter") %>% filter(Type == "Oyster"))
summary(WinterConcSeawater <- concData %>% group_by(Season) %>% filter(Season == "Winter") %>% filter(Type == "Seawater"))
summary(WinterConcSediment <- concData %>% group_by(Season) %>% filter(Season == "Winter") %>% filter(Type == "Sediment"))

summary(SpringConcOyster <- concData %>% group_by(Season) %>% filter(Season == "Spring") %>% filter(Type == "Oyster"))
summary(SpringConcSeawater <- concData %>% group_by(Season) %>% filter(Season == "Spring") %>% filter(Type == "Seawater"))
summary(SpringConcSediment <- concData %>% group_by(Season) %>% filter(Season == "Spring") %>% filter(Type == "Sediment"))

# Compare groups in seasons
# OYSTER
OysterSeason <- rbind(summerConcOyster, FallConcOyster, WinterConcOyster, SpringConcOyster)
OysterSeason <- as.data.frame(oysterSeason)
shapiro.test(oysterSeason$Conc) # data is abnormal
# Compute Kruskal-Wallis test
kr.OysterSeason <- OysterSeason %>% kruskal_test(Conc ~ Season)
print(kr.oysterSeason) # significant between seasons
# Pairwise comparisons
pwc.OysterSeason <- OysterSeason %>% dunn_test(Conc ~ Season, p.adjust.method = "bonferroni")
pwc.OysterSeason
pwc.OysterSeason <- pwc.OysterSeason %>% add_xy_position(x = "Season")
(Fig.OysterSeason <- ggboxplot(OysterSeason, x = "Season", y = "Conc",
                               order = c("Winter", "Spring", "Summer", "Fall"),
                               xlab = "\nSeason",
                               ylab = "Concentration of Vp in oysters \n(log MPN/g)") +
    stat_pvalue_manual(data = pwc.OysterSeason, hide.ns = T, 
                       xmin = "group1", xmax = "group2",
                       label = "p.adj.signif", tip.length = 0.01,
                       y.position = "y.position") + ggtitle("A"))
ggsave(filename = file.path("Figures/Kruskal","OysterSeason.jpeg"))

# SEAWATER
SeawaterSeason <- rbind(summerConcSeawater, FallConcSeawater, WinterConcSeawater, SpringConcSeawater)
SeawaterSeason <- as.data.frame(SeawaterSeason)
shapiro.test(SeawaterSeason$Conc) # data is abnormal
# Compute Kruskal-Wallis test
kr.SeawaterSeason <- SeawaterSeason %>% kruskal_test(Conc ~ Season)
print(kr.SeawaterSeason) # significant between seasons
# Pairwise comparisons
pwc.SeawaterSeason <- SeawaterSeason %>% dunn_test(Conc ~ Season, p.adjust.method = "bonferroni")
pwc.SeawaterSeason
pwc.SeawaterSeason <- pwc.SeawaterSeason %>% add_xy_position(x = "Season")
(Fig.SeawaterSeason <- ggboxplot(SeawaterSeason, x = "Season", y = "Conc",
                                 order = c("Winter", "Spring", "Summer", "Fall"),
                                 xlab = "\nSeason",
                                 ylab = "Concentration of Vp in seawater \n(log MPN/mL)") +
    stat_pvalue_manual(data = pwc.SeawaterSeason, hide.ns = T, 
                       xmin = "group1", xmax = "group2",
                       label = "p.adj.signif", tip.length = 0.01,
                       y.position = "y.position") +ggtitle("B"))
ggsave(filename = file.path("Figures/Kruskal","SeawaterSeason.jpeg"))

# SEDIMENT
SedimentSeason <- rbind(summerConcSediment, FallConcSediment, WinterConcSediment, SpringConcSediment)
SedimentSeason <- as.data.frame(SedimentSeason)
shapiro.test(SedimentSeason$Conc) # data is abnormal
# Compute Kruskal-Wallis test
kr.SedimentSeason <- SedimentSeason %>% kruskal_test(Conc ~ Season)
print(kr.SedimentSeason) # significant between seasons
# Pairwise comparisons
pwc.SedimentSeason <- SedimentSeason %>% dunn_test(Conc ~ Season, p.adjust.method = "bonferroni")
pwc.SedimentSeason
pwc.SedimentSeason <- pwc.SedimentSeason %>% add_xy_position(x = "Season")
(Fig.SedimentSeason <- ggboxplot(SedimentSeason, x = "Season", y = "Conc",
                                 order = c("Winter", "Spring", "Summer", "Fall"),
                                 xlab = "\nSeason",
                                 ylab = "Concentration of Vp in sediment \n(log MPN/g)") +
    stat_pvalue_manual(data = pwc.SedimentSeason, hide.ns = T, 
                       xmin = "group1", xmax = "group2",
                       label = "p.adj.signif", tip.length = 0.01,
                       y.position = "y.position")  + ggtitle("C"))
ggsave(filename = file.path("Figures/Kruskal","SedimentSeason.jpeg"))


# Compare groups in season in all type of samples
SeasonConcData$OysterSummer  <- summerConcOyster$Conc
summary(summerConc)
fallConc<- concData %>% group_by(Season) %>% filter(Season == "Fall")
summary(fallConc)
winterConc <- concData %>% group_by(Season) %>% filter(Season == "Winter")
summary(winterConc)
springConc <- concData %>% group_by(Season) %>% filter(Season == "Spring")
summary(springConc)
# test the normality of sample type data
sampleType <- data2
sampleType <- select(sampleType, -c("Season"))
sampleType <- sampleType %>%rename(
  Oyster = Oy,
  Seawater = Sw,
  Sediment = Sed
)
sampleType <- stack(sampleType)

shapiro.test(sampleType$values) # data is abnormal
# Compute Kruskal-Wallis test
kr.sType <- sampleType %>% kruskal_test(values ~ ind)
print(kr.sType) # significant between seasons
# Pairwise comparisons
pwc.sType <- sampleType %>% dunn_test(values ~ ind, p.adjust.method = "bonferroni")
pwc.sType

# Visualization: box plots with p-values
pwc.sType <- pwc.sType %>% add_xy_position(x = "ind")
(sType <- ggboxplot(sampleType, x = "ind", y = "values",
                    xlab = "\nType of sample",
                    ylab = "Concentration of Vp in samples \n(log MPN/g or log MPN/mL)") +
    stat_pvalue_manual(data = pwc.sType, hide.ns = T, 
                       xmin = "group1", xmax = "group2",
                       label = "p.adj.signif", tip.length = 0.01,
                       y.position = "y.position") )
ggsave(filename = file.path("Figures/Kruskal","sampleTypes.jpeg"))

# Comparison between locations
LocData <- allData %>% select(Loc, Conc, Type)
LocData$Conc <- log10(LocData$Conc)

summary(SenggangLocOyster <- LocData %>% group_by(Loc) %>% filter(Loc == "Senggang") %>% filter(Type == "Oyster"))
summary(SenggangLocSeawater <- LocData %>% group_by(Loc) %>% filter(Loc == "Senggang") %>% filter(Type == "Seawater"))
summary(SenggangLocSediment <- LocData %>% group_by(Loc) %>% filter(Loc == "Senggang") %>% filter(Type == "Sediment"))
summary(QiguLocOyster <- LocData %>% group_by(Loc) %>% filter(Loc == "Qigu") %>% filter(Type == "Oyster"))
summary(QiguLocSeawater <- LocData %>% group_by(Loc) %>% filter(Loc == "Qigu") %>% filter(Type == "Seawater"))
summary(QiguLocSediment <- LocData %>% group_by(Loc) %>% filter(Loc == "Qigu") %>% filter(Type == "Sediment"))

# Compare groups in Locs
# OYSTER
OysterLoc <- rbind(SenggangLocOyster, QiguLocOyster)
OysterLoc <- as.data.frame(OysterLoc)
shapiro.test(OysterLoc$Conc) # data is abnormal
# Compute Kruskal-Wallis test
kr.OysterLoc <- OysterLoc %>% kruskal_test(Conc ~ Loc)
print(kr.OysterLoc) # significant between Locs
# Pairwise comparisons
pwc.OysterLoc <- OysterLoc %>% dunn_test(Conc ~ Loc, p.adjust.method = "bonferroni")
pwc.OysterLoc
pwc.OysterLoc <- pwc.OysterLoc %>% add_xy_position(x = "Loc")
(Fig.OysterLoc <- ggboxplot(OysterLoc, x = "Loc", y = "Conc",
                            xlab = "\nLoc",
                            ylab = "Concentration of Vp in oysters \n(log MPN/g)") +
    stat_pvalue_manual(data = pwc.OysterLoc, hide.ns = T, 
                       xmin = "group1", xmax = "group2",
                       label = "p.adj.signif", tip.length = 0.01,
                       y.position = "y.position"))
ggsave(filename = file.path("Figures/Kruskal","OysterLoc.jpeg"))

# SEAWATER
SeawaterLoc <- rbind(SenggangLocSeawater, QiguLocSeawater)
SeawaterLoc <- as.data.frame(SeawaterLoc)
shapiro.test(SeawaterLoc$Conc) # data is abnormal
# Compute Kruskal-Wallis test
kr.SeawaterLoc <- SeawaterLoc %>% kruskal_test(Conc ~ Loc)
print(kr.SeawaterLoc) # significant between Locs
# Pairwise comparisons
pwc.SeawaterLoc <- SeawaterLoc %>% dunn_test(Conc ~ Loc, p.adjust.method = "bonferroni")
pwc.SeawaterLoc
pwc.SeawaterLoc <- pwc.SeawaterLoc %>% add_xy_position(x = "Loc")
(Fig.SeawaterLoc <- ggboxplot(SeawaterLoc, x = "Loc", y = "Loc",
                              xlab = "\nLoc",
                              ylab = "Locentration of Vp in seawater \n(log MPN/mL)") +
    stat_pvalue_manual(data = pwc.SeawaterLoc, hide.ns = T, 
                       xmin = "group1", xmax = "group2",
                       label = "p.adj.signif", tip.length = 0.01,
                       y.position = "y.position") +
    labs(subtitle = get_test_label(kr.SeawaterLoc, detailed = TRUE)))
ggsave(filename = file.path("Figures/Kruskal","SeawaterLoc.jpeg"))

# SEDIMENT
SedimentLoc <- rbind(SenggangLocSediment, QiguLocSediment)
SedimentLoc <- as.data.frame(SedimentLoc)
shapiro.test(SedimentLoc$Conc) # data is abnormal
# Compute Kruskal-Wallis test
kr.SedimentLoc <- SedimentLoc %>% kruskal_test(Conc ~ Loc)
print(kr.SedimentLoc) # significant between Locs
# Pairwise comparisons
pwc.SedimentLoc <- SedimentLoc %>% dunn_test(Conc ~ Loc, p.adjust.method = "bonferroni")
pwc.SedimentLoc
pwc.SedimentLoc <- pwc.SedimentLoc %>% add_xy_position(x = "Loc")
(Fig.SedimentLoc <- ggboxplot(SedimentLoc, x = "Loc", y = "Loc",
                              xlab = "\nLoc",
                              ylab = "Locentration of Vp in sediment \n(log MPN/g)") +
    stat_pvalue_manual(data = pwc.SedimentLoc, hide.ns = T, 
                       xmin = "group1", xmax = "group2",
                       label = "p.adj.signif", tip.length = 0.01,
                       y.position = "y.position") +
    labs(subtitle = get_test_label(kr.SedimentLoc, detailed = TRUE)))
ggsave(filename = file.path("Figures/Kruskal","SedimentLoc.jpeg"))


# Compare groups in Loc in all type of samples
shapiro.test(LocData$Conc) # data is abnormal
# Compute Kruskal-Wallis test
kr.LocData <- LocData %>% kruskal_test(Conc ~ Loc)
print(kr.LocData) # significant between Locs
# Pairwise comparisons
pwc.LocData <- sampleType %>% dunn_test(values ~ ind, p.adjust.method = "bonferroni")
pwc.LocData

# Visualization: box plots with p-values
pwc.LocData <- pwc.LocData %>% add_xy_position(x = "ind")
(Fig.LocData <- ggboxplot(sampleType, x = "ind", y = "values",
                          xlab = "\nType of sample",
                          ylab = "Locentration of Vp in samples \n(log MPN/g or log MPN/mL)") +
    stat_pvalue_manual(data = pwc.LocData, hide.ns = T, 
                       xmin = "group1", xmax = "group2",
                       label = "p.adj.signif", tip.length = 0.01,
                       y.position = "y.position") +
    labs(subtitle = get_test_label(kr.LocData, detailed = TRUE)))
ggsave(filename = file.path("Figures/Kruskal","sampleTypes.jpeg"))

data3a <- select(data2, c(Oy, Season))
names(data3a) <-  c("Conc", "Season")
data3b <- select(data2, c(Sw, Season))
names(data3b) <-  c("Conc", "Season")
data3c <- select(data2, c(Sed, Season))
names(data3c) <-  c("Conc", "Season")
data3 <- rbind(data3a, data3b, data3c)
rm(data3a, data3b, data3c)

shapiro.test(data3$Conc) # data is abnormal
ggplot(data3, aes(x = Conc)) + geom_histogram(binwidth = 0.5, color="black", fill="green") +
  geom_vline(aes(xintercept=mean(Conc)),
             color="blue", linetype="dashed", size=1) +
  labs(title="Conc. of Vp histogram plot",x="Conc(log MPN/g)", y = "Frequency")
ggsave(filename = file.path("Figures/Kruskal","season.jpeg"))
# Compute Kruskal-Wallis test
kr.test <- data3 %>% kruskal_test(Conc ~ Season)
print(kr.test) # significant between seasons
# check the effect size
eSize <- data3 %>% kruskal_effsize(Conc ~ Season)
eSize

# Pairwise comparisons
(pwcAll1 <- data3 %>% dunn_test(Conc ~ Season, p.adjust.method = "bonferroni"))

# Visualization: box plots with p-values
(pwcAll1 <- pwcAll1 %>% add_xy_position(x = "Season"))
(Season <- ggboxplot(data3, x = "Season", y = "Conc",
                     order = c("Winter", "Spring", "Summer", "Fall"),
                     xlab = "\nSeason",
                     ylab = "Concentration of Vp in all types of sample \n(log MPN/g or log MPN/mL)") +
    stat_pvalue_manual(data = pwcAll1, hide.ns = T, 
                       xmin = "group1", xmax = "group2",
                       label = "p.adj.signif", tip.length = 0.01,
                       y.position = "y.position") + ggtitle("D"))
ggsave(filename = file.path("Figures/Kruskal","allseason.jpeg"))

# test the normality of oyster data only
concOyster <- select(data2, c(Oy, Season))
concOyster$Season <- as.factor(concOyster$Season)

shapiro.test(concOyster$Oy) # data is abnormal
# Compute Kruskal-Wallis test
kr.test.Oyster <- concOyster %>% kruskal_test(Oy ~ Season)
print(kr.test.Oyster) # significant between seasons
# Pairwise comparisons
pwcOyster <- concOyster %>% dunn_test(Oy ~ Season, p.adjust.method = "bonferroni")
pwcOyster

# Visualization: box plots with p-values
pwcOyster <- pwcOyster %>% add_xy_position(x = "Season")
sType3 <- ggboxplot(concOyster, x = "Season", y = "Oy",
                    xlab = "\nSeason",
                    ylab = "Concentration of Vp in oyster\n",
                    order = c("Winter", "Spring", "Summer", "Fall")) +
  stat_pvalue_manual(pwcOyster, hide.ns = TRUE) +ggtitle("A")
sType3
ggsave(filename = file.path("Figures/Kruskal","oysterOnly.jpeg"))
# test the normality of seawater data only
concSeawater <- select(data2, c(Sw, Season))
concSeawater$Season <- as.factor(concSeawater$Season)

shapiro.test(concSeawater$Sw) # data is abnormal
# Compute Kruskal-Wallis test
kr.test.Seawater <- concSeawater %>% kruskal_test(Sw ~ Season)
print(kr.test.Seawater) # significant between seasons
# Pairwise comparisons
pwcSeawater <- concSeawater %>% dunn_test(Sw ~ Season, p.adjust.method = "bonferroni")
pwcSeawater

# Visualization: box plots with p-values
pwcSeawater <- pwcSeawater %>% add_xy_position(x = "Season")
sType4 <- ggboxplot(concSeawater, x = "Season", y = "Sw",
                    xlab = "\nSeason",
                    ylab = "Concentration of Vp in seawater\n") +
  stat_pvalue_manual(pwcSeawater, hide.ns = T, label = "p.adj.signif") +
  labs(
    subtitle = get_test_label(kr.test.Seawater, detailed = TRUE)
  )
sType4
ggsave(filename = file.path("Figures/Kruskal","seawaterOnly.jpeg"))
# test the normality of sediment data only
concSediment <- select(data2, c(Sed, Season))
concSediment$Season <- as.factor(concSediment$Season)

shapiro.test(concSediment$Sed) # data is abnormal
# Compute Kruskal-Wallis test
kr.test.Sediment <- concSediment %>% kruskal_test(Sed ~ Season)
print(kr.test.Sediment) # significant between seasons
# Pairwise comparisons
pwcSediment <- concSediment %>% dunn_test(Sed ~ Season, p.adjust.method = "bonferroni")
pwcSediment

# Visualization: box plots with p-values
pwcSediment <- pwcSediment %>% add_xy_position(x = "Season")
ggboxplot(concSediment, x = "Season", y = "Sed",
          xlab = "\nSeason",
          ylab = "Concentration of Vp in sediment\n") +
  stat_pvalue_manual(pwcSediment, hide.ns = F, label = "p.adj.signif") +
  labs(
    subtitle = get_test_label(kr.test.Sediment, detailed = TRUE)
  )
ggsave(filename = file.path("Figures/Kruskal","SedimentOnly.jpeg"))

#arrange the plots in a page
(kr.testAll <- ggarrange(sType,
                         Season,
                         ncol = 2, nrow = 1))
ggsave(filename = file.path("Figures/Kruskal","combinedAll.jpeg"))


SeasonType <- ggarrange(Fig.OysterSeason, 
                        Fig.SeawaterSeason,
                        Fig.SedimentSeason,
                        Season,
                        ncol = 2, nrow = 2)
SeasonType
ggsave(filename = file.path("Figures/Kruskal","combinedAll2.jpeg"))

###########################################
######## Random Forest Modeling ###########
###########################################

# Data Preparation and Pre-processing
rfData <- oysterData
anyNA(rfData)

rfData$Oy[is.na(rfData$Oy)] <- 1.5
rfData$Sw[is.na(rfData$Sw)] <- 1.5
rfData$Sed[is.na(rfData$Sed)] <- 1.5
rfData$Oy <- log10(rfData$Oy)
rfData$Sw <- log10(rfData$Sw)
rfData$Sed <- log10(rfData$Sed)
rfData <- select(rfData, -c(Year, Month, Season, Repl, Loc, Year, Date))
str(rfData)
dim(rfData) # check the row and column
class(rfData) #check the data class
names(rfData) # check the variables names
glimpse(rfData)

# imputing data
library(missForest)
rfData <- as.data.frame(rfData)
imprfData <- missForest(rfData)
rfData <- imprfData$ximp
anyNA(rfData)
str(rfData)

names(rfData)
# randomize the data
set.seed(123)
rand_rfData <-  rfData[sample(1:nrow(rfData)), ]
dim(rand_rfData)
str(rand_rfData)
library(pastecs)
names(rand_rfData)
stat.desc(rand_rfData$SST_avgX_3, basic = F)

library(caret)

#dependent variable columns
dependent1 <- c('Oy', 'Sw', 'Sed')
#remove variable columns
rand_rfData2 <- rand_rfData[!names(rand_rfData) %in% dependent1]
str(rand_rfData2)
#run correlations
cor_mat1 <- cor(rand_rfData2, method = "spearman")

#the index of the columns to be removed because they have a high correlation
index1 <- findCorrelation(cor_mat1, .99)

#the name of the columns chosen above
to_be_removed1 <- colnames(cor_mat1)[index1]

#now go back to df and use to_be_removed to subset the original df
rand_rfData <- rand_rfData[!names(rand_rfData) %in% to_be_removed1]
glimpse(rand_rfData)

# RF MODEL FOR OYSTER
rfData1 <- rand_rfData
rfData1 <- select(rfData1, -c("Sw", "Sed"))

# splitting data
splitDataOyster <- createDataPartition(rfData1$Oy, p = 0.75, list = FALSE)
trainDataOyster <- rfData1[splitDataOyster,]
testDataOyster <- rfData1[-splitDataOyster,]

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
rf.Oyster_mtry <- train(Oy ~ ., 
                        data = trainDataOyster, 
                        method = "ranger", 
                        metric = "RMSE", 
                        trControl = fitControl,
                        importance = "permutation",
                        tuneGrid = tuneGrid1,
                        num.trees = 2000)
print(rf.Oyster_mtry)
saveRDS(rf.Oyster_mtry,"./Result/rf.Oyster_mtry.rds")
rf.Oyster_mtry <- readRDS("./Result/rf.Oyster_mtry.rds")
plot(rf.Oyster_mtry)
best_mtry.Oyster <- rf.Oyster_mtry$bestTune$mtry 
best_splitrule.Oyster <- rf.Oyster_mtry$bestTune$splitrule
best_nodesize.Oyster <- rf.Oyster_mtry$bestTune$min.node.size
# tune the grids
tuneGrid.Oyster <- expand.grid(.mtry = best_mtry.Oyster,
                               .splitrule = best_splitrule.Oyster, 
                               .min.node.size = best_nodesize.Oyster)

set.seed(222)
fit_rf.Oyster <- train(Oy ~ ., 
                       data=trainDataOyster, 
                       method="ranger", 
                       metric="RMSE", 
                       tuneGrid = tuneGrid.Oyster,
                       trControl=fitControl,
                       importance = "permutation",
                       num.trees = 20000)
fit_rf.Oyster
saveRDS(fit_rf.Oyster,"./Result/fit_rf.Oyster.rds")
fit_rf.Oyster <- readRDS("./Result/fit_rf.Oyster.rds")

# prediction 
rf_predict.Oyster <- predict(fit_rf.Oyster, testDataOyster)

#performance
rFperformance.Oyster <- postResample(pred = rf_predict.Oyster, obs = testDataOyster$Oy)
rFperformance.Oyster

# plot the important variables
library(pdp)
varimp_oyster <- varImp(fit_rf.Oyster)
rfOyster <- ggplot(varimp_oyster, aes(x = reorder(variable, importance), 
                                      y=importance,
                                      fill=importance), 
                   top = 10) + 
  geom_bar(stat="identity", position="dodge") +
  ylab("\nImportance")+
  xlab("Important variables\n") +
  theme_classic() + ggtitle("A")
rfOyster
ggsave(filename = file.path("Figures/rF","varimp_rf_tuned_oysters.jpeg"))

# plot the partial dependence plots
(pdpOyster1 <- partial(fit_rf.Oyster, pred.var = "SST_avgX_3", plot = T, plot.engine = "ggplot2") +
    theme_classic() +
    theme_classic() + 
    ylab("Concentration of Vp\n") + 
    xlab("\nAverage SST d-3") + 
    scale_y_continuous(labels = scales::comma_format(accuracy = 0.01)))
pdpOyster1
ggsave(filename = file.path("Figures","pdpOyster1.jpeg"))
pdpOyster2 <- partial(fit_rf.Oyster, pred.var = "SST_imX_10", plot = T, plot.engine = "ggplot2") +
  theme_classic() +
  theme_classic() + 
  ylab("Concentration of Vp\n") + 
  xlab("\nMinimum SST d-10") + 
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.01)) 
pdpOyster2
ggsave(filename = file.path("Figures","pdpOyster2.jpeg"))

#arrange the plots in a page
pdpOysterAll <- ggarrange(pdpOyster1, 
                          pdpOyster2, 
                          ncol = 2, nrow = 1)
pdpOysterAll
ggsave(filename = file.path("Figures/rF","pdpOysterAll.jpeg"))
# --------------------------------------------------------------------

# RF MODEL FOR SEAWATER
rfData2 <- rand_rfData
rfData2 <- select(rfData2, -c("Oy", "Sed"))

# splitting data
splitDataSeawater <- createDataPartition(rfData2$Sw, p = 0.75, list = FALSE)
trainDataSeawater <- rfData2[splitDataSeawater,]
testDataSeawater <- rfData2[-splitDataSeawater,]
set.seed(333)
rf.Seawater_mtry <- train(Sw ~ ., 
                          data = trainDataSeawater, 
                          method = "ranger", 
                          metric = "RMSE", 
                          trControl = fitControl,
                          importance = "permutation",
                          tuneGrid = tuneGrid1,
                          num.trees = 2000)
print(rf.Seawater_mtry)
saveRDS(rf.Seawater_mtry,"./Result/rf.Seawater_mtry.rds")
rf.Seawater_mtry <- readRDS("./Result/rf.Seawater_mtry.rds")
plot(rf.Seawater_mtry)
best_mtry.Seawater <- rf.Seawater_mtry$bestTune$mtry
best_splitrule.Seawater <- rf.Seawater_mtry$bestTune$splitrule
best_nodesize.Seawater <- rf.Seawater_mtry$bestTune$min.node.size

# tune the grids
tuneGrid.Seawater <- expand.grid(.mtry = best_mtry.Seawater,
                                 .splitrule = best_splitrule.Seawater, 
                                 .min.node.size = best_nodesize.Seawater)

set.seed(444)
fit_rf.Seawater <- train(Sw ~ ., 
                         data=trainDataSeawater, 
                         method="ranger", 
                         metric="RMSE", 
                         tuneGrid = tuneGrid.Seawater,
                         trControl=fitControl,
                         importance = "permutation",
                         num.trees = 20000)
fit_rf.Seawater
saveRDS(fit_rf.Seawater,"./Result/fit_rf.Seawater.rds")
fit_rf.Seawater <- readRDS("./Result/fit_rf.Seawater.rds")

# prediction 
rf_predict.Seawater <- predict(fit_rf.Seawater, testDataSeawater)

#performance
rFperformance.Seawater <- postResample(pred = rf_predict.Seawater, obs = testDataSeawater$Sw)
rFperformance.Seawater

# plot the important variables
varimp_Seawater <- varImp(fit_rf.Seawater)
rfSeawater <- ggplot(varimp_Seawater, aes(x = reorder(variable, importance), 
                                          y=importance,
                                          fill=importance), 
                     top = 10) + 
  geom_bar(stat="identity", position="dodge") +
  ylab("\nImportance")+
  xlab("Important variables\n") +
  theme_classic() + ggtitle("B")
rfSeawater
ggsave(filename = file.path("Figures/rF","varimp_rf_tuned_Seawaters.jpeg"))

# plot the partial dependence plots
pdpSeawater1 <- partial(fit_rf.Seawater, pred.var = "SST_imX_10", plot = T, plot.engine = "ggplot2") +
  theme_classic() +
  theme_classic() + 
  ylab("Concentration of Vp\n") + 
  xlab("\nMinimum SST d-10") + 
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.01))
pdpSeawater1
ggsave(filename = file.path("Figures","pdpSeawater1.jpeg"))
pdpSeawater2 <- partial(fit_rf.Seawater, pred.var = "SST_avgX_3", plot = T, plot.engine = "ggplot2") +
  theme_classic() +
  theme_classic() + 
  ylab("Concentration of Vp\n") + 
  xlab("\nAverage SST d-3") + 
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.01)) 
pdpSeawater2
ggsave(filename = file.path("Figures","pdpSeawater2.jpeg"))

#arrange the plots in a page
pdpSeawaterAll <- ggarrange(pdpSeawater1, 
                            pdpSeawater2, 
                            ncol = 2, nrow = 1)
pdpSeawaterAll
ggsave(filename = file.path("Figures/rF","pdpSeawaterAll.jpeg"))
# --------------------------------------------------------------------

# RF MODEL FOR SEDIMENT
rfData3 <- rand_rfData
rfData3 <- select(rfData3, -c("Sw", "Oy"))

# splitting data
splitDataSediment <- createDataPartition(rfData3$Sed, p = 0.75, list = FALSE)
trainDataSediment <- rfData3[splitDataSediment,]
testDataSediment <- rfData3[-splitDataSediment,]
set.seed(555)
rf.Sediment_mtry <- train(Sed ~ ., 
                          data = trainDataSediment, 
                          method = "ranger", 
                          metric = "RMSE", 
                          trControl = fitControl,
                          importance = "permutation",
                          tuneGrid = tuneGrid1,
                          num.trees = 2000)
print(rf.Sediment_mtry)
saveRDS(rf.Sediment_mtry,"./Result/rf.Sediment_mtry.rds")
rf.Sediment_mtry <- readRDS("./Result/rf.Sediment_mtry.rds")
plot(rf.Sediment_mtry)
best_mtry.Sediment <- rf.Sediment_mtry$bestTune$mtry 
best_splitrule.Sediment <- rf.Sediment_mtry$bestTune$splitrule
best_nodesize.Sediment <- rf.Sediment_mtry$bestTune$min.node.size

# tune the grids
tuneGrid.Sediment <- expand.grid(.mtry = best_mtry.Sediment,
                                 .splitrule = best_splitrule.Sediment, 
                                 .min.node.size = best_nodesize.Sediment)

set.seed(666)
fit_rf.Sediment <- train(Sed ~ ., 
                         data = trainDataSediment, 
                         method = "ranger", 
                         metric = "RMSE", 
                         tuneGrid = tuneGrid.Sediment,
                         trControl = fitControl,
                         importance = "permutation",
                         num.trees = 20000)
fit_rf.Sediment
saveRDS(fit_rf.Sediment,"./Result/fit_rf.Sediment.rds")
fit_rf.Sediment <- readRDS("./Result/fit_rf.Sediment.rds")

# prediction 
rf_predict.Sediment <- predict(fit_rf.Sediment, testDataSediment)

#performance
rFperformance.Sediment <- postResample(pred = rf_predict.Sediment, obs = testDataSediment$Sed)
rFperformance.Sediment

# plot the important variables
varimp_Sediment <- varImp(fit_rf.Sediment)
rfSediment <- ggplot(varimp_Sediment, aes(x = reorder(variable, importance), 
                                          y=importance,
                                          fill=importance), 
                     top = 10) + 
  geom_bar(stat="identity", position="dodge") +
  ylab("\nImportance")+
  xlab("Important variables\n") +
  theme_classic()+ ggtitle("C")
rfSediment
ggsave(filename = file.path("Figures/rF","varimp_rf_tuned_Sediments.jpeg"))

# plot the partial dependence plots
pdpSediment1 <- partial(fit_rf.Sediment, pred.var = "SST_avgX_3", plot = T, plot.engine = "ggplot2") +
  theme_classic() +
  theme_classic() + 
  ylab("Concentration of Vp\n") + 
  xlab("\nAverage SST d-3") + 
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.01))
pdpSediment1
ggsave(filename = file.path("Figures","pdpSediment1.jpeg"))
pdpSediment2 <- partial(fit_rf.Sediment, pred.var = "SST_maxX_0", plot = T, plot.engine = "ggplot2") +
  theme_classic() +
  theme_classic() + 
  ylab("Concentration of Vp\n") + 
  xlab("\nMaximum SST d-0") + 
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.01)) 
pdpSediment2
ggsave(filename = file.path("Figures","pdpSediment2.jpeg"))

#arrange the plots in a page
pdpSedimentAll <- ggarrange(pdpSediment1, 
                            pdpSediment2, 
                            ncol = 2, nrow = 1)
pdpSedimentAll
ggsave(filename = file.path("Figures/rF","pdpSedimentAll.jpeg"))
# --------------------------------------------------------------------

# rF model for all samples
data5 <- data.frame(allData)
data5$Conc[is.na(data5$Conc)] <- 1.5
data5$Conc <- log10(data5$Conc)
data5 <- select(data5, -c(Year, Type, Month, Season, Loc, Date))
anyNA(data5)
imp.data5 <- missForest(data5)
data5 <- imp.data5$ximp
anyNA(data5)

# Store X and Y for later use.
x_All = data5[, 2:111]
y_All = data5$Conc

#dependent variable columns
dependent2 <- c('Conc')
#remove variable columns
data5_2 <- data5[!names(data5) %in% dependent1]

#run correlations
cor_mat2 <- cor(data5_2, method = "spearman")

#the index of the columns to be removed because they have a high correlation
index2 <- findCorrelation(cor_mat2, .99)

#the name of the columns chosen above
to_be_removed2 <- colnames(cor_mat2)[index2]

#now go back to df and use to_be_removed to subset the original df
data5 <- data5[!names(data5) %in% to_be_removed2]
glimpse(data5)

# splitting data
split_data5 <- createDataPartition(data5$Conc, p = 0.75, list=FALSE)
trainDataAll <- data5[split_data5,]
testDataAll <- data5[-split_data5,]

# train model based on provided tuning hyperparameters
set.seed(777)
rf.All_mtry <- train(Conc ~ ., 
                     data = trainDataAll, 
                     method = "ranger", 
                     metric = "RMSE", 
                     trControl = fitControl,
                     importance = "permutation",
                     tuneGrid = tuneGrid1,
                     num.trees = 2000)
print(rf.All_mtry)
saveRDS(rf.All_mtry,"./Result/rf.All_mtry.rds")
rf.All_mtry <- readRDS("./Result/rf.All_mtry.rds")
rf.All_mtry
rf.All_mtry$bestTune$mtry # the best mtry value
best_mtry.All <- rf.All_mtry$bestTune$mtry 
best_splitrule.All <- rf.All_mtry$bestTune$splitrule
best_nodesize.All <- rf.All_mtry$bestTune$min.node.size

# Search the best ntrees
tuneGrid.All <- expand.grid(.mtry = best_mtry.All,
                            .splitrule = best_splitrule.All, 
                            .min.node.size = best_nodesize.All)
set.seed(888)
fit_rf.All <- train(Conc ~ ., 
                    data = trainDataAll, 
                    method = "ranger", 
                    metric = "RMSE", 
                    trControl = fitControl,
                    importance = "permutation",
                    tuneGrid = tuneGrid.All,
                    num.trees = 20000)
fit_rf.All
saveRDS(fit_rf.All,"./Result/fit_rf.All.rds")
fit_rf.All <- readRDS("./Result/fit_rf.All.rds")

# prediction 
rf_predict.All <- predict(fit_rf.All, testDataAll)

# performance
rFperformance.All <- postResample(pred = rf_predict.All, obs = testDataAll$Conc)
rFperformance.All

# plot the important variables
varimp_All <- varImp(fit_rf.All)
rfAll <- ggplot(varimp_All, aes(x = reorder(variable, importance), 
                                y=importance,
                                fill=importance), 
                top = 10) + 
  geom_bar(stat="identity", position="dodge") +
  ylab("\nImportance")+
  xlab("Important variables\n") +
  theme_classic() + ggtitle("D")
rfAll
ggsave(filename = file.path("Figures/rF","varimp_rf_All.jpeg"))

# plot the partial dependence plots
pdpAll1 <- partial(fit_rf.All, pred.var = "SST_maxX_2", plot = T, plot.engine = "ggplot2") +
  theme_classic() +
  theme_classic() + 
  ylab("Concentration of Vp\n") + 
  xlab("\nMaximum SST d-2") + 
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.01)) 
pdpAll1
ggsave(filename = file.path("Figures","pdpAll2.jpeg"))

pdpAll2 <- partial(fit_rf.All, pred.var = "SST_avgX_3", plot = T, plot.engine = "ggplot2") +
  theme_classic() +
  theme_classic() + 
  ylab("Concentration of Vp\n") + 
  xlab("\nAverage SST d-3") + 
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.01)) 
pdpAll2
ggsave(filename = file.path("Figures","pdpAll2.jpeg"))

#arrange the plots in a page
pdpAllAll <- ggarrange(pdpAll1, 
                       pdpAll2,
                       ncol = 2, nrow = 1)
pdpAllAll
ggsave(filename = file.path("Figures/rF","pdpAllAll.jpeg"))

varImpCombined <- ggarrange(rfOyster,
                            rfSeawater,
                            rfSediment, 
                            rfAll,
                            ncol = 2, nrow = 2)
varImpCombined
ggsave(filename = file.path("Figures/rF","varImpCombined.jpeg"))

# ---------------------------------------------------------------------

# Performance summary
modelPerformance_trainData <- dplyr::bind_rows(list(oysterModel = fit_rf.Oyster$results,
                                                    seawaterModel = fit_rf.Seawater$results,
                                                    sedimentModel = fit_rf.Sediment$results,
                                                    allTypeModel = fit_rf.All$results))
rownames(modelPerformance_trainData) <- c("oysterModel", 
                                          "seawaterModel", 
                                          "sedimentModel",
                                          "allTypes")
modelPerformance_trainData

modelPerformance_testData <- dplyr::bind_cols(list(
  perf_OysterModel = rFperformance.Oyster,
  perf_SeawaterModel = rFperformance.Seawater,
  perf_SedimentModel = rFperformance.Sediment,
  perf_AllType = rFperformance.All))
modelPerformance_testData <- t(modelPerformance_testData)
colnames(modelPerformance_testData) <- c("RMSE", "Rsquared", "MAE")

library(writexl)
modelPerformance_trainData
write_xlsx(modelPerformance_trainData,  "Result/modelPerformance_trainData.xlsx")
modelPerformance_testData
modelPerformance_testData <- as.data.frame(modelPerformance_testData)
write_xlsx(modelPerformance_testData,  "Result/modelPerformance_testData.xlsx")



pdpAllAll2 <- ggarrange(pdpOyster1, 
                        pdpOyster2,
                        pdpSeawater1, 
                        pdpSeawater2,
                        pdpSediment1, 
                        pdpSediment2,
                        pdpAll1, 
                        pdpAll2,
                        labels = "AUTO", hjust = -6.5,
                        ncol = 4, nrow = 2)
pdpAllAll2
ggsave(filename = file.path("Figures/rF","pdpAllAll2.jpeg"))

# NMRSE
library(hydroGOF)

nrmseOyster <- nrmse(sim = rf_predict.Oyster, obs = testDataOyster$Oy, norm="maxmin")
nrmseSeawater <- nrmse(sim = rf_predict.Seawater, obs = testDataSeawater$Sw, norm="maxmin")
nrmseSediment <- nrmse(sim = rf_predict.Sediment, obs = testDataSediment$Sed, norm="maxmin")
nrmseAll <- nrmse(sim = rf_predict.All, obs = testDataAll$Conc, norm="maxmin")

nrmseList <- list(Oyster = nrmseOyster,
                  Seawater = nrmseSeawater,
                  Sediment = nrmseSediment,
                  allTypes = nrmseAll)
nrmseValue <- rbind(nrmseList)

OysterInd <- (rf_predict.Oyster - testDataOyster$Oy)/rf_predict.Oyster
OysterRE <- 100*(length(which(OysterInd > -1 & OysterInd < 0.5))/length(OysterInd))
SeawaterInd <- (rf_predict.Seawater - testDataSeawater$Sw)/rf_predict.Seawater
SeawaterRE <- 100*(length(which(SeawaterInd > -1 & SeawaterInd < 0.5))/length(SeawaterInd))
SedimentInd <- (rf_predict.Sediment - testDataSediment$Sed)/rf_predict.Sediment
SedimentRE <- 100*(length(which(SedimentInd > -1 & SedimentInd < 0.5))/length(SedimentInd))
AllInd <- (rf_predict.All - testDataAll$Conc)/rf_predict.All
AllRE <- 100*(length(which(AllInd > -1 & AllInd < 0.5))/length(AllInd))
pRE <- rbind(list(Oyster = OysterRE,
                  Seawater = SeawaterRE,
                  Sediment = SedimentRE,
                  allTypes = AllRE))

glimpse(oysterData)
dateData <- select(allData, c(Conc, Date, Type))
dateData$Conc <- log10(dateData$Conc)

library(lubridate)
# set strings as factors to false
options(stringsAsFactors = FALSE)
class(dateData$Date)
# plot the data using ggplot2 and pipes
dateData %>%
  ggplot(aes(x = Date, y = Conc, group = Type, color = Type)) +  
  geom_point(na.rm = T) + geom_smooth(se = F) +
  labs(y = "log MPN/g or log MPN/mL",
       x = "Time") +
  scale_y_continuous(expand = c(0, 0), limits=c(0, 10)) +
  theme_classic()
