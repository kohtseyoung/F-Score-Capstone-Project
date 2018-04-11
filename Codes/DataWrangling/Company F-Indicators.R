#load library
library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)


##################################### Inputting respective financial datas #########################################

Company_IncomeStatement <- read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Full Data Set\\Income Statement\\companylist(IS Final).csv")
Company_CashFlow <- read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Full Data Set\\Cash FLow\\companylist(CF Final).csv")
Company_BalanceSheet <- read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Full Data Set\\Balance Sheet\\companylist(BS Final).csv")
Company_KeyRatio <- read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Full Data Set\\Key Ratios\\companylist(KR Final).csv")


Company_IncomeStatement <- Company_IncomeStatement[,c(3:14)]
Company_BalanceSheet <- Company_BalanceSheet[,c(2:9)]
Company_KeyRatio <-Company_KeyRatio[,c(2:18)]
Company_CashFlow <-Company_CashFlow[,c(2:14)]
head(Company_IncomeStatement)
head(Company_BalanceSheet)
head(Company_KeyRatio)
head(Company_CashFlow)

##################################### Creating Basic Data for F Data Set ###########################################

#-----Net Income-----#
  #Source: Income Statement
NetIncome <- Company_IncomeStatement %>% filter(Income_Statement_Items=="Net income")
NetIncome <- data.frame(NetIncome[,c(1,2,3,4,5,6,9)],stringsAsFactors=FALSE)
colnames(NetIncome) <- c("Items","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
NetIncome[is.na(NetIncome)] <-0
head(NetIncome)

  #Source: Key Ratio
NetIncome_KR <- Company_KeyRatio %>% filter(grepl('^Net Income', Key_Ratio_Items))
NetIncome_KR<-NetIncome_KR[,c(1:11,14)]
colnames(NetIncome_KR) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
NetIncome_KR <- NetIncome_KR %>% filter(!grepl('%', Items))
NetIncome_KR$Items <-as.character(NetIncome_KR$Items)
NetIncome_KR$Items[grepl("^Net Income",NetIncome_KR$Items)] <- "NetIncome_Mil"
NetIncome_KR[,c(2:11)] <-sapply(NetIncome_KR[,c(2:11)], function (x) {as.double(gsub(",","",as.character(x)))})
NetIncome_KR[is.na(NetIncome_KR)] <-0
head(NetIncome_KR)
str(NetIncome_KR)


#-----Year End Assets-----#
  #Source: Balance Sheet
YearEndAssets <-Company_BalanceSheet %>% filter(Balance_Sheet_Items=="Total assets")
YearEndAssets <- YearEndAssets[,c(1,2,3,4,5,6,8)]
colnames(YearEndAssets) <- c("Items","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
YearEndAssets$Items <- as.character(YearEndAssets$Items)
YearEndAssets$Items[YearEndAssets$Items=="Total assets"] <- "YearEndAssets"
YearEndAssets[,c(2:11)] <-sapply(YearEndAssets[,c(2:11)], function (x) {as.double(gsub(",","",as.character(x)))})
YearEndAssets[is.na(YearEndAssets)] <-0
YearEndAssets$Y2008 <- "0"
YearEndAssets$Y2009 <- "0"
YearEndAssets$Y2010 <- "0"
YearEndAssets$Y2011 <- "0"
YearEndAssets$Y2012 <- "0"
YearEndAssets <- YearEndAssets[,c(1,7,8:12,2:6)]
head(YearEndAssets)
str(YearEndAssets)  

#-----Return on Assets-----#
  #Source: Key Ratio
ROA <- Company_KeyRatio %>% filter(Key_Ratio_Items=="Return on Assets %")
ROA <- ROA[,c(1:11,14)]
colnames(ROA) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
ROA$Items <-as.character(ROA$Items)
ROA$Items[ROA$Items=="Return on Assets %"] <- "ROA"
ROA[,c(2:11)] <-sapply(ROA[,c(2:11)], function (x) {as.double(gsub(",","",as.character(x)))})
ROA[is.na(ROA)] <-0
head(ROA)
str(ROA)

#-----Operating Cash Flow-----#
  #Source: Cash Flow 
OperatingCashFlow <-Company_CashFlow %>% filter(Cash_Flow_Items=="Operating cash flow")
OperatingCashFlow <- OperatingCashFlow[,c(1:6,9)]
head(OperatingCashFlow)

  #Source: Key Ratio
OperatingCashFlow_KR <- Company_KeyRatio %>% filter(grepl('^Operating Cash Flow', Key_Ratio_Items))
OperatingCashFlow_KR<-OperatingCashFlow_KR[,c(1:11,14)]
colnames(OperatingCashFlow_KR) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
OperatingCashFlow_KR <- OperatingCashFlow_KR %>% filter(!grepl('%', Items)) #To filter out Operating Cash Flow Growth %
OperatingCashFlow_KR$Items <-as.character(OperatingCashFlow_KR$Items)
OperatingCashFlow_KR$Items[grepl("^Operating Cash Flow",OperatingCashFlow_KR$Items)] <- "OperatingCashFlow_Mil" # To remove currency tied to Items name, ensuring all item names are similar
OperatingCashFlow_KR[,c(2:11)] <- sapply(OperatingCashFlow_KR[,c(2:11)], function (x) {as.double(gsub(",","",as.character(x)))})
OperatingCashFlow_KR[is.na(OperatingCashFlow_KR)] <- 0
head(OperatingCashFlow_KR)
str(OperatingCashFlow_KR)
  
#-----Long-term Debt-----#
  #Source: Balance Sheet
LongtermDebt <- Company_BalanceSheet %>% filter(Balance_Sheet_Items == "Long-term debt")
LongtermDebt <-LongtermDebt[,c(1:6,8)]
head(LongtermDebt)

  #Source: Key Ratio
LongtermDebt_KR <- Company_KeyRatio %>% filter(grepl('Long-Term Debt', Key_Ratio_Items))
LongtermDebt_KR<-LongtermDebt_KR[,c(1:11,14)]
colnames(LongtermDebt_KR) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
LongtermDebt_KR$Items <-as.character(LongtermDebt_KR$Items)
LongtermDebt_KR$Items[grepl("Long-Term Debt",LongtermDebt_KR$Items)] <- "LongtermDebt"
LongtermDebt_KR[,c(2:11)] <-sapply(LongtermDebt_KR[,c(2:11)], function (x) {as.double(gsub(",","",as.character(x)))})
LongtermDebt_KR[is.na(LongtermDebt_KR)] <-0
str(LongtermDebt_KR)
head(LongtermDebt_KR)

#-----Current Ratio-----#
  #Source: Key Ratio
CurrentRatio <- Company_KeyRatio %>% filter(grepl("^Current Ratio",Key_Ratio_Items))
CurrentRatio <-CurrentRatio[,c(1:11,14)]
colnames(CurrentRatio) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
CurrentRatio$Items <-as.character(CurrentRatio$Items)
CurrentRatio[,c(2:11)] <- sapply(CurrentRatio[,c(2:11)], function (x) {as.double(gsub(",","",as.character(x)))})
CurrentRatio[is.na(CurrentRatio)] <- 0
head(CurrentRatio)
str(CurrentRatio)

#-----Number of Shares-----#
  #Source: Key Ratio
NumberShares <- Company_KeyRatio %>% filter(Key_Ratio_Items=="Shares Mil")
NumberShares <-NumberShares[,c(1:11,14)]
colnames(NumberShares) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
NumberShares$Items <-as.character(NumberShares$Items)
NumberShares[,c(2:11)] <- sapply(NumberShares[,c(2:11)], function (x) {as.double(gsub(",","",as.character(x)))})
NumberShares[is.na(NumberShares)] <-0
str(NumberShares)
head(NumberShares)
  
#-----Gross Profit Margin-----#
  #Source: Key Ratio
GrossProfit <- Company_KeyRatio %>% filter(Key_Ratio_Items=="Gross Margin %")
GrossProfit <-  GrossProfit [,c(1:11,14)]
colnames(GrossProfit) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
GrossProfit$Items <-as.character(GrossProfit$Items)
GrossProfit[,c(2:11)] <- sapply(GrossProfit[,c(2:11)], function (x) {as.double(gsub(",","",as.character(x)))})
GrossProfit[is.na(GrossProfit)] <- 0
str(GrossProfit)
head(GrossProfit)
  
#-----Sales-----#
  #Source: Key Ratio
Sales <-Company_KeyRatio %>% filter(grepl("^Revenue ",Key_Ratio_Items))
Sales <- Sales[,c(1:11,14)]
colnames(Sales) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
Sales <- Sales %>% filter(!grepl('%', Items))
Sales$Items <-as.character(Sales$Items)
Sales$Items[grepl("^Revenue",Sales$Items)] <- "Revenue_Mil"
GrossProfit$Items <-as.character(GrossProfit$Items)
Sales[,c(2:11)] <- sapply(Sales[,c(2:11)], function (x) {as.double(gsub(",","",as.character(x)))})
Sales[is.na(Sales)] <- 0
Sales <- Sales[,c(1,12,2:11)]
head(Sales)
str(Sales)

##################################### Creating Modified Data for F Data Set ###########################################

#-----Beginning Year Assets from Y2014-Y2017-----#
  #Source: Year End Assets

BeginYearAssets <-YearEndAssets
BeginYearAssets <- BeginYearAssets %>% gather("Year","Amount",Y2008:Y2017)
BeginYearAssets$Year <- gsub("Y","",BeginYearAssets$Year)
BeginYearAssets$Year <- as.double(BeginYearAssets$Year)+1
BeginYearAssets <-filter(BeginYearAssets,Year!="2018")
BeginYearAssets$Year <- gsub("^","Y",BeginYearAssets$Year)
BeginYearAssets <- BeginYearAssets %>% spread(Year,Amount)
BeginYearAssets$Items <- "BeginYearAssets"
BeginYearAssets$Y2008 <- "0"
BeginYearAssets<-BeginYearAssets[,c(1:2,12,3:11)]
head(BeginYearAssets)


#-----Beginning Year Assets from Y2008-Y2013 and Average Year Assets from Y2008-Y2017-----#
  #Source: Year End Assets, ROA, Net Income_KR
BeginYearAssets<- rbind(BeginYearAssets,YearEndAssets,ROA,NetIncome_KR)
BeginYearAssets <- BeginYearAssets %>% gather("Year","Amount",Y2008:Y2017)
BeginYearAssets <- BeginYearAssets %>% unite(Items,c("Year","Items"),sep="_")
BeginYearAssets <- BeginYearAssets %>% spread(Items,Amount)
BeginYearAssets[,2:41] <-as.double(unlist(BeginYearAssets[,2:41]))
str(BeginYearAssets)
BeginYearAssets$Y2013_BeginYearAssets <- BeginYearAssets$Y2013_NetIncome_Mil/BeginYearAssets$Y2013_ROA*100*1000000*2-BeginYearAssets$Y2013_YearEndAssets
BeginYearAssets$Y2012_YearEndAssets <-  BeginYearAssets$Y2013_BeginYearAssets 
BeginYearAssets$Y2012_BeginYearAssets <- BeginYearAssets$Y2012_NetIncome_Mil/BeginYearAssets$Y2012_ROA*100*1000000*2-BeginYearAssets$Y2012_YearEndAssets
BeginYearAssets$Y2011_YearEndAssets <-  BeginYearAssets$Y2012_BeginYearAssets 
BeginYearAssets$Y2011_BeginYearAssets <- BeginYearAssets$Y2011_NetIncome_Mil/BeginYearAssets$Y2011_ROA*100*1000000*2-BeginYearAssets$Y2011_YearEndAssets
BeginYearAssets$Y2010_YearEndAssets <-  BeginYearAssets$Y2011_BeginYearAssets 
BeginYearAssets$Y2010_BeginYearAssets <- BeginYearAssets$Y2010_NetIncome_Mil/BeginYearAssets$Y2010_ROA*100*1000000*2-BeginYearAssets$Y2010_YearEndAssets
BeginYearAssets$Y2009_YearEndAssets <-  BeginYearAssets$Y2010_BeginYearAssets 
BeginYearAssets$Y2009_BeginYearAssets <- BeginYearAssets$Y2009_NetIncome_Mil/BeginYearAssets$Y2009_ROA*100*1000000*2-BeginYearAssets$Y2009_YearEndAssets
BeginYearAssets$Y2008_YearEndAssets <-  BeginYearAssets$Y2009_BeginYearAssets 
BeginYearAssets$Y2008_BeginYearAssets <- BeginYearAssets$Y2008_NetIncome_Mil/BeginYearAssets$Y2008_ROA*100*1000000*2-BeginYearAssets$Y2008_YearEndAssets
BeginYearAssets$Y2017_AverageYearAssets <- (BeginYearAssets$Y2017_BeginYearAssets+BeginYearAssets$Y2017_YearEndAssets)/2
BeginYearAssets$Y2016_AverageYearAssets <- (BeginYearAssets$Y2016_BeginYearAssets+BeginYearAssets$Y2016_YearEndAssets)/2
BeginYearAssets$Y2015_AverageYearAssets <- (BeginYearAssets$Y2015_BeginYearAssets+BeginYearAssets$Y2015_YearEndAssets)/2
BeginYearAssets$Y2014_AverageYearAssets <- (BeginYearAssets$Y2014_BeginYearAssets+BeginYearAssets$Y2014_YearEndAssets)/2
BeginYearAssets$Y2013_AverageYearAssets <- (BeginYearAssets$Y2013_BeginYearAssets+BeginYearAssets$Y2013_YearEndAssets)/2
BeginYearAssets$Y2012_AverageYearAssets <- (BeginYearAssets$Y2012_BeginYearAssets+BeginYearAssets$Y2012_YearEndAssets)/2
BeginYearAssets$Y2011_AverageYearAssets <- (BeginYearAssets$Y2011_BeginYearAssets+BeginYearAssets$Y2011_YearEndAssets)/2
BeginYearAssets$Y2010_AverageYearAssets <- (BeginYearAssets$Y2010_BeginYearAssets+BeginYearAssets$Y2010_YearEndAssets)/2
BeginYearAssets$Y2009_AverageYearAssets <- (BeginYearAssets$Y2009_BeginYearAssets+BeginYearAssets$Y2009_YearEndAssets)/2
BeginYearAssets$Y2008_AverageYearAssets <- (BeginYearAssets$Y2008_BeginYearAssets+BeginYearAssets$Y2008_YearEndAssets)/2
BeginYearAssets_Single <- BeginYearAssets %>% gather(Items,Amount,c(2:51))
BeginYearAssets_Single <- BeginYearAssets_Single %>% separate(Items,c("Year","Items"),sep="_")
BeginYearAssets_Single <- BeginYearAssets_Single %>%  spread("Year","Amount")
AverageYearAssets_Single <- BeginYearAssets_Single %>% filter(Items=="AverageYearAssets")
YearEndAssets_Single <- BeginYearAssets_Single %>% filter(Items=="YearEndAssets")
BeginYearAssets_Single <- BeginYearAssets_Single %>% filter(Items=="BeginYearAssets")
head(BeginYearAssets_Single)
head(AverageYearAssets_Single)
head(YearEndAssets_Single)
head(BeginYearAssets)


#-----Modified Long-term Debt Ratio-----#

#             Computation: Conversion from original long-term debt ratio to modified long-term debt ratio. 
#             Note: Modified Long-term debt ratio is based on Assets*Average Year Assets. 
#             Note: Original ratio from MorningStar computation is based on Assets*Year End Assets.

  #Source: Average Year Assets, Year End Assets, Long-term Debt_KR.
LongtermDebt_Modified <- rbind(AverageYearAssets_Single,YearEndAssets_Single,LongtermDebt_KR)
LongtermDebt_Modified <- LongtermDebt_Modified %>% gather("Year","Amount",Y2008:Y2017)
LongtermDebt_Modified <- LongtermDebt_Modified %>% spread(Items,Amount)
LongtermDebt_Modified [,3:5] <-as.double(unlist(LongtermDebt_Modified[,3:5]))
LongtermDebt_Modified$LongtermDebtModified <- LongtermDebt_Modified$LongtermDebt*LongtermDebt_Modified$YearEndAssets/100/LongtermDebt_Modified$AverageYearAssets
LongtermDebt_Modified$Year <- as.double(gsub("Y","",LongtermDebt_Modified$Year))

#-----Modified Beginning Year Long-term Debt Ratio-----#
  #Source: Modified Long-term Debt Ratio
Beginning_LongtermDebtModified <- LongtermDebt_Modified[,c(1,2,6)]
Beginning_LongtermDebtModified$Year <- Beginning_LongtermDebtModified$Year+1
colnames(Beginning_LongtermDebtModified)[3] <- "Beginning_LongtermDebtModified"
head(Beginning_LongtermDebtModified)
Beginning_LongtermDebtModified<- filter(Beginning_LongtermDebtModified, Year != 2018)

#-----Computation of change in modified Long-term debt ratio----#
  #Source: Modified Long-term Debt Ratio, Beginning Year Long-term Debt Ratio
LongtermDebt_Modified <- left_join(LongtermDebt_Modified,Beginning_LongtermDebtModified)
LongtermDebt_Modified$ChangeinLongtermDebtModified <- LongtermDebt_Modified$LongtermDebtModified - LongtermDebt_Modified$Beginning_LongtermDebtModified
LongtermDebt_Modified$Year<-gsub("^","Y",LongtermDebt_Modified$Year)

#------Finalizing Modified Long-term Debt Ratio-----#
LongtermDebt_Modified_Single <- LongtermDebt_Modified[,c(1,2,6)]
LongtermDebt_Modified_Single <- LongtermDebt_Modified_Single %>% spread("Year","LongtermDebtModified")
LongtermDebt_Modified_Single$Item <- "LongtermDebtModified"
LongtermDebt_Modified_Single <- LongtermDebt_Modified_Single[,c(1,12,2:11)]
LongtermDebt_Modified_Single <- LongtermDebt_Modified %>% gather(Items,Amount,c(2:50))
LongtermDebt_Modified_Single <- LongtermDebt_Modified_Single %>% separate(Items,c("Year","Items"),sep="_")
LongtermDebt_Modified_Single <- LongtermDebt_Modified_Single %>%  spread("Year","Amount")


#-----Change in Long-term Debt-----#
ChangeinLongtermDebtModified <- LongtermDebt_Modified[,c(1,2,8)]
ChangeinLongtermDebtModified <- ChangeinLongtermDebtModified %>% spread("Year","ChangeinLongtermDebtModified")
ChangeinLongtermDebtModified$Items <- "ChangeinLongtermDebtModified"
ChangeinLongtermDebtModified <- ChangeinLongtermDebtModified[,c(1,12,2:11)]

head(ChangeinLongtermDebtModified)
head(LongtermDebt_Modified_Single)
str(BeginYearAssets_Single)
str(NetIncome_KR)

#####################################F-Score Indicators computed from basic data set (yet to convert to binomial values)#####################################

#Indicator1 - ROA  
Indicator1 <- rbind(NetIncome_KR, BeginYearAssets_Single)
Indicator1 <- Indicator1 %>% gather("Year","Amount",Y2008:Y2017)
Indicator1 <- Indicator1 %>% spread(Items,Amount)
Indicator1$ROA <- Indicator1$NetIncome_Mil/Indicator1$BeginYearAssets*1000000
Indicator1<- Indicator1[,c(1,2,5)]
Indicator1 <- Indicator1 %>%  spread("Year","ROA")
Indicator1$Items <- "ROA"
Indicator1 <- Indicator1[,c(1,12,2:11)]
head(Indicator1)

#Indicator2 - CFO
Indicator2 <-rbind(OperatingCashFlow_KR,BeginYearAssets_Single)
Indicator2 <- Indicator2 %>% gather("Year","Amount",Y2008:Y2017)
Indicator2 <- Indicator2 %>% spread(Items,Amount)
Indicator2$CFO <- Indicator2$OperatingCashFlow_Mil/Indicator2$BeginYearAssets*1000000
Indicator2 <- Indicator2[,c(1,2,5)]
Indicator2 <- Indicator2 %>%  spread("Year","CFO")
Indicator2$Items <- "CFO"
Indicator2 <- Indicator2[,c(1,12,2:11)]
head(Indicator2)

#Indicator3 - Change in ROA
Indicator3 <-Indicator1
Indicator3[is.na(Indicator3)] <-0
Indicator3.1<-Indicator3[,c(-1,-2)]
#Indicator3.1[-1] represents Y2009-Y2017 dataset, while Indicator3.1[-length(Indicator3.1)] represents Y2008-Y2016 dataset
Indicator3 <- cbind(Indicator3[,c(1,2)],Indicator3.1[-1]-Indicator3.1[-length(Indicator3.1)])
Indicator3$Y2008 <- 0
Indicator3 <- Indicator3[,c(1:2,12,3:11)]
Indicator3$Items[Indicator3$Items=="ROA"] <- "Change_ROA"
head(Indicator3)


#Indicator4 - Accruals
Indicator4 <-rbind(Indicator1,Indicator2)
Indicator4 <- Indicator4 %>% gather("Year","Amount",Y2008:Y2017)
Indicator4 <- Indicator4 %>% spread(Items,Amount)
Indicator4$Accruals <- Indicator4$CFO - Indicator4$ROA
Indicator4 <- Indicator4[,c(1,2,5)]
Indicator4 <- Indicator4 %>%  spread("Year","Accruals")
Indicator4$Items <- "Accruals"
Indicator4 <- Indicator4[,c(1,12,2:11)]
head(Indicator4)

#Indicator 5 - Leverage
Indicator5 <-ChangeinLongtermDebtModified
head(Indicator5)


#Indicator 6 - Change in Current Ratio
Indicator6<-CurrentRatio
Indicator6[is.na(Indicator6)] <-0
Indicator6 <- Indicator6[,c(1,12,2:11)]
Indicator6.1<-Indicator6[,c(-1,-2)]
#Indicator6.1[-1] represents Y2009-Y2017 dataset, while Indicator6.1[-length(Indicator6.1)] represents Y2008-Y2016 dataset
Indicator6 <- cbind(Indicator6[,c(1,2)],Indicator6.1[-1]-Indicator6.1[-length(Indicator6.1)])
Indicator6$Y2008 <- 0
Indicator6 <- Indicator6[,c(1:2,12,3:11)]
head(Indicator6)

#Indicator 7 - Change in Shares
Indicator7 <- NumberShares
Indicator7 <- Indicator7[,c(1,12,2:11)]
Indicator7.1<-Indicator7[,c(-1,-2)]
#Indicator7.1[-1] represents Y2009-Y2017 dataset, while Indicator7.1[-length(Indicator7.1)] represents Y2008-Y2017 dataset
Indicator7 <- cbind(Indicator7[,c(1,2)],Indicator7.1[-1]-Indicator7.1[-length(Indicator7.1)])
Indicator7$Y2008 <- 0
Indicator7 <- Indicator7[,c(1:2,12,3:11)]
Indicator7_Modified <- Indicator7 
head(Indicator7)


#Indicator 8 - Change in Gross Profit Margin
Indicator8 <- GrossProfit
Indicator8[is.na(Indicator8)] <-0
Indicator8 <- Indicator8[,c(1,12,2:11)]
Indicator8.1<-Indicator8[,c(-1,-2)]
#Indicator8.1[-1] represents Y2009-Y2018 dataset, while Indicator8.1[-length(Indicator8.1)] represents Y2008-Y2018 dataset
Indicator8 <- cbind(Indicator8[,c(1,2)],Indicator8.1[-1]-Indicator8.1[-length(Indicator8.1)])
Indicator8$Y2008 <- 0
Indicator8 <- Indicator8[,c(1:2,12,3:11)]
head(Indicator8)

#Indicator 9 - Change in Asset Turnover


#Calculating yearly Asset Turnover
Indicator9 <- rbind(BeginYearAssets_Single,Sales)
Indicator9 <- Indicator9 %>% gather("Year","Amount",Y2008:Y2017)
Indicator9 <- Indicator9 %>% spread(Items,Amount)
Indicator9$AssetTurnover <- Indicator9$Revenue_Mil/Indicator9$BeginYearAssets*1000000
Indicator9 <- Indicator9[,c(1,2,5)]
Indicator9 <- Indicator9 %>%  spread("Year","AssetTurnover")
Indicator9$Items <- "AssetTurnover"
Indicator9 <- Indicator9[,c(1,12,2:11)]
Indicator9 <- cbind(Indicator9[,c(1,2)],lapply(Indicator9[,c(3:12)], function(x) ifelse(is.finite(x), x , 0)))

#Calculating yearly Change in Asset Turnover
Indicator9.1<-Indicator9[,c(-1,-2)]
#Indicator9.1[-1] represents Y2009-Y2019 dataset, while Indicator9.1[-length(Indicator9.1)] represents Y2009-Y2019 dataset
Indicator9 <- cbind(Indicator9[,c(1,2)],Indicator9.1[-1]-Indicator9.1[-length(Indicator9.1)])
Indicator9$Y2008 <- 0
Indicator9 <- Indicator9[,c(2:1,12,3:11)]
head(Indicator9)
str(Indicator9)

##################################### Binary Conversion and Tabulating F-Scores #####################################



#-----Merging all Indicators (pre-binary conversion)-----#

  #Positive Signals are those when amount above 0 means a positive change in the company, therefore increase in F-Score
Indicators_PositiveSignal <- rbind(Indicator1,Indicator2,Indicator3,Indicator4,Indicator6,Indicator8,Indicator9)
Indicators_PositiveSignal <- gather(Indicators_PositiveSignal,"Year","Amount",3:12)
Indicators_PositiveSignal <- Indicators_PositiveSignal %>% mutate(Score = if_else(Amount >0,1,0))
Indicators_PositiveSignal[is.na(Indicators_PositiveSignal)] <- 0
head(Indicators_PositiveSignal)

  #Negative Signals are those when amount above 0 means a negative change in the company, therefore decrease in F-Score
Indicators_NegativeSignal <- rbind(Indicator5,Indicator7)
Indicators_NegativeSignal <- gather(Indicators_NegativeSignal,"Year","Amount",3:12)
Indicators_NegativeSignal <- Indicators_NegativeSignal %>% mutate(Score = if_else(Amount >0,0,1))
Indicators_NegativeSignal[is.na(Indicators_NegativeSignal)] <- 0
head(Indicators_NegativeSignal)
  
  #Merging both signals

AllIndicators <-rbind(Indicators_PositiveSignal,Indicators_NegativeSignal)
write.csv(AllIndicators,file="C:/Users/Tse Young/Desktop/AllIndicators.csv")
AllIndicators_Clean <- AllIndicators[,c(3,1:2,4,5)]
head(AllIndicators_Clean)
write.csv(AllIndicators_Clean,file="C:a/Users/Tse Young/Desktop/AllIndicators_Clean.csv")


  #Tabulating F-Score
F_Score_Tally <- as.data.table(AllIndicators_Clean)[, sum(Score), by = .(Symbol, Year)]
colnames(F_Score_Tally)[3] <- "F-Score"
write.csv(F_Score_Tally,file="C:/Users/Tse Young/Desktop/F_Score_Tally.csv")

F_Score_Full_Amount <- spread(AllIndicators_Clean[1:4],Items,Amount)
F_Score_Full_Score <- spread(AllIndicators_Clean[,c(1:3,5)],Items,Score)
F_Score_Full <-left_join(F_Score_Full_Amount,F_Score_Full_Score,by=c("Year","Symbol"))
F_Score_Full_Final <- left_join(F_Score_Full,F_Score_Tally) 
head(F_Score_Full_Score)

write.csv(F_Score_Full_Final,file="C:/Users/Tse Young/Desktop/F_Score_Full_Final.csv")

  #Classifying F_Score Indicators into their specific F_Score Classes 

F_Score_Classes <- AllIndicators_Clean
F_Score_Classes$Items[grepl("^ROA",AllIndicators_Clean$Items)] <- "Profitability_related"
F_Score_Classes$Items[grepl("^CFO",AllIndicators_Clean$Items)] <- "Profitability_related"
F_Score_Classes$Items[grepl("^Change_ROA",AllIndicators_Clean$Items)] <- "Profitability_related"
F_Score_Classes$Items[grepl("^Accruals",AllIndicators_Clean$Items)] <- "Profitability_related"
F_Score_Classes$Items[grepl("^ChangeinLongtermDebtModified",AllIndicators_Clean$Items)] <- "Capital_structure"
F_Score_Classes$Items[grepl("^Current Ratio",AllIndicators_Clean$Items)] <- "Capital_structure"
F_Score_Classes$Items[grepl("^Shares Mil",AllIndicators_Clean$Items)] <- "Capital_structure"
F_Score_Classes$Items[grepl("^Gross Margin",AllIndicators_Clean$Items)] <- "Efficiency"
F_Score_Classes$Items[grepl("^AssetTurnover",AllIndicators_Clean$Items)] <- "Efficiency"
F_Score_Classes_Tally <- as.data.table(F_Score_Classes)[, sum(Score), by = .(Symbol, Year,Items)]
F_Score_Classes_Tally_Gather <- F_Score_Classes_Tally %>% spread("Items","V1")
F_Score_Classes_Tally_Gather <- F_Score_Classes_Tally_Gather %>% mutate (F.Score=Capital_structure+Efficiency+Profitability_related)
head(F_Score_Classes_Tally_Gather) 


write.csv(F_Score_Classes_Tally_Gather,file="C:/Users/Tse Young/Desktop/F_Score_Tally_Full_Final.csv")



##################################### Modified Binary Conversion and Tabulating F-Scores for Testing #####################################



#Merging all Indicators (Shares Modified-Indicator 7 changed to a positive signal) (pre-binary conversion)
  #Positive Signals are those when amount above 0 means a positive change in the company, therefore increase in F-Score
Indicators_Modified_PositiveSignal <- rbind(Indicator1,Indicator2,Indicator3,Indicator4,Indicator6,Indicator7,Indicator8,Indicator9)
Indicators_Modified_PositiveSignal <- gather(Indicators_Modified_PositiveSignal,"Year","Amount",3:12)
Indicators_Modified_PositiveSignal <- Indicators_Modified_PositiveSignal %>% mutate(Score = if_else(Amount >0,1,0))
Indicators_Modified_PositiveSignal[is.na(Indicators_Modified_PositiveSignal)] <- 0
head(Indicators_Modified_PositiveSignal)

  #Negative Signals are those when amount above 0 means a negative change in the company, therefore decrease in F-Score
Indicators_Modified_NegativeSignal <- rbind(Indicator5)
Indicators_Modified_NegativeSignal <- gather(Indicators_Modified_NegativeSignal,"Year","Amount",3:12)
Indicators_Modified_NegativeSignal <- Indicators_Modified_NegativeSignal %>% mutate(Score = if_else(Amount >0,0,1))
Indicators_Modified_NegativeSignal[is.na(Indicators_Modified_NegativeSignal)] <- 0
head(Indicators_Modified_NegativeSignal)

  #Merging both signals

AllIndicators_Modified <-rbind(Indicators_Modified_PositiveSignal,Indicators_Modified_NegativeSignal)
write.csv(AllIndicators_Modified,file="C:/Users/Tse Young/Desktop/AllIndicators_Modified.csv")
AllIndicators_Modified_Clean <- AllIndicators_Modified[,c(3,1:2,4,5)]
head(AllIndicators_Modified_Clean)
write.csv(AllIndicators_Modified_Clean,file="C:a/Users/Tse Young/Desktop/AllIndicators_Modified_Clean.csv")


  #Tabulating F-Score
F_Score_Modified_Tally <- as.data.table(AllIndicators_Clean)[, sum(Score), by = .(Symbol, Year)]
colnames(F_Score_Modified_Tally)[3] <- "F-Score"
write.csv(F_Score_Modified_Tally,file="C:/Users/Tse Young/Desktop/F_Score_Modified_Tally.csv")

F_Score_Modified_Full_Amount <- spread(AllIndicators_Clean[1:4],Items,Amount)
F_Score_Modified_Full_Score <- spread(AllIndicators_Clean[,c(1:3,5)],Items,Score)
F_Score_Modified_Full <-left_join(F_Score_Modified_Full_Amount,F_Score_Modified_Full_Score,by=c("Year","Symbol"))
F_Score_Modified_Full_Final <- left_join(F_Score_Modified_Full,F_Score_Modified_Tally) 
head(F_Score_Modified_Full_Score)

write.csv(F_Score_Modified_Full_Final,file="C:/Users/Tse Young/Desktop/F_Score_Modified_Full_Final.csv")



