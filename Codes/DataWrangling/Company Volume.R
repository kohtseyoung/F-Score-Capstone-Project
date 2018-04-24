library(quantmod)
library(tidyr)
library(dplyr)
library(data.table)
library(plyr)
  

WoW <- new.env()

#-------- Loading Data from Morning Star Links--------

#load NASDAQ company list
companylist <-read.csv(file="companylist(Final).csv")

#-------------Loading Returns Data using quantmod---------------------

companylist_only<- companylist[,1]
companylist_only<-as.character(companylist_only)

sapply(companylist_only, function(x){
  try(
    getSymbols(
      x,
      src="yahoo",
      from="2005-01-01",
      env=WoW),
    silent=TRUE)
})

#For Companies with available data, the objects will be created under the new enviroment. This is subsequently- 
#compared with the initial input list to filter out those with data from those without

company_available <- ls(WoW) 
company_available <- data.frame(matrix(company_available,ncol =1))
colnames(company_available) <- "Symbol"

company_full_list <-data.frame(matrix(companylist_only,ncol=1))
colnames(company_full_list) <- "Symbol"

company_not_available <- anti_join(company_full_list,company_available,by="Symbol")
Company_Symbol_List <- mget(ls(WoW),envir= WoW) 

#-------------Computing of average daily trading volumes---------------------
 
  #Formatting Trading Volume Data
  ga <- do.call(cbind,Company_Symbol_List)
  ga_Volume <- ga[,grep("Volume",names(ga),ignore.case=T)]
  ga_Volume <- data.frame(ga_Volume[-c(3309,3312,3321)])
  ga_Volume<- add_rownames(ga_Volume,"Year")
  ga_Volume$Year <-gsub('.{6}$','',ga_Volume$Year)
  ga_Volume <- ga_Volume %>%  gather("Symbol","Trading_Volume",2:2051)
  ga_Volume$Symbol<-gsub(".Volume",'',ga_Volume$Symbol)
  head(ga_Volume)

  #Computing Average Daily Volume 
  ga_Volume_Average <- ga_Volume %>% ddply(~~Symbol~Year,summarise,Average_Volume=mean(Trading_Volume,na.rm=TRUE))
  ga_Volume_Average <- ga_Volume_Average %>% filter(Year>2008 &Year<2018)
  ga_Volume_Average_NaN <- subset(ga_Volume_Average,is.nan(ga_Volume_Average[,3]))
  ga_Volume_Average_Low <- ga_Volume_Average %>% filter(Average_Volume<10000)
  ga_Volume_Average_Low_NaN<- rbind(ga_Volume_Average_Low,ga_Volume_Average_NaN)
  ga_Volume_Average_Low_NaN$Year <-gsub('^','Y',ga_Volume_Average_Low_NaN$Year)

  write.csv(ga_Volume_Average_Low_NaN,
          "Company_Volume_Low_Trades.csv")

  write.csv(ga_Volume_Average,
          "Company_Volume_Trades.csv")




####################################### Codes not in-use ###########################################

###Average Share Price (Not in-used)

SP <- ga[,grep("Adjusted",names(ga),ignore.case=T)] # To filter out Adjusted Close Price
SP <- data.frame(SP[-c(3309,3312,3321)]) # To remove repeated Columns - Does not affect our dataset
SP <- add_rownames(SP,"Year") # To add rownames
SP$Year <-gsub('.{6}$','',SP$Year) #To modify Dates to Year Only
SP <- SP %>%  gather("Symbol","Share_Price",2:2051)
SP$Symbol <- gsub(".Adjusted","",SP$Symbol) #To modify Symbol Names 

SP_Average <- SP %>% ddply(~~Symbol~Year,summarise,Average_Share_Price=mean(Share_Price,na.rm=TRUE)) #To sum closing prices and mean 

SP_Average <- SP_Average %>% filter(Year>2007 &Year<2018)
SP_Average$Year <-gsub('^','Y',SP_Average$Year)


Fiscal_Month_Modified <- Fiscal_Month #Fiscal Month is based on object from "getSymbols.r"
Fiscal_Month_Modified$Dates <-gsub('.{3}$','',Fiscal_Month_Modified$Dates)
colnames(Fiscal_Month_Modified)[4] <- "Modified_Year"
Fiscal_Month_NumberShares<- left_join(NumberShares[,c(1,3:4)],Fiscal_Month_Modified[,c(1,3:4)])
Fiscal_Month_NumberShares<- Fiscal_Month_NumberShares[,c(1,4,3)]
colnames(Fiscal_Month_NumberShares)[2] <- "Year"
Fiscal_Month_NumberShares$Year <- gsub("^","Y",Fiscal_Month_NumberShares$Year)
head(NumberShares)
head(Fiscal_Month_NumberShares)

SP_Average_MCAP <- left_join(SP_Average,Fiscal_Month_NumberShares) #Add Number of Shares data from Morning Star
SP_Average_MCAP <- SP_Average_MCAP[,c(1,2,3,4)]
SP_Average_MCAP <- SP_Average_MCAP %>% mutate(Market_Cap= Average_Share_Price*Dates) #Multiply to get the Market Cap
head(SP_Average_MCAP)
filter(SP_Average,Symbol=="FLWS")


is.nan.data.frame <- function(x) #Create is.nan for data.frames
  do.call(cbind, lapply(x, is.nan))

SP_Average_MCAP[is.nan(SP_Average_MCAP)] <- 0                                        

Total_MCAP <- SP_Average_MCAP %>% ddply(~Year,summarise,Total_MCAP = sum(Market_Cap,na.rm=TRUE))
SP_Average_MCAP <- left_join(SP_Average_MCAP,Total_MCAP)
SP_Average_MCAP <- SP_Average_MCAP %>% mutate (Weigthed_Cap = Market_Cap/Total_MCAP)

write.csv(SP_Average_MCAP,
          "Company_Weight_Cap.csv")


####Average P/E 

  #Read KR Ratio
  Company_KeyRatio <- read.csv(file="companylist(KR Final).csv")
  Company_CashFlow <-Company_CashFlow[,c(3:14)]

  #EPS
  EPS_KR <- Company_KeyRatio %>% filter(grepl('^Earnings Per Share', Key_Ratio_Items))
  EPS_KR<-EPS_KR[,c(2:12,15)]
  colnames(EPS_KR) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
  EPS_KR <- EPS_KR %>% filter(!grepl('%', Items))
  EPS_KR$Items <-as.character(EPS_KR$Items)
  EPS_KR$Items[grepl("^Operating Cash Flow",EPS_KR$Items)] <- "EPS_Mil"
  EPS_KR$Y2008 <- gsub("^$",0,EPS_KR$Y2008)
  EPS_KR$Y2009 <- gsub("^$",0,EPS_KR$Y2009)
  EPS_KR$Y2010 <- gsub("^$",0,EPS_KR$Y2010)
  EPS_KR$Y2011 <- gsub("^$",0,EPS_KR$Y2011)
  EPS_KR$Y2012 <- gsub("^$",0,EPS_KR$Y2012)
  EPS_KR$Y2013 <- gsub("^$",0,EPS_KR$Y2013)
  EPS_KR$Y2014 <- gsub("^$",0,EPS_KR$Y2014)
  EPS_KR$Y2015 <- gsub("^$",0,EPS_KR$Y2015)
  EPS_KR$Y2016 <- gsub("^$",0,EPS_KR$Y2016)
  EPS_KR$Y2017 <- gsub("^$",0,EPS_KR$Y2017)

  EPS_KR$Y2008 <-as.double(gsub(",","",as.character(EPS_KR$Y2008)))
  EPS_KR$Y2009 <-as.double(gsub(",","",as.character(EPS_KR$Y2009)))
  EPS_KR$Y2010 <-as.double(gsub(",","",as.character(EPS_KR$Y2010)))
  EPS_KR$Y2011 <-as.double(gsub(",","",as.character(EPS_KR$Y2011)))
  EPS_KR$Y2012 <-as.double(gsub(",","",as.character(EPS_KR$Y2012)))
  EPS_KR$Y2013 <-as.double(gsub(",","",as.character(EPS_KR$Y2013)))
  EPS_KR$Y2014 <-as.double(gsub(",","",as.character(EPS_KR$Y2014)))
  EPS_KR$Y2015 <-as.double(gsub(",","",as.character(EPS_KR$Y2015)))
  EPS_KR$Y2016 <-as.double(gsub(",","",as.character(EPS_KR$Y2016)))
  EPS_KR$Y2017 <-as.double(gsub(",","",as.character(EPS_KR$Y2017)))
  head(EPS_KR)


  EPS_KR_Gather <- EPS_KR %>% gather("Year","Amount",Y2008:Y2017)

  Fiscal_Month_Modified2 <- Fiscal_Month  # To ensure the correct Fiscal Year is corresponded to its respecitive EPS
  Fiscal_Month_Modified2$Dates <-gsub('.{3}$','',Fiscal_Month_Modified2$Dates)
  colnames(Fiscal_Month_Modified2)[4] <- "Modified_Year"
  EPS_KR_Gather<- left_join(EPS_KR_Gather,Fiscal_Month_Modified2[,c(1,3:4)])
  colnames(EPS_KR_Gather)[3] <- "Original_Year" # This is for the EPS holding period once the modified year is used to accurately compute the share price
  colnames(EPS_KR_Gather)[5] <- "Year"
  EPS_KR_Gather$Year <- gsub("^","Y",EPS_KR_Gather$Year)
  head(EPS_KR_Gather)


  PE_KR_Gather<- left_join(EPS_KR_Gather,SP_Average) #Add Average Share Price
  PE_KR_Gather <- PE_KR_Gather %>% mutate(PE_Ratio= Average_Share_Price/Amount) #Multiply to get the Market Cap
  PE_KR_Gather <- PE_KR_Gather[,c(1:4,6:7)] # Converting back to original year
  colnames(PE_KR_Gather)[3] <- "Year"
  head(PE_KR_Gather)

  write.csv(PE_KR_Gather,
            "Company_PE_Ratio.csv")


  # To convert PE_Ratio Returns to USD
  PE_Ratio <- read.csv("Company_PE_Ratio.csv")
  PE_Ratio$Currency <- str_sub(PE_Ratio$Items, start=-3)
  PE_Ratio$Currency <- gsub("$","/USD",PE_Ratio$Currency)
  PE_Ratio$Items <- str_sub(PE_Ratio$Items, start=1, end=18)
  PE_Ratio <- PE_Ratio[,c(1,2,8,3:7)]
  head(PE_Ratio)

  Currency_List <- unique(PE_Ratio$Currency)
  getFX(Currency_List, from="2017-12-31",to="2017-12-31")
  Currency_List_Modified <- gsub("/","",Currency_List)
  Currency_Data <- mget(Currency_List_Modified)
  Currency_Data <-cbind(Currency_Data)
  Currency_Data <-data.frame(Currency_Data)
  Currency_Data<- setDT(Currency_Data,keep.rownames = TRUE)[]
  Currency_Data$rn <- str_sub(Currency_Data$rn,start=1,end=3)
  Currency_Data$rn <- gsub("$","/USD",Currency_Data$rn)
  Currency_Data$Currency_Data <- as.double(Currency_Data$Currency_Data)
  colnames(Currency_Data)[1] <- "Currency"
  PE_Ratio <- left_join(PE_Ratio,Currency_Data)
  PE_Ratio<- data.frame(PE_Ratio)
  PE_Ratio <-PE_Ratio %>% mutate(Modified_PE_Ratio = PE_Ratio*Currency_Data)
  head(PE_Ratio)

  write.csv(PE_Ratio,
            "Company_PE_Ratio_Modified.csv")
