library(quantmod)
library(tidyr)
library(dplyr)
library(data.table)
library(plyr)

WoW <- new.env()

#-------- Loading Data from Morning Star Links--------

#load NASDAQ company list
companylist <-read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(Final).csv")



#-------- Creating Full Key Ratio Data Set--------

Company_KeyRatio <- read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Full Data Set\\Key Ratios\\companylist(KR Final).csv")
Company_KeyRatio <-Company_KeyRatio[,c(2:18)]

head(Company_KeyRatio)

#-------- Checking Company's Fiscal Month based on Margins % Sales under Key Ratio Data------

Fiscal_Month <- Company_KeyRatio %>% filter(grepl('Margins % of Sales', Key_Ratio_Items))
Fiscal_Month<-Fiscal_Month[,c(1:11,14)]
head(Fiscal_Month)
colnames(Fiscal_Month) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
Fiscal_Month$Items <-as.character(Fiscal_Month$Items)
Fiscal_Month$Items[grepl("Margins",Fiscal_Month$Items)] <- "Fiscal_Month"
Fiscal_Month[is.na(Fiscal_Month)] <-0
Fiscal_Month[,c(2:11)] <- sapply(Fiscal_Month[,c(2:11)], function (x){as.character(x)})
Fiscal_Month <- Fiscal_Month[,c(12,1,2:11)]
Fiscal_Month <-gather(Fiscal_Month,"Year","Dates",3:12)
head(Fiscal_Month)
str(Fiscal_Month)



#-------- Checking Company's List Date based on Number of Shares under Key Ratio Data------

NumberShares <- Company_KeyRatio %>% filter(Key_Ratio_Items=="Shares Mil")
NumberShares <-NumberShares[,c(1:11,14)]
colnames(NumberShares) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
NumberShares$Items <-as.character(NumberShares$Items)
NumberShares[,c(2:11)] <- sapply(NumberShares[,c(2:11)], function (x) {(gsub("^$","NA",as.character(x)))})
NumberShares[,c(2:11)] <- sapply(NumberShares[,c(2:11)], function (x) {as.double(gsub(",","",as.character(x)))})
NumberShares <- NumberShares[,c(12,1,2:11)]
NumberShares <-gather(NumberShares,"Year","Dates",3:12)
str(NumberShares)
head(NumberShares)
write.csv(NumberShares,file="C:\\Users\\Tse Young\\Desktop\\NumberShares.csv")


Modified_Fiscal_Month <-inner_join(Fiscal_Month,NumberShares,by=c("Symbol","Year")) #Based on the column from Number of Shares, inner_join is used to determine the annual fiscal closing date for each company
Modified_Fiscal_Month <- Modified_Fiscal_Month %>% mutate(Fiscal_Year_End=if_else(Dates.y=="NA","NA",Dates.x))
Modified_Fiscal_Month <- Modified_Fiscal_Month[,c(1:3,7)]
Modified_Fiscal_Month <- Modified_Fiscal_Month %>% spread(Year,Fiscal_Year_End)

Modified_Fiscal_Month$Na_count <- apply(Modified_Fiscal_Month, 1, function(x) sum(is.na(x)))


head(Modified_Fiscal_Month)
Modified_Fiscal_Month <-Modified_Fiscal_Month %>% mutate(Start_Year_count = 2008+ Na_count)
Modified_Fiscal_Month$Start_Year_count <- gsub("^","Y",Modified_Fiscal_Month$Start_Year_count)
Fiscal_Month_Count <- Fiscal_Month
colnames(Fiscal_Month_Count)[3] <- "Start_Year_count"
Modified_Fiscal_Month <- left_join(Modified_Fiscal_Month,Fiscal_Month_Count)
head(Fiscal_Month_Count)

Modified_Fiscal_Month$EndDate <- Modified_Fiscal_Month$Y2017

Modified_Fiscal_Month_Clean <- Modified_Fiscal_Month[,c(1,16,17)]
colnames(Modified_Fiscal_Month_Clean)[2] <- "StartDate"
head(Modified_Fiscal_Month_Clean)


####################################################################################################

#-------------Loading Returns Data using quantmod---------------------


companylist
str(companylist)
companylist_only<- companylist[,1]
companylist_only<-as.character(companylist_only)
companylist_only



#For extracting a  list of Symbols that are successfully downloaded

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
company_available_string <- company_available
company_available <- data.frame(matrix(company_available,ncol =1))
head(company_available)
colnames(company_available) <- "Symbol"
company_full_list <-data.frame(matrix(companylist_only,ncol=1))
colnames(company_full_list) <- "Symbol"
head(company_full_list)

company_not_available <- anti_join(company_full_list,company_available,by="Symbol")
str(company_not_available)

chickennnn <- mget(ls(WoW),envir= WoW) 

write.csv(company_not_available,"C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\company_not_available.csv")

#------------Extracting monthly and yearly returns-------------------
low <- lapply(chickennnn,monthlyReturn)
low <- do.call(cbind,low) #columnbind the list
low <-data.frame(low) #convert into data.frame format
names(low)<-ls(WoW) #apply company names

#To create date in corresponse to the row number
taxi <- data.frame(rownames(low))

taxi$rownames.low. <- gsub('.{3}$','',taxi$rownames.low.)
taxi <-unique(taxi[1])
str(taxi)
taxi$rn <- 1:159
taxi$rn <- as.numeric(taxi$rn)

low <- setDT(low,keep.rownames = TRUE)[]
low$rn <- gsub('.{3}$', '', low$rn)
low[is.na(low)] <- 0
low <-data.frame(ddply(low, "rn", numcolwise(sum)))

#Modified Share Information to take into account stocksplits

chickennnn_adjusted_stocksplit <- lapply(company_available_string,function(x) { adjustOHLC(get(x, pos=WoW), symbol.name=x, adjust=c("split"), 
                                                              use.Adjusted=FALSE)})




#Finding quarterly return based on adjusted stock-split share price

yearlow_quaterly<-lapply(chickennnn_adjusted_stocksplit,quarterlyReturn)
yearlow_quaterly <- do.call(cbind,yearlow_quaterly) #columnbind the list
yearlow_quaterly<- data.frame(yearlow_quaterly) #convert into data.frame format
names(yearlow_quaterly) <-ls(WoW) #apply company names
yearlow_quaterly <- setDT(yearlow_quaterly,keep.rownames = TRUE)[]
yearlow_quaterly$rn <- gsub('.{3}$', '', yearlow_quaterly$rn)
yearlow_quaterly[is.na(yearlow_quaterly)] <- 0
yearlow_quaterly <-data.frame(ddply(yearlow_quaterly, "rn", numcolwise(sum)))
colnames(yearlow_quaterly)[1] <-"Symbol"
yearlow_quaterly<- yearlow_quaterly %>% mutate(Total = rowSums(yearlow_quaterly[,-1]))
yearlow_quaterly$Total <- rowSums(yearlow_quaterly[,c(2:81)])
yearlow_quaterly <- filter(yearlow_quaterly,Total != 0)
head(yearlow_quaterly)

yearlow_quaterly_colnames <- colnames(yearlow_quaterly)
yearlow_quaterly_rownames <- yearlow_quaterly$Symbol
yearlow_quaterly <- t(yearlow_quaterly)
yearlow_quaterly<- data.frame(yearlow_quaterly)
yearlow_quaterly<- setDT(yearlow_quaterly,keep.rownames = TRUE)
colnames(yearlow_quaterly)[1] <- "Symbol"
colnames(yearlow_quaterly)[2:54] <- yearlow_quaterly_rownames
yearlow_quaterly <- yearlow_quaterly[-1,]
yearlow_quaterly<-yearlow_quaterly[,c(1:54)]
yearlow_quaterly <- yearlow_quaterly %>% gather(Year,Quarterly_Return,c(2:54))
yearlow_quaterly <- yearlow_quaterly %>% separate(Year, c("Year","Month"))
yearlow_quaterly$Year <- as.numeric(yearlow_quaterly$Year)
yearlow_quaterly$Quarterly_Return <- as.double(yearlow_quaterly$Quarterly_Return)
yearlow_quaterly$Quarterly_Return <- yearlow_quaterly$Quarterly_Return+1
head(yearlow_quaterly)
str(yearlow_quaterly_Q2)





#Q3 onwards (March This Year)

yearlow_quaterly_Q3 <- yearlow_quaterly %>%  mutate (Modified_Year = ifelse(Month=="03", Year-1,
                                                                            ifelse(Month=="06",Year-1,Year)))
yearlow_quaterly_Q3 <- yearlow_quaterly_Q3[,c(1,5,3,4)]
yearlow_quaterly_Q3[,c(4)] <- as.double(unlist(yearlow_quaterly_Q3[,c(4)]))
yearlow_quaterly_Q3[,c(4)] <- yearlow_quaterly_Q3[,c(4)]
yearlow_quaterly_Q3 <- yearlow_quaterly_Q3 %>% spread (Month, Quarterly_Return)

yearlow_quaterly_Q3 <- yearlow_quaterly_Q3 %>% mutate (Yearly_Return= yearlow_quaterly_Q3[,3]*yearlow_quaterly_Q3[,4]*yearlow_quaterly_Q3[,5]*yearlow_quaterly_Q3[,6])
yearlow_quaterly_Q3$Yearly_Return <- yearlow_quaterly_Q3$Yearly_Return -1
head(yearlow_quaterly_Q3)

yearlow_quaterly_Q3_Clean <- yearlow_quaterly_Q3[,c(1,2,7)]
colnames(yearlow_quaterly_Q3_Clean)[c(2,3)] <- c("Year","Q3_Yearly_Return")
yearlow_quaterly_Q3_Clean$Year <- gsub("^","Y",yearlow_quaterly_Q3_Clean$Year)
head(yearlow_quaterly_Q3_Clean)



#Q4 onwards (June This Year)

yearlow_quaterly_Q4 <- yearlow_quaterly %>%  mutate (Modified_Year = ifelse(Month=="03", Year-1,
                                                                            ifelse(Month=="06", Year-1,
                                                                                   ifelse(Month=="09",Year-1,Year))))
yearlow_quaterly_Q4 <- yearlow_quaterly_Q4[,c(1,5,3,4)]
yearlow_quaterly_Q4[,c(4)] <- as.double(unlist(yearlow_quaterly_Q4[,c(4)]))
yearlow_quaterly_Q4[,c(4)] <- yearlow_quaterly_Q4[,c(4)]
yearlow_quaterly_Q4 <- yearlow_quaterly_Q4 %>% spread (Month, Quarterly_Return)

yearlow_quaterly_Q4 <- yearlow_quaterly_Q4 %>% mutate (Yearly_Return= yearlow_quaterly_Q4[,3]*yearlow_quaterly_Q4[,4]*yearlow_quaterly_Q4[,5]*yearlow_quaterly_Q4[,6])
yearlow_quaterly_Q4$Yearly_Return <- yearlow_quaterly_Q4$Yearly_Return -1
head(yearlow_quaterly_Q4)

yearlow_quaterly_Q4_Clean <- yearlow_quaterly_Q4[,c(1,2,7)]
colnames(yearlow_quaterly_Q4_Clean)[c(2,3)] <- c("Year","Q4_Yearly_Return")
yearlow_quaterly_Q4_Clean$Year <- gsub("^","Y",yearlow_quaterly_Q4_Clean$Year)
head(yearlow_quaterly_Q4_Clean)


#Next Q1 onwards (September This Year)
yearlow_quaterly_Q1 <- yearlow_quaterly %>%  mutate (Modified_Year = ifelse(Month=="03",Year-1,
                                                                            ifelse(Month=="06",Year-1,
                                                                                   ifelse(Month=="09",Year-1,
                                                                                          ifelse(Month=="12",Year-1,Year)))))
yearlow_quaterly_Q1 <- yearlow_quaterly_Q1[,c(1,5,3,4)]
yearlow_quaterly_Q1[,c(4)] <- as.double(unlist(yearlow_quaterly_Q1[,c(4)]))
yearlow_quaterly_Q1[,c(4)] <- yearlow_quaterly_Q1[,c(4)]
yearlow_quaterly_Q1 <- yearlow_quaterly_Q1 %>% spread (Month, Quarterly_Return)

yearlow_quaterly_Q1 <- yearlow_quaterly_Q1 %>% mutate (Yearly_Return= yearlow_quaterly_Q1[,3]*yearlow_quaterly_Q1[,4]*yearlow_quaterly_Q1[,5]*yearlow_quaterly_Q1[,6])
yearlow_quaterly_Q1$Yearly_Return <- yearlow_quaterly_Q1$Yearly_Return -1
head(yearlow_quaterly_Q1)

yearlow_quaterly_Q1_Clean <- yearlow_quaterly_Q1[,c(1,2,7)]
colnames(yearlow_quaterly_Q1_Clean)[c(2,3)] <- c("Year","Q1_Yearly_Return")
yearlow_quaterly_Q1_Clean$Year <- gsub("^","Y",yearlow_quaterly_Q1_Clean$Year)
head(yearlow_quaterly_Q1_Clean)

#Next Q2 onwards (December This Year)

yearlow_quaterly_Q2 <- yearlow_quaterly %>%  mutate (Modified_Year = ifelse(Month=="03",Year-1,Year))
                                                                            ifelse(Month=="06",Year-1,
                                                                                   ifelse(Month=="09",Year-1,
                                                                                          ifelse(Month=="12",Year-1,Year)))
yearlow_quaterly_Q2 <- yearlow_quaterly_Q2[,c(1,5,3,4)]
yearlow_quaterly_Q2[,c(4)] <- as.double(unlist(yearlow_quaterly_Q2[,c(4)]))
yearlow_quaterly_Q2[,c(4)] <- yearlow_quaterly_Q2[,c(4)]
yearlow_quaterly_Q2 <- yearlow_quaterly_Q2 %>% spread (Month, Quarterly_Return)

yearlow_quaterly_Q2 <- yearlow_quaterly_Q2 %>% mutate (Yearly_Return= yearlow_quaterly_Q2[,3]*yearlow_quaterly_Q2[,4]*yearlow_quaterly_Q2[,5]*yearlow_quaterly_Q2[,6])
yearlow_quaterly_Q2$Yearly_Return <- yearlow_quaterly_Q2$Yearly_Return -1
head(yearlow_quaterly_Q2)

yearlow_quaterly_Q2_Clean <- yearlow_quaterly_Q2[,c(1,2,7)]
colnames(yearlow_quaterly_Q2_Clean)[c(2,3)] <- c("Year","Q2_Yearly_Return")
yearlow_quaterly_Q2_Clean$Year <- gsub("^","Y",yearlow_quaterly_Q2_Clean$Year)
head(yearlow_quaterly_Q2)
head(yearlow_quaterly_Q2_Clean)
filter(yearlow_quaterly_Q2_Clean,Symbol=="MSFT")
#Combnin all annual returns by quarter


yearlow_quaterly_all <-  left_join(yearlow_quaterly_Q1_Clean, yearlow_quaterly_Q2_Clean) %>%
                            left_join(., yearlow_quaterly_Q3_Clean) %>% 
                                  left_join(.,yearlow_quaterly_Q4_Clean) 
head(yearlow_quaterly_all)




write.csv(yearlow_quaterly,file="C:\\Users\\Tse Young\\Desktop\\yearlow_quaterly.csv")
write.csv(yearlow_quaterly_all,file="C:\\Users\\Tse Young\\Desktop\\yearlow_quaterly_all.csv")




##################################################################################################

#To find whether the true start date by comparing the company's earliest trading dates and earliest available fiscal financial data
#True Start Date being the earliest trading date or Testing Time Period(Y2008-Y2017), whichever later

original_low <- low
original_low[original_low == 0] <- NA


NonNAindex <- function (x) {min(which(!is.na(x)))}  # To find the earliest non-NA row for the earliest trading dates
Toilet <- lapply(original_low,NonNAindex) #lapply the function across all companies
Toilet <- data.frame(Toilet)
Toilet <-t(Toilet) #transpose the data
Toilet <-data.frame(Toilet)
setDT(Toilet, keep.rownames = TRUE)[] #Convert rownames into column "row" data
colnames(Toilet)[1] <- "Symbol"
colnames(Toilet)[2] <- "rn"
Toilet <-Toilet[-1,]


#Based on the row number, use inner_join as vlookup to find the dates aka. row.names
taxitaxi<- inner_join( x=Toilet,y=taxi) 
colnames(taxitaxi) <-c("Symbol","rn","Earliest_Trading_Date")
Earliest_Fiscal_Date <- Modified_Fiscal_Month_Clean[,c(1,2)]

#To compare whether the trading date or Financial Data date is earlier
Fiscal_Date <- inner_join(taxitaxi,Earliest_Fiscal_Date)
head(Fiscal_Date)
Fiscal_Date$StartDate <- gsub('-','.',Fiscal_Date$StartDate)
Fiscal_Date$Earliest_Trading_Date_Modified<- gsub('-','.',Fiscal_Date$Earliest_Trading_Date) #To modify data for comparision
Fiscal_Date$StartDate <-as.numeric(Fiscal_Date$StartDate) #To modify data for comparision
Fiscal_Date$Earliest_Trading_Date_Modified<-as.numeric(Fiscal_Date$Earliest_Trading_Date_Modified) #To modify data for comparision
Fiscal_Date <- Fiscal_Date%>% mutate(Final_Start_Date = if_else(StartDate+1>Earliest_Trading_Date_Modified-1,StartDate+1,Earliest_Trading_Date_Modified-1)) # Fiscal Financial Data superseeds Trading Dates superseeds
head(Fiscal_Date)
Final_Fiscal_Date_Start <- Fiscal_Date[,c(1,6)]
Final_Fiscal_Date_Start_End <- inner_join(Final_Fiscal_Date_Start,Modified_Fiscal_Month_Clean)
Final_Fiscal_Date_Start_End$EndDate <-gsub('-','.',Final_Fiscal_Date_Start_End$EndDate)
Final_Fiscal_Date_Start_End_Clean <- Final_Fiscal_Date_Start_End[,c(1,2,4)]
colnames(Final_Fiscal_Date_Start_End_Clean) <- c("Symbol","StartDate","EndDate")
head(Final_Fiscal_Date_Start_End_Clean) #Combining end date as well

###############################################################################################################################

###Based on start/end dates, compute holding period and holding returns

Holding_Period <- Final_Fiscal_Date_Start_End_Clean
Holding_Period$EndDate <-as.numeric(Holding_Period$EndDate)
Holding_Period$StartHolding <-round((Holding_Period$StartDate),digits=0)
Holding_Period$EndHolding <-round((Holding_Period$EndDate),digits=0)
Holding_Period <- Holding_Period[,c(1,4,5)]
Holding_Period$Y2008 <- ifelse(Holding_Period$StartHolding<2009 & Holding_Period$EndHolding>2007,1,0)
Holding_Period$Y2009 <- ifelse(Holding_Period$StartHolding<2010 & Holding_Period$EndHolding>2008,1,0)
Holding_Period$Y2010 <- ifelse(Holding_Period$StartHolding<2011 & Holding_Period$EndHolding>2009,1,0)
Holding_Period$Y2011 <- ifelse(Holding_Period$StartHolding<2012 & Holding_Period$EndHolding>2010,1,0)
Holding_Period$Y2012 <- ifelse(Holding_Period$StartHolding<2013 & Holding_Period$EndHolding>2011,1,0)
Holding_Period$Y2013 <- ifelse(Holding_Period$StartHolding<2014 & Holding_Period$EndHolding>2012,1,0)
Holding_Period$Y2014 <- ifelse(Holding_Period$StartHolding<2015 & Holding_Period$EndHolding>2013,1,0)
Holding_Period$Y2015 <- ifelse(Holding_Period$StartHolding<2016 & Holding_Period$EndHolding>2014,1,0)
Holding_Period$Y2016 <- ifelse(Holding_Period$StartHolding<2017 & Holding_Period$EndHolding>2015,1,0)
Holding_Period$Y2017 <- ifelse(Holding_Period$StartHolding<2018 & Holding_Period$EndHolding>2016,1,0) 

Holding_Period_Matrix <- Holding_Period[,c(1,4:13)]
Holding_Period_Matrix <- Holding_Period_Matrix %>% gather("Year","Indicator",Y2008:Y2017)
head(Holding_Period_Matrix)
head(Holding_Period)

write.csv(Holding_Period_Matrix,"C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Holding_Period_Matrix.csv")

#Combining yearly returns with holding period matrix

#Annual Returns starting from Q2 to next year Q1 (not in-used)
yearlow_quaterly_clean <- yearlow_quaterly[,c(1,2,7)]
colnames(yearlow_quaterly_clean)[2] <- "Year"
yearlow_quaterly_clean$Year <- gsub("Y","",yearlow_quaterly_clean$Year)
colnames(yearlow_quaterly_clean)[3] <- "Return"
head(yearlow_quaterly_clean)
write.csv(yearlow_quaterly_clean,"C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Company_Holding_Quarterly_Return.csv")

#Combine

modified_return_quaterly <- inner_join(yearlow_quaterly_all,Holding_Period_Matrix)
modified_return_quaterly <- modified_return_quaterly %>% mutate(Q1_Holding_Return=ifelse(Indicator==0,NA,Q1_Yearly_Return))
modified_return_quaterly <- modified_return_quaterly %>% mutate(Q2_Holding_Return=ifelse(Indicator==0,NA,Q2_Yearly_Return))
modified_return_quaterly <- modified_return_quaterly %>% mutate(Q3_Holding_Return=ifelse(Indicator==0,NA,Q3_Yearly_Return))
modified_return_quaterly <- modified_return_quaterly %>% mutate(Q4_Holding_Return=ifelse(Indicator==0,NA,Q4_Yearly_Return))

modified_return_quaterly_clean <- modified_return_quaterly[,c(1,2,8:11)]
modified_return_quaterly_clean <- modified_return_quaterly_clean %>%gather ("Quarter","Holding_Return",3:6)
modified_return_quaterly_clean$Quarter <- substr(modified_return_quaterly_clean$Quarter,1,2)
head(modified_return_quaterly_clean)

write.csv(modified_return_quaterly_clean,"C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Company_Holding_Quaterly_Return_Final.csv")



