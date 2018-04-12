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

#-------- Extracting Company's Fiscal Month based on Margins % Sales under Key Ratio Data------

#Extraction of Monthly Data
Fiscal_Month <- Company_KeyRatio %>% 
                                  filter(
                                     grepl('Margins % of Sales', Key_Ratio_Items)
                                  )

#Removing irrelevant data
Fiscal_Month<-Fiscal_Month[,c(1:11,14)]
head(Fiscal_Month)

#Renaming column names
colnames(Fiscal_Month) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")

#Renaming "Naming" column data
Fiscal_Month$Items <-as.character(Fiscal_Month$Items)
Fiscal_Month$Items[grepl("Margins",Fiscal_Month$Items)] <- "Fiscal_Month"
Fiscal_Month[is.na(Fiscal_Month)] <-0

#Modifying Fiscal Dates into character type
Fiscal_Month[,c(2:11)] <- sapply(Fiscal_Month[,c(2:11)], 
                                 function (x){
                                   as.character(x)
                                 }
                                )
#Removing irrelevant data
Fiscal_Month <- Fiscal_Month[,c(12,1,2:11)]

#Reshaping data presentation to "long" format 
Fiscal_Month <-gather(Fiscal_Month,"Year","Dates",3:12)
head(Fiscal_Month)
str(Fiscal_Month)

#-------- Checking Company's List Date based on Number of Shares under Key Ratio Data------

#Extracting Shares Data
NumberShares <- Company_KeyRatio %>% 
                                  filter(Key_Ratio_Items=="Shares Mil")

#Removing irrelevant data
NumberShares <-NumberShares[,c(1:11,14)]

#Renaming column names
colnames(NumberShares) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")

#Converting "Naming" column from vector to character type
NumberShares$Items <-as.character(NumberShares$Items)

#Adding "NA" to data with missing values, indicating no shares
NumberShares[,c(2:11)] <- sapply(
                                  NumberShares[,c(2:11)], 
                                  function (x) {
                                    (gsub("^$","NA",as.character(x))
                                    )
                                  }
)

#To remove "," from dataset for future conversion from character to numeric type
NumberShares[,c(2:11)] <- sapply(
                                  NumberShares[,c(2:11)], 
                                  function (x) {
                                     as.double(
                                       gsub(",","",as.character(x))
                                     )
                                  }
)

#Removing irrelevant data
NumberShares <- NumberShares[,c(12,1,2:11)]

#Reshaping data presentation to "long" format 
NumberShares <-gather(NumberShares,"Year","Dates",3:12)

write.csv(NumberShares,
          file="C:\\Users\\Tse Young\\Desktop\\NumberShares.csv")

#--------------Determining exact fiscal start dates of each company based on number of shares and fiscal month end------------

#Based on the column from Number of Shares, inner_join is used to determine the annual fiscal closing date for each company
Modified_Fiscal_Month <-inner_join(Fiscal_Month,NumberShares,by=c("Symbol","Year"))

#Indicating "NA" in a new column if there is no shares for that particular year
Modified_Fiscal_Month <- Modified_Fiscal_Month %>% mutate(Fiscal_Year_End=if_else(Dates.y=="NA","NA",Dates.x))

#Removing irrelevant data
Modified_Fiscal_Month <- Modified_Fiscal_Month[,c(1:3,7)]

#Reshaping data presentation to "wide" format 
Modified_Fiscal_Month <- Modified_Fiscal_Month %>% 
                                                spread(Year,Fiscal_Year_End)

#Calculate the number of "NA" for each year under individual companies rows
Modified_Fiscal_Month$Na_count <- apply(Modified_Fiscal_Month, 
                                        1, 
                                        function(x) sum(is.na(x))
                                        )

#Based on number of NA, compute the begin year where company have share
Modified_Fiscal_Month <-Modified_Fiscal_Month %>% 
                                                mutate(Start_Year_count = 2008+ Na_count)
#Modify Starting Year Dataset
Modified_Fiscal_Month$Start_Year_count <- gsub("^","Y",
                                               Modified_Fiscal_Month$Start_Year_count)

#Creating dataset for vlookup purpose to find fiscal dates for each companies
Fiscal_Month_Count <- Fiscal_Month
colnames(Fiscal_Month_Count)[3] <- "Start_Year_count"

#Vlookup fiscal dates based on Starting Year Dataset
Modified_Fiscal_Month <- left_join(Modified_Fiscal_Month,
                                   Fiscal_Month_Count)

#End Date is based on the latest column of the dataset
Modified_Fiscal_Month$EndDate <- Modified_Fiscal_Month$Y2017

#Removing irrelevant data
Modified_Fiscal_Month_Clean <- Modified_Fiscal_Month[,c(1,16,17)]

#Renaming column names
colnames(Modified_Fiscal_Month_Clean)[2] <- "StartDate"
head(Modified_Fiscal_Month_Clean)


####################################################################################################

#-------------Loading Returns Data using quantmod---------------------

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
    silent=TRUE)}
      )

#For Companies with available data, the objects will be created under the new enviroment; 
#This is subsequently compared with the initial input list to filter out those with data from those without
company_available <- ls(WoW) 

#Transforming data into a data.frame format
company_available <- data.frame(
                                matrix(company_available,ncol =1))

#Renaming column names
colnames(company_available) <- "Symbol"

#Transforming data into a data.frame format
company_full_list <-data.frame(
                                 matrix(companylist_only,ncol=1))

#Renaming column names
colnames(company_full_list) <- "Symbol"

#Extracting a list of companies that does not have data based on comparision
company_not_available <- anti_join(company_full_list,
                                   company_available,by="Symbol")

#Storing companies' symbol that has been convereted into objects into a list
chickennnn <- mget(ls(WoW),
                   envir= WoW) 

write.csv(company_not_available,
          "C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\company_not_available.csv")

#------------Extracting quarterly returns-------------------


#Extracting quarterly returns from modified Share Information that take into account stocksplits

chickennnn_adjusted_stocksplit <- lapply(company_available_string,
                                         function(x) {
                                           #adjusting share prices to reflect stock splits
                                           adjustOHLC(
                                             get(x, pos=WoW), 
                                             symbol.name=x, 
                                             adjust=c("split"), 
                                             use.Adjusted=FALSE)
                                         }
                                        )

#Finding quarterly return based on adjusted stock-split share price
yearlow_quaterly<-lapply(chickennnn_adjusted_stocksplit,quarterlyReturn)

#Columnbind the list
yearlow_quaterly <- do.call(cbind,yearlow_quaterly)

#Convert into data.frame format
yearlow_quaterly<- data.frame(yearlow_quaterly) 

#Apply company names
names(yearlow_quaterly) <-ls(WoW) 

#Setting rownames "Dates" as an additional column
yearlow_quaterly <- setDT(yearlow_quaterly,keep.rownames = TRUE)[]

#Removing months from date
yearlow_quaterly$rn <- gsub('.{3}$', '', yearlow_quaterly$rn)

#For missing data values, convert to 0
yearlow_quaterly[is.na(yearlow_quaterly)] <- 0

#To apply sum to each columm
yearlow_quaterly <-data.frame(ddply(yearlow_quaterly, "rn", numcolwise(sum)))

#To rename column names
colnames(yearlow_quaterly)[1] <-"Symbol"

#To compute row sums
yearlow_quaterly<- yearlow_quaterly %>% mutate(Total = rowSums(yearlow_quaterly[,-1]))
yearlow_quaterly$Total <- rowSums(yearlow_quaterly[,c(2:81)])

#To filter out companies with no return data
yearlow_quaterly <- filter(yearlow_quaterly,Total != 0)
head(yearlow_quaterly)

#Storing col and row names as objects for future use
yearlow_quaterly_colnames <- colnames(yearlow_quaterly)
yearlow_quaterly_rownames <- yearlow_quaterly$Symbol

#Transposing dataset and subsequent cleaning
yearlow_quaterly <- t(yearlow_quaterly)
yearlow_quaterly<- data.frame(yearlow_quaterly)
yearlow_quaterly<- setDT(yearlow_quaterly,keep.rownames = TRUE)
colnames(yearlow_quaterly)[1] <- "Symbol"
colnames(yearlow_quaterly)[2:54] <- yearlow_quaterly_rownames
yearlow_quaterly <- yearlow_quaterly[-1,]
yearlow_quaterly<-yearlow_quaterly[,c(1:54)]

#Reformating Dataset
yearlow_quaterly <- yearlow_quaterly %>% gather(Year,Quarterly_Return,c(2:54))
yearlow_quaterly <- yearlow_quaterly %>% separate(Year, c("Year","Month"))
yearlow_quaterly$Year <- as.numeric(yearlow_quaterly$Year)
yearlow_quaterly$Quarterly_Return <- as.double(yearlow_quaterly$Quarterly_Return)

#Adding 1 to returns so to allow multiplication 
yearlow_quaterly$Quarterly_Return <- yearlow_quaterly$Quarterly_Return+1
head(yearlow_quaterly)


##################################################################################################

#-------Using quarterly returns to compute annual returns based on starting holding period-------

#Q3 onwards (March This Year)

  #Returns include Q3,Q4 and next year Q1,Q2
  yearlow_quaterly_Q3 <- yearlow_quaterly %>%  mutate (Modified_Year = ifelse(Month=="03", Year-1,
                                                                       ifelse(Month=="06",Year-1,Year)))
  #Cleaning dataset
  yearlow_quaterly_Q3 <- yearlow_quaterly_Q3[,c(1,5,3,4)]

  #Modifying dataset type
  yearlow_quaterly_Q3[,c(4)] <- as.double(
                                        unlist(yearlow_quaterly_Q3[,c(4)]))

  #Formatting dataset to "wide"
  yearlow_quaterly_Q3 <- yearlow_quaterly_Q3 %>% 
                                            spread (Month, Quarterly_Return)

  #Multiplication of quarterly returns to compute annual returns
  yearlow_quaterly_Q3 <- yearlow_quaterly_Q3 %>% mutate (Yearly_Return = 
                                                      yearlow_quaterly_Q3[,3]*
                                                      yearlow_quaterly_Q3[,4]*
                                                      yearlow_quaterly_Q3[,5]*
                                                      yearlow_quaterly_Q3[,6])

  #Remove 1 to compute returns
  yearlow_quaterly_Q3$Yearly_Return <- yearlow_quaterly_Q3$Yearly_Return -1

  #Formatting dataset
  yearlow_quaterly_Q3_Clean <- yearlow_quaterly_Q3[,c(1,2,7)]

  #Renaming dataset
  colnames(yearlow_quaterly_Q3_Clean)[c(2,3)] <- c("Year","Q3_Yearly_Return")
  yearlow_quaterly_Q3_Clean$Year <- gsub("^","Y",yearlow_quaterly_Q3_Clean$Year)

  head(yearlow_quaterly_Q3_Clean)

#Q4 onwards (June This Year)

  #Returns include Q4 and next year Q1,Q2,Q3
  yearlow_quaterly_Q4 <- yearlow_quaterly %>%  mutate (Modified_Year = ifelse(Month=="03", Year-1,
                                                                            ifelse(Month=="06", Year-1,
                                                                                   ifelse(Month=="09",Year-1,Year))))
  #Cleaning dataset
  yearlow_quaterly_Q4 <- yearlow_quaterly_Q4[,c(1,5,3,4)]

  #Modifying dataset type
  yearlow_quaterly_Q4[,c(4)] <- as.double(
                                          unlist(yearlow_quaterly_Q4[,c(4)]))
  #Formatting dataset to "wide"
  yearlow_quaterly_Q4 <- yearlow_quaterly_Q4 %>% spread (Month, Quarterly_Return)

  #Multiplication of quarterly returns to compute annual returns
  yearlow_quaterly_Q4 <- yearlow_quaterly_Q4 %>% mutate (Yearly_Return = 
                                                       yearlow_quaterly_Q4[,3]*
                                                       yearlow_quaterly_Q4[,4]*
                                                       yearlow_quaterly_Q4[,5]*
                                                       yearlow_quaterly_Q4[,6])

  #Remove 1 to compute returns
  yearlow_quaterly_Q4$Yearly_Return <- yearlow_quaterly_Q4$Yearly_Return -1

  #Formatting dataset
  yearlow_quaterly_Q4_Clean <- yearlow_quaterly_Q4[,c(1,2,7)]

  #Renaming dataset
  colnames(yearlow_quaterly_Q4_Clean)[c(2,3)] <- c("Year","Q4_Yearly_Return")
  yearlow_quaterly_Q4_Clean$Year <- gsub("^","Y",yearlow_quaterly_Q4_Clean$Year)

  head(yearlow_quaterly_Q4_Clean)


#Next Q1 onwards (September This Year)

  #Returns include next year Q1,Q2,Q3,Q4
  yearlow_quaterly_Q1 <- yearlow_quaterly %>%  mutate (Modified_Year = ifelse(Month=="03",Year-1,
                                                                            ifelse(Month=="06",Year-1,
                                                                                   ifelse(Month=="09",Year-1,
                                                                                       ifelse(Month=="12",Year-1,Year)))))
  #Cleaning dataset
  yearlow_quaterly_Q1 <- yearlow_quaterly_Q1[,c(1,5,3,4)]

  #Modifying dataset type
  yearlow_quaterly_Q1[,c(4)] <- as.double(unlist(yearlow_quaterly_Q1[,c(4)]))

  #Formatting dataset to "wide"
  yearlow_quaterly_Q1 <- yearlow_quaterly_Q1 %>% spread (Month, Quarterly_Return)

  #Multiplication of quarterly returns to compute annual returns
  yearlow_quaterly_Q1 <- yearlow_quaterly_Q1 %>% mutate (Yearly_Return = 
                                                       yearlow_quaterly_Q1[,3]*
                                                       yearlow_quaterly_Q1[,4]*
                                                       yearlow_quaterly_Q1[,5]*
                                                       yearlow_quaterly_Q1[,6])

  #Remove 1 to compute returns
  yearlow_quaterly_Q1$Yearly_Return <- yearlow_quaterly_Q1$Yearly_Return -1

  #Formatting dataset
  yearlow_quaterly_Q1_Clean <- yearlow_quaterly_Q1[,c(1,2,7)]

  #Renaming dataset
  colnames(yearlow_quaterly_Q1_Clean)[c(2,3)] <- c("Year","Q1_Yearly_Return")
  yearlow_quaterly_Q1_Clean$Year <- gsub("^","Y",yearlow_quaterly_Q1_Clean$Year)

  head(yearlow_quaterly_Q1_Clean)

#Next Q2 onwards (December This Year)

  #Returns include next year Q2,Q3,Q4 and following next year Q1
  yearlow_quaterly_Q2 <- yearlow_quaterly %>%  mutate (Modified_Year = ifelse(Month=="03",Year-1,Year))
                                                                            ifelse(Month=="06",Year-1,
                                                                                   ifelse(Month=="09",Year-1,
                                                                                          ifelse(Month=="12",Year-1,Year)))

  #Cleaning dataset
  yearlow_quaterly_Q2 <- yearlow_quaterly_Q2[,c(1,5,3,4)]

  #Modifying dataset type
  yearlow_quaterly_Q2[,c(4)] <- as.double(unlist(yearlow_quaterly_Q2[,c(4)]))

  #Formatting dataset to "wide"
  yearlow_quaterly_Q2 <- yearlow_quaterly_Q2 %>% spread (Month, Quarterly_Return)

  #Multiplication of quarterly returns to compute annual returns
  yearlow_quaterly_Q2 <- yearlow_quaterly_Q2 %>% mutate (Yearly_Return = 
                                                       yearlow_quaterly_Q2[,3]*
                                                       yearlow_quaterly_Q2[,4]*
                                                       yearlow_quaterly_Q2[,5]*
                                                       yearlow_quaterly_Q2[,6])

  #Remove 1 to compute returns
  yearlow_quaterly_Q2$Yearly_Return <- yearlow_quaterly_Q2$Yearly_Return -1

  #Formatting dataset
  yearlow_quaterly_Q2_Clean <- yearlow_quaterly_Q2[,c(1,2,7)]

  #Renaming dataset
  colnames(yearlow_quaterly_Q2_Clean)[c(2,3)] <- c("Year","Q2_Yearly_Return")
  yearlow_quaterly_Q2_Clean$Year <- gsub("^","Y",yearlow_quaterly_Q2_Clean$Year)

  head(yearlow_quaterly_Q2_Clean)

                                        
#Combning all annual returns by quarter
yearlow_quaterly_all <-  left_join(yearlow_quaterly_Q1_Clean, yearlow_quaterly_Q2_Clean) %>%
                            left_join(., yearlow_quaterly_Q3_Clean) %>% 
                                  left_join(.,yearlow_quaterly_Q4_Clean) 
head(yearlow_quaterly_all)

write.csv(yearlow_quaterly,file="C:\\Users\\Tse Young\\Desktop\\yearlow_quaterly.csv")
write.csv(yearlow_quaterly_all,file="C:\\Users\\Tse Young\\Desktop\\yearlow_quaterly_all.csv")




##################################################################################################

#To find whether the true start date by comparing the company's earliest trading dates and earliest available fiscal financial data
#True Start Date being the earliest trading date or Testing Time Period(Y2008-Y2017), whichever later

#To create monthly data set for the purpose of looking for missing values to specify earliest trading dates by month
low <- lapply(chickennnn,
              monthlyReturn)
#columnbind the list
low <- do.call(cbind,low) 

#convert into data.frame format
low <-data.frame(low) 

#apply company names
names(low)<-ls(WoW) 

#To create date in corresponse to the row number
taxi <- data.frame(rownames(low))

taxi$rownames.low. <- gsub('.{3}$','',taxi$rownames.low.)
taxi <-unique(taxi[1])
taxi$rn <- 1:159
taxi$rn <- as.numeric(taxi$rn)

low <- setDT(low,keep.rownames = TRUE)[]
low$rn <- gsub('.{3}$', '', low$rn)
low[is.na(low)] <- 0
low <-data.frame(ddply(low, "rn", numcolwise(sum)))


original_low <- low
original_low[original_low == 0] <- NA


# To find the earliest non-NA row for the earliest trading dates
NonNAindex <- function (x) {
                            min(
                              which(!is.na(x))
                            )} 

#lapply the function across all companies
Toilet <- lapply(original_low,NonNAindex) 
Toilet <- data.frame(Toilet)

#transpose the data
Toilet <-t(Toilet) 
Toilet <-data.frame(Toilet)

#Convert rownames into column "row" data and renaming of column names
setDT(Toilet, keep.rownames = TRUE)[] 
colnames(Toilet)[1] <- "Symbol"
colnames(Toilet)[2] <- "rn"
Toilet <-Toilet[-1,]

#Based on the row number, use inner_join as vlookup to find the dates aka. row.names
taxitaxi<- inner_join( x=Toilet,y=taxi) 
colnames(taxitaxi) <-c("Symbol","rn","Earliest_Trading_Date")
Earliest_Fiscal_Date <- Modified_Fiscal_Month_Clean[,c(1,2)]

#To compare whether the trading date or Financial Data date is earlier
Fiscal_Date <- inner_join(taxitaxi,Earliest_Fiscal_Date)

#Conversion of dataset for comparision
Fiscal_Date$StartDate <- gsub('-','.',Fiscal_Date$StartDate)
Fiscal_Date$Earliest_Trading_Date_Modified<- gsub('-','.',Fiscal_Date$Earliest_Trading_Date)
Fiscal_Date$StartDate <-as.numeric(Fiscal_Date$StartDate) 
Fiscal_Date$Earliest_Trading_Date_Modified<-as.numeric(Fiscal_Date$Earliest_Trading_Date_Modified) 

#Comparing Start Date from Financial Data and Trading Date 
#StartDate + 1 as Dataset requires prior year data for computation; Earliest Trading Date -1 as prior year financial data can be used 
Fiscal_Date <- Fiscal_Date%>% mutate(Final_Start_Date = if_else(StartDate+1>Earliest_Trading_Date_Modified-1,StartDate+1,Earliest_Trading_Date_Modified-1))

#Cleaning of dataset
Final_Fiscal_Date_Start <- Fiscal_Date[,c(1,6)]

#Summarizing and cleaning of dataset
Final_Fiscal_Date_Start_End <- inner_join(Final_Fiscal_Date_Start,Modified_Fiscal_Month_Clean)
Final_Fiscal_Date_Start_End$EndDate <-gsub('-','.',Final_Fiscal_Date_Start_End$EndDate)
Final_Fiscal_Date_Start_End_Clean <- Final_Fiscal_Date_Start_End[,c(1,2,4)]
colnames(Final_Fiscal_Date_Start_End_Clean) <- c("Symbol","StartDate","EndDate")

head(Final_Fiscal_Date_Start_End_Clean) #Combining end date as well

###############################################################################################################################

###Based on start/end dates, compute holding period and holding returns

#Cleaning and formating dataset to compute holding period
Holding_Period <- Final_Fiscal_Date_Start_End_Clean
Holding_Period$EndDate <-as.numeric(Holding_Period$EndDate)
Holding_Period$StartHolding <-round((Holding_Period$StartDate),digits=0)
Holding_Period$EndHolding <-round((Holding_Period$EndDate),digits=0)
Holding_Period <- Holding_Period[,c(1,4,5)]

#Holding Period is determined to be within the range of the starting and ending date
Holding_Period$Y2008 <- ifelse(Holding_Period$StartHolding<2009 & 
                               Holding_Period$EndHolding>2007,1,0)

Holding_Period$Y2009 <- ifelse(Holding_Period$StartHolding<2010 &
                               Holding_Period$EndHolding>2008,1,0)

Holding_Period$Y2010 <- ifelse(Holding_Period$StartHolding<2011 &
                               Holding_Period$EndHolding>2009,1,0)

Holding_Period$Y2011 <- ifelse(Holding_Period$StartHolding<2012 & 
                               Holding_Period$EndHolding>2010,1,0)

Holding_Period$Y2012 <- ifelse(Holding_Period$StartHolding<2013 & 
                               Holding_Period$EndHolding>2011,1,0)

Holding_Period$Y2013 <- ifelse(Holding_Period$StartHolding<2014 & 
                               Holding_Period$EndHolding>2012,1,0)

Holding_Period$Y2014 <- ifelse(Holding_Period$StartHolding<2015 & 
                               Holding_Period$EndHolding>2013,1,0)

Holding_Period$Y2015 <- ifelse(Holding_Period$StartHolding<2016 &
                               Holding_Period$EndHolding>2014,1,0)

Holding_Period$Y2016 <- ifelse(Holding_Period$StartHolding<2017 & 
                               Holding_Period$EndHolding>2015,1,0)

Holding_Period$Y2017 <- ifelse(Holding_Period$StartHolding<2018 & 
                               Holding_Period$EndHolding>2016,1,0) 

#Cleaning and formatting of dataset
Holding_Period_Matrix <- Holding_Period[,c(1,4:13)]
Holding_Period_Matrix <- Holding_Period_Matrix %>% gather("Year","Indicator",Y2008:Y2017)

head(Holding_Period_Matrix)
head(Holding_Period)

write.csv(Holding_Period_Matrix,"C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Holding_Period_Matrix.csv")

#Combining yearly returns with holding period matrix
modified_return_quaterly <- inner_join(yearlow_quaterly_all,Holding_Period_Matrix)

#Based on holding period matrix, holding returns = NA when it is not within holding period
modified_return_quaterly <- modified_return_quaterly %>% mutate(Q1_Holding_Return=ifelse(Indicator==0,NA,Q1_Yearly_Return))
modified_return_quaterly <- modified_return_quaterly %>% mutate(Q2_Holding_Return=ifelse(Indicator==0,NA,Q2_Yearly_Return))
modified_return_quaterly <- modified_return_quaterly %>% mutate(Q3_Holding_Return=ifelse(Indicator==0,NA,Q3_Yearly_Return))
modified_return_quaterly <- modified_return_quaterly %>% mutate(Q4_Holding_Return=ifelse(Indicator==0,NA,Q4_Yearly_Return))

#Summurizing and formatting of dataset
modified_return_quaterly_clean <- modified_return_quaterly[,c(1,2,8:11)]
modified_return_quaterly_clean <- modified_return_quaterly_clean %>%gather ("Quarter","Holding_Return",3:6)
modified_return_quaterly_clean$Quarter <- substr(modified_return_quaterly_clean$Quarter,1,2)
head(modified_return_quaterly_clean)

write.csv(modified_return_quaterly_clean,"C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Company_Holding_Quaterly_Return_Final.csv")



