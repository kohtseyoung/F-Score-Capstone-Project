#load library
library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)

#-------- Loading Data from Morning Star Links--------

#load NASDAQ company list
companylist <-read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(Final).csv")

#adding Balance Sheet Links
companylist <- companylist %>% mutate (
                                        BS_Link=paste("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?&t=XNAS:",
                                                        Symbol,"&region=usa&culture=en-US&cur=&reportType=bs&period=12&dataType=A&order=asc&columnYear=10&curYearPart=1st10year&rounding=1&view=raw&r=865827&denominatorView=raw&number=1",
                                                          sep=""))

#-------- Creating new table for Companies' Details for future merges--------

#creating companies name and symbol list
company_name_symbol <-data.frame(companylist$Symbol,
                                 companylist$Name,
                                 companylist$Sector,
                                 companylist$Industry)

colnames(company_name_symbol) <- c("Symbol","Name","Sector","Industry")

write.csv(companylist,
          file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylinks.csv")


#-------- Creating Full Balance Sheet Data Set--------


#function for reading balance sheet links into CSVs  
read_bscsv_filename <- function(bsfilename){
  
  ret <- NULL
  attempt <- 0
  while(is.null(ret) && attempt <=9){
    attempt<-attempt+1;
    if(attempt>=9) {
      ret <- NULL
      }
     else {
      try(ret<-read.csv(bsfilename,skip=2,header=FALSE))
    }
    #to add a 1 second buffer    
    Sys.sleep (1)
  }
  
  #Adding Source  
  ret$Source <- bsfilename 
  ret$Symbol<-gsub("http://financials.morningstar.com/ajax/ReportProcess4CSV.html\\?\\&t=XNAS:","",
                     ret$Source)
  ret$Symbol<-gsub("\\&region=usa\\&culture=en-US\\&cur=\\&reportType=bs&period=12\\&dataType=A\\&order=asc\\&columnYear=10\\&curYearPart=1st10year\\&rounding=1\\&view=raw\\&r=865827\\&denominatorView=raw\\&number=1","",
                     ret$Symbol)

  ret
}

#Balance Sheet- Applying read function to allocated key ratio link files
Company_BalanceSheet <- lapply(companylist$BS_Link,
                               read_bscsv_filename)
head(Company_BalanceSheet)

#Balance Sheet - Converting list into vectors
Company_BalanceSheet<-rbindlist(Company_BalanceSheet,
                                fill=TRUE)

#Balance Sheet - "Vlookup" companies's details

Company_BalanceSheet <- left_join(Company_BalanceSheet,company_name_symbol_fiscalmonth,
                                  by="Symbol")

write.csv(Company_BalanceSheet,
          file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(BS).csv")

#To create a list of companies without data
CompanyBalanceSheet_Leftout_Link <- subset(Company_BalanceSheet,
                                           is.na(Company_BalanceSheet$V1))

#To reattempt data collection for companies without data
CompanyBalanceSheet_Leftout <- lapply(CompanyBalanceSheet_Leftout_Link$Source,
                                      read_bscsv_filename)

CompanyBalanceSheet_Leftout <- rbindlist(CompanyBalanceSheet_Leftout,
                                         fill=TRUE)

write.csv(CompanyBalanceSheet_Leftout,
          file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(BS Left).csv")

#To combine datasets from data collection attempt 1 & 2
CompanyBalanceSheet_Final <-rbind(
                                  subset(Company_BalanceSheet,
                                         !is.na(Company_BalanceSheet$V1)),
                                  subset(CompanyBalanceSheet_Leftout,
                                          !is.na(CompanyBalanceSheet_Leftout$V1)),
                                  fill=TRUE)

#To remove a specifc company details as its details are not found in other financial statements, rendering its incomplete data unusable 
CompanyBalanceSheet_Final <-subset(CompanyBalanceSheet_Final, 
                                    Symbol !="HUNT")
#Renaming column names
colnames(CompanyBalanceSheet_Final)[1:7] <- c("Balance_Sheet_Items","Y2013","Y2014","Y2015","Y2016","Y2017","TTM") 

write.csv(CompanyBalanceSheet_Final,
          file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(BS Final).csv")

                  
