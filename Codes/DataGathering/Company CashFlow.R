#load library
library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)

#-------- Loading Data from Morning Star Links--------

#load NASDAQ company list
companylist <-read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(Final).csv")

#adding Cash Flow Links
companylist <- companylist %>% mutate (
                                        CF_Link=paste("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?&t=XNAS:",
                                                      Symbol,"&region=usa&culture=en-US&cur=&reportType=cf&period=12&dataType=A&order=asc&columnYear=10&curYearPart=1st10year&rounding=1&view=raw&r=865827&denominatorView=raw&number=1",
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


#-------- Creating Full Cash Flow Data Set--------


#function for reading balance sheet links into CSVs  
read_cfcsv_filename <- function(cffilename){
  
  ret <- NULL
      attempt <- 0
    while(is.null(ret) && attempt <=9){
      attempt<-attempt+1;
      if(attempt>=9) { 
        ret <- NULL}
      else {
        try(ret<-read.csv(cffilename,skip=2,header=FALSE))
      }
      
      #to add a 1 second buffer
      Sys.sleep (1)
    }
    
  ret$Source <- cffilename #Adding Source
  ret$Symbol<-gsub("http://financials.morningstar.com/ajax/ReportProcess4CSV.html\\?\\&t=XNAS:","",
                   ret$Source)
  ret$Symbol<-gsub("\\&region=usa\\&culture=en-US\\&cur=\\&reportType=cf&period=12\\&dataType=A\\&order=asc\\&columnYear=10\\&curYearPart=1st10year\\&rounding=1\\&view=raw\\&r=865827\\&denominatorView=raw\\&number=1","",
                   ret$Symbol)
  
  ret
}

#Cash Flow- Applying read function to allocated cash flow link files
Company_CashFlow <- lapply(companylist$CF_Link,
                           read_cfcsv_filename)

#Cash Flow - Converting list into vectors
Company_CashFlow<-rbindlist(Company_CashFlow,
                            fill=TRUE)

#Cash Flow - "Vlookup companies's details
Company_CashFlow <- left_join(Company_CashFlow,company_name_symbol_fiscalmonth,
                              by="Symbol")

write.csv(Company_CashFlow,
          file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(CF).csv")

#To create a list of companies without data
CompanyCashFlow_Leftout_Link <- subset(Company_CashFlow,
                                       is.na(Company_CashFlow$V1))

#To reattempt data collection for companies without data
CompanyCashFlow_Leftout <- lapply(CompanyCashFlow_Leftout_Link$Source,
                                  read_cfcsv_filename)

CompanyCashFlow_Leftout <- rbindlist(CompanyCashFlow_Leftout,
                                     fill=TRUE)
write.csv(CompanyCashFlow_Leftout,
          file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(CF Left).csv")

#Final set of companies without data
CompanyCashFlow_Leftout_Link2 <- subset(CompanyCashFlow_Leftout,
                                        is.na(CompanyCashFlow_Leftout$V1))

#To combine datasets from data collection attempt 1 & 2
CompanyCashFlow_Final <-rbind(
                                subset(Company_CashFlow,
                                       !is.na(Company_CashFlow$V1)),
                                subset(CompanyCashFlow_Leftout,
                                       !is.na(CompanyCashFlow_Leftout$V1)),
                                fill=TRUE)

#To remove a specifc company details as its details are not found in other financial statements, rendering its incomplete data unusable 
CompanyCashFlow_Final <-subset(CompanyCashFlow_Final,
                               Symbol !="HUNT")

#Renaming column names
colnames(CompanyCashFlow_Final)[1:7] <- c("Cash_Flow_Items","Y2013","Y2014","Y2015","Y2016","Y2017","TTM") 

write.csv(CompanyCashFlow_Final,
          file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(CF Final).csv")


