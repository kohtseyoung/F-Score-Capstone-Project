#load library
library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)

#-------- Loading Data from Morning Star Links--------

#load NASDAQ company list
companylist <-read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(Final).csv")
#adding Income Statement Links 
companylist <- companylist %>% mutate (IS_Link=paste("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?&t=XNAS:",Symbol,"&region=usa&culture=en-US&cur=&reportType=is&period=12&dataType=A&order=asc&columnYear=10&curYearPart=1st10year&rounding=1&view=raw&r=865827&denominatorView=raw&number=1"
                                                     ,sep=""))
#adding Balance Sheet Links
companylist <- companylist %>% mutate (BS_Link=paste("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?&t=XNAS:",Symbol,"&region=usa&culture=en-US&cur=&reportType=bs&period=12&dataType=A&order=asc&columnYear=10&curYearPart=1st10year&rounding=1&view=raw&r=865827&denominatorView=raw&number=1"
                                                     ,sep=""))
#adding Cash Flow Links
companylist <- companylist %>% mutate (CF_Link=paste("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?&t=XNAS:",Symbol,"&region=usa&culture=en-US&cur=&reportType=cf&period=12&dataType=A&order=asc&columnYear=10&curYearPart=1st10year&rounding=1&view=raw&r=865827&denominatorView=raw&number=1"
                                                     ,sep=""))
#adding Key Ratios Links
companylist <- companylist %>% mutate (KR_Link=paste("http://financials.morningstar.com/ajax/exportKR2CSV.html?&callback=?&t=XNAS:",Symbol,"&region=usa&culture=en-US&cur=&order=",sep=""))
#adding Valuation Links               
companylist <- companylist %>% mutate (Val_Link=paste("http://financials.morningstar.com/valuate/valuation-history.action?&t=XNAS:",Symbol,"&region=usa&culture=en-US&cur=&type=price-book&_=1515700583028",sep=""))

#-------- Creating new table for Companies' Details for future merges--------

#creating companies name and symbol list
company_name_symbol <-data.frame(companylist$Symbol,companylist$Name,companylist$Sector,companylist$Industry)
colnames(company_name_symbol) <- c("Symbol","Name","Sector","Industry")

write.csv(companylist,file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylinks.csv")


#-------- Creating Full Balance Sheet Data Set--------


#function for reading balance sheet links into CSVs  
read_bscsv_filename <- function(bsfilename){
  
  ret <- NULL
  attempt <- 0
  while(is.null(ret) && attempt <=9){
    attempt<-attempt+1;
    if(attempt>=9) { ret <- NULL}
    else {
      try(ret<-read.csv(bsfilename,skip=2,header=FALSE))
    }
    
    Sys.sleep (1)
  }
  
  
  ret$Source <- bsfilename #Adding Source
  ret$Symbol<-gsub("http://financials.morningstar.com/ajax/ReportProcess4CSV.html\\?\\&t=XNAS:","",ret$Source)
  ret$Symbol<-gsub("\\&region=usa\\&culture=en-US\\&cur=\\&reportType=bs&period=12\\&dataType=A\\&order=asc\\&columnYear=10\\&curYearPart=1st10year\\&rounding=1\\&view=raw\\&r=865827\\&denominatorView=raw\\&number=1","",ret$Symbol)

  ret
}
#Balance Sheet- Applying read function to allocated key ratio link files
Company_BalanceSheet <- lapply(companylist$BS_Link,read_bscsv_filename)
head(Company_BalanceSheet)
#Balance Sheet - Converting list into vectors
Company_BalanceSheet<-rbindlist(Company_BalanceSheet,fill=TRUE)
#Balance Sheet - "Vlookup companies's details

Company_BalanceSheet <- left_join(Company_BalanceSheet,company_name_symbol_fiscalmonth,by="Symbol")

write.csv(Company_BalanceSheet,file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(BS).csv")

CompanyBalanceSheet_Leftout_Link <- subset(Company_BalanceSheet,is.na(Company_BalanceSheet$V1))

CompanyBalanceSheet_Leftout <- lapply(CompanyBalanceSheet_Leftout_Link$Source,read_bscsv_filename)
CompanyBalanceSheet_Leftout <- rbindlist(CompanyBalanceSheet_Leftout,fill=TRUE)
write.csv(CompanyBalanceSheet_Leftout,file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(BS Left).csv")


CompanyBalanceSheet_Final <-rbind(subset(Company_BalanceSheet,!is.na(Company_BalanceSheet$V1)),subset(CompanyBalanceSheet_Leftout,!is.na(CompanyBalanceSheet_Leftout$V1)),fill=TRUE)
CompanyBalanceSheet_Final <-subset(CompanyBalanceSheet_Final, Symbol !="HUNT")
colnames(CompanyBalanceSheet_Final)[1:7] <- c("Balance_Sheet_Items","Y2013","Y2014","Y2015","Y2016","Y2017","TTM") 

write.csv(CompanyBalanceSheet_Final,file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(BS Final).csv")

                  