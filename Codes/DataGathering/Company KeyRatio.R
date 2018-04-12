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

#-------- Creating Full Key Ratio Data Set--------


#function for reading key ratio links into CSVs  
read_krcsv_filename <- function(krfilename){
  ret <- NULL
  attempt <- 0
  while(is.null(ret) && attempt <=9){
    attempt<-attempt+1;
    if(attempt>=9) { ret <- NULL}
    else {
      try(ret<-read.csv(krfilename,skip=3,header=FALSE))
    }
    
    Sys.sleep (1)
  }
  
  ret$Source <- krfilename #Adding Source
  ret$Symbol<-gsub("http://financials.morningstar.com/ajax/exportKR2CSV.html\\?\\&callback=\\?\\&t=XNAS:","",ret$Source)
  ret$Symbol<-gsub("\\&region=usa&culture=en-US\\&cur=&order=","",ret$Symbol)
  
  ret
}
#Key Ratio- Applying read function to allocated key ratio link files
Company_KeyRatio <- lapply(companylist$KR_Link,read_krcsv_filename)
head(Company_KeyRatio)
#Key Ratio - Converting list into vectors
Company_KeyRatio<-rbindlist(Company_KeyRatio,fill=TRUE)
#Key Ratio - "Vlookup companies's details

Company_KeyRatio <- left_join(Company_KeyRatio,company_name_symbol_fiscalmonth,by="Symbol")


write.csv(Company_KeyRatio,file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(KR).csv")


CompanyKeyRatio_Final <-subset(Company_KeyRatio,!is.na(Company_KeyRatio$V1))
CompanyKeyRatio_Final <-subset(CompanyKeyRatio_Final, Symbol !="HUNT")
colnames(Company_KeyRatio)[1:12] <-c("Key_Ratio_Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","TTM")

write.csv(CompanyKeyRatio_Final,file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(KR Final).csv")
