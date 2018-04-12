#load library
library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)

#-------- Loading Data from Morning Star Links--------

#load NASDAQ company list
companylist <-read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(Final).csv")

#adding Key Ratios Links
companylist <- companylist %>% mutate (
                                        KR_Link=paste("http://financials.morningstar.com/ajax/exportKR2CSV.html?&callback=?&t=XNAS:",
                                                     Symbol,"&region=usa&culture=en-US&cur=&order=",
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

#-------- Creating Full Key Ratio Data Set--------


#function for reading key ratio links into CSVs  
read_krcsv_filename <- function(krfilename){
  ret <- NULL
  attempt <- 0
  while(is.null(ret) && attempt <=9){
    attempt<-attempt+1;
    if(attempt>=9) { 
      ret <- NULL}
    else {
      try(ret<-read.csv(krfilename,skip=3,header=FALSE))
    }
    
    Sys.sleep (1)
  }
  
  ret$Source <- krfilename #Adding Source
  ret$Symbol<-gsub("http://financials.morningstar.com/ajax/exportKR2CSV.html\\?\\&callback=\\?\\&t=XNAS:","",
                   ret$Source)
  ret$Symbol<-gsub("\\&region=usa&culture=en-US\\&cur=&order=","",
                   ret$Symbol)
  
  ret
}

#Key Ratio- Applying read function to allocated key ratio link files
Company_KeyRatio <- lapply(companylist$KR_Link,
                           read_krcsv_filename)
head(Company_KeyRatio)

#Key Ratio - Converting list into vectors
Company_KeyRatio<-rbindlist(Company_KeyRatio,
                            fill=TRUE)

#Key Ratio - "Vlookup companies's details
Company_KeyRatio <- left_join(Company_KeyRatio,company_name_symbol_fiscalmonth,
                              by="Symbol")

write.csv(Company_KeyRatio,
          file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(KR).csv")

#No reattempts as datasets are very complete in the 1st attempt of data collection
CompanyKeyRatio_Final <-subset(Company_KeyRatio,
                               !is.na(Company_KeyRatio$V1))

#To remove a specifc company details as its details are not found in other financial statements, rendering its incomplete data unusable 
CompanyKeyRatio_Final <-subset(CompanyKeyRatio_Final, 
                               Symbol !="HUNT")

#Renaming column names
colnames(Company_KeyRatio)[1:12] <-c("Key_Ratio_Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","TTM")

write.csv(CompanyKeyRatio_Final,
          file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(KR Final).csv")
