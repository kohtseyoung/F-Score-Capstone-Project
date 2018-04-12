#load library
library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)

#-------- Loading Data from Morning Star Links--------

#load NASDAQ company list
companylist <-read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(Final).csv")

#adding Income Statement Links 
companylist <- companylist %>% mutate (IS_Link=paste("http://financials.morningstar.com/ajax/ReportProcess4CSV.html?&t=XNAS:",
                                                     Symbol,"&region=usa&culture=en-US&cur=&reportType=is&period=12&dataType=A&order=asc&columnYear=10&curYearPart=1st10year&rounding=1&view=raw&r=865827&denominatorView=raw&number=1",
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

#-------- Creating Full Income Statement Data Set--------


#function for reading income statement links into CSVs  
read_iscsv_filename <- function(isfilename){
  ret <- NULL
  attempt <- 0
  while(is.null(ret) && attempt <=9){
    attempt<-attempt+1;
    if(attempt>=9) {
      ret <- NULL}
    else {
      try(ret<-read.csv(isfilename,skip=2,header=FALSE))
    }
    
    Sys.sleep (1)
  }
  
  ret$Source <- isfilename #Adding Source
  ret$Symbol<-gsub("http://financials.morningstar.com/ajax/ReportProcess4CSV.html\\?\\&t=XNAS:","",
                   ret$Source)
  ret$Symbol<-gsub("\\&region=usa\\&culture=en-US\\&cur=\\&reportType=is&period=12\\&dataType=A\\&order=asc\\&columnYear=10\\&curYearPart=1st10year\\&rounding=1\\&view=raw\\&r=865827\\&denominatorView=raw\\&number=1","",
                   ret$Symbol)
  
  ret
}


#Income Statement - Applying read function to allocated income statement link files
Company_IncomeStatement <- lapply(companylist$IS_Link,
                                  read_iscsv_filename)

#Income Statement - Converting list into vectors
Company_IncomeStatement<-rbindlist(Company_IncomeStatement[1:2051],
                                   fill=TRUE)

#Remove character strings from dataset
Company_IncomeStatement$FiscalMonth <- gsub("Fiscal.year.ends.in.","",
                                            Company_IncomeStatement$FiscalMonth)

Company_IncomeStatement$FiscalMonth <- gsub("\\.","",
                                            Company_IncomeStatement$FiscalMonth)

#Income Statement - "Vlookup companies's details
Company_IncomeStatement <- left_join(Company_IncomeStatement,company_name_symbol,
                                     by="Symbol")

write.csv(Company_IncomeStatement,
          file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(IS).csv")

#To create a list of companies without data
CompanyIncomeStatement_Leftout_Link <- subset(Company_IncomeStatement,
                                              is.na(Company_IncomeStatement$V1))

#To reattempt data collection for companies without data
CompanyIncomeStatement_Leftout <- lapply(CompanyIncomeStatement_Leftout_Link$Source,
                                         read_iscsv_filename)

CompanyIncomeStatement_Leftout <- rbindlist(CompanyIncomeStatement_Leftout,
                                            fill=TRUE
                                            
write.csv(CompanyIncomeStatement_Leftout,
          file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(IS Left).csv")

#Final dataset of companies without data
CompanyIncomeStatement_Leftout_Link2 <- subset(CompanyIncomeStatement_Leftout,
                                               is.na(CompanyIncomeStatement_Leftout$V1))

#To combine datasets from data collection attempt 1 & 2
CompanyIncomeStatement_Final <-rbind(subset(Company_IncomeStatement,
                                            !is.na(Company_IncomeStatement$V1)),
                                     subset(CompanyIncomeStatement_Leftout,
                                            !is.na(CompanyIncomeStatement_Leftout$V1)),
                                     fill=TRUE)

                                            #To remove a specifc company details as its details are not found in other financial statements, rendering its incomplete data unusable 
CompanyIncomeStatement_Final <-subset(CompanyIncomeStatement_Final, 
                                      Symbol !="HUNT")
                                            
#Renaming column names
colnames(CompanyIncomeStatement_Final)[1:7] <- c("Income_Statement_Items","Y2013","Y2014","Y2015","Y2016","Y2017","TTM") 
                                            
write.csv(CompanyIncomeStatement_Final,
          file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylist(IS Final).csv")
