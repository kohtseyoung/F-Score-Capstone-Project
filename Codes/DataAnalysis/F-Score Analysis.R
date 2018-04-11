#load library
library(ggplot2)
library(tidyr)
library(dplyr)
library(data.table)
library(plyr)
library(car)
library(stringr)
library(ggpubr)
library(agricolae)
library(hexbin)
library(Hmisc)
library(wesanderson)
#To read Company F-Scores and Returns

Company_F_Score_Full <-read.csv(file="C:\\Users\\Tse Young\\Desktop\\F_Score_Full_Final.csv")
Company_Returns <-read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Company_Holding_Quaterly_Return_Final.csv")
Company_F_Score_Fiscal_Year <- read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Holding_Period_Matrix.csv")
Company_Low_Trading_Volume<-read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Company_Volume_Low_Trades.csv")
Company_F_Score_Classes <- read.csv(file="C:/Users/Tse Young/Desktop/F_Score_Tally_Full_Final.csv")

#-------- Creating Full Key Ratio Data Set--------

Company_KeyRatio <- read.csv(file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Full Data Set\\Key Ratios\\companylist(KR Final).csv")
Company_KeyRatio <-Company_KeyRatio[,c(2:18)]

head(Company_KeyRatio)

#-------- Checking Company's Fiscal Month based on Margins % Sales under Key Ratio Data------

Company_F_Score_Fiscal_Year <- Company_KeyRatio %>% filter(grepl('Margins % of Sales', Key_Ratio_Items))
Company_F_Score_Fiscal_Year<-Company_F_Score_Fiscal_Year[,c(1:11,14)]
head(Company_F_Score_Fiscal_Year)
colnames(Company_F_Score_Fiscal_Year) <- c("Items","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013","Y2014","Y2015","Y2016","Y2017","Symbol")
Company_F_Score_Fiscal_Year$Items <-as.character(Company_F_Score_Fiscal_Year$Items)
Company_F_Score_Fiscal_Year$Items[grepl("Margins",Company_F_Score_Fiscal_Year$Items)] <- "Company_F_Score_Fiscal_Year"
Company_F_Score_Fiscal_Year[is.na(Company_F_Score_Fiscal_Year)] <-0
Company_F_Score_Fiscal_Year[,c(2:11)] <- sapply(Company_F_Score_Fiscal_Year[,c(2:11)], function (x){as.character(x)})
Company_F_Score_Fiscal_Year <- Company_F_Score_Fiscal_Year[,c(12,1,2:11)]
Company_F_Score_Fiscal_Year <-gather(Company_F_Score_Fiscal_Year,"Year","Dates",3:12)
Company_F_Score_Fiscal_Year <-separate(Company_F_Score_Fiscal_Year,"Dates",c("Dates_Modified","Month"),sep="-")
Company_F_Score_Fiscal_Year$Dates_Modified <- gsub("^","Y",Company_F_Score_Fiscal_Year$Dates_Modified)
Company_F_Score_Fiscal_Year$Month <-as.double(Company_F_Score_Fiscal_Year$Month)
Company_F_Score_Fiscal_Year <- Company_F_Score_Fiscal_Year %>% mutate(Quarter=ifelse(Month<4,"Q3",ifelse(Month<7,"Q4",ifelse(Month<10,"Q1",ifelse(Month<13,"Q2")))))
head(Company_F_Score_Fiscal_Year)
str(Company_F_Score_Fiscal_Year)


#---------Company_F_Score_Inputs----------#


Company_F_Score_Only <- Company_F_Score_Full[,c(2:3,22)] # To filter out F-Score Only


#Combining F_Score and its Holding Date to correspond with Holding Returns computed
Company_F_Score_Only <- left_join(Company_F_Score_Only,Company_F_Score_Fiscal_Year)
Company_F_Score_Only <- Company_F_Score_Only[,c(5,7,2:3)]
colnames(Company_F_Score_Only)[1] <- 'Year' 
head(Company_F_Score_Only)


#Combining F_Score and Company_Returns

Company_F_Score_Returns <- left_join(Company_F_Score_Only,Company_Returns) # Joint both F-Score and Returns
Company_F_Score_Returns_Full <- left_join(Company_F_Score_Full,Company_Returns,by=c("Symbol","Year"))
head(Company_F_Score_Returns_Full)
head(Company_F_Score_Returns)
head(Company_Returns)
#Filtering out outliers, unused & NA data

Company_F_Score_Returns_Filtered <- filter(Company_F_Score_Returns,Holding_Return != 0) # To remove Holding_Return that are NA
Company_F_Score_Returns_Filtered <- filter(Company_F_Score_Returns_Filtered,Year != "Y2008") # To remove Y2018 as it is unused
Company_F_Score_Returns_Filtered <- filter(Company_F_Score_Returns_Filtered,F.Score != "0") # To remove Y2018 as it is unused
Company_F_Score_Returns_Filtered_Excess_Returns <- filter(Company_F_Score_Returns_Filtered,Holding_Return >2) # To remove Holding Return that are too high, excluding outliers
Company_F_Score_Returns_Filtered_Excess_Returns <- Company_F_Score_Returns_Filtered_Excess_Returns[,c(3)]
Company_F_Score_Returns_Filtered_Excess_Returns <-data.frame(Company_F_Score_Returns_Filtered_Excess_Returns)
colnames(Company_F_Score_Returns_Filtered_Excess_Returns) <- "Symbol"
head(Company_F_Score_Returns_Filtered_Excess_Returns)
Company_F_Score_Returns_Filtered<- anti_join(Company_F_Score_Returns_Filtered,Company_F_Score_Returns_Filtered_Excess_Returns)
head(Company_F_Score_Returns_Filtered)

#To add the various F-Scores into groups 
Company_F_Score_Grouping <- Company_F_Score_Returns_Filtered %>% mutate(F.Score_Grouping = ifelse(F.Score>6, "High_F.Score",ifelse(F.Score<4,"Low_F.Score","Neither")))
Company_F_Score_Grouping_Only <- Company_F_Score_Grouping[,c(1,3,7,6)]


Company_F_Score_Grouping_Only<- filter(Company_F_Score_Grouping_Only, F.Score_Grouping != "Neither")
Company_F_Score_Grouping_Only_Spread<- Company_F_Score_Grouping_Only %>% spread(F.Score_Grouping, Holding_Return)                                                                       
head(Company_F_Score_Grouping_Only_Spread)

#Filter Companies with Low Trade Volume

Company_F_Score_Grouping_Only_Volume_Filtered <- anti_join(Company_F_Score_Grouping_Only,Company_Low_Trading_Volume)
Company_F_Score_Returns_Volume_Filtered <- anti_join(Company_F_Score_Returns_Filtered[,c(1,3,4,6)],Company_Low_Trading_Volume,by=c("Year","Symbol"))
head(Company_F_Score_Returns_Volume_Filtered)


write.csv(Company_F_Score_Grouping_Only_Volume_Filtered,file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\FScore_Group.csv")
write.csv(Company_F_Score_Returns_Full,file="C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\FScore_Group_Full.csv")


Company_F_Score_Summary <- Company_F_Score_Grouping_Only_Volume_Filtered %>% ddply(~Year~F.Score_Grouping,summarise,mean=mean(Holding_Return,na.rm=TRUE))
Company_F_Score_Summary<- Company_F_Score_Summary %>% spread(F.Score_Grouping,mean)
Company_F_Score_Summary<- Company_F_Score_Summary %>% mutate(Difference = High_F.Score-Low_F.Score)
Company_F_Score_Summary_Test <- Company_F_Score_Summary %>% summarise(sum=sum(Difference))
head(Company_F_Score_Summary_Test)
head(Company_F_Score_Summary)



#Convert F Score into Factor levels 
Company_F_Score_Grouping_Only_Volume_Filtered$F.Score_Grouping <- as.factor(Company_F_Score_Grouping_Only_Volume_Filtered$F.Score_Grouping)
Company_F_Score_Returns_Volume_Filtered$F.Score <- as.factor(Company_F_Score_Returns_Volume_Filtered$F.Score)

#Add PE_Ratio
PE_Ratio <- read.csv("C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\Company_PE_Ratio_Modified.csv")
PE_Ratio_Clean<- PE_Ratio[,c(5,6,11)]
head(PE_Ratio_Clean)
PE_Ratio_Clean <- PE_Ratio_Clean %>% mutate(PE_Ratio = ifelse(Modified_PE_Ratio>0 & Modified_PE_Ratio<100,Modified_PE_Ratio,0))
                                        
head(PE_Ratio_Clean)
Company_F_Score_Grouping_Only_Volume_Filtered_PE <- left_join(Company_F_Score_Grouping_Only_Volume_Filtered,PE_Ratio_Clean,by=c("Year","Symbol"))
head(Company_F_Score_Grouping_Only_Volume_Filtered_PE)
Company_F_Score_Returns_Volume_Filtered_PE <- left_join(Company_F_Score_Returns_Volume_Filtered,PE_Ratio_Clean, by=c("Year","Symbol"))
head(Company_F_Score_Returns_Volume_Filtered_PE)
head(PE_Ratio)

#Add Industry

Company_Industy <-read.csv("C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\companylinks.csv")
Company_Industry_Clean <- Company_Industy[,c(2,8,9)]
head(Company_Industry_Clean)
Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry <- left_join(Company_F_Score_Grouping_Only_Volume_Filtered_PE,Company_Industry_Clean,by="Symbol")
head(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry)
Company_F_Score_Returns_Volume_Filtered_PE_Industry <- left_join(Company_F_Score_Returns_Volume_Filtered_PE,Company_Industry_Clean, by="Symbol")
head(Company_F_Score_Returns_Volume_Filtered_PE_Industry)
write.csv(Company_F_Score_Returns_Volume_Filtered_PE_Industry,"C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\PE_Industry_Filtered.csv")
Company_F_Score_Returns_Volume_Filtered_PE_Industry <- Company_F_Score_Returns_Volume_Filtered_PE_Industry[,-4]


#Add F Score Classes for further testing

head(Company_F_Score_Classes)
head(Company_F_Score_Returns_Volume_Filtered_PE_Industry)
Company_F_Score_Classes$F.Score <-as.factor(Company_F_Score_Classes$F.Score)
str(Company_F_Score_Returns_Volume_Filtered_PE_Industry)

Company_F_Score_Returns_Volume_Filtered_PE_Industry_Classes <- left_join(Company_F_Score_Returns_Volume_Filtered_PE_Industry,Company_F_Score_Classes)
Company_F_Score_Returns_Volume_Filtered_PE_Industry_Classes <- Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry_Classes[,-9]
head(Company_F_Score_Returns_Volume_Filtered_PE_Industry_Classes)
Company_F_Score_Returns_Volume_Filtered_PE_Industry_Classes <-Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry_Classes[,c(1,2,9:11,3:8)]
Company_F_Score_Returns_Volume_Filtered_PE_Industry_Classes$F.Score <- as.integer(Company_F_Score_Returns_Volume_Filtered_PE_Industry_Classes$F.Score)
head(Company_F_Score_Returns_Volume_Filtered_PE_Industry_Classes)


write.csv(Company_F_Score_Returns_Volume_Filtered_PE_Industry_Classes,"C:\\Users\\Tse Young\\Desktop\\Data Science research\\F-score\\Company List\\PE_Industry_Classes_Filtered.csv")


##################################################################################################################################

#-----------Hypothesiss Testing------------------#

#Hypothesis 1: - The difference between High F-Score and Low F-Score group's market adjusted return = 0

ANOVA_Base_Group <- aov(Holding_Return ~ F.Score_Grouping,data = Company_F_Score_Grouping_Only_Volume_Filtered)
ANOVA_Base_Score <- aov(Holding_Return ~ F.Score,data = Company_F_Score_Returns_Volume_Filtered)
Hypothesis1_Data<- HSD.test(ANOVA_Base_Group,"F.Score_Grouping",group=TRUE)


summary(ANOVA_Base_Group)
summary(ANOVA_Base_Score)
Hypothesis1_Data

  #Hypothesis 1 Plots

ScatterPlot_Base <- ggplot(Company_F_Score_Returns_Volume_Filtered,aes(x=F.Score,y=Holding_Return,color=F.Score)) +
                    geom_point() + 
                    stat_summary(fun.y=mean,geom="line",lwd=2,aes(group=1))

ScatterPlot_Group <- ggplot (Company_F_Score_Grouping_Only_Volume_Filtered,aes(x=F.Score_Grouping,y=Holding_Return,color=F.Score_Grouping)) +
                      geom_point() +
                      scale_x_discrete(labels=c("High","Low"))
ScatterPlot_Group
ScatterPlot_Base


GG_Line <- ggline(Company_F_Score_Grouping_Only_Volume_Filtered, x = "F.Score_Grouping", y = "Holding_Return", 
                  add = c("mean_se", "jitter"), 
                  order = c("Low_F.Score", "High_F.Score"),
                  ylab = "Holding_Return", xlab = "F.Score_Grouping")
GG_Line

Violin_Plot_Group <- ggplot(Company_F_Score_Grouping_Only_Volume_Filtered_PE, aes(x=F.Score_Grouping, y=Holding_Return,fill=F.Score_Grouping)) + 
                       geom_violin(trim=FALSE) + 
                       stat_summary(fun.data="mean_sdl", fun.args = list(mult=1.96),geom="crossbar", width=0.2 )+
                       scale_fill_manual(values=c("#E69F00", "#56B4E9")) +scale_x_discrete(labels=c("High","Low"))+
                        theme(axis.text=element_text(size=10),
                        axis.title=element_text(size=12,face="bold"))
Violin_Plot_Group


#Hypothesis 2: - The High/Low grouping based on F-Score is consistent with the original F-Score grouping

TukeyHSD(ANOVA_Base_Score)
Hypothesis2 <- HSD.test(ANOVA_Base_Score, "F.Score", group=TRUE)
Hypothesis2

  #Hypothesis 2 Plots

Violin_Plot_Base <- ggplot(Company_F_Score_Returns_Volume_Filtered_PE, aes(x=F.Score, y=Holding_Return,fill=F.Score)) + 
  geom_violin(trim=FALSE) + 
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),geom="crossbar", width=0.2 )



Violin_Plot_Base

GG_Line_All <- ggline(Company_F_Score_Returns_Volume_Filtered, x = "F.Score", y = "Holding_Return", 
                      add = c("mean_se", "jitter"),
                      order = c(1,2,3,4,5,6,7,8,9),
                      ylab = "Holding_Return", xlab = "F.Score")

GG_Line_All



#Hypothesis 3: - Changing increase in number of shares as a positive indicator yields greater returns (>Original F-Score)
t.test(Holding_Return ~ F.Score_Grouping,data = Company_Modified_F_Score_Grouping_Only_Volume_Filtered) #Obtained from F-Score Anlaysis(Modified)
t.test(Holding_Return ~ F.Score_Grouping,data = Company_F_Score_Grouping_Only_Volume_Filtered)



#Hypothesis 4: - If Hypothesis 1 is null, the difference between High F-Score and Low F-Score group's market adjusted return across different industries = 0


t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Sector=="Technology"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Sector=="Transportation"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Sector=="Capital Goods"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Sector=="Consumer Services"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Sector=="Health Care"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Sector=="Miscellaneous"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Sector=="Public Utilities"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Sector=="Basic Industries"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Sector=="Energy"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Sector=="Consumer Non-Durables"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Sector=="Consumer Durables"))


ANOVA_Sector_Group <- aov(Holding_Return ~ Sector,data = Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry) # For generation of Summary Data only
Hypothesis4_Data<-HSD.test(ANOVA_Sector_Group, "Sector", group=TRUE) # For generation of Summary Data only
Hypothesis4_Data


  #Hypothesis 4 Plots

ScatterPlot_Base_Sector <-ggplot (Company_F_Score_Returns_Volume_Filtered_PE_Industry,aes(x=F.Score,y=Holding_Return,color=Sector)) +geom_point()+facet_grid(~Sector) +stat_summary(fun.y=mean,geom="line",lwd=2,aes(group=1))
ScatterPlot_Group_Sector <-ggplot (Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,aes(x=F.Score_Grouping,y=Holding_Return,color=Sector)) +geom_point()+facet_grid(~Sector) +stat_summary(fun.y=mean,geom="line",lwd=2,aes(group=1)) +scale_x_discrete(labels=c("High","Low"))
ScatterPlot_Group_Sector
ScatterPlot_Base_Sector


Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry_Split1 <- subset(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry, Sector=="Basic Industries"|Sector=="Public Utilities"|Sector=="Basic Industries" |Sector=="Consumer Non-Durables"|Sector=="Energy")
Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry_Split2 <- subset(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry, Sector=="Health Care"|Sector=="Miscellaneous"|Sector=="Capital Goods"|Sector=="Technology"|Sector=="Transportation"|Sector=="Consumer Services"|Sector=="Consumer Durables")
ScatterPlot_Group_Sector_Part1 <-ggplot (Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry_Split1,aes(x=F.Score_Grouping,y=Holding_Return,color=Sector)) +geom_point()+geom_jitter()+facet_grid(~Sector) +stat_summary(fun.y=mean,geom="line",lwd=2,aes(group=1)) +scale_x_discrete(labels=c("High","Low")) + scale_color_manual(values=wes_palette(n=5, name="Rushmore"))
ScatterPlot_Group_Sector_Part2 <-ggplot (Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry_Split2,aes(x=F.Score_Grouping,y=Holding_Return,color=Sector)) +geom_point()+geom_jitter()+facet_grid(~Sector) +stat_summary(fun.y=mean,geom="line",lwd=2,aes(group=1)) +scale_x_discrete(labels=c("High","Low")) 

ScatterPlot_Group_Sector_Part1 
ScatterPlot_Group_Sector_Part2



#Hypothesis 5: - If Hypothesis 1 is null, all 3 groups to tabulate F-Score are significant independent factors in assessing returns

t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Year=="Y2009"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Year=="Y2010"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Year=="Y2011"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Year=="Y2012"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Year=="Y2013"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Year=="Y2014"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Year=="Y2015"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Year=="Y2016"))
t.test(Holding_Return ~ F.Score_Grouping,data =filter(Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,Year=="Y2017"))

ANOVA_Year_Group <- aov(Holding_Return ~ Year,data = Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry) # For generation of Summary Data only
summary(ANOVA_Year_Group) # For generation of Summary Data only
Hypothesis5_Data <- HSD.test(ANOVA_Year_Group, "Year", group=TRUE) # For generation of Summary Data only
Hypothesis5_Data

  #Hypothesis 5 Plots

ScatterPlot_Group_Year <-ggplot (Company_F_Score_Grouping_Only_Volume_Filtered_PE_Industry,aes(x=F.Score_Grouping,y=Holding_Return,color=Year)) +geom_point()+facet_grid(~Year) +geom_jitter()+stat_summary(fun.y=mean,geom="line",lwd=2,aes(group=1)) +scale_x_discrete(labels=c("High","Low"))

ScatterPlot_Group_Year




