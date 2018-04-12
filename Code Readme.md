Please refer to the file "Codes" for the R.Codes

The Codes can be split into 3 main portions. Please refer to the flowchart below for more information on the flow of the data.

![](https://github.com/kohtseyoung/F-Score-Capstone-Project/blob/master/Images/Research%20Layout%20(Modified).png)


1.) Data Collection
-------------------

#### Extractions from MorningStar for the following financial data

Company BalanceSheet.R -> companylist(BS Final).csv

Company CashFlow.R -> companylist(CF Final).csv

Company IncomeStatement.R -> companylist(IS Final).csv

Company KeyRatio.R -> companylist(KR Final).csv

2.) Data Wrangling 
----------------------------------------------------

#### Extraction and Computation from Yahoo Finance for Shares Holding Return
getSymbols.R -> Company_Holding_Quaterly_Return_Final.csv

#### Computation and Tabulation of F-Indicators

Company F-Indicators.R - > F_Score_Full_Final.csv, Holding_Period_Matrix.csv

#### Computation of Company Trading Volume
Company Volume.R  -> Company_Volume_Low_Trades.csv, Company_PE_Ratio

3.) Data Analysis
-------------------

#### Ploting and Statistical Testing based on F-Score and Industry

F-Score Analysis.R -> PE_Industry_Classes_Filtered.csv, FScore_Group.csv
