Milestone Report
================
Introduction
------------
This study seeks to examine the usefulness of Joseph D. Piotroski’s F-Score which uses historical financial statement information to identify companies with superior performance as well as deploy it as a foundational base where other inputs can be modified to further improve on the original’s effectiveness.

Literature Review and Research's Motivation
-----------------
The F-score is a widely known performance/investment strategy due to its simplicity and accessibility. It uses public published information readily available for investor to separate strong performers from weak performers among high book-to-market firms. A score is computed based on 9 criteria categorized into 3 different groups and are summarized as follows:



|	|Profitability|
|:---:|:---:|
|1|Return on Assets	1 point if it is positive in the current year, 0 otherwise|
|2|	Operating Cash Flow	1 point if it is positive in the current year, 0 otherwise|
|3|	Change in Return of Assets (ROA)	1 point if ROA is higher in the current year compared to the previous one, 0 otherwise)|
|4|Accruals	1 point if Operating Cash Flow/Total Assets is higher than ROA in the current year, 0 otherwise|

| |Leverage, Liquidity and Source of Funds|
|:---:|:---:|
|5|	Change in Leverage (long-term) ratio	1 point if the ratio is lower this year compared to the previous one, 0 otherwise|
|6|	Change in Current ratio	1 point if it is higher in the current year compared to the previous one, 0 otherwise
|7|	Change in the number of shares	1 point if no new shares were issued during the last year

| |Operating Efficiency|
|:---:|:---:|
|8|	Change in Gross Margin	1 point if it is higher in the current year compared to the previous one, 0 otherwise
|9	|Change in Asset Turnover ratio	1 point if it is higher in the current year compared to the previous one, 0 otherwise

**Relevancy of F-Score in Today’s Economy**

The empirical study of the F-Score is based on companies selected from 1976 and 1996. Since then, in the past 20 years, the overall economy landscape has experience massive seismic shifts; from multiple recessions (Dot.com bubble burst from 1997-2001, The Great Recession from 2007-2012) to the explosive growth of tech-based companies and slow-decline of manufacturing firms. Besides, there have been monumental changes to organization structure of companies, many opting for a more flat-based structure rather than a hierarchical one to cope with the ever-changing economy. This has in turn impact on how companies’ resources and finance are managed. 

While the aforementioned events may undermine the credibility of the F-Score, amongst the many changes, stronger financial regulations and corporate governance have been consistently implemented across the decades, aiming for higher financial reporting transparency and accountability. These developments have strengthened F-Score’s core idea of utilizing historical financial performance data to predict strong performers.  To name one, the enactment of the Sarbanes-Oxley Act also known the “Public Company Accounting Reform and Investor Protection Act in response to the financial scandal revolving Enron mandates strict reforms to improve financial disclosures from  corporations from possibly fraudulent activities. With better financial reporting, financial data provided from companies can be used more reliably for analytic purposes.

In conclusion, while recent economy changes may downplay F-Score’s usefulness, the stringent financial disclosures required from companies serves as an overarching foundation for F-Score due to its heavy reliance on such data. These circumstances serve as a strong motivation to reassess the credibility of the F-Score and modify the indicators accordingly if needed to suit the stance of today’s economy. 

Research Overview
-----------------
The overall research process is summarized in the flowchart as follows:

![](https://github.com/kohtseyoung/F-Score-Capstone-Project/blob/master/Images/Research%20Layout.png)

As seen from the table above, the main three data sources are from NASDAQ, Morning Star and Yahoo Finance. From these three data sources, the raw data files are company list, income statement, balance sheet, cash flow statement, key ratio share price and industry.  The raw data after processed by R are grouped into the following: F-Score Indicators, Grouping (High/Low) and Holding Return. Lastly, based on the results from the analysis of the processed data, it will provide evidence on whether following the hypotheses raised (Please refer to *Research Analysis* Section) can be confirm or falsify.

To organize the research process and its findings, the report is split into the following sections to further elaborate on each topic area:-

1.	Data Collection
2.	Data Wrangling
3.	Research Analysis

Data Collection
---------------
### Financial Information

The sample population of companies used is based from NASDAQ. There is a list of companies listed on NASDAQ available for download in excel format. The total number in the original list is 3288. For completeness of data, companies with insufficient information (etc. Market Cap = 0, no Sector/Industry information) are filtered out. Besides, we also removed companies that belong to the finance sector due to the uniqueness of its financial reporting standard for consistency purposes. The final sample population numbered to 2053.

For financial information that is required for F-Score testing, MorningStar is used due to the consistency in formatting of the relevant financial information. Besides, the extraction of information is relatively straightforward due to the structured format of its web links. The information extracted per company includes 4 data sets: “Income Statement”, “Balance Sheet”, “Cash Flow Statement” and “Key Ratios”. 

Due to the large number of companies’ information extracted, the function “try” and “sys.sleep” is used to ensure that all information is captured. Besides, a second run of extraction is performed if there are unusually large amount of missing values noted.

### Share Prices and Holding Returns

The R Package “Quantmod”, specifically its’ function “getSymbols” was used to extract companies’ share prices and holding returns from Yahoo Finance based on their respective symbols/tickers. Companies’ information are stored as objects named after companies’ symbols. Similarly to the extraction of information from MorningStar, some companies’ information are not available. The matter is resolved by creating a new environment to store these newly created objects. Under the assumption that no objects will be created if no data are available from Yahoo Finance, the function “ls” is used to list out the objects created in the new environment, and subsequently compared against the original company list used to identify companies with missing data. 
Instead of computing the returns from the share prices, the function “yearlyReturn” is used to extract the annual returns of each company. Furthermore, the function “adjustOHLC” is used in conjunction to readjust returns on share price based on stock-splits or reverse stock-splits during the period. 

## Data Collection Limitation & Biases

Data Collection is limited by broken links or unavailable data from each data source. 8 companies have been removed from the sample population due to no Financial Information available at MorningStar. 

Data Wrangling
--------------
### 1. Financial Information
The information from income statement, balance sheet and cash flow statement consist of 5 year data, while key ratio consists of 10 years of data. As a business cycle tend to be around 10 years, using 10 years’ worth of financial data would be helpful to show the usefulness of the model across an entire business cycle. It should be noted that 10 years’ worth of data would be computed into a 9 year series as some indicators rely on changes between two years, as such, the 1st year data cannot be computed solely by itself.

**F-Score Indicators**

Before diving head-in into the extraction of the respective indicators, only relevant and required information required for computational purposes are initially pulled from the respective financial data and stored as objects under R. This includes the following:  Net Income, Year End Assets, ROA, Operating Cash Flow, Long-term Debt Ratio, Current Ratio, Number of Shares, Gross Profit Margin and Sales.

**_Indicator 1 – Return on Assets (ROA)_**

ROA under F-Score and the ROA under Morning Star are computed differently. The former uses beginning year-end assets, while the latter using average assets as the denominator for computing ROA. F-Score and MorningStar’s ROA shall be coined as ROA(Beginning Assets) and ROA(Average Assets) respectively. As such, we are to re-compute the 1st 5 years ROA using information from the income statement and balance sheet data. For the last 5 years, the beginning of the year assets is recomputed based on the MorningStar’s ROA extracted from the key ratio statement. The formulas are as follows:

>Beginning Year Assets = Current Year Net Income/ROA(Average Assets) – Year End Assets

>ROA (Beginning Assets) = Current Year Net Income/ Beginning Year Assets

**_Indicator 2 – Operating Cash Flow (CFO)_**

Operating Cash Flow numerator data is obtained from the key ratio statement, which is subsequently divided by Beginning Year Assets as computed under the above section.

**_Indicator 3 – Change in ROA_**

Change in ROA is the difference between current year and past year ROA based on the computed ROA.  

**_Indicator 4 – Accruals_**

The difference between CFO and ROA as previously computed.


**_Indicator 5 – Change in Leverage (Long-Term) Ratio_**

There is a difference between the denominator of the leverage ratio between F-Score and MorningStar, the former being the average assets during the year, the latter being year-end assets.
The respective leverages shall be coined as Leverage (Average) and Leverage (Year-end) respectively.
The latest 5 years long-term debt is extracted from MorningStar’s balance sheet while the earliest 5 years are derived from the Leverage (Year-end) extracted from the key ratio statement. The formulas are as follows:

>Leverage (Average) = Leverage (Year-end)* Year-end Assets /100*Average Assets

>Change in Leverage is subsequently the difference between current year and past year Leverage (Average).

**_Indicator 6 – Change in Current Ratio_**

Current Ratio data is extracted from key ratio statement.  Change in current ratio is the difference between current year and past year current ratio

**_Indicator 7 – Change in Shares_**

Number of Shares data is extracted from key ratio statement.  Change in Shares is the difference between current year and past year Number of Shares.

**_Indicator 8 – Change in Gross Profit Margin_**

Gross profit margin data is extracted from key ratio statement.  Change in gross profit margin is the difference between current year and past year gross profit margin.

**_Indicator 9 – Change in Asset Turnover_**

Asset turnover is defined as total sales scaled by Beginning Year Assets. Sales are extracted from key ratio statement while Beginning Year Assets as computed as per above.  Change in asset turnover is the difference between current year and past year asset turnover.

### 2. Financial Fiscal Date Period

Having selected companies from a list under NASDAQ in 2017/2018, some of these companies would not have been publicly listed for the entire past 10 years since 2008. Therefore, the holding period of these companies would be based on public listed date up till 2017 or its delisted date, whichever earlier. Such information is gathered based on two fronts, from the availability of financial data and trading share price on MorningStar and Yahoo Finance respectively. Only under the circumstance that information is available from both sources for the same period, F-Score can be computed.

**Availability of Financial Information from MorningStar**

First, the companies’ entire fiscal period is identified from the “Margin % Sales” row under key ratio statement as seen below:

![](https://github.com/kohtseyoung/F-Score-Capstone-Project/blob/master/Images/Margin%25Sales.png)
 
Under the assumption that a company is not publicly-listed based if the company does not have any shares/equity, Number of Shares is used as an indicator to identify the company’s start and end date. First, the total sum for each year column without shares (etc. NA) is tabulated. Second, this sum is added to the first date as per above to acquire the start date. There is no needed to take end date/delisting date into account as using the list of companies extracted from NASDAQ in 2017/2018 already nullify this potential issue. 

**Availability of Trading Share Price from Yahoo Finance**

The extraction of earlier trading share price dates is similar to the above from MorningStar, except that it deals with rows data instead of columns.  A function is written to identify the earliest non-NA row for each company. The row number is then used to identify the specific dates using the “inner_join” function.

**Setting Starting Date by comparing and combining both data sets**

As both data sets (Financial Information and Share Price) are required, the later of the two dates is used as the companies’ starting date for testing.  However, it should be noted that the starting date, if based on trading share price is minus by 1, as the financial information prior to the trading share price date can be used to assess for F-Score.  The formula in R would be as follows:

>if_else(Financial Information Date > Trading Share Price Date , Financial Information, Trading Share Price Date -1))

### 3. Share Price and Returns

Data wrangling for share price is quite straight forward as data pulled from quantmod are clear and easy to use. The yearly holding returns extracted from quantmod are subsequently filtered using the Financial Fiscal Date Period as computed above. 
Besides, quarterly returns were used to compute returns from Q2 to the following year Q1, in order to take into account for information time lag. However, this return is currently not in-use for the current testing as early preliminary testing shows that yearly returns are more accurate.

### 4. Combining F-Score and Holding Returns

Combining F-Score and Holding Returns is relatively straightforward with the R package “dplyr”. To ensure that the companies’ shares are readily available for trades, an average daily trading volume of 10,000 is set as a basis for trade liquidity requirement. Holding returns with 0% are further filtered out as they represent trades inactivity.  Besides, holding returns above 200% are filtered out as outliers.   

Additional information is added such as industry/sector groupings for each company for more detailed testing. 

