Data Collection
---------------
### Financial Information

The sample population of companies used is based on NASDAQ. There is a list of companies listed on NASDAQ available for download in excel format. The total number in the original list is 3288. For completeness of data, companies with insufficient information (etc. Market Cap = 0, no Sector/Industry information) are filtered out. Besides, we also removed companies that belong to the finance sector due to the uniqueness of its financial reporting standard for consistency purposes. The final sample population numbered to 2053.

For financial information that is required for F-Score testing, Morningstar is used due to the consistency in the formatting of the relevant financial information. Besides, the extraction of information is relatively straightforward due to the structured format of its web links. The information extracted per company includes 4 data sets: “Income Statement”, “Balance Sheet”, “Cash Flow Statement” and “Key Ratios”. 

### Share Prices 
The R Package “Quantmod” was used to extract companies’ share prices and holding returns from Yahoo Finance based on their respective symbols/tickers. Similarly to the extraction of information from Morningstar, some companies’ information is not available. These companies are removed from the testing population.



### Data Collection Limitation & Biases

Data Collection is limited by broken links or unavailable data from each data source. 11 companies have been removed from the sample population due to no financial Information (8 companies) and no share price data (3 companies) available respectively from Morningstar and Yahoo Finance.

As the companies tested are those listed on NASDAQ in 2017 for the period 2008-2017, there will be a significant survivorship bias as companies with the low performance that are subsequently delisted from the period 2008-2017 would not be part of the testing population. This would inevitably overstate the returns of low F-Score companies.

Lastly, some financial data collected are rounded up to the nearest million. This may pose as an issue for identifying and analyzing activities of companies with the smaller market cap. 

Data Wrangling
--------------
### 1. Financial Information
The information from the income statement, balance sheet, and cash flow statement consist of 5-year data, while key ratio consists of 10 years of data. As a business cycle tend to be around 10 years, using 10 years’ worth of financial data would be helpful to show the usefulness of the model across an entire business cycle. It should be noted that 10 years’ worth of data would be computed into a 9 year series as some indicators rely on changes between two years, as such, the 1st year data cannot be computed solely by itself.

**F-Score Indicators**

Before the extraction of the respective indicators, only relevant and required information required for computational purposes are initially pulled from the respective financial data and stored as objects under R. This includes the following:  Net Income, Year End Assets, ROA, Operating Cash Flow, Long-term Debt Ratio, Current Ratio, Number of Shares, Gross Profit Margin, and Sales.

**_Indicator 1 – Return on Assets (ROA)_**

ROA under F-Score and the ROA under Morningstar are computed differently. The former uses beginning year-end assets, while the latter using average assets as the denominator for computing ROA. F-Score and Morningstar’s ROA shall be coined as ROA(Beginning Assets) and ROA(Average Assets) respectively. As such, we are to re-compute the 1st 5 years ROA using information from the income statement and balance sheet data. For the last 5 years, the beginning of the year assets is recomputed based on the Morningstar’s ROA extracted from the key ratio statement. The formulas are as follows:

>Beginning Year Assets = Current Year Net Income/ROA(Average Assets) – Year End Assets

>ROA (Beginning Assets) = Current Year Net Income/ Beginning Year Assets

**_Indicator 2 – Operating Cash Flow (CFO)_**

Operating Cash Flow numerator data is obtained from the key ratio statement, which is subsequently divided by Beginning Year Assets as computed under the above section.

**_Indicator 3 – Change in ROA_**

Change in ROA is the difference between the current year and past year ROA based on the computed ROA.  

**_Indicator 4 – Accruals_**

The difference between CFO and ROA as previously computed.


**_Indicator 5 – Change in Leverage (Long-Term) Ratio_**

There is a difference between the denominator of the leverage ratio between F-Score and Morningstar, the former being the average assets during the year, the latter being year-end assets.
The respective leverages shall be coined as Leverage (Average) and Leverage (Year-end) respectively.
The latest 5 years long-term debt is extracted from Morningstar’s balance sheet while the earliest 5 years are derived from the Leverage (Year-end) extracted from the key ratio statement. The formulas are as follows:

>Leverage (Average) = Leverage (Year-end)* Year-end Assets /100*Average Assets

>Change in Leverage is subsequently the difference between the current year and past year Leverage (Average).

**_Indicator 6 – Change in Current Ratio_**

Current Ratio data is extracted from key ratio statement.  Change in current ratio is the difference between current year and past year current ratio

**_Indicator 7 – Change in Shares_**

Number of Shares data is extracted from key ratio statement.  Change in Shares is the difference between current year and past year Number of Shares.

**_Indicator 8 – Change in Gross Profit Margin_**

Gross profit margin data is extracted from key ratio statement.  Change in gross profit margin is the difference between current year and past year gross profit margin.

**_Indicator 9 – Change in Asset Turnover_**

Asset turnover is defined as total sales scaled by Beginning Year Assets. Sales are extracted from key ratio statement while Beginning Year Assets as computed as per above.  Change in asset turnover is the difference between current year and past year asset turnover.

**Tabulating F-Scores**

As noted under the literature review, instead of adhering to the original score groupings, High F-Score group consists of scores from 7-9 while Low F-Score group consists of score from 1-3.


### 2. Financial Fiscal Date Period

Having selected companies from a list under NASDAQ in 2017/2018, some of these companies would not have been publicly listed for the entire past 10 years since 2008. Therefore, the holding period of these companies would be based on public listed date up till 2017 or its delisted date, whichever earlier. Such information is gathered based on two fronts, from the availability of financial data and trading share price on Morningstar and Yahoo Finance respectively. Only under the circumstance that information is available from both sources for the same period, F-Score can be computed.

**Availability of Financial Information from Morningstar**


Under the assumption that a company is not publicly-listed based if the company does not have any shares/equity, shares are used as an indicator to identify the company’s start and end date. There is no needed to take end date/delisting date into account as using the list of companies extracted from NASDAQ in 2017/2018 already nullify this potential issue. 

**Availability of Trading Share Price from Yahoo Finance**

Based on the extraction of share price from Yahoo Finance, the earliest trading date can be identified by the earliest available share price information.

**Setting holding period by comparing and combining both data sets**

As both data sets (Financial Information and Share Price) are required, the later of the two dates is used as the companies’ starting date for testing.  However, it should be noted that the starting date based on trading share price is minus by 1, as the financial information prior to the trading share price date can be used to assess for F-Score.  

### 3. Share Price Returns

For the computation of share price returns, quarterly returns are computed initially as stand-alone variables, and subsequently combined as yearly returns, take into account companies' different fiscal year-end period. Returns are computed from the beginning of the fourth month after the firm's fiscal year-end to ensure that the annual financial statement is available by then. Furthermore, the returns on share price based on stock-splits or reverse stock-splits are readjusted to ensure that returns are not inflated/deflated during the relevant testing period. 

### 4. Filtering of outliers 

Companies with 0 F-Scores are filtered out due to the low number (22) of such companies,  which may cause the violation of the normal distribution assumption for statistical testing. Besides, holding returns above 200% are filtered out as outliers. Companies with daily trading volumes below 10,000 shares are also excluded from testing to take into account the trading liquidity of tested companies. 
    
Additional information is also added such as industry/sector groupings for each company for more detailed testing. 
