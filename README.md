<h1>The Great “Transfer”-mation</h1>
<h2>How American Communities became reliant on income from government</h2>

***

This repository includes the data and necessary code to support EIG's analysis, The Great “Transfer”-mation. You can find the interactive webpage here [link](ADD LINK HERE) and the full draft here [link](ADD LINK HERE). 

All links are current at the time of publication.

Contact Benjamin Glasner with any questions at benjamin@eig.org.

***

<h2>DATA SOURCES</h2>

<h3>Income and population</h3>

The Bureau of Economic Analysis’s (BEA) [regional economic account files](https://apps.bea.gov/regional/downloadzip.htm) are the primary source of data for this report. These files cover a range of local socioeconomic data topics including local Gross Domestic Product, Personal Income, and Personal Consumption Expenditures for a range of locality sizes. We use two files for our county-level and national-level analysis:

<ol>
<li>CAINC4 - “Personal income and employment by major component by county.” This file provides information related to the components of income and population at the county, state, and national levels for 1969-2022. This file combines all transfer programs, so an additional source file is required.</li>
<li>CAINC35 - “Personal current transfer receipts.” This file provides a breakdown of transfer receipts from government and non-government sources at the county, state, and national levels for 1969-2022.</li>
</ol>


<h3>Metro status</h3>

County metropolitan classifications are derived from the NCHS’ [Rural-Urban Continuum Codes for 2023](https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx). The categories used in this report are simplified from the NCHS’s scheme according to the method outlined in the “GEOGRAPHY AND TIME VARS” section below.


<h3>Old-age population</h3>

Supplementary data for the old-age population, defined as individuals 65 years of age and older, comes from the Census Bureau’s County Intercensal Tables Program. These tables provide annual county population estimates by age, sex, and race. Files are published separately by year. We rely on the following files: [1970-1979](https://www.census.gov/data/tables/time-series/demo/popest/pre-1980-county.html), [1980-1989](https://www.census.gov/data/tables/time-series/demo/popest/1980s-county.html) 
, [1990-1999](https://www.census.gov/data/tables/time-series/demo/popest/1990s-county.html), [2000-2009](https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-counties.html) 
, [2010-2019](https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html), and [2020-2022](https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-detail.html).



<h3>Poverty rates</h3>

Poverty rates come from the Census Bureau Small Area Income and Poverty Estimates program, and are available at the decade marks. [link](https://www.census.gov/data/tables/time-series/dec/census-poverty.html)


***

<h2>GEOGRAPHY AND TIME VARS</h2>

We perform the analysis using both national and county-level data. The “County FIPS Code” is only available for the county-level data file.

“County FIPS Code”: FIPS code. 
Note: Connecticut’s 2022 boundary changes are not reflected by the Bureau of Economic Analysis, and 2022 values for Connecticut reflect the historical codes.

“County, State”: Name of the county in {County}, {State} format where {State} is the two-letter abbreviation. 

“year”: Year 

“Metro status”: Either large metro (population 1 million +; NCHS RUCC code = 1), medium metro (population 250k-1 million; NCHS RUCC code = 2), small metro (<250K; NCHS RUCC code = 3), or non-metro (NCHS RUCC codes 4 through 9). Codes 4-9 cover counties with few people living in urban areas, including counties both adjacent and not adjacent to metro areas. See the NCHS website linked above for further information.

***

<h2>POPULATION VARS</h2>

“population”: Total population ( BEA CAINC4, line code 20)

“old age share”: Share of the geographic area’s population over 65, estimated from the Census Bureau’s County Intercensal Tables Program.

***

<h2>INCOME SUMMARY VARIABLES</h2>

“total income from all sources (2022 USD)”: total personal income (BEA CAINC4, line code 10). This includes:
<ol>
<li>Wages and salaries from all industries (CAINC4 line code 50) </li>
<li>Supplements to wages and salaries, covering employer payments made on behalf of employees such as employee pension and insurance funds as well as employer contributions for government social insurance. (CAINC4 line code 60) </li>
<li>Proprietors’ income for both farm and non-farm sole proprietorships, partnerships, and tax-exempt cooperatives. Excludes dividends, money interest from non-financial businesses, and rental income received by individuals not engaged in real estate (CAINC4 line code 70) </li>
<li>Rental income, dividends, interests, and rents (CAINC4 line code 46) </li>
<li>Transfer receipts from the government, non-for-profits, and businesses. (CAINC4 line code 47) </li>
</ol>

“per capita total income from all sources (2022 USD)”: total personal income (BEA CAINC4, line code 10) divided by geographic area population (BEA CAINC4, line code 20)

“government transfers (2022 USD)”: CAINC35 line code 1000. This includes:
<ol>
<li>Retirement and disability insurance benefits (CAINC35 line code 2000), which includes both Social Security (CAINC35 line code 2110), as well as old-age disability insurance. </li>
<li>Medical benefits (CAINC35 line code 2200), which includes Medicare (line code 22210), Medicaid (line code 2220), and military medical insurance benefits (line code 2230) </li>
<li>Income maintenance benefits (CAINC35 line code 2300), which includes Supplemental Security Income (line code 2310), Earned Income Tax Credit (EITC) (line code 2320), SUpplemental Nutrition Assistance Program (SNAP) (line code 2330), and other temporary programs, such as some COVID relief programs. </li>
<li>Unemployment insurance compensation (CAINC35 line code 2400), including state unemployment insurance compensation (2410) and most temporary pandemic relief programs. </li>
<li>Veterans’ benefits (CAINC35 line code 2500), including pension and disability. </li>
<li>Education and training assistance (CAINC35 line code 2600) </li>
<li>Other uncategorized (CAINC35 line code 2700) </li>
</ol>

“per capita government transfers (2022 USD)”: total government transfers (CAINC35 line code 1000) divided by geographic area population (BEA CAINC4, line code 20)


“total income excluding government transfers (2022 USD)”: all categories of total income apart from government transfers. This includes:
<ol>
<li>Wages and salaries from all industries (CAINC4 line code 50) </li>
<li>Supplements to wages and salaries, covering employer payments made on behalf of employees such as employee pension and insurance funds as well as employer contributions for government social insurance. (CAINC4 line code 60) </li>
<li>Proprietors’ income for both farm and non-farm sole proprietorships, partnerships, and tax-exempt cooperatives. Excludes dividends, money interest from non-financial businesses, and rental income received by individuals not engaged in real estate (CAINC4 line code 70) </li>
<li>Rental income, dividends, interests, and rents (CAINC4 line code 46) </li>
<li>non-for-profits, and businesses. (CAINC4 line code 47 less of CAINC35 line code 1000) </li>
</ol>

“per capita total income excluding government transfers (2022 USD)”: this is all non-transfer income as enumerated above divided by geographic area population (BEA CAINC4, line code 20)

“government transfer share of total income”: this “government transfers (2022 USD)” / “total income from all sources (2022 USD)”. Reported as a percentage.


***

<h2>TRANSFER SUB-CATEGORIES</h2>

For simplicity, we do not report all sub-categories. Please see the BEA’s published files (linked above), or our expanded dataset available in the \data\ folder.

“social security transfers (2022 USD)”: all social security transfers, excluding disability insurance benefits. (CAINC35 line code 2110)

“medicare transfers (2022 USD)”: (CAINC35 line code 2110)

“medicaid transfers (2022 USD)”: (CAINC35 line code 2220)

“income maintenance (2022 USD)”: (CAINC35 line code 2300) 

“other transfers (2022 USD)””: (CAINC35 line code 1000) less of CAINC35 line codes 2110, 2110, 2220, 2300.


***

<h3>ADDITIONAL NOTES ON THE COMPONENTS OF GOVERNMENT TRANSFERS</h3>

We follow the BEA’s transfer income categorizations throughout this report, which covers all "receipts of persons from government and business for which no current services are performed. Current transfer receipts from government include Social Security benefits, medical benefits, veterans' benefits, and unemployment insurance benefits. Current transfer receipts from business include liability payments for personal injury and corporate gifts to nonprofit institutions."

To view the detailed BEA discussion on concepts and methods to identity the categorization and inclusion of specific programs, please see the Bureau of Economic Analysis State Personal Income and Employment: Concepts and Methods document [link](https://www.bea.gov/system/files/methodologies/SPI-Methodology.pdf), page 59 under the section Current Transfer Receipts of Individuals from Governments.
