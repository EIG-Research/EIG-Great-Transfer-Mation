This file includes the data and analysis to support EIG's Great Transfer-Mation analysis.


############################################

**DATA SOURCES**

Bureau of Economic Analysis (BEA) CAIN4 1969-2022 and CAINC35 1969-2022 files
	for all transfers, income, total population variables
	https://apps.bea.gov/regional/downloadzip.cfm

BLS QCEW county-MSA crosswalk
	MSA definitions for counties
	https://www.bls.gov/cew/classifications/areas/county-msa-csa-crosswalk.htm

Census inter-census population estimates by age
	for estimating old-age population share
	1970-1979 https://www.census.gov/data/tables/time-series/demo/popest/pre-1980-county.html
	1980-1989 https://www.census.gov/data/tables/time-series/demo/popest/1980s-county.html
	1990-1999 https://www.census.gov/data/tables/time-series/demo/popest/1990s-county.html
	2000-2009 https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-counties.html
	
ACS 5-year samples
	2009-2022 population estimates by age for estimating old-age population share
	https://usa.ipums.org/usa/

############################################

**GEOGRAPHY VARS**

**County FIPS Code**
FIPS code.
Note: Connecticut’s 2022 boundary changes are not reflected by the Bureau of Economic Analysis, and 2022 values for Connecticut reflect the historical codes.

**County, State**
Name of the county in {County}, {State} format.

**year**
Year

############################################

**POPULATION VARS**

**population**
Total population, as reported by the BEA

**old age share**
Share of geographic area’s population over 65, as reported by the Census Bureau

############################################

**INCOME SUMMARY VARIABLES**

**total income from all sources**
(2022 USD)
This category includes:
	Wages and salaries from all industries including government workers
	Supplements to wages and salaries covering employer pensions, insurance funds, and social insurance contributions
	Proprietors income including inventory valuation for both farm and non-farm
	Rental income, dividends, interests and rents
	Transfer receipts from the government
	Transfer receipts from non for profits and businesses (employees compensation)


**per capita total income from all sources**
(2022 USD)				
Total income per capita, where income per capita is defined as above

**government transfers**
(2022 USD)
total transfers from government sources, including:
	Retirement and disability insurance benefits including social security
	Medical benefits including medicare, medicaid, and military medical insurance
	Income maintenance benefits including SSI, EITC, SNAP, and others
	Unemployment insurance compensation, including most pandemic relief programs
	Veterans’ benefits
	Education and training assistance
	Other, including some pandemic relief programs.

**per capita government transfers** 
(2022 USD)
per capita total transfers from all government sources enumerated above

**total income excluding government transfers** 
(2022 USD)
total income - government transfers
This includes:
Wages and salaries from all industries including government workers
	Supplements to wages and salaries covering employer pensions, insurance funds, and social insurance contributions
	Proprietors income including inventory valuation for both farm and non-farm
	Rental income, dividends, interests and rents
	Transfer receipts from non for profits and businesses (employees compensation)


**per capita total income excluding government transfers**
(2022 USD)
per capita total income - per capita government transfers

**government transfer as a share of total income**
government transfers / total income, reported as a percentage

############################################

**TRANSFER SUB-CATEGORIES**

**Social security transfers**
(2022 USD)
all social security transfers. This category does not include disability insurance benefits

**medicare transfers**
(2022 USD)
all Medicare transfers

**medicaid transfers**
(2022 USD)
all Medicaid transfers

**income maintenance**
(2022 USD)
All income maintenance transfers, includes SSI, EITC, SNAP programs and other targeted programs

**other transfers**
(2022 USD)
All other transfers not otherwise categorized. This includes:
Unemployment insurance
Disability insurance
Veterans’ benefits
Military medical insurance
Education and training assistance
Other sources not categorized by the BEA. These include some pandemic relief programs
