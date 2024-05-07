# ECON5253-Final-Project
This project investigates potential impacts of tornadoes


This research combines data from several sources: 

 [1] Tornado Information: <https://www.spc.noaa.gov/wcm/>
 
 [2] Race and Gender: <https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-county-detail.html>
 
 [3] Population: <https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-total.html>
 
 [4] Unemployment: <https://www.ers.usda.gov/data-products/county-level-data-sets/county-level-data-sets-download-data/>
 
 [5] County-level Presidential Election: <https://dataverse.harvard.edu/file.xhtml?fileId=4788675&version=11.0>
 
 [6] Housing Price Data provided by Riley Wilson <riley_wilson@byu.edu>
 
 [7] FHFA House Price Index: <https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index.aspx>
 
 [8] Weekly Wage and Employment: <https://www.bls.gov/cew/downloadable-data-files.htm#naics-based>
 

Data Explained:

- 1950-2021_all_tornadoes: This dataset includes information for tornadoes, which is from data source [1]
  
- all_industry: This dataset describes the employment and wage information, obtained from data source [8]
  
- election: This dataset contains information of Republican election on the county level, note that I utilized interpolation method to generate this data, originated from data source [5]
  
- Median_Housing: This dataset combines median housing price (see [6]) combined with data [7], for details, see Wilson's paper: <https://research.upjohn.org/cgi/viewcontent.cgi?article=1038&context=up_policybriefs>
  
- soi_pop: This dataset describe population information of origins and destinations, for gravity model use, data from [2] and [3]
  
- unemp.1: Contains employment and labor force, generated using data source [4]

Code Explained:

- Final Project (1) generates results for migration, social outcomes, industry. This file contains OLS, gravity model for migration, social outcomes, and industry outcomes, as well as event study for migration and social outcome variables.

- Final Project (2) solely contains event study for industry.

- Follow codes in Table file, one can replicate the tables in the paper.




