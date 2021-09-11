# Organization
Data housed in this folder are produced either manually or from our adaptations of proprietary data. Upon running the code in the `scripts` folder, this folder will be further populated with .csv and .dbf files used to produce the dataset. Here is a brief description of the pre-populated tables:

- `county_tracts.csv` - contains `STATE` and `COUNTY` names, a modified unique county ID (`JOIN_CO`), a 2010 tract unique ID (`GISJOIN`), and county-level housing units gathered from the [NHGIS](https://data2.nhgis.org/main) and [US Census Bureau](https://www.census.gov/prod/www/decennial.html) from 1940 to 1980 (`hu40co`, `hu50co`, `hu60co`, `hu70co`, `hu80co`). Housing units from 1940, 1970, and 1980 are all gathered from the NHGIS, whereas data from 1950 and 1960 are manually entered from the Census Bureau website. County or county-equivalents that changed borders during this timeframe are aggregated into a single county with matching `COUNTY` and `JOIN_CO` values, and their housing counts are summed.

- 

# Further Instructions
