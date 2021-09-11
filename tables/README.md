# Organization
Data housed in this folder are produced either manually or from our adaptations of proprietary data. Upon running the code in the `scripts` folder, this folder will be further populated with .csv and .dbf files used to produce the dataset. Here is a brief description of the pre-populated tables:

- `county_tracts.csv` - contains `STATE` and `COUNTY` names, a modified unique county ID (`JOIN_CO`), a 2010 tract unique ID (`GISJOIN`), and county-level housing units gathered from the [NHGIS](https://data2.nhgis.org/main) and [US Census Bureau](https://www.census.gov/prod/www/decennial.html) from 1940 to 1980 (`hu40co`, `hu50co`, `hu60co`, `hu70co`, `hu80co`). Housing units from 1940, 1970, and 1980 were all gathered from the NHGIS, whereas data from 1950 and 1960 were manually entered from the Census Bureau website. County or county-equivalents that changed borders during this timeframe were aggregated into a single county with matching `COUNTY` and `JOIN_CO` values, and their housing counts are summed.

- `golf_table.dbf` - contains a unique `OBJECTID`, golf course `NAME`, and estimated `YEAR` of opening. This data was gathered in a much fuller, geocoded format via private correspondence. With the `OBJECTID` identifier, this table is joined to a golf course polygon within the scripts.

- `t2010_2019_xwalk.csv` - contains the NHGIS unique idnetifiers (`GISJOIN10`, `GISJOIN19`) for the 24 tracts in the continental U.S. that changed FIPS codes between 2010 and 2019.

- `urb_renewal_tracts.dbf` - contains the 1,990 tracts that intersect with one of the over 700 urban renewal projects recorded by the Digital Scholarship Lab's [Renewing Inequality](https://dsl.richmond.edu/panorama/renewal/#view=0/0/1&viz=cartogram) project. The data columns include a unique tract identifier (`GISJOIN`) and the percent of the tract's area overlapped by an urban renewal project (`p_urb_ren`).

# Further Instructions

