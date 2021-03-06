# Organization
Data housed in this folder are produced either manually or from our adaptations of proprietary data. Upon running the code in the `scripts` folder, this folder will be further populated with .csv and .dbf files used to produce the dataset. Here is a brief description of the pre-populated tables:

- `airports_yr.dbf` - This table was produced from airport data provided by the Federal Aviation Administration (FAA) in script `07_pull_in_airport_data.R` in the original run of this code in June, 2021. However, the FAA site has since been replaced, so the link in the (now commented-out) script no longer works. If users wish to rerun the code with the most up-to-date FAA file, they can download that data [here](https://adip.faa.gov/agis/public/#/airportSearch/advanced) by clicking the **Download All Data** button. Users can then save that file in the `tables` folder on their local machine under the name *faa_airports_data_new.csv*.

- `county_tracts.csv` - contains `STATE` and `COUNTY` names, a modified unique county ID (`JOIN_CO`), a 2010 tract unique ID (`GISJOIN`), and county-level housing units gathered from the [NHGIS](https://data2.nhgis.org/main) and [US Census Bureau](https://www.census.gov/prod/www/decennial.html) from 1940 to 1980 (`hu40co`, `hu50co`, `hu60co`, `hu70co`, `hu80co`). Housing units from 1940, 1970, and 1980 were gathered from the NHGIS, whereas data from 1950 and 1960 were manually entered by referencing pdf tables from the Census Bureau website. County or county-equivalents that changed borders between 1940 and 2010 were aggregated into a single county with matching `COUNTY` and `JOIN_CO` values, and their housing counts were summed.

- `golf_table.dbf` - contains a unique `OBJECTID`, golf course `NAME`, and estimated `YEAR` of opening. This data was gathered in a much fuller, geocoded format via private correspondence. With the `OBJECTID` identifier, this table is joined to a golf course polygon within the scripts.

- `t2010_2019_xwalk.csv` - contains the NHGIS unique identifiers (`GISJOIN10`, `GISJOIN19`) for the 24 tracts in the continental U.S. that changed FIPS codes between 2010 and 2019.

- `urb_renewal_tracts.dbf` - contains information for the 1,990 tracts that intersect one or more of the hundreds of urban renewal projects recorded by the Digital Scholarship Lab's [Renewing Inequality](https://dsl.richmond.edu/panorama/renewal/#view=0/0/1&viz=cartogram) project. The data columns include a unique tract identifier (`GISJOIN`) and the percent of the tract's area overlapped by an urban renewal project (`p_urb_ren`).

# Further Instructions
For the scripts to run properly, users *must* manually download two zipped files from the NHGIS. These include a *1990-block-group-part-to-2010-block-group crosswalk* file and their *2010 NLCD environmental summaries (time varies by column)* file, which are not yet available through their API. Users should click the links below to download these zip files. To do so, they will need to create a [free account with the NHGIS](https://uma.pop.umn.edu/nhgis/user/new). Users will then need to save these files, keeping them zipped and their names as they are, in this folder.

- [`nhgis_bgp1990_bg2010.zip`](https://data2.nhgis.org/crosswalks/nhgis_bgp1990_bg2010.zip) - 1990-block-group-part-to-2010-block-group crosswalk

- [`us_tract_2010_nlcd_timevariesbycolumn.zip`](https://data2.nhgis.org/environmental/us_tract_2010_nlcd_timevariesbycolumn.zip) - 2010 NLCD environmental summaries (time varies by column)

