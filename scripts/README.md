# Organization and Further Instructions
This folder contains the R and Python scripts used to create HHUUD10. Here, we list each script and provide a brief description, as well as any additional instructions needed to ensure the code runs smoothly. These instructions are also listed within the scripts themselves. We recommend users run the Python scripts within ArcGIS Pro 2.7 or later, except for script `14_maximum_reabsorption_pt3.py`, which should be run in ArcGIS Desktop 10.7 or later. Each script name is led by a number, which represents the order in which the scripts should be run. Users should run all of this code line-by-line rather than all at once. This is necessary for the code to run successfully because the API data pulls take time to process, and there are some quirks with ArcGIS Pro that may need to be handled manually.

`00_preample.R` - is a simple R script that sets up the workspace and loads the libraries used in the subsequent R scripts.

`01_pull_in_shapefiles.R` - pulls in tract and block group shapefiles from the NHGIS API.
  - `Line 34` - Users must set up a free [API key](https://account.ipums.org/api_keys) through the NHGIS and insert it where the code says "\<YOUR KEY HERE\>".
  - `Lines 67, 133, 190` - After requesting their extract, users must wait approximately 30-60 seconds before it is prepared. NHGIS will send an email when the data is ready for download. If users do not wait, the code will not work.
  - `Lines 83, 147, 204` - R's `download.file()` function can only handle small-to-medium size data downloads. This call will not work on every machine and will probably fail on most laptops. For best results, we recommend running this code on a desktop with good specs.

`02_pull_in_tables.R` - pulls in housing unit (HU) and year structure built (YSB) data from the NHGIS API.
  - `Line 29` - Must set up a free [API key](https://account.ipums.org/api_keys) through the NHGIS and insert it where the code says "\<YOUR KEY HERE\>".
  - `Line 96` -  Must wait approximately 3 minutes for the extract to be prepared. NHGIS will send an email when the data is ready for download.
  -  `Line 113` - For best results, we recommend running R's `download.file()` function on a desktop with good specs.

`03_xwalk90_10.R` - pulls in NHGIS's crosswalk files to put 1990 block group HU data in 2010 block group shapes.
  - `Line 26` - Must set up a free [API key](https://account.ipums.org/api_keys) through the NHGIS and insert it where the code says "\<YOUR KEY HERE\>".
  - `Line 61` - Must wait approximately 90-120 seconds for the extract to be prepared. NHGIS will send an email when the data is ready for download.
  -  `Line 75` - For best results, we recommend running R's `download.file()` function on a desktop with good specs.
  -  `Line 110` - Users need to have already downloaded the [`nhgis_bgp1990_bg2010.zip`](https://data2.nhgis.org/crosswalks/nhgis_bgp1990_bg2010.zip) file into the `tables` folder.

`04_organizing_tract_shp.py` - organizes data pulled in by previous scripts into appropriate folders and geodatabases to be processed via Python in ArcGIS Pro.
  - `Line 22` - Set up as if the repo was cloned in an external hard drive located in the `D:/` drive. Users may change this location if they wish. However, we recommend running it in an external hard drive with at least half a terabyte of memory because the processes run later will eat up a lot of memory.

`05_nlcd92_to_tracts.py` - assigns [1992 NLCDe](https://water.usgs.gov/GIS/metadata/usgswrd/XML/nlcde92.xml#stdorder) categories to 2010 tract geographies.
  - `Line 23` - Set to `D:/` drive.
  - `Line 56` - This block of code spit back errors in our first run but worked on the second run.

`06_nlcd_organize.R` - organizes the NLCD data produced in script 05 and gathered from the NHGIS's [environmental summaries files](https://www.nhgis.org/user-resources/environmental-summaries).
  - `Line 25` - Users need to have already downloaded the [`us_tract_2010_nlcd_timevariesbycolumn.zip`](https://data2.nhgis.org/environmental/us_tract_2010_nlcd_timevariesbycolumn.zip) file into the `tables` folder.

`07_pull_in_airport_data.R` - import activation date information for U.S. airports from the [Federal Aviation Administration (FAA)](https://www.faa.gov/airports/airport_safety/airportdata_5010/).
  - `Line 21` - This data was imported successfully on June 28, 2021 and was still operative by September 13, 2021. If this link is broken, users may [create an issue](https://github.com/[removed]/HHUUD10/issues).

`08_sparse_tracts.py` - 
