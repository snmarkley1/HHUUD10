# Organization and Further Instructions
This folder contains the R and Python scripts used to create HHUUD10. Here, we list each script and provide a brief description, as well as any additional instructions needed to ensure the code runs smoothly. These instructions are also listed within the scripts themselves. We recommend users run the Python scripts within ArcGIS Pro 2.7 or later, except for script `14_maximum_reabsorption_pt3.py`, which should be run in ArcGIS Desktop 10.7 or later. Each script name is led by a number, which represents the order in which the scripts should be run. Users should run all of this code line-by-line rather than all at once. This is necessary for the code to run successfully because the API data pulls take time to process, and there are some quirks with ArcGIS Pro that may need to be handled manually.

- `00_preample.R` - is a simple R script that sets up the workspace and loads the libraries used in the subsequent R scripts.

- `01_pull_in_shapefiles.R` - pulls in tract and block group shapefiles from the NHGIS API.
  - Line 34: Users must set up their free [API key](https://account.ipums.org/api_keys) to get this data.
  - Lines 67, 133, 190: After requesting their extract, users must wait approximately 30-60 seconds before it is prepared. The NHGIS will send them an email when their data is ready. If users do not wait, the code will not work.
  - Lines 83, 147, 204: R's download.file() function can only handle small-to-medium size data downloads. This call will not work on every machine and will probably fail on most laptops. For best results, we recommend running this code on a desktop with good specs.
