# Organization and Further Instructions
This folder contains the R and Python scripts used to run the validation for HHUUD10. Here, we list each script and provide a brief description, as well as any additional instructions needed to ensure the code runs smoothly. These instructions are also listed within the scripts themselves. 

## General Instructions

- We recommend users run the Python scripts within ArcGIS Pro 2.7 or later. 
- Each script name is led by a number, which represents the order in which the scripts should be run. 
- Users should run this code line-by-line rather than all at once. This is necessary for the code to run successfully because the API data pulls take time to process, and there are some quirks with ArcGIS Pro that may need to be handled manually. 
- All scripts are run with the assumption that users have cloned the repo into a folder called "D:/HHUUD10/validation/".
- The Python scripts are written as if users are running them in an external hard drive located in the `D:/` drive. Users may change this location within the scripts if they prefer. However, we recommend cloning the repo and running all code in an external hard drive with at least half a terabyte of memory.

## Script-by-Script Description and Instructions
- `00_preample.R` - is a simple R script that sets up the workspace and loads the libraries used in the subsequent R scripts.

- `01_read_in_tables.R` - imports data used for validation.
  - *Line 19* - Users must set up a [Census API key](https://api.census.gov/data/key_signup.html) if they do not already have one and enter it where the code says "<YOUR KEY HERE>".
  - *Line 27* - Must set up a free [API key](https://account.ipums.org/api_keys) through the NHGIS and enter it where the code says "\<YOUR KEY HERE\>".
  - *Line 99* - Must wait approximately 5 minutes for the extract to be prepared. NHGIS will send an email when the data is ready for download.
  - *Line 113* - For best results, we recommend running R's `download.file()` function on a desktop with good specs.

- `02_pull_in_shapefiles.R` - imports shapefiles for validation.
  - *Line 23* - Must set up a free [API key](https://account.ipums.org/api_keys) through the NHGIS and enter it where the code says "\<YOUR KEY HERE\>".
  - *Line 54* - Must wait approximately 30-60 seconds for the extract to be prepared. NHGIS will send an email when the data is ready for download.
  - *Line 68* - For best results, we recommend running R's `download.file()` function on a desktop with good specs.

- `03_buffer.R` - selects sample tracts to be removed in validation procedure.
  
- `04_combine_for_output.R` - combines files to generate shapefile outputs.
  
- `05_python_prep.py` - runs through maximum reabsorption prep and execution with sample data.
  
- `06_compare_aw_tdw.py` - conducts intersections using complete data to allow a comparison of AW and TDW results.

- `07_build_hhuud_valid.R` - builds HHUUD10's housing unit estimates with removed validation sample and generates output table.
  
- `08_build_aw_tdw_compare.R` - builds HHUUD10's housing unit estimates with complete validation sample to be compared to AW and TDW and generates output table.
  
- `09_valid_graph.R` - builds validation graphs.

# Correspondence
For any issues with these scripts, please [create an issue](https://github.com/[removed]/HHUUD10/issues).

## License
The data collected and presented are licensed under the [Creative Commons Attribution 4.0 International license](https://creativecommons.org/licenses/by/4.0/), and the underlying code used to format, analyze, and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
