# Introduction

Here we provide code for HHUUD10 (Historical Housing and Urbanization Database 2010). The write-up is published in *Scientific Data* here: [**Housing Unit and Urbanization Estimates for the Continental U.S. in Consistent Tract Boundaries, 1940–2019**](https://www.nature.com/articles/s41597-022-01184-x). And the dataset itself is available for download at *Open Science Framework* here: [**HHUUD10**](https://osf.io/fzv5e/).

### Citation
> Markley, S., Holloway, S., Hafley, S., & Hauer, M. 2022. Housing Unit and Urbanization Estimates for the Continental U.S. in Consistent Tract Boundaries, 1940–2019. *Scientific Data* 9(82) https://www.nature.com/articles/s41597-022-01184-x.

# Abstract
Subcounty housing unit counts are important for studying geo-historical patterns of (sub)urbanization, land-use change, and residential loss and gain. The most commonly used subcounty geographical unit for social research in the United States is the census tract. However, their changing geometries and historically incomplete coverage present significant obstacles for longitudinal analysis that existing datasets do not adequately address. Overcoming these barriers, we provide housing unit estimates in consistent 2010 tract boundaries for every census year from 1940 to 2010 plus 2019 for the entire continental US. Moreover, we develop an “urbanization year” indicator that denotes if and when tracts became “urbanized” during this timeframe. We produce these data by blending existing interpolation techniques with a novel procedure we call “maximum reabsorption”. Conducting out-of-sample validation, we find that our hybrid approach generally produces more reliable estimates than existing alternatives. The final dataset, Historical Housing Unit and Urbanization Database 2010 (HHUUD10), has myriad potential uses for research involving housing, population, and land-use change, as well as (sub)urbanization.

<p style="text-align: center">
  <img src="https://raw.githubusercontent.com/snmarkley1/snmarkley1.github.io/master/Projects/atl.gif" width="600" title="Urbanization in Metro Atlanta, 1940-2019"/>
  <figcaption>Urbanization in Metro Atlanta by 2010 Census Tracts Using HHUUD10 Categories, 1940-2019</figcaption>
</p>

# Organization
- `map` - ArcGIS Pro project used to run the Python scripts. Please ensure that the coordinate system is USA Contiguous Albers Equal Area Conic (ESRI:102003).
- `scripts` - R and Python scripts used to produce the dataset.
- `tables` - Input data resources. These include data that have been adapted from proprietary sources and data that have been manually recorded.
- `validation` - Scripts used to run the validation tests.

# Data
The final data product, HHUUD10, is available at the [Open Science Framework](https://doi.org/10.17605/OSF.IO/FZV5E). It includes an Esri Shapefile and identical GeoJSON file, as well as CSVs in long and wide formats.

# Codebook
The codebook for the long CSV file is as follows:

- `STATE` - Refers to each census tract's state abbreviation,
- `COUNTY` - Refers to each tract's county name.
- `GISJOIN10` - Unique tract ID in NHGIS format.
- `GEOID10` - Unique tract ID in Census Bureau format.
- `YEAR` - Year of housing unit and area data (1940 - 2019). In wide format, the last two digits in the year trail the variable name (e.g., hu40, sqmi40, etc.).
- `HU` - Tract housing unit count estimate.
- `SQMI` - Area of dasymetrically refined tract in square miles.
- `pdev92` - Percent of a tract's land area that was covered by an urban land use according to the NLCDe 1992.
- `pdev01` - Percent of a tract's land area that was covered by an urban land use according to the NLCD 2001.
- `pdev11` - Percent of a tract's land area that was covered by an urban land use according to the NLCD 2011.
- `UY1` - Urbanization year according to when a tract surpassed 200 HU / SQMI in its dasymetrically refined area.
- `UY2` - Same as UY1, except urbanized non-residential areas identified by the pdev variable and tract adjacency are included.

# Correspondence
If you encounter any problems with these scripts, please [create an issue](https://github.com/snmarkley1/HIST_HU_URB/issues). In addition, if you have local knowledge of a region and believe a housing unit estimate is dramatically off due to an estimation error, please report the tracts in question, including their 11-digit Tract ID, and a description of why you think the estimate is in error via the [same link](https://github.com/snmarkley1/HIST_HU_URB/issues). We may not be able to address all petitions in a timely manner, but we will do our best to update the dataset when convincing cases can be made.

## License
The data collected and presented are licensed under the [Creative Commons Attribution 4.0 International license](https://creativecommons.org/licenses/by/4.0/), and the underlying code used to format, analyze, and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
