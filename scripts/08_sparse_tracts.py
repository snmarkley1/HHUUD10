
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####                      ORGANIZING TRACT/BG SHAPEFILES                         ####
#### --------------------------------------------------------------------------- ####
#####################################################################################
#####################################################################################

### RUN in ArcGIS PRO 2.7

##################################
## PREPARE WORKSPACE            ##
##################################

## Import packages
import arcpy, os, re  # need ArcGIS license
from arcpy import env

## Establish base
base = "D:/HIST_HU_URB"

## Set preferences
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")  # coordinate system in use
#env.extent = "MAXOF"  # for raster operations
env.qualifiedFieldNames = False  # good for joins

# Establish Map
aprx = arcpy.mp.ArcGISProject("CURRENT")  # project

# Establish path to gdb (database2)
db1 = os.path.join(base, "gis_files/database1.gdb")

#######################################################################
##  JOIN TRACTS to 1990-2019 HU COUNTS + NLCD VALUES                 ##
#######################################################################

# Grab 2010 tracts, 90-19 HU file, & NLCD values
t10 = os.path.join(db1, "t10")
nlcd = os.path.join(base, "tables/nlcd9211.dbf")

# Add to display (remove AK, HI, & PR)
arcpy.management.MakeFeatureLayer(t10, "t10_nlcd")

## Join NLCD table to t10
arcpy.management.AddJoin("t10_nlcd", "GISJOIN", nlcd, "GISJOIN10", "KEEP_ALL")

## Save shapefile in new geodatabase
arcpy.conversion.FeatureClassToGeodatabase("t10_nlcd", db1)

## Clear map display
for m in aprx.listMaps():
    for lyr in m.listLayers("t10*"):
        m.removeLayer(lyr)

## Clean up fields
t10_nlcd = os.path.join(db1, "t10_nlcd")
arcpy.management.DeleteField(t10_nlcd, "GISJOIN10")  # delete field

##########################################################################################
## SELECT SPARSELY BUILT-UP TRACTS for DASYMETRIC PROCESS & SAVE OUT                    ##
##########################################################################################

## Set env. workspace
env.workspace = db1

## Create sparsely populated tracts (hu90 or hu80 < 100 & pdev < 20th percentile for 1992 or 2001)
arcpy.management.MakeFeatureLayer('t10_nlcd', 't10_sparse', "(hu90 < 334 OR hu80 < 60) AND pdev92 < 0.172895")

## Create tracts with zero HUs in 1990 + low pdev (hu90 < 20th percentile)
arcpy.management.MakeFeatureLayer('t10_nlcd', 't10_zero', "(hu90 < 10) AND pdev92 < 0.172895")

## Save out
for i in ['t10_sparse', 't10_zero']:
  arcpy.conversion.FeatureClassToGeodatabase(i, db1)

## Clear map display
for m in aprx.listMaps():
    for lyr in m.listLayers("t10*"):
        m.removeLayer(lyr)
        



  
  
  
  
