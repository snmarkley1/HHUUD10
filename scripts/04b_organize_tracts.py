
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



## Create Geodatabase & path
arcpy.management.CreateFileGDB(os.path.join(base, "gis_files"), "database2.gdb")


#######################################################################
##  JOIN TRACTS to 1990-2019 HU COUNTS + NLCD VALUES                 ##
#######################################################################

# Establish path to gdb (database2)
db2 = os.path.join(base, "gis_files/database2.gdb")

# Grab 2010 tracts, 90-19 HU file, & NLCD values
t10 = os.path.join(base, "gis_files/tracts/t10.shp")
hu9019 = os.path.join(base, "tables/hu9019.dbf")
nlcd = os.path.join(base, "tables/nlcd9211.dbf")
ysb90_t = os.path.join(base, "tables/ysb90_t.dbf")

# Add to display (remove AK, HI, & PR)
arcpy.management.MakeFeatureLayer(t10, "t10_temp", "GISJOIN NOT LIKE 'G02%' AND GISJOIN NOT LIKE 'G15%' AND GISJOIN NOT LIKE 'G72%'")

## Join tables to t10
for i in [ysb90_t, hu9019, nlcd]:
  arcpy.management.AddJoin('t10_temp', 'GISJOIN', i, 'GISJOIN10', 'KEEP_ALL')

## Save shapefile in new geodatabase
arcpy.conversion.FeatureClassToFeatureClass("t10_temp", db2, "t10")

## Clean up
del_fields = ["OID_1", "GISJOIN10", "hu90", "OID_12", "GISJOIN10_1", "GISJOIN19", "OID_12_13", "GISJOIN10_12"]
for i in del_fields:
  arcpy.management.DeleteField("t10", i)
  
# Change name
arcpy.management.AlterField('t10', 'hu90_1', 'hu90', 'hu90')


##########################################################################################
## SELECT SPARSELY BUILT-UP TRACTS for DASYMETRIC PROCESS & SAVE OUT                    ##
##########################################################################################

## Create sparsely populated tracts (hu90 or hu80 < 100 & pdev < 20th percentile for 1992 or 2001)
arcpy.management.MakeFeatureLayer('t10', 't10_sparse', "(hu90 < 334 OR hu80 < 60) AND pdev92 < 0.172895")

## Create tracts with zero HUs in 1990 + low pdev (hu90 < 20th percentile)
arcpy.management.MakeFeatureLayer('t10', 't10_zero', "(hu90 < 10) AND pdev92 < 0.172895")

## Save out
for i in ['t10_sparse', 't10_zero']:
  arcpy.conversion.FeatureClassToGeodatabase(i, db2)

## Clear map display
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)
        



  
  
  
  
