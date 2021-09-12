
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####              MAXIMUM REABSORPTION STEP 3 (ArcMap 10.7.1)                    ####
#### --------------------------------------------------------------------------- ####
#####################################################################################
#####################################################################################

### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
### RUN in ArcGIS *Desktop* 10.7.1 !!!!!
### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

##################################
## PREPARE WORKSPACE            ##
##################################

## Import packages
import arcpy, os, re  # need ArcGIS license
from arcpy import env

## Set base folder
base = "D:/HHUUD10"

## Set workspace
env.workspace = os.path.join(base, "gis_files/database1.gdb")

## Set preferences
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")  # coordinate system in use
env.extent = "MAXOF"  # for raster operations--output uses max. extent of inputs
env.qualifiedFieldNames = False  # good for joins

## Create output GDB
env.workspace = "D:/HHUUD10/gis_files/rasters.gdb"  # set workspace to rasters gdb


#######################################################################
## STEP 3: REABSORB HU MAXIMUMS into 2010 TRACT GEOGRAPHIES          ##
#######################################################################

## Prepare Shapefile needed (make sure workspace is set to "rasters.gdb")
db1 = os.path.join(base, "gis_files/database1.gdb")  # establish database1.gdb for export
t10 = os.path.join(db1, "t10")  # using full tract to reabsorb

## Set lists
rasters = arcpy.ListRasters("max*")
outputs = ["tab40", "tab50", "tab60", "tab70", "tab80"]  # check that order matches rasters

## -----------------------------------------------------------------
## Zonal Statistics (Sum raster cell values by tract) 
## -----------------------------------------------------------------

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
## ONLY WORKS in ArcGIS DESKTOP (Done on 10.7.1) !!!!!!!!!!!!!!!! ##
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
for i, j in zip(rasters, outputs):
    outpath = os.path.join(db1, j)
    arcpy.gp.ZonalStatisticsAsTable_sa(t10, "GISJOIN", i, outpath, "DATA", "SUM")



