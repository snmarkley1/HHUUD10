
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
#### --------------------------------------------------------------------------- ####
####                             NEIGHBORS 1/30/2021                             ####
#### --------------------------------------------------------------------------- ####
#### --------------------------------------------------------------------------- #### 
#####################################################################################
#####################################################################################

### PREPARE WORKSPACE
import arcpy
import os

from arcpy import env
env.workspace = "C:/Users/scott/Dropbox/urb_proj"
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")
env.qualifiedFieldNames = False  ## good for joins

### LOAD NEEDED VARS
t10 = "shp_work/tracts1/tracts10.shp"  ## 2010 tract shapefile
nlcd = "tables/nlcd9211.dbf"  ## NLCD file
hu4019 = "tables/hu4019_urb.dbf"  ## hu estimates

### CREATE FILE GDB and move files there
arcpy.CreateFileGDB_management("shp_work","urb_fin.gdb")

## move files to gdb
arcpy.FeatureClassToGeodatabase_conversion(t10,"shp_work/urb_fin.gdb")  # fc

## tables
tables = [hu4019,nlcd]
for i in tables:
  arcpy.TableToGeodatabase_conversion(i,"shp_work/urb_fin.gdb")
  
## change env.workspace to urb_fin.gdb
env.workspace = "C:/Users/scott/Dropbox/urb_proj/shp_work/urb_fin.gdb"

### JOIN TRACTS w/ NLCD & HU4019 TABLES

## prepare join
t10 = "tracts10"
tables = ["hu4019_urb","nlcd9211"]
arcpy.MakeFeatureLayer_management(t10,"t10_layer")

## do join
for i in tables:
  arcpy.AddJoin_management("t10_layer","GISJOIN",i,"GISJOIN10","KEEP_ALL")
  
## save as new layer
arcpy.CopyFeatures_management("t10_layer","t10_urb")

## clean up
del_fields = ["GISJOIN","Shp_Len","OBJECTID_1","OBJECTID_12","GISJOIN10_1","GISJOIN15_1"]
arcpy.DeleteField_management("t10_urb",del_fields)

## n = 179 tracts w/ < 0.05 pdev92 & UY1 < 1990 (subtract 15 after removing zeroes)
## n = 225 tracts w/ > 0.5 pdev92 & UY1 > 1990

### NEIGHBORS ANALYSIS
arcpy.PolygonNeighbors_analysis("t10_urb","neighbors","GISJOIN15","NO_AREA_OVERLAP","BOTH_SIDES","","METERS")

## tract join to get length variable
tracts = "C:/Users/scott/Dropbox/urb_proj/shp_work/tracts1/tracts10.shp"
arcpy.AddField_management(tracts,"Shp_Len","DOUBLE")
arcpy.CalculateField_management(tracts,"Shp_Len","!Shape.Length@METERS!","PYTHON_9.3")

## join tracts to neighbors
arcpy.AddJoin_management("neighbors","src_GISJOIN15",tracts,"GISJOIN")

## save
arcpy.TableToTable_conversion("neighbors",env.workspace,"neighbors1")

