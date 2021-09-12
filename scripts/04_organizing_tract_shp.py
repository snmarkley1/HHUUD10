
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####               ORGANIZING TRACT SHAPEFILES in ArcGIS PRO                     ####
#### --------------------------------------------------------------------------- ####
#####################################################################################
#####################################################################################

### RUN in ArcGIS PRO 2.8.1

##################################
## PREPARE WORKSPACE            ##
##################################

## Import packages
import arcpy  # need ArcGIS license
from arcpy import env
import os, glob   # for downloading, grabbing files

## Set workspace
base = "D:/HHUUD10"  # GitHub Repo Folder in External Hard Drive
env.workspace = base

## Set preferences
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")  # coordinate system in use
env.extent = "MAXOF"  # for raster operations
env.qualifiedFieldNames = False  # good for joins

# Create temp folder
arcpy.management.CreateFolder(env.workspace, "temp")
path = os.path.join(env.workspace, "temp")  # create path

# Establish Map
aprx = arcpy.mp.ArcGISProject("CURRENT")

##----------------------------------------------------
## Create GDB & make it the output GDB
##----------------------------------------------------
arcpy.management.CreateFileGDB(os.path.join(base, "gis_files"), "database1.gdb")
output_gdb = os.path.join(base, "gis_files/database1.gdb")


#############################################################
## STEP 1: Clean up/Organize Tract/BG Shapefiles           ##
#############################################################

##------------------------------------------
##  Remove AK, HI, PR Tract Shapefiles
##------------------------------------------

## Change workspace and grab tract shapefiles
env.workspace = os.path.join(base, "gis_files/tracts")
polygons = arcpy.ListFeatureClasses()
names = ["bg10_temp", "t40_temp", "t50_temp", "t60_temp", "t70_temp", "t80_temp", "t10_temp"]  # ensure they're in same order as polygons

## Remove AK, HI, PR
for i, j in zip(polygons, names):
  arcpy.management.MakeFeatureLayer(i, j, "GISJOIN NOT LIKE 'G02%' AND GISJOIN NOT LIKE 'G15%' AND GISJOIN NOT LIKE 'G72%'")

## move tract shps to output gdb
for m in aprx.listMaps():
    for lyr in m.listLayers("*temp"):
        arcpy.conversion.FeatureClassToGeodatabase(lyr, output_gdb)
        
## Clear map display
for m in aprx.listMaps():
    for lyr in m.listLayers("*temp"):
        m.removeLayer(lyr)


##------------------------------------------
##  Clean up tract shapefiles
##------------------------------------------

env.workspace = output_gdb
polygons = arcpy.ListFeatureClasses()

## Establish fields to keep
keep_fields = ["OBJECTID", "Shape", "GISJOIN", "Shape_Length", "Shape_Area"]

## Delete all but essential fields but keep_fields
for i in polygons:
  fields = arcpy.ListFields(i)
  del_fields = [x.name for x in fields if x.name not in keep_fields]
  arcpy.management.DeleteField(i, del_fields)


######################################E##############################
## STEP 2: Join and Clean 1940-80 Shapefiles/Tables                ##
#####################################################################

# Collect polygons in list
polygons_40_80 = ["t40_temp", "t50_temp", "t60_temp", "t70_temp", "t80_temp"]

# Collect ysb tables in list
tables = glob.glob(os.path.join(base, "tables", "*0.dbf"))[:5]  # grabs 1940-80 ysb tables

## Join
for i, j in zip(polygons_40_80, tables):
  arcpy.management.AddJoin(i, "GISJOIN", j, "GISJOIN", "KEEP_ALL")

names = ["t40", "t50", "t60", "t70", "t80"]

## Save out to GDB
for i, j in zip(polygons_40_80, names):
  arcpy.conversion.FeatureClassToFeatureClass(i, output_gdb, j)
  
## Delete extraneous fields
polygons_40_80 = names

del_fields = ["OID_1", "GISJOIN_1"]

for i in polygons_40_80:
    arcpy.management.DeleteField(i, del_fields)


## Clean up map display
for m in aprx.listMaps():
    for lyr in m.listLayers("*temp") + m.listLayers("*0"):
        m.removeLayer(lyr)


######################################E##############################
## STEP 3: Fix up 2010 files                                       ##
#####################################################################

polygons10 = ["bg10_temp", "t10_temp"]
names = ["bg90", "t10"]
ysb_tables = glob.glob(os.path.join(base, "tables", "ysb90*.dbf"))  # 1: bg; 2: tract
hu9019 = os.path.join(base, "tables/hu9019.dbf")

## Add to display
for i, j in zip(polygons10, names):
    arcpy.management.MakeFeatureLayer(i, j)

## Do joins with 1990 data
for i, j in zip(names, ysb_tables):
  arcpy.management.AddJoin(i, "GISJOIN", j, "GISJOIN", "KEEP_ALL")

## Join tracts with 1990-2019 data
arcpy.management.AddJoin("t10", "GISJOIN", hu9019, "GISJOIN10", "KEEP_ALL")

## Move layers to GDB
for i in names:
    arcpy.conversion.FeatureClassToGeodatabase(i, output_gdb)
    
## Clear map display
for m in aprx.listMaps():
    for lyr in m.listLayers("*0"):
        m.removeLayer(lyr)
        
        
##--------------------------------------------
##  Clean up 2010 polygons
##--------------------------------------------

## Delete extraneous fields for bg90 & t10
del_fields = ["OID", "GISJOIN_1", "OID_1", "GISJOIN10", "GISJOIN19"]
for i in names:
  arcpy.management.DeleteField(i, del_fields)

## Delete and rename hu90_1 in t10
arcpy.management.DeleteField("t10", "hu90")
arcpy.management.AlterField('t10', 'hu90_1', 'hu90', 'hu90')


##-------------------------------------------------
##  Clean up GDB & map display
##-------------------------------------------------

## Delete temp files in database1
del_polygons = arcpy.ListFeatureClasses("*temp")
for i in del_polygons:
    if arcpy.Exists(i):
        arcpy.management.Delete(i)
        
## Clear map display
for m in aprx.listMaps():
    for lyr in m.listLayers("*0"):
        m.removeLayer(lyr)
        



