
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####         ADDING/ORGANIZING DASYMETRIC MAP LAYERS from ArcGIS Online          ####
#### --------------------------------------------------------------------------- ####
#####################################################################################
#####################################################################################

### RUN in ArcGIS PRO 2.7

##################################
## PREPARE WORKSPACE            ##
##################################

## Import packages
import arcpy  # need ArcGIS license
from arcpy import env
from arcgis.gis import GIS  # import ArcGIS Online
import os, zipfile  # for file paths

## Set workspace
env.workspace = "< YOUR FILE PATH HERE >"
env.workspace = "C:/Users/scott/Dropbox/HIST_HU_URB"

## Set preferences
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")  # coordinate system in use
#env.extent = "MAXOF"  # for raster operations
env.qualifiedFieldNames = False  # good for joins

# Create temp folder
arcpy.management.CreateFolder(env.workspace, "temp")
path = os.path.join(env.workspace, "temp")  # create path

# Establish Map
aprx = arcpy.mp.ArcGISProject("CURRENT")

#############################################################################
## Import Clip File from ArcGIS Online to Set Coordinate System            ##
#############################################################################

gis = GIS()  # anon access to ArcGIS Online (need account: https://doc.arcgis.com/en/arcgis-online/get-started/create-account.htm & requisite licences)

## Get Mercer & Hayden Islands clip out file & Download
mh = gis.content.get(itemid = "85bbe4ec20304c2bb32a63a138a931a2")  # Mercer & Hayden Islands needed to fix water file
mh.download(path)

## unzip mercer_hayden file
with zipfile.ZipFile(os.path.join(path, "mercerhayden.zip"), 'r') as zip_ref:
    zip_ref.extractall(path)  # shapefile called 'mh'
    

## Add to map display
arcpy.management.MakeFeatureLayer(os.path.join(path, "mh.shp"), "mh")
  
  
######################################################
## IMPORT Other ArcGIS Online Shapefiles            ##
######################################################

### Want following features as shapefiles: USA Detailed Water Bodies (feature layer collection) & USA Landmarks (layer package)

## Pull in files

landmarks = gis.content.get(itemid = "6ffa5cb05c3b4978bd96b8a4b416ffa6")  # USA Landmarks (layer pkg)
water = gis.content.get(itemid = "84e780692f644e2d93cefc80ae1eba3a")  # USA Detailed Water Bodies (layer pkg)
parks = gis.content.get(itemid = "578968f975774d3fab79fe56c8c90941")  # USA Parks (layer pkg)

shapefiles = [landmarks, water, parks]  # make list

## write out layer packages into temp folder
for item in shapefiles:
  item.download(path)
  

################################################
## Fix up Dasymetric Zones (0 HU)             ##
################################################

## Create output GDB
arcpy.management.CreateFileGDB("gis_files", "database1.gdb")
output = os.path.join(env.workspace, "gis_files/database1.gdb")

## Add to Display and update
arcpy.management.MakeFeatureLayer(os.path.join(path,"USALandmarks.lpk"), "golf_temp", "FEATTYPE = 'Golf Course Ground'")
arcpy.management.MakeFeatureLayer(os.path.join(path,"USALandmarks.lpk"), "cemeteries","FEATTYPE = 'Cemetery Ground'")
arcpy.management.MakeFeatureLayer(os.path.join(path,"USAParks.lpk"), "parks_temp")
arcpy.management.MakeFeatureLayer(os.path.join(path,"USADetailedWaterBodies.lpk"), "water_temp", "FTYPE <> 'Swamp/Marsh'")

##------------------------------------------
## Add golf years (tables)
##------------------------------------------
golf_table = "tables/golf_table.dbf"
arcpy.management.JoinField("golf_temp", "OBJECTID", golf_table, "OBJECTID")  # join shapefile with golf year table
arcpy.management.MakeFeatureLayer("golf_temp","golf","YEAR > 0")  # get rid of NULL values (Hawaii)
arcpy.conversion.FeatureClassToGeodatabase("golf", output)  # save out to database1 folder

# remove golf shapefiles from layout
for m in aprx.listMaps():
    for lyr in m.listLayers("golf*"):
        m.removeLayer(lyr)

##-------------------------------------------
## Write out cemeteries to database1 folder
##-------------------------------------------
arcpy.conversion.FeatureClassToGeodatabase("cemeteries", output)

# remove cemetery shapefils from layout
for m in aprx.listMaps():
    for lyr in m.listLayers("cem*"):
        m.removeLayer(lyr)

##----------------------------------------------------------------------------------------------
## Refine parks (dissolve by name/adjacency, remove those above 5 sq. mi. + Presidio in SF)
##----------------------------------------------------------------------------------------------

## Dissolve to get adjacent parks w/ same names
arcpy.management.Dissolve("parks_temp", os.path.join(path, "parks_diss.shp"), "NAME", "SQMI SUM", "SINGLE_PART", "UNSPLIT_LINES")
# ^ Error in Python notebook --> works manually using Dissolve tool using these speculations

## Calcualte Sq. Mi. and Send to database1 GDB
arcpy.management.CalculateField("parks_diss", "SUM_SQMI", "!Shape.Area@SQUAREMILES!", "PYTHON3")
arcpy.management.MakeFeatureLayer("parks_diss", "parks", "SUM_SQMI < 5 And NAME <> 'The Presidio of San Francisco'")
arcpy.conversion.FeatureClassToGeodatabase("parks", output)  # save out to database1 folder

# remove park shapefiles from layout
for m in aprx.listMaps():
    for lyr in m.listLayers("park*"):
        m.removeLayer(lyr)
##---------------------------------------------------------------------------------------------
## Water --> Fix King Co., WA & Multnomah Co., OR and save out to database1
##---------------------------------------------------------------------------------------------

## Erase island features from water file
arcpy.analysis.Erase("water_temp", "mh", os.path.join(output, "water"))
# ^ Must do Erase manually because of unsolved topology error in ArcGIS Pro (160196)

# remove water and mh from layout
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)

## DELETE TEMP FOLDER
arcpy.management.Delete(path)









