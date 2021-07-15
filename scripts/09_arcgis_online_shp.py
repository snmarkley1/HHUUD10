
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####         ADDING/ORGANIZING DASYMETRIC MAP LAYERS from ArcGIS Online          ####
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
from arcgis.gis import GIS  # import ArcGIS Online
import os, zipfile  # for file paths

## Set workspace
env.workspace = "D:/HIST_HU_URB"

## Set preferences
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")  # coordinate system in use
#env.extent = "MAXOF"  # for raster operations
env.qualifiedFieldNames = False  # good for joins

# Establish Map
aprx = arcpy.mp.ArcGISProject("CURRENT")


##-------------------------------
## Create temp folder
##-------------------------------
arcpy.management.CreateFolder(env.workspace, "temp")
path = os.path.join(env.workspace, "temp")  # create path

#############################################################################
## Import Clip File from ArcGIS Online to Set Coordinate System            ##
#############################################################################

gis = GIS()  # anon access to ArcGIS Online (need account: https://doc.arcgis.com/en/arcgis-online/get-started/create-account.htm & requisite licences)

## Get Mercer & Hayden Islands clip out file & Download
mh = gis.content.get(itemid = "85bbe4ec20304c2bb32a63a138a931a2")  # Mercer & Hayden Islands needed to fix water file
ry = gis.content.get(itemid = "59d26cbc21534cb1b50a37b44d948a53")  # Railyards
golf_temp = gis.content.get(itemid = "24949bc60a0f4754893e7ebabe2989fd")  # Golf courses

# Download shapefile to temp folder
for item in [mh, ry, golf_temp]:
  item.download(path)

## unzip mercer_hayden file
for item in ["mercerhayden.zip", "rail.zip", "usagolfcourses.zip"]:
  with zipfile.ZipFile(os.path.join(path, item), 'r') as zip_ref:
    zip_ref.extractall(path)  # shapefiles called 'mh' and 'rail'
    
## Add to map display
for item in ["temp/mh.shp", "temp/rail.shp", "temp/golf_courses.shp"]:
  name = arcpy.Describe(item).baseName
  arcpy.management.MakeFeatureLayer(item, name)
  
  
######################################################
## IMPORT Other ArcGIS Online Shapefiles            ##
######################################################

## Pull in files
landmarks = gis.content.get(itemid = "6ffa5cb05c3b4978bd96b8a4b416ffa6")  # USA Landmarks (layer pkg)
water = gis.content.get(itemid = "84e780692f644e2d93cefc80ae1eba3a")  # USA Detailed Water Bodies (layer pkg)
parks = gis.content.get(itemid = "578968f975774d3fab79fe56c8c90941")  # USA Parks (layer pkg)
airports = gis.content.get(itemid = "2706fbe2d7c74b488a609938df8f9578")  # USA Airport Areas (layer pkg)

shapefiles = [landmarks, water, parks, airports]  # make list

## write out layer packages into temp folder
for item in shapefiles:
  item.download(path)
  

################################################
## Fix up Dasymetric Zones (0 HU)             ##
################################################

## Set up output GDB
output_gdb = os.path.join(env.workspace, "gis_files/database1.gdb")

## Add to Display and update
arcpy.management.MakeFeatureLayer(os.path.join(path, "USALandmarks.lpk"), "cemeteries", "FEATTYPE = 'Cemetery Ground'")  # cemeteries
arcpy.management.MakeFeatureLayer(os.path.join(path, "USALandmarks.lpk"), "industrial_areas_temp", "FEATTYPE = 'Industrial Area'")  # industrial areas
arcpy.management.MakeFeatureLayer(os.path.join(path, "USAParks.lpk"), "parks_temp")  # parks
arcpy.management.MakeFeatureLayer(os.path.join(path, "USADetailedWaterBodies.lpk"), "water_temp", "FTYPE <> 'Swamp/Marsh'")  # water bodies
arcpy.management.MakeFeatureLayer(os.path.join(path, "USAAirportAreas.lpk"), "airports_temp")  # airports


##------------------------------------------
## Add golf years (tables)
##------------------------------------------
golf_table = "tables/golf_table.dbf"
arcpy.management.AddJoin("golf_courses", "COURSEID", golf_table, "OBJECTID")  # join shapefile with golf year table
arcpy.management.MakeFeatureLayer("golf_courses", "golf", "YEAR > 0")  # get rid of NULL values (Hawaii)
arcpy.conversion.FeatureClassToGeodatabase("golf", output_gdb)  # save out to database1 folder

# remove golf shapefiles from layout
for m in aprx.listMaps():
    for lyr in m.listLayers("golf*"):
        m.removeLayer(lyr)

# remove golf_table
for m in aprx.listMaps():
  for tab in m.listTables("golf*"):
    m.removeTable(tab)

##------------------------------------------
## Add airport years (tables)
##------------------------------------------

airports_yr = os.path.join(env.workspace, "tables/airports_yr.dbf")
arcpy.management.JoinField("airports_temp", "LOC_ID", airports_yr, "LOC_ID", "KEEP_COMMON")  # join shapefile with golf year table
arcpy.conversion.FeatureClassToFeatureClass('airports_temp', path, 'airports_temp1')

## Remove extraneous columns
airports_temp1 = os.path.join(path, "airports_temp1")
#arcpy.management.DeleteField(airports_temp1, "MNFC;LOC_ID_1")

## Remove airport grounds
arcpy.management.EliminatePolygonPart("airports_temp1", os.path.join(path, "airports_temp2"), "PERCENT", "0 SquareMeters", 50, "CONTAINED_ONLY")  # absorbs polygon parts w/in runways that are < 50% total area
arcpy.management.MakeFeatureLayer('airports_temp2', 'airports_temp3', "FEATTYPE = 'Airport runway' AND STATE IS NOT NULL")  # also remove NULL values

## Create buffer of 100m & keep only continental US & save to GDB
arcpy.analysis.Buffer("airports_temp3", os.path.join(path, "airports_buff100_temp"), "100 Meters", "FULL", "ROUND", "LIST", "NAME;LOC_ID;STATE;ACT_DATE", "PLANAR")
arcpy.management.MakeFeatureLayer("airports_buff100_temp", "airports_buff100", "STATE <> ' '")
arcpy.conversion.FeatureClassToGeodatabase("airports_buff100", output_gdb)

# remove airport_temp shapefiles from layout
for m in aprx.listMaps():
    for lyr in m.listLayers("airports*"):
        m.removeLayer(lyr)

##-------------------------------------------
## Write out cemeteries to database1 folder
##-------------------------------------------
arcpy.conversion.FeatureClassToGeodatabase("cemeteries", output_gdb)

# remove cemetery shapefils from layout
for m in aprx.listMaps():
    for lyr in m.listLayers("cem*"):
        m.removeLayer(lyr)

##----------------------------------------------------------------------------------------------
## Refine parks (dissolve by name/adjacency, remove those above 5 sq. mi. + Presidio in SF)
##----------------------------------------------------------------------------------------------

## Dissolve to get adjacent parks w/ same names
arcpy.management.Dissolve("parks_temp", os.path.join(path, "parks_diss.shp"), "NAME", "SQMI SUM", "SINGLE_PART", "UNSPLIT_LINES")
# ^ Error in Python window sometimes --> works manually using Dissolve tool with these entries

## Calcualte Sq. Mi. and Send to database1 GDB
arcpy.management.CalculateField("parks_diss", "SUM_SQMI", "!Shape.Area@SQUAREMILES!", "PYTHON3")
arcpy.management.MakeFeatureLayer("parks_diss", "parks", "SUM_SQMI < 5 And NAME <> 'The Presidio of San Francisco'")
arcpy.conversion.FeatureClassToGeodatabase("parks", output_gdb)  # save out to database1 folder

# remove park shapefiles from layout
for m in aprx.listMaps():
    for lyr in m.listLayers("park*"):
        m.removeLayer(lyr)
        
        
##---------------------------------------------------------------------------------------------
## Water --> Fix King Co., WA & Multnomah Co., OR and save out to database1
##---------------------------------------------------------------------------------------------

## Erase island features from water file
arcpy.analysis.Erase("water_temp", "mh", os.path.join(output_gdb, "water"))
# ^ Must do Erase manually because of unsolved topology error in ArcGIS Pro                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           60196)

# remove water and mh from layout
for m in aprx.listMaps():
    for lyr in m.listLayers("mh"):
        m.removeLayer(lyr)
        
for m in aprx.listMaps():
    for lyr in m.listLayers("water*"):
        m.removeLayer(lyr)


##---------------------------------------------------------------------------------------------
## Railyards --> Add 50m buffer
##---------------------------------------------------------------------------------------------

## Create buffer of 50m; Dissolve; Eliminate Polygon Parts
arcpy.analysis.Buffer("rail", os.path.join(path, "rail_buff50_temp"), "50 Meters", "FULL", "ROUND", "LIST", "STATEAB;YARDNAME", "PLANAR")
arcpy.management.Dissolve("rail_buff50_temp", os.path.join(path, "rail_buff50_diss"), None, None, "SINGLE_PART", "DISSOLVE_LINES")
arcpy.management.EliminatePolygonPart("rail_buff50_diss", os.path.join(output_gdb, "rail_buff50"), "PERCENT", "0 SquareMeters", 50, "CONTAINED_ONLY")

# Remove railyards from display
for m in aprx.listMaps():
    for lyr in m.listLayers("rail*"):
        m.removeLayer(lyr)
        
        
##---------------------------------------------------------------------------------------------
## Industrial Areas --> Remove
##---------------------------------------------------------------------------------------------


## Identify overlaps
#arcpy.management.MakeFeatureLayer(os.path.join(output_gdb, "t10"), "t10", "hu90 > 1")
#arcpy.management.AddField('t10', 'sqmi', "DOUBLE")
#arcpy.management.CalculateField('t10', 'sqmi', "!Shape.Area@SQUAREMILES!", 'PYTHON3')
#arcpy.analysis.Intersect('industrial_areas_temp;t10', 'ia_int', 'NO_FID')

#arcpy.management.AddField('ia_int', 'sqmi_int', "DOUBLE")
#arcpy.management.CalculateField('ia_int', 'sqmi_int', "!Shape.Area@SQUAREMILES!", 'PYTHON3')
#arcpy.management.AddField('ia_int', 'pcover', "DOUBLE")
#arcpy.management.CalculateField('ia_int', 'pcover', "!sqmi_int! / !sqmi!", 'PYTHON3')

## TRACTS w/ > 95% coverage (G0600370532400, G2600990982200)
arcpy.management.MakeFeatureLayer(os.path.join(output_gdb, 't10'), 't10_indust', "GISJOIN = 'G0600370532400' OR GISJOIN = 'G2600990982200'")
arcpy.conversion.FeatureClassToGeodatabase('t10_indust', output_gdb)

## Save out industrial areas as they are & rename
arcpy.conversion.FeatureClassToGeodatabase("industrial_areas_temp", output_gdb)
arcpy.management.Rename(os.path.join(output_gdb, "industrial_areas_temp"), "industrial_areas")


## Clear workspace
for m in aprx.listMaps():
    for lyr in m.listLayers("t10*") + m.listLayers("ind*"):
        m.removeLayer(lyr)

## DELETE TEMP FOLDER
arcpy.management.Delete(path)



