
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####                           CREATING DASYMETRIC ZONES                         ####
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

## Set base
base = "D:/HIST_HU_URB"

## Set preferences
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")  # coordinate system in use
#env.extent = "MAXOF"  # for raster operations
env.qualifiedFieldNames = False  # good for joins

# Establish Map
aprx = arcpy.mp.ArcGISProject("CURRENT")  # project
m = aprx.listMaps()  # map (only 1)

############################################################
## STEP 1: Join and Clean Shapefiles/Tables               ##
############################################################

## List files in tract folder
env.workspace = os.path.join(base, "gis_files/tracts")
polygons = arcpy.ListFeatureClasses()
polygons

## Delete unnecessary columns
for item in polygons:
  keep_fields = ["FID", "Shape", "GISJOIN"]  # keep these three fields
  fields = arcpy.ListFields(item)
  del_fields = [x.name for x in fields if x.name not in keep_fields]
  arcpy.management.DeleteField(item, del_fields)


## Make feature layer and remove AK, HI, & PR
for i in polygons:
  arcpy.management.MakeFeatureLayer(i, i, "GISJOIN NOT LIKE 'G02%' AND GISJOIN NOT LIKE 'G15%' AND GISJOIN NOT LIKE 'G72%'")
  
  
## Remove t10 (already handled in previous script)
for m in aprx.listMaps():
    for lyr in m.listLayers("t10"):
        m.removeLayer(lyr)
        
## Reorganize list
for m in aprx.listMaps():
  layers = m.listLayers()
  

new_names = ["bg90","t40", "t50", "t60", "t70", "t80", "t10"]  # make sure order is correct
for i, j in zip(polygons, new_names):
  arcpy.management.Rename(i, j)
  
## make new polygon list
polygons = arcpy.ListFeatureClasses()
polygons

## List tables from tables folder
env.workspace = "< YOUR FILE PATH to HIST_HU_URB/tables HERE >"
env.workspace = "C:/Users/scott/Dropbox/HIST_HU_URB/tables"

polygons_ysb = polygons[2:7] + polygons[:1]  ## do only 1940-1990 (ysb) polygons
tables_ysb = arcpy.ListTables("ysb*") #+ arcpy.ListTables("hu90*")  # put ysb & hu9019 tables in list
polygons_ysb, tables_ysb  # take a look (make sure they are in same order)

## Join
for i, j in zip(polygons_ysb, tables_ysb):
    arcpy.management.AddJoin(i, "GISJOIN", j, "GISJOIN", 'KEEP_COMMON')  # KEEP_COMMON gets rid of Alaska and Hawaii
    
## Send to GDB
polygons_ysb = ["t40", "t50", "t60", "t70", "t80", "bg90"]
output = "C:/Users/scott/Dropbox/HIST_HU_URB/gis_files/database1.gdb"
for item in polygons_ysb:
    arcpy.conversion.FeatureClassToGeodatabase(item, output)
    
    
### TAKE care of hu9019 in t10
hu9019 = "hu9019.dbf"  # load hu9019

# Join & Send to GDB
arcpy.management.AddJoin("t10", "GISJOIN", hu9019, "GISJOIN10", "KEEP_COMMON")
arcpy.conversion.FeatureClassToGeodatabase("t10", output)


###################################################################
## STEP 2: CREATE 0 HU Dasymetric File for Each Decade           ##
###################################################################

## Prepare erase layer
env.workspace = output  # change workspace to output GDB
arcpy.management.MakeFeatureLayer("golf", "golf40", "YEAR < 1941")
erase_features = ["cemeteries", "parks", "water", "golf40"] # includ only golf < 1941
erase_features

## Merge erase features to repair geometry
arcpy.management.Merge(erase_features, "dasym")  # merge erase features together (all but golf)
arcpy.management.RepairGeometry("dasym")  # fix up merged layer for later use (may take some time)

## Clear layers
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)


###################################################################
## STEP 3: CREATE Dasymetric Layers for Each Decade              ##
###################################################################

## Create Golf Course Layers by Decade
years = ["40", "50", "60", "70", "80", "90", "00", "10", "19"]
queries = ["YEAR < 1941", "YEAR < 1951", "YEAR < 1961", "YEAR < 1971", "YEAR < 1981", "YEAR < 1991", "YEAR < 2001", "YEAR < 2011", "YEAR < 2020"]


for y, q in zip(years, queries):
  outlayer = "golf" + y
  arcpy.management.MakeFeatureLayer("golf", outlayer, q)


### Create Dasymetric Layer for Each Decade

## Create list for polygons
polygons = arcpy.ListFeatureClasses("*0")

## -------------------------
## RUn loop for 1940
## -------------------------
polygons50 = []
for p in polygons:
  outlayer = arcpy.Describe(p).baseName + "_das"
  arcpy.analysis.Erase(p, "dasym", outlayer)
  
  yr_str = re.findall("\\d+", p)
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    if yr >= 50:
      polygons50.append(outlayer)
      
## Clear layers
for m in aprx.listMaps():
    for lyr in m.listLayers("*das"):
        m.removeLayer(lyr)
        
## -------------------------------
## Run loop for 1950
## -------------------------------
polygons60 = []
for p in polygons50:
    outlayer = arcpy.Describe(p).baseName + "50"
    arcpy.analysis.Erase(p, "golf50", outlayer)
        
    yr_str = re.findall("\\d+", p)
    yr_int = [int(yr_int) for yr_int in yr_str]
        
    for yr in yr_int:
      if yr >= 60:
        polygons60.append(outlayer)

## Clear layers
for m in aprx.listMaps():
    for lyr in m.listLayers("*das*"):
        m.removeLayer(lyr)
        
## -------------------------------
## Run loop for 1960
## -------------------------------
polygons70 = []
for p in polygons60:
  x = arcpy.Describe(p).baseName
  outlayer = x.replace("50", "60")
  arcpy.analysis.Erase(p, "golf60", outlayer)
              
  yr_str = re.findall("\\d+", p)
  yr_int = [int(yr_int) for yr_int in yr_str]
              
  for yr in yr_int:
    if yr >= 70:
      polygons70.append(outlayer)
            
## Clear layers
for m in aprx.listMaps():
    for lyr in m.listLayers("*das*"):
        m.removeLayer(lyr)
                

## -------------------------      
## Run loop for 1970
## -------------------------
polygons80 = []
for p in polygons70:
  x = arcpy.Describe(p).baseName
  outlayer = x.replace("60", "70")
  arcpy.analysis.Erase(p, "golf70", outlayer)
  
  yr_str = re.findall("\\d+", p)
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    if yr >= 80:
      polygons80.append(outlayer)
      
      
## Clear layers
for m in aprx.listMaps():
    for lyr in m.listLayers("*das*"):
        m.removeLayer(lyr)
        
        
## -------------------------      
## Run loop for 1980
## -------------------------
for p in polygons80:
  x = arcpy.Describe(p).baseName
  outlayer = x.replace("70", "80")
  arcpy.analysis.Erase(p, "golf80", outlayer)
  
  yr_str = re.findall("\\d+", p)
  yr_int = [int(yr_int) for yr_int in yr_str]
  
## Clear layers
for m in aprx.listMaps():
    for lyr in m.listLayers("*das*"):
        m.removeLayer(lyr)


## ----------------------------------------------------
## Cut t10 by All Golf Shapefiles & Calc. Sq. Mi.
## ----------------------------------------------------

t10_das = "t10_das"  # designate polygon
years1 = years[1:]  # no need to run with 1940 data

## Run Loop created dasymetric zones for 2010 (takes some time)
for yr in years1:
  golf = "golf" + yr
  outlayer = "t10_das" + yr
  sqmi = "sqmi" + yr
  arcpy.analysis.Erase(t10_das, golf, outlayer)
  arcpy.management.AddField(outlayer, sqmi, "DOUBLE")
  arcpy.management.CalculateField(outlayer, sqmi, "!Shape.Area@SQUAREMILES!", 'PYTHON3')
  
## Calc. sqmi for 1940
arcpy.management.AddField(t10_das, "sqmi40", "DOUBLE")
arcpy.management.CalculateField(t10_das, "sqmi40", "!Shape.Area@SQUAREMILES!", 'PYTHON3')

## Clear all golf layers
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)

## ------------------------------------       
## Clean up and Join into one
## ------------------------------------

## create list of shapefiles
polygons = arcpy.ListFeatureClasses("t10_das*")

## Delete GISJOIN & OID fields + hu fields for all but t10_das
del_fields1 = ["GISJOIN", "OID"]
del_fields2 = ["hu90", "hu00", "hu10", "hu19"]

for p in polygons:
  arcpy.management.DeleteField(p, del_fields1)
  
  if p != "t10_das":
    arcpy.management.DeleteField(p, del_fields2)
    
## Clear Layers from Display
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)
