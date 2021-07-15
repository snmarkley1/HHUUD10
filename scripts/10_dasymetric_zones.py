
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####                           CREATING DASYMETRIC ZONES                         ####
#### --------------------------------------------------------------------------- ####
#####################################################################################
#####################################################################################

### RUN in ArcGIS PRO 2.8.1

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


###################################################################
## STEP 1: CREATE 0 HU Dasymetric File for Each Decade           ##
###################################################################

## Establish workpsace
env.workspace = os.path.join(base, "gis_files/database1.gdb")

## Prepare erase layer
arcpy.management.MakeFeatureLayer("golf", "golf40", "YEAR < 1942")  # add extra year to account for construction
arcpy.management.MakeFeatureLayer("airports_buff100", "airports40", "ACT_DATE < 1943")  # add extra two years to account for construction
erase_features = ["cemeteries", "industrial_areas", "parks", "rail_buff50", "water", "t10_zero", "golf40", "airports40"] # include only golf & airports built by early 40s

## Merge erase features to repair geometry
arcpy.management.Merge(erase_features, "dasym")  # merge erase features together
arcpy.management.RepairGeometry("dasym")  # fix up merged layer for later use (may take some time)

## Clear layers
for m in aprx.listMaps():
    for lyr in m.listLayers("*40"):
        m.removeLayer(lyr)


###################################################################
## STEP 2: CREATE Dasymetric Layers for Each Decade              ##
###################################################################

## Create Golf Course Layers by Decade
years = ["40", "50", "60", "70", "80", "90", "00", "10", "19"]
golf_queries = ["YEAR < 1942", "YEAR < 1952", "YEAR < 1962", "YEAR < 1972", "YEAR < 1982", "YEAR < 1992", "YEAR < 2002", "YEAR < 2012", "YEAR < 2022"]
airport_queries = ["ACT_DATE < 1943", "ACT_DATE < 1953", "ACT_DATE < 1963", "ACT_DATE < 1973", "ACT_DATE < 1983", "ACT_DATE < 1993", "ACT_DATE < 2003", "ACT_DATE < 2013", "ACT_DATE < 2023"]

## Golf
for i, j in zip(years, golf_queries):
  outlayer = "golf" + i
  arcpy.management.MakeFeatureLayer("golf", outlayer, j)

## Airports
for i, j in zip(years, airport_queries):
  outlayer = "airport" + i
  arcpy.management.MakeFeatureLayer("airports_buff100", outlayer, j)

### Create Dasymetric Layer for Each Decade

## Create list for polygons
polygons = ["t40", "t50", "t60", "t70", "t80", "bg90"]

## -------------------------
## Run loop for 1940
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

## ---------------------------------------------------------------------
## Merge golf and airports by decade (needed for Erase function)
## ---------------------------------------------------------------------

years1 = years[1:]
golf_list = ["golf" + sub for sub in years1]
airport_list = ["airport" + sub for sub in years1]

for i, j in zip(golf_list, airport_list):
  yr_str = re.findall("\\d+", i)
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    if yr == 0:
      outlayer = "golf_airport00"
    else:
      outlayer = "golf_airport" + str(yr)
    
    arcpy.management.Merge([i, j], outlayer)

## Clean up
for m in aprx.listMaps():
  for lyr in m.listLayers("airport*") + m.listLayers("golf*"):
    m.removeLayer(lyr)

## -------------------------------
## Run loop for 1950
## -------------------------------
polygons60 = []
for p in polygons50:
    outlayer = arcpy.Describe(p).baseName + "50"
    arcpy.analysis.Erase(p, "golf_airport50", outlayer)
        
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
  arcpy.analysis.Erase(p, "golf_airport60", outlayer)
              
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
  arcpy.analysis.Erase(p, "golf_airport70", outlayer)
  
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
  arcpy.analysis.Erase(p, "golf_airport80", outlayer)
  
  yr_str = re.findall("\\d+", p)
  yr_int = [int(yr_int) for yr_int in yr_str]
  
## Clear layers
for m in aprx.listMaps():
    for lyr in m.listLayers("*das*"):
        m.removeLayer(lyr)

