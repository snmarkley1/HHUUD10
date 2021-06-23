
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####            INTERSECTIONS for AREAL WEIGHTING, TDW, & TDW90                  ####
#### --------------------------------------------------------------------------- ####
#####################################################################################
#####################################################################################

### RUN in Python 3 window of ArcGIS PRO 2.7

##################################
## PREPARE WORKSPACE            ##
##################################

## Import packages
import arcpy, os, re  # need ArcGIS license
from arcpy import env

## Set base folder
base = "D:/HIST_HU_URB"

## Set workspace
env.workspace = "< YOUR FILE PATH to HIST_HU_URB HERE >"
env.workspace = os.path.join(base, "gis_files/database1.gdb")

## Set preferences
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")  # coordinate system in use
#env.extent = "MAXOF"  # for raster operations
env.qualifiedFieldNames = False  # good for joins

# Establish Map
aprx = arcpy.mp.ArcGISProject("CURRENT")  # project
m = aprx.listMaps()  # map (only 1)

############################################################
## CLEANING UP ATTRIBUTE TABLES                           ##
############################################################

## Make list for cleaning up attribute tables
polygons = arcpy.ListFeatureClasses("t*")

## Remove OID and GISJOIN_1 fields if they exist
del_fields = ["OID", "GISJOIN_1"]
for p in polygons:
  fields = arcpy.ListFields(p)
  df2 = [x.name for x in fields if x.name in del_fields]
  arcpy.management.DeleteField(p, df2)

## Clear Layers from Display
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)
        

############################################################
## CALCULATING SQ. MI                                     ##
############################################################

## Make lists for Intersection
source_zones = ["t40_das", "t50_das50", "t60_das60", "t70_das70", "t80_das80"]  # source zones
p_target_zones = ["t50_das", "t60_das50", "t70_das60", "t80_das70"] # pseudo-target zones (SZ + 10) --> exclude 1990 data for 1980
bg_target_zones = arcpy.ListFeatureClasses("bg90_*")  # 2010 target zones (bg) w/ 1990 HU data


## Calculate Sq. Mi. for Source and Target Layers
polygons = source_zones + p_target_zones + bg_target_zones  # combine those lists into one for loop
for p in polygons:
  arcpy.management.AddField(p, "sqmi", "DOUBLE")  # add field
  arcpy.management.CalculateField(p, "sqmi", "!Shape.Area@SQUAREMILES!", "PYTHON3")  # calculate geometry (sq. mi.)
  
## Create 1990 data in 2010 Tracts via Dissolve
for bg in bg_target_zones:
  output = bg.replace("bg", "t")  # change names from bg90... to t90...
  arcpy.management.AddField(bg, "JOIN_T", "TEXT")  # add TRACT join field
  arcpy.management.CalculateField(bg, "JOIN_T", "!GISJOIN![:-1]", "PYTHON3")  # create JOIN_T field by shaving off last # in BG ID
  arcpy.management.Dissolve(bg, output, "JOIN_T", "hu40 SUM;hu50 SUM;hu60 SUM;hu70 SUM;hu80 SUM;hu90 SUM;sqmi SUM", "MULTI_PART", "DISSOLVE_LINES")  # Dissolve & sum HU counts + sqmi

## Clear Layers from Display
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)
        

############################################################
## Do INTERSECTION                                        ##
############################################################

## Create tract list
p_target_zones = p_target_zones + ["t90_das80"]  # add new layer
target_zones = arcpy.ListFeatureClasses("t90_*")

## Intersect and Calculate Sq. Mi. for atom
for i, j, k in zip(source_zones, p_target_zones, target_zones):
  yr_str = re.findall("\\d+", i)[:1]
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
      output = "int" + str(yr)
      
      if output == "int80":
        inputs = i + ";" + j
      
      else:
        inputs = i + ";" + j + ";" + k
      
      arcpy.analysis.Intersect(inputs, output, "NO_FID")
      arcpy.management.AddField(output, "sqmi_int", "DOUBLE")
      arcpy.management.CalculateField(output, "sqmi_int", "!Shape.Area@SQUAREMILES!", "PYTHON3")

## Clear Layers from Display
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)  
