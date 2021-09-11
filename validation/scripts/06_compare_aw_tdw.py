#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####                 PREP for 1990 Vector-based Comparisons                      ####
#### --------------------------------------------------------------------------- ####
#####################################################################################
#####################################################################################

### RUN in Python 3 window of ArcGIS PRO 2.8.1

##################################
## PREPARE WORKSPACE            ##
##################################

## Import packages
import arcpy, os, re  # need ArcGIS license
from arcpy import env

## Set base folder
base = "D:/validation"

# set output gdb
output_gdb = os.path.join(base, "gis_files", "database1.gdb")

## Set preferences
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")  # coordinate system in use
env.extent = "MAXOF"  # for raster operations
env.qualifiedFieldNames = False  # good for joins

# Establish Map
aprx = arcpy.mp.ArcGISProject("CURRENT")  # project

## Establish workspace
env.workspace = output_gdb

##################################################
## Calc sq. mi
##################################################

polygons = ["t90_das", "t00_das", "bg19_das"]

## Calc. sq. mi.
for p in polygons:
  arcpy.management.DeleteField(p, "sqmi")
  arcpy.management.AddField(p, "sqmi", "DOUBLE")
  arcpy.management.CalculateField(p, "sqmi", "!Shape.Area@SQUAREMILES!", 'PYTHON3')
  

##################################################
## Do intersections 
##################################################

## Make lists for Intersection
source_zones = ["t90_das"]  # source zones
p_target_zones = ["t00_das"] # pseudo-target zones (SZ + 10) --> exclude 1990 data for 1980
bg_target_zones = ["bg19_das"]  # 2010 target zones (bg) w/ 1990 HU data

## Intersect and Calculate Sq. Mi. for atom (takes a long time)
for i, j, k in zip(source_zones, p_target_zones, bg_target_zones):
  yr_str = re.findall("\\d+", i)[:1]
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    
    output = "int_full_" + str(yr)
    inputs = i + ";" + j + ";" + k
      
    arcpy.analysis.Intersect(inputs, output, "NO_FID")
    arcpy.management.AddField(output, "sqmi_int", "DOUBLE")
    arcpy.management.CalculateField(output, "sqmi_int", "!Shape.Area@SQUAREMILES!", "PYTHON3")
      
      
## Clear Layers from Display
for m in aprx.listMaps():
    for lyr in m.listLayers("*0") + m.listLayers("*das"):
        m.removeLayer(lyr)


