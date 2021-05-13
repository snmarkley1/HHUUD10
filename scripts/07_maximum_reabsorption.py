#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####                            MAXIMUM REABSORPTION                             ####
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
#base = "C:/Users/scott/Dropbox/HIST_HU_URB"
base = "D:/HIST_HU_URB"

## Set workspace
#env.workspace = "< YOUR FILE PATH to HIST_HU_URB HERE >"
#env.workspace = "C:/Users/scott/Dropbox/HIST_HU_URB/gis_files/database1.gdb"
env.workspace = os.path.join(base, "gis_files/database1.gdb")

## Set preferences
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")  # coordinate system in use
env.extent = "MAXOF"  # for raster operations--output uses max. extent of inputs
env.qualifiedFieldNames = False  # good for joins

# Establish Map
aprx = arcpy.mp.ArcGISProject("CURRENT")  # project
m = aprx.listMaps()  # map (only 1)

# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)

############################################################
## STEP 1: CREATE RASTERS OUT of HU DATA                  ##
############################################################

## Create output GDB
arcpy.management.CreateFileGDB("D:/HIST_HU_URB/gis_files", "rasters.gdb")
outpath = "D:/HIST_HU_URB/gis_files/rasters.gdb"

##------------------------
##  1940
##------------------------

## Create list for 1940 polygons
polygons = ["t40_das", "t50_das", "t60_das", "t70_das", "t80_das", "bg90_das"]

## Run loop to generate rasters (takes time)
for p in polygons:
  arcpy.AddField_management(p,"cells30","DOUBLE")
  arcpy.AddField_management(p,"hu40_cells","DOUBLE")
  # Divide Sq. M. by 900 to get 30x30m cell value
  arcpy.CalculateField_management(p,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON3")
  arcpy.CalculateField_management(p,"hu40_cells","!hu40! / !cells30!","PYTHON3")
  
  yr_str = re.findall("\\d+", p)
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    output = os.path.join(outpath, "rast40_" + str(yr))
    # Polygon to Raster: cell center values at 30x30m 
    arcpy.conversion.PolygonToRaster(p, "hu40_cells", output, "CELL_CENTER", "NONE", "30")
    
# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)
    

##------------------------
##  1950
##------------------------

## Create list for 1950 polygons
polygons = ["t50_das50", "t60_das50", "t70_das50", "t80_das50", "bg90_das50"]

## Run loop to generate rasters (takes time)
for p in polygons:
  arcpy.AddField_management(p,"cells30","DOUBLE")
  arcpy.AddField_management(p,"hu50_cells","DOUBLE")
  arcpy.CalculateField_management(p,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON3")
  arcpy.CalculateField_management(p,"hu50_cells","!hu50! / !cells30!","PYTHON3")
  
  yr_str = re.findall("\\d+", p)[:1]  # include only first set of numbers in list item name
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    output = os.path.join(outpath, "rast50_" + str(yr))
    arcpy.conversion.PolygonToRaster(p, "hu50_cells", output, "CELL_CENTER", "NONE", "30")
    
# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)
    
    
##------------------------
##  1960
##------------------------

## Create list for 1960 polygons
polygons = ["t60_das60", "t70_das60", "t80_das60", "bg90_das60"]

## Run loop to generate rasters (takes time)
for p in polygons:
  arcpy.AddField_management(p,"cells30","DOUBLE")
  arcpy.AddField_management(p,"hu60_cells","DOUBLE")
  arcpy.CalculateField_management(p,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON3")
  arcpy.CalculateField_management(p,"hu60_cells","!hu60! / !cells30!","PYTHON3")
  
  yr_str = re.findall("\\d+", p)[:1]  # include only first set of numbers in list item name
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    output = os.path.join(outpath, "rast60_" + str(yr))
    arcpy.conversion.PolygonToRaster(p, "hu60_cells", output, "CELL_CENTER", "NONE", "30")
    
# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)
   
    
##------------------------
##  1970
##------------------------

## Create list for 1970 polygons
polygons = ["t70_das70", "t80_das70", "bg90_das70"]

## Run loop to generate rasters (takes time)
for p in polygons:
  arcpy.AddField_management(p,"cells30","DOUBLE")
  arcpy.AddField_management(p,"hu70_cells","DOUBLE")
  arcpy.CalculateField_management(p,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON3")
  arcpy.CalculateField_management(p,"hu70_cells","!hu70! / !cells30!","PYTHON3")
  
  yr_str = re.findall("\\d+", p)[:1]  # include only first set of numbers in list item name
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    output = os.path.join(outpath, "rast70_" + str(yr))
    arcpy.conversion.PolygonToRaster(p, "hu70_cells", output, "CELL_CENTER", "NONE", "30")
    
# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)
    
  
##------------------------
##  1980
##------------------------

## Create list for 1980 polygons
polygons = ["t80_das80", "bg90_das80"]

## Run loop to generate rasters (takes time)
for p in polygons:
  arcpy.AddField_management(p,"cells30","DOUBLE")
  arcpy.AddField_management(p,"hu80_cells","DOUBLE")
  arcpy.CalculateField_management(p,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON3")
  arcpy.CalculateField_management(p,"hu80_cells","!hu80! / !cells30!","PYTHON3")
  
  yr_str = re.findall("\\d+", p)[:1]  # include only first set of numbers in list item name
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    output = os.path.join(outpath, "rast80_" + str(yr))
    arcpy.conversion.PolygonToRaster(p, "hu80_cells", output, "CELL_CENTER", "NONE", "30")
    
# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)

############################################################
## STEP 2: COMPUTE CELL MAXIMUMS                          ##
############################################################

## Create output GDB
env.workspace = "D:/HIST_HU_URB/gis_files/rasters.gdb"  # set workspace to 
#arcpy.management.CreateFileGDB("D:/HIST_HU_URB/gis_files", "hu_max.gdb")
#outpath = "D:/HIST_HU_URB/gis_files/hu_max.gdb"

## Run Decade-by-Decade---each takes ~1.5-3 hours

## 1940 -------------------------------------------------------
rasters = arcpy.ListRasters("rast40*")[::-1]
arcpy.gp.CellStatistics_sa(rasters, "max40","MAXIMUM", "DATA", "SINGLE_BAND")

# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)

## 1950 -------------------------------------------------------
rasters = arcpy.ListRasters("rast50*")[::-1]
arcpy.gp.CellStatistics_sa(rasters, "max50","MAXIMUM", "DATA", "SINGLE_BAND")

# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)

## 1960 -------------------------------------------------------
rasters = arcpy.ListRasters("rast60*")[::-1]
arcpy.gp.CellStatistics_sa(rasters, "max60","MAXIMUM", "DATA", "SINGLE_BAND")

# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)

## 1970 -------------------------------------------------------
rasters = arcpy.ListRasters("rast70*")[::-1]
arcpy.gp.CellStatistics_sa(rasters, "max70","MAXIMUM", "DATA", "SINGLE_BAND")

# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)

## 1980 -------------------------------------------------------
rasters = arcpy.ListRasters("rast80*")[::-1]
arcpy.gp.CellStatistics_sa(rasters, "max80","MAXIMUM", "DATA", "SINGLE_BAND")

# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers():
        m.removeLayer(lyr)

#######################################################################
## STEP 3: REABSORB HU MAXIMUMS into 2010 TRACT GEOGRAPHIES          ##
#######################################################################

## Prepare Shapefile needed (make sure workspace is set to "rasters.gdb")
db1 = os.path.join(base, "gis_files/database1.gdb")  # establish database1.gdb for export
t10 = os.path.join(db1, "t10")  # using full tract to reabsorb

## Set lists
rasters = arcpy.ListRasters("max*")
outputs = ["tab40", "tab50", "tab60", "tab70", "tab80"]  # check that order matches rasters

## Zonal Statistics (Sum raster cell values by tract)
for i, j in zip(rasters, outputs):
    output = os.path.join(db1, j)
    arcpy.gp.ZonalStatisticsAsTable_sa(t10, "GISJOIN10", i, output, "DATA", "SUM")
    
# Clear Map of Tables
for m in aprx.listMaps():
    for tab in m.listTables():
        m.removeTable(tab)
        

