#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####                           VALIDATION 1: PREP                                ####
#### --------------------------------------------------------------------------- ####
#####################################################################################
#####################################################################################

### RUN in Python 3 window of ArcGIS PRO 2.8.1

##################################
## PREPARE WORKSPACE            
##################################

## Import packages
import arcpy, os, re  # need ArcGIS license
from arcpy import env

## Set base folder as workspace
base = "D:/validation"
env.workspace = os.path.join(base, "gis_files")


## Set preferences
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")  # coordinate system in use
env.extent = "MAXOF"  # for raster operations
env.qualifiedFieldNames = False  # good for joins

# Establish Map
aprx = arcpy.mp.ArcGISProject("CURRENT")  # project


##-----------------------------------------
## CREATE GBD
##-----------------------------------------
arcpy.management.CreateFileGDB(env.workspace, "database1.gdb")
output_gdb = os.path.join(env.workspace, "database1.gdb")


#################################################
## STEP 1: MOVE FILES to GDB & Rename          
#################################################

## Grab shapefiles
t19 = os.path.join("tracts_2019", "shp1_tracts1519.shp")
t00 = os.path.join("tracts_2000", "shp2_tracts00.shp")
t90 = os.path.join("tracts_1990", "shp3_tracts90.shp")
bg19 = os.path.join("bgroups_2019", "shp4_bgs19.shp")
t10 = os.path.join("tracts_2010", "shp5_tracts10.shp")


## Put in list (reorder them)
polygons = [t90, t00, t10, bg19, t19]

# send to gdb
for i in polygons:
  arcpy.conversion.FeatureClassToGeodatabase(i, output_gdb)

##------------------------------------
## Rename
##------------------------------------

## Change working directory
env.workspace = output_gdb

## list files and rename
polygons = arcpy.ListFeatureClasses()
new_names = ["t90", "t00", "t10", "bg19", "t19"]

## Rename
for i, j in zip(polygons, new_names):
  arcpy.management.Rename(i, j)


## Add to display 
for i in new_names:
  
  if (i == "bg19" or i == "t19" or i == "t10"):
    arcpy.management.MakeFeatureLayer(i, i)
    
  else: 
    # filter out N tracts
    arcpy.management.MakeFeatureLayer(i, i, "stay = 'Y'")


##################################################
## STEP 2: Prepare for dasymetric refinement
##################################################

## Set folder to get old dasym files
old_gdb = "D:/HIST_HU_URB/gis_files/set_aside.gdb"

# get needed dasym layers and add to display
dasym_layers = [os.path.join(old_gdb, "golf_airport90"), os.path.join(old_gdb, "golf_airport00"), os.path.join(old_gdb, "golf_airport10"),
                os.path.join(old_gdb, "cemeteries"), os.path.join(old_gdb, "industrial_areas"), os.path.join(old_gdb, "parks"), 
                os.path.join(old_gdb, "rail_buff50"), os.path.join(old_gdb, "water")]

for i in dasym_layers:
    arcpy.management.MakeFeatureLayer(i, i)

# create erase_features (remove golf_airport00)
erase_features = dasym_layers[0:1] + dasym_layers[3:]
    
# Merge & move golf_aiport00 & golf_airport10 over
arcpy.management.Merge(erase_features, "dasym90") # merge erase features together --> RUN MANUALLY if ERROR 99999
arcpy.management.RepairGeometry("dasym90")  # fix up merged layer for later use (may take some time)

## Move golf_airport00 to database1
for i in ["golf_airport00", "golf_airport10"]:
  arcpy.conversion.FeatureClassToGeodatabase(i, output_gdb)

## clean up
for m in aprx.listMaps():
  for lyr in m.listLayers("*0") + m.listLayers("*9") + m.listLayers("*e*") + m.listLayers("parks"):
    m.removeLayer(lyr)


##################################################
## STEP 3: Do dasymetric refinement
##################################################

## set list
polygons = new_names  

## Add polygons to display
for i in polygons:
    arcpy.management.MakeFeatureLayer(i, i)
arcpy.management.MakeFeatureLayer("dasym90", "dasym90")  # dasym layer
    
## create dasymetric layers for 1990
polygons00 = []  # prep outlist
for p in polygons:
    outlayer_erase = arcpy.Describe(p).baseName + "_temp"
    arcpy.analysis.Erase(p, "dasym90", outlayer_erase)
    
    outlayer_clip = arcpy.Describe(p).baseName + "_das"
    arcpy.analysis.Clip(outlayer_erase, "t19", outlayer_clip)
    
    yr_str = re.findall("\\d+", p)
    yr_int = [int(yr_int) for yr_int in yr_str]
    
    for yr in yr_int:
        if yr < 90:
            polygons00.append(outlayer_clip)
            
            
## Create dasymetric layer for 2000
polygons10 = []
for p in polygons00:
    outlayer = arcpy.Describe(p).baseName + "00"
    arcpy.analysis.Erase(p, "golf_airport00", outlayer)
    
    yr_str = re.findall("\\d+", p)
    yr_int = [int(yr_int) for yr_int in yr_str]
    
    for yr in yr_int:
        if yr >= 10:
            polygons10.append(outlayer)
            
            
## Create dasymetric layer for 2010
for p in polygons10:
    outlayer = arcpy.Describe(p).baseName[:-2] + "10"
    arcpy.analysis.Erase(p, "golf_airport10", outlayer)


## Clean up
for m in aprx.listMaps():
  for lyr in m.listLayers("*0*") + m.listLayers("*9*") + m.listLayers("dasym"):
    m.removeLayer(lyr)


##################################################
## STEP 4: Calc sq. mi
##################################################

polygons = arcpy.ListFeatureClasses("*_das*")

## Calc. sq. mi.
for p in polygons:
  arcpy.management.AddField(p, "sqmi", "DOUBLE")
  arcpy.management.CalculateField(p, "sqmi", "!Shape.Area@SQUAREMILES!", 'PYTHON3')
  
## Clear Layers from Display
for m in aprx.listMaps():
    for lyr in m.listLayers("*0") + m.listLayers("*das"):
        m.removeLayer(lyr) 


##################################################
## STEP 4b: Add only portions of each
##################################################

for p in polygons:
  
  # 2010 geographies all stay
  if(re.findall("\\d+", p)[:1] == ['19'] or re.findall("\\d+", p)[:1] == ['10']):
    arcpy.management.MakeFeatureLayer(p, p)
  
  ## keep only certain ones
  else:
    arcpy.management.MakeFeatureLayer(p, p, "stay = 'Y'")


##################################################
## STEP 5: Do intersections 
##################################################

## Make lists for Intersection
source_zones = ["t90_das", "t00_das00", "t10_das10"]  # source zones
p_target_zones = ["t00_das", "t10_das00", "bg19_das10"] # pseudo-target zones (SZ + 10) --> exclude 1990 data for 1980
bg_target_zones = ["bg19_das", "bg19_das00", "bg19_das10"]  # 2010 target zones (bg) w/ 1990 HU data

## Intersect and Calculate Sq. Mi. for atom (takes a long time)
for i, j, k in zip(source_zones, p_target_zones, bg_target_zones):
  yr_str = re.findall("\\d+", i)[:1]
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    
    output = "int" + str(yr)
    inputs = i + ";" + j + ";" + k
      
    arcpy.analysis.Intersect(inputs, output, "NO_FID")
    arcpy.management.AddField(output, "sqmi_int", "DOUBLE")
    arcpy.management.CalculateField(output, "sqmi_int", "!Shape.Area@SQUAREMILES!", "PYTHON3")
      
      
## Rename
arcpy.management.Rename("int0", "int00")
      

## Clear Layers from Display
for m in aprx.listMaps():
    for lyr in m.listLayers("*0") + m.listLayers("*9") + m.listLayers("*das"):
        m.removeLayer(lyr)  

 
##################################################
## STEP 6: Rasterization
##################################################       

## Create output GDB
arcpy.management.CreateFileGDB("D:/validation/gis_files", "rasters.gdb")
outpath = "D:/validation/gis_files/rasters.gdb"


##------------------------
##  Add portions of each
##------------------------

polygons = ['t90_das', 't00_das', 't10_das', 'bg19_das', 't00_das00', 't10_das00', 'bg19_das00', 't10_das10', 'bg19_das10']

for p in polygons:
  
  # 2010 geographies all stay
  if(re.findall("\\d+", p)[:1] == ['19'] or re.findall("\\d+", p)[:1] == ['10']):
    arcpy.management.MakeFeatureLayer(p, p)
  
  ## keep only certain ones
  else:
    arcpy.management.MakeFeatureLayer(p, p, "stay = 'Y'")
      
##------------------------
##  1990
##------------------------

polygons = ['t90_das', 't00_das', 't10_das', 'bg19_das']

## Calculate necessary fields
for p in polygons:
  arcpy.AddField_management(p,"cells30","DOUBLE")
  arcpy.AddField_management(p,"hu40_cells","DOUBLE")
  # Divide Sq. M. by 900 to get 30x30m cell value
  arcpy.CalculateField_management(p,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON3")
  
  if p == "t90_das":
    arcpy.CalculateField_management(p,"hu40_cells","!HU1990_90! / !cells30!","PYTHON3")
    
  elif p == "t00_das":
    arcpy.CalculateField_management(p,"hu40_cells","!HU1990_00! / !cells30!","PYTHON3")
    
  elif p == "t10_das":
    arcpy.CalculateField_management(p,"hu40_cells","!HU_1990! / !cells30!","PYTHON3")
  
  else:
    arcpy.CalculateField_management(p,"hu40_cells","!HU1990! / !cells30!","PYTHON3")
  

## Generate rasters
for p in polygons:
  yr_str = re.findall("\\d+", p)
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    output = os.path.join(outpath, "rast90_" + str(yr))
    # Polygon to Raster: cell center values at 30x30m 
    arcpy.conversion.PolygonToRaster(p, "hu40_cells", output, "CELL_CENTER", "NONE", "30")
    
    
## Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers("rast*"):
        m.removeLayer(lyr)


##------------------------
##  2000
##------------------------

polygons = ['t00_das00', 't10_das00', 'bg19_das00']

## Calculate necessary fields
for p in polygons:
  arcpy.AddField_management(p,"cells30","DOUBLE")
  arcpy.AddField_management(p,"hu40_cells","DOUBLE")
  # Divide Sq. M. by 900 to get 30x30m cell value
  arcpy.CalculateField_management(p,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON3")
    
  if p == "t00_das00":
    arcpy.CalculateField_management(p,"hu40_cells","!HU2000_00! / !cells30!","PYTHON3")
    
  elif p == "t10_das00":
    arcpy.CalculateField_management(p,"hu40_cells","!HU_2000! / !cells30!","PYTHON3")
    
  else:
    arcpy.CalculateField_management(p,"hu40_cells","!HU2000! / !cells30!","PYTHON3")
  

## Generate rasters (takes time)
for p in polygons:
  yr_str = re.findall("\\d+", p)
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    output = os.path.join(outpath, "rast00_" + str(yr))
    # Polygon to Raster: cell center values at 30x30m 
    arcpy.conversion.PolygonToRaster(p, "hu40_cells", output, "CELL_CENTER", "NONE", "30")

# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers("*0*"):
        m.removeLayer(lyr)

##------------------------
##  2010
##------------------------

polygons = ['t10_das10', 'bg19_das10']

## Calculate necessary fields
for p in polygons:
  arcpy.AddField_management(p,"cells30","DOUBLE")
  arcpy.AddField_management(p,"hu40_cells","DOUBLE")
  # Divide Sq. M. by 900 to get 30x30m cell value
  arcpy.CalculateField_management(p,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON3")
    
  if p == "t10_das10":
    arcpy.CalculateField_management(p,"hu40_cells","!HU2006_10! / !cells30!","PYTHON3")
    
  else:
    arcpy.CalculateField_management(p,"hu40_cells","!HU2010! / !cells30!","PYTHON3")
  

## Generate rasters (takes time)
for p in polygons:
  yr_str = re.findall("\\d+", p)
  yr_int = [int(yr_int) for yr_int in yr_str]
  
  for yr in yr_int:
    output = os.path.join(outpath, "rast10_" + str(yr))
    # Polygon to Raster: cell center values at 30x30m 
    arcpy.conversion.PolygonToRaster(p, "hu40_cells", output, "CELL_CENTER", "NONE", "30")

# Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers("*1*"):
        m.removeLayer(lyr)


##################################################
## STEP 7: Compute cell maximums
##################################################

## Change workspace
env.workspace = outpath

## Rename
arcpy.management.Rename("rast90_0", "rast90_00")
arcpy.management.Rename("rast00_0", "rast00_00")

## Cell statistics for 1990------------------------------------------------------------
rasters = ["rast90_90", "rast90_00", "rast90_10", "rast90_19"]
arcpy.gp.CellStatistics_sa(rasters, "max90", "MAXIMUM", "DATA", "SINGLE_BAND") 

## Cell statistics for 2000------------------------------------------------------------
rasters = ["rast00_00", "rast00_10", "rast00_19"]
arcpy.gp.CellStatistics_sa(rasters, "max00", "MAXIMUM", "DATA", "SINGLE_BAND") 

## Cell statistics for 2010------------------------------------------------------------
rasters = ["rast10_10", "rast10_19"]
arcpy.gp.CellStatistics_sa(rasters, "max10", "MAXIMUM", "DATA", "SINGLE_BAND") 

## Clear Map
for m in aprx.listMaps():
    for lyr in m.listLayers("max*"):
        m.removeLayer(lyr)
        
        
##################################################
## STEP 8: Reabsorption
##################################################      

# prepare 
db1 = os.path.join(base, "gis_files/database1.gdb")  # establish database1.gdb for export
t19 = os.path.join(db1, "t19")  # using full tract to reabsorb
rasters = ["max90", "max00", "max10"]
outputs = ["tab90", "tab00", "tab10"]

## Zonal statistics
for i, j in zip(rasters, outputs):
    outpath = os.path.join(db1, j)
    arcpy.gp.ZonalStatisticsAsTable_sa(t19, "GISJOIN", i, outpath, "DATA", "SUM")
    
# Clear Map
for m in aprx.listMaps():
    for lyr in m.listTables("tab*"):
        m.removeTable(lyr)


