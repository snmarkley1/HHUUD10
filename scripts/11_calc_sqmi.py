
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####                    PREPARING 2010 TRACTS --> SQMI + NLCD                    ####
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
## STEP 1: CREATE Dasym file for 2010 tract geographies          ##
###################################################################

## Establish workpsace
env.workspace = os.path.join(base, "gis_files/database1.gdb")

## Prepare erase layer--CALCULATING SQMI ELIGIBLE for HOUSING
arcpy.management.MakeFeatureLayer("golf", "golf40", "YEAR < 1941")  # add extra year to account for construction
arcpy.management.MakeFeatureLayer("airports", "airports40", "ACT_DATE < 1943")  # add extra two years to account for construction
erase_features = ["cemeteries", "parks", "rail_buff50", "water", "golf40", "airports40"] 

## Merge erase features to repair geometry
arcpy.management.Merge(erase_features, "dasym10")  # merge erase features together
arcpy.management.RepairGeometry("dasym10")  # fix up merged layer for later use (may take some time)

## Clear layers
for m in aprx.listMaps():
    for lyr in m.listLayers("*0"):
        m.removeLayer(lyr)

###################################################################
## STEP 2: ORGANIZE t10_nlcd file                                ##
###################################################################

## BRING in t10_nlcd file and delete unneeded columns
arcpy.management.MakeFeatureLayer("t10_nlcd", "t10_prep")  # create new layer from t10_nlcd
years = ["40", "50", "60", "70", "80", "90", "00", "10", "19"]  # years list
del_fields = ["hu" + sub for sub in years] + ["OID_1"]  # list of fields to delete

## Delete fields & do erase
arcpy.management.DeleteField('t10_prep', del_fields)
arcpy.analysis.Erase('t10_prep', "dasym10", "t10_prep")

## Clear layers
for m in aprx.listMaps():
    for lyr in m.listLayers("t10*"):
        m.removeLayer(lyr)

## add back to display and calc. sq. mi. for 1940
arcpy.management.AddField("t10_prep", "sqmi40", "DOUBLE")
arcpy.management.CalculateField("t10_prep", "sqmi40", "!Shape.Area@SQUAREMILES!", "PYTHON3")

# establish object & new list (remove 1940)
t10_prep = "t10_prep"
years1 = years[1:]  # no 1940

## Generate decade-by-decade sq. mi. estimates (takes some time)
for yr in years1:
  golf_airport = "golf_airport" + yr
  outlayer = "t10_prep" + yr
  sqmi = "sqmi" + yr
  arcpy.analysis.Erase(t10_prep, golf_airport, outlayer)
  arcpy.management.AddField(outlayer, sqmi, "DOUBLE")
  arcpy.management.CalculateField(outlayer, sqmi, "!Shape.Area@SQUAREMILES!", 'PYTHON3')



###################################################################
## STEP 3: Join into One                                         ##
###################################################################

t10_list = []
for m in aprx.listMaps():
    for lyr in m.listLayers("t10_prep*"):
        t10_list.append(lyr)
        
del_fields = ["pdev92", "pdev92_dhi", "pdev01", "pdev01_dhi", "pdev11", "pdev11_dhi", "p_urb_ren", "sqmi"]
for i in t10_list:
    arcpy.management.DeleteField(i, del_fields)

## Join t10_prep to others
arcpy.management.MakeFeatureLayer("t10_nlcd", "t10_nlcd")
for i in t10_list:
  arcpy.management.AddJoin('t10_nlcd', "GISJOIN", i, "GISJOIN", 'KEEP_ALL')

## Save out
arcpy.conversion.FeatureClassToFeatureClass("t10_nlcd", env.workspace, "t10_join")

## Clear display
for m in aprx.listMaps():
    for lyr in m.listLayers("t10_prep*") + m.listLayers("t10_nlcd"):
        m.removeLayer(lyr)
 
       
## Clean up fields
keep_fields = ["sqmi" + sub for sub in years] +  del_fields[:-1] + ["OBJECTID_12_13_14_15_16_17_18_19", "Shape", "GISJOIN", "Shape_Length", "Shape_Area"]  # keep these fields
del_fields1 = [x.name for x in arcpy.ListFields("t10_join") if x.name not in keep_fields]  # delete these
arcpy.management.DeleteField("t10_join", del_fields1)  # do delete

# calculate non-DR area of t10_join
new_fields = ["sqmi", "pdasym"]
calcs = ["!Shape.Area@SQUAREMILES!", "1 - !sqmi80! / !sqmi!"]  # use 1980 dasym. geography to get most recent picture for 1940-80 interpolations
for i, j in zip(new_fields, calcs):
  arcpy.management.AddField("t10_join", i, "DOUBLE")
  arcpy.management.CalculateField("t10_join", i, j, "PYTHON3")


## Clear display
for m in aprx.listMaps():
    for lyr in m.listLayers("t10*"):
        m.removeLayer(lyr)


###################################################################
## STEP 4: Clean up database1                                    ##
###################################################################

## Create new GDB to house old polygons
arcpy.management.CreateFileGDB(os.path.join(base, "gis_files"), "set_aside.gdb")

## Create lists of old polygons
set_aside_gdb = os.path.join(base, "gis_files", "set_aside.gdb")
polygons = arcpy.ListFeatureClasses("*airport*") + arcpy.ListFeatureClasses("*prep*") + arcpy.ListFeatureClasses("*dasym*") + ["cemeteries", "golf", "industrial_areas", "parks", "rail_buff50", "water"]

## Move polygons to set_aside.gdb
for i in polygons:
  arcpy.conversion.FeatureClassToGeodatabase(i, set_aside_gdb)

## Delete old polygons in database1
for i in polygons:
  if arcpy.Exists(i):
    arcpy.management.Delete(i)
  



