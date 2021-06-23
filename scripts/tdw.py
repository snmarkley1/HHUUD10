
#######################################################
#######################################################
#### --------------------------------------------- ####
####             INTERSECTION FOR DR-TDW           ####
#### --------------------------------------------- ####
#######################################################
#######################################################

## prepare workspace
import arcpy
import os
from arcpy import env
env.workspace = "C:/Users/scott/Dropbox/urb_proj/shp_work"

## create file gdb for max files
arcpy.CreateFileGDB_management(env.workspace,"tdw.gdb")

## move das tracts to new gdb
polygons = ["tracts1940/tracts40_das.shp","tracts1940/tracts50_das.shp","tracts1940/tracts60_das.shp",
            "tracts1940/tracts70_das.shp","tracts1940/tracts80_das.shp","tracts1940/tracts90_das.shp",
            "tracts1940/tracts10_das.shp"]

## move FCs to new GDB
for i in polygons:
  arcpy.FeatureClassToGeodatabase_conversion(i,"tdw.gdb")
            
## golf update for 1950 - 1980
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf50","YEAR < 1951")
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf60","YEAR < 1961")
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf70","YEAR < 1971")
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf80","YEAR < 1981")

##########################################################
#### do intersections and update dasym areas by decade
########################################################## 

## change workspace
env.workspace = "C:/Users/scott/Dropbox/urb_proj/shp_work/tdw.gdb"

##### 1940/50 int
arcpy.Intersect_analysis("tracts10_das;tracts40_das;tracts50_das", "int40_50", "NO_FID", "", "INPUT")


##### calc 50/60 new area and then do intersection
polygons = ["tracts50_das","tracts60_das","tracts10_das"]

for i in polygons:
  output = arcpy.Describe(i).baseName + "50"
  arcpy.Erase_analysis(i, "golf50", output)

## now calc geometry and do intersect
polygons = ["tracts10_das50","tracts50_das50", "tracts60_das50"]

for i in polygons:
  arcpy.CalculateField_management(i, "sqmi", "!Shape.Area@SQUAREMILES!", "PYTHON_9.3")

inputs = ";".join(polygons)
arcpy.Intersect_analysis(inputs, "int50_60", "NO_FID", "", "INPUT")

## Delete das50s
del_fc = arcpy.ListFeatureClasses("*das50")
for i in del_fc:
   fc_path = os.path.join(env.workspace,i)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)



##### calc 60/70 new area and then do intersection
polygons = ["tracts60_das","tracts70_das","tracts10_das"]

for i in polygons:
  output = arcpy.Describe(i).baseName + "60"
  arcpy.Erase_analysis(i, "golf60", output)

## now calc geometry and do intersect
polygons = ["tracts10_das60", "tracts60_das60", "tracts70_das60"]

for i in polygons:
  arcpy.CalculateField_management(i, "sqmi", "!Shape.Area@SQUAREMILES!", "PYTHON_9.3")

inputs = ";".join(polygons)
arcpy.Intersect_analysis(inputs, "int60_70", "NO_FID", "", "INPUT")

## Delete das60s
del_fc = arcpy.ListFeatureClasses("*das60")
for i in del_fc:
   fc_path = os.path.join(env.workspace,i)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)
      
      


##### calc 70/80 new area and then do intersection
polygons = ["tracts70_das","tracts80_das","tracts10_das"]

for i in polygons:
  output = arcpy.Describe(i).baseName + "70"
  arcpy.Erase_analysis(i, "golf70", output)

## now calc geometry and do intersect
polygons = ["tracts10_das70", "tracts70_das70", "tracts80_das70"]

for i in polygons:
  arcpy.CalculateField_management(i, "sqmi", "!Shape.Area@SQUAREMILES!", "PYTHON_9.3")

inputs = ";".join(polygons)
arcpy.Intersect_analysis(inputs, "int70_80", "NO_FID", "", "INPUT")

## Delete das70's
del_fc = arcpy.ListFeatureClasses("*das70")
for i in del_fc:
   fc_path = os.path.join(env.workspace,i)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)




##### calc 80/90 new area and then do intersection
polygons = ["tracts80_das","tracts90_das","tracts10_das"]

for i in polygons:
  output = arcpy.Describe(i).baseName + "80"
  arcpy.Erase_analysis(i, "golf80", output)

## now calc geometry and do intersect
polygons = ["tracts10_das80", "tracts80_das80", "tracts90_das80"]

for i in polygons:
  arcpy.CalculateField_management(i, "sqmi", "!Shape.Area@SQUAREMILES!", "PYTHON_9.3")

inputs = ";".join(polygons)
arcpy.Intersect_analysis(inputs, "int80_90", "NO_FID", "", "INPUT")

## Delete das80s
del_fc = arcpy.ListFeatureClasses("*das80")
for i in del_fc:
   fc_path = os.path.join(env.workspace,i)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## caculate areas (SQMI) for all files
for i in arcpy.ListFeatureClasses("int*"):
     arcpy.AddField_management(i,"sqmi_int","DOUBLE")
     arcpy.CalculateField_management(i,"sqmi_int","!Shape.Area@SQUAREMILES!","PYTHON_9.3")
     

###########################################################################################
### ----------------------------------------------------------------------------------- ###
###                            TDW w/ 1990 in 2010 tracts                               ###
### ----------------------------------------------------------------------------------- ###
###########################################################################################


env.workspace = "C:/Users/scott/Dropbox/urb_proj/shp_work"

## create file gdb for max files
arcpy.CreateFileGDB_management(env.workspace,"tdw90.gdb")

## join tracts10_das with data


## move das tracts to new gdb
polygons = ["tracts1940/tracts40_das.shp","tracts1940/tracts50_das.shp","tracts1940/tracts60_das.shp",
            "tracts1940/tracts70_das.shp","tracts1940/tracts80_das.shp","tracts1940/tracts10_das.shp"]

## move FCs to new GDB
for i in polygons:
  arcpy.FeatureClassToGeodatabase_conversion(i,"tdw90.gdb")
  
## move tables there
arcpy.TableToGeodatabase_conversion("C:/Users/scott/Dropbox/urb_proj/tables/ysb90_10.dbf","tdw90.gdb")


## golf update for 1950 - 1980
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf50","YEAR < 1951")
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf60","YEAR < 1961")
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf70","YEAR < 1971")
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf80","YEAR < 1981")
    
## switch workspace and join dbf to 2010 tracts
env.workspace = "C:/Users/scott/Dropbox/urb_proj/shp_work/tdw90.gdb"

## join shapefile with ysb info in dbf
arcpy.MakeFeatureLayer_management("tracts10_das","t10_layer")
arcpy.AddJoin_management("t10_layer","GISJOIN","ysb90_10","GISJOIN","KEEP_ALL")
arcpy.CopyFeatures_management("t10_layer","t10_ysb90")

## clean up
arcpy.DeleteField_management("t10_ysb90","GISJOIN_1")

###################################################################
###                     DO INTERSECTIONS                        ###
###################################################################

##### 1940/50 int
arcpy.Intersect_analysis("t10_ysb90;tracts40_das", "int40_50", "NO_FID", "", "INPUT")


##### calc 50/60 new area and then do intersection
polygons = ["t10_ysb90","tracts50_das"]

for i in polygons:
  output = arcpy.Describe(i).baseName + "50"
  arcpy.Erase_analysis(i, "golf50", output)

## now calc geometry and do intersect
#polygons = ["tracts50_das;t10_ysb90"]

for i in polygons:
  arcpy.CalculateField_management(i, "sqmi", "!Shape.Area@SQUAREMILES!", "PYTHON_9.3")

inputs = ";".join(polygons)
arcpy.Intersect_analysis(inputs, "int50_60", "NO_FID", "", "INPUT")

## Delete das50s
del_fc = arcpy.ListFeatureClasses("*das50")
for i in del_fc:
   fc_path = os.path.join(env.workspace,i)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)



##### calc 60/70 new area and then do intersection
polygons = ["t10_ysb90","tracts60_das"]

for i in polygons:
  output = arcpy.Describe(i).baseName + "60"
  arcpy.Erase_analysis(i, "golf60", output)

## now calc geometry and do intersect
#polygons = ["tracts60_das;t10_ysb90"]

for i in polygons:
  arcpy.CalculateField_management(i, "sqmi", "!Shape.Area@SQUAREMILES!", "PYTHON_9.3")

inputs = ";".join(polygons)
arcpy.Intersect_analysis(inputs, "int60_70", "NO_FID", "", "INPUT")

## Delete das60s
del_fc = arcpy.ListFeatureClasses("*das60")
for i in del_fc:
   fc_path = os.path.join(env.workspace,i)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)
      
      


##### calc 70/80 new area and then do intersection
polygons = ["t10_ysb90","tracts70_das"]

for i in polygons:
  output = arcpy.Describe(i).baseName + "70"
  arcpy.Erase_analysis(i, "golf70", output)

## now calc geometry and do intersect
#polygons = ["tracts70_das;t10_ysb90"]

for i in polygons:
  arcpy.CalculateField_management(i, "sqmi", "!Shape.Area@SQUAREMILES!", "PYTHON_9.3")

inputs = ";".join(polygons)
arcpy.Intersect_analysis(inputs, "int70_80", "NO_FID", "", "INPUT")

## Delete das70's
del_fc = arcpy.ListFeatureClasses("*das70")
for i in del_fc:
   fc_path = os.path.join(env.workspace,i)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)




##### calc 80/90 new area and then do intersection
polygons = ["t10_ysb90","tracts80_das"]

for i in polygons:
  output = arcpy.Describe(i).baseName + "80"
  arcpy.Erase_analysis(i, "golf80", output)

## now calc geometry and do intersect
#polygons = ["tracts80_das","t10_ysb90"]

for i in polygons:
  arcpy.CalculateField_management(i, "sqmi", "!Shape.Area@SQUAREMILES!", "PYTHON_9.3")

inputs = ";".join(polygons)
arcpy.Intersect_analysis(inputs, "int80_90", "NO_FID", "", "INPUT")

## Delete das80s
del_fc = arcpy.ListFeatureClasses("*das80")
for i in del_fc:
   fc_path = os.path.join(env.workspace,i)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## caculate areas (SQMI) for all files
for i in arcpy.ListFeatureClasses("int*"):
     arcpy.AddField_management(i,"sqmi_int","DOUBLE")
     arcpy.CalculateField_management(i,"sqmi_int","!Shape.Area@SQUAREMILES!","PYTHON_9.3")
     

