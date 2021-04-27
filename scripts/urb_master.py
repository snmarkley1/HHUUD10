
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
#### --------------------------------------------------------------------------- ####
####                          REABSORPTION PROCESS 1/1/2021                      ####
#### --------------------------------------------------------------------------- ####
#### --------------------------------------------------------------------------- #### 
#####################################################################################
#####################################################################################

## prepare workspace
import arcpy
import os
import re  ## string package

from arcpy import env
#env.workspace = "C:/Users/scott/Dropbox/MappingData/urbanization2"
env.workspace = "D:/MappingData/urbanization2"
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")
env.extent = "MAXOF"

#######################################################
#######################################################
#### --------------------------------------------- ####
####            1940 REABSORPTION PROCESS          ####
#### --------------------------------------------- ####
#######################################################
#######################################################

## create file gdb for max files
arcpy.CreateFileGDB_management(env.workspace,"urb_max.gdb")
#arcpy.FeatureClassToGeodatabase_conversion("tracts1/tracts10.shp","urb_max.gdb")

#########################################################
####         STEP 1: CREATE DASYMETRIC ZONES         ####
#########################################################

## create new folder
arcpy.management.CreateFolder(env.workspace,"tracts1940")

## set up polygons list (1940-1990)
polygons = ["tracts1/tracts40.shp","tracts1/tracts50.shp","tracts1/tracts60.shp",
            "tracts1/tracts70.shp","tracts1/tracts80.shp","tracts1/bg90.shp"]

## bring in erase features
parks = "clip_out/parks_fin.shp"
rec_areas = "clip_out/rec_areas_fin.shp"
water = "clip_out/water_fin1.shp"
cemeteries = "clip_out/cemeteries.shp"

## include only golf courses built before 1940
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf40","YEAR < 1941")
golf = "golf40"

## make list
erase_features = [parks,rec_areas,water,cemeteries,golf]

## merge erase features to smooth next step
merge_fc = os.path.join("clip_out","merge40")
arcpy.Merge_management(erase_features,merge_fc)
arcpy.RepairGeometry_management("merge40")

## for loop: erase features from all decades' polygons and iterates thru names
for poly in polygons:
   output = os.path.join("tracts1940",arcpy.Describe(poly).baseName + "_das")
   arcpy.Erase_analysis(poly,"merge40",output)

## fix hu40 in 1940 and 1950 polygons
## 1940
tracts40 = "tracts1940/tracts40_das.shp"
arcpy.AddField_management(tracts40,"hu40","DOUBLE","","","","","NULLABLE","NON_REQUIRED","")
arcpy.CalculateField_management(tracts40,"hu40","[hu]","VB","")
arcpy.DeleteField_management(tracts40,"hu")

## 1950
tracts50 = "tracts1940/tracts50_das.shp"
arcpy.AddField_management(tracts50,"hu40","DOUBLE","","","","","NULLABLE","NON_REQUIRED","")
arcpy.CalculateField_management(tracts50,"hu40","[bb1940]","VB","")
arcpy.DeleteField_management(tracts50,"bb1940")
   
############################################################################
####        STEP 1B: MOVE ALL NEW POLYGONS to NEW FILE GEODATABASE      ####
############################################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
      arcpy.mapping.RemoveLayer(df, lyr)
      
## create new polygon list
env.workspace = "C:/Users/scott/Dropbox/MappingData/urbanization2/tracts1940"
polygons = arcpy.ListFeatureClasses()


## create new gdb and move polygons there
env.workspace = "C:/Users/scott/Dropbox/MappingData/urbanization2"
arcpy.CreateFileGDB_management(env.workspace,"urb40.gdb")
for i in polygons:
   x = "tracts1940/" + i
   arcpy.FeatureClassToGeodatabase_conversion(x,"urb40.gdb")

######################################################
####        STEP 2: CONVERT TO RASTER CELLS       ####
######################################################

## set new workspace
env.workspace = "C:/Users/scott/Dropbox/MappingData/urbanization2/urb40.gdb"

## set list
polygons = arcpy.ListFeatureClasses()

## add field
for i in polygons:
     arcpy.AddField_management(i,"cells30","DOUBLE")
     arcpy.AddField_management(i,"hu40_cells","DOUBLE")
     
## calculate hu40/30x30m cell
for i in polygons:
    arcpy.CalculateField_management(i,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON_9.3")
    arcpy.CalculateField_management(i,"hu40_cells","!hu40! / !cells30!","PYTHON_9.3")

## raster conversion using hu40_cells
## set output
outputs = []    
for poly in polygons:
   s = re.findall("\\d+",poly)
   t = [int(t) for t in s]
   
   for i in t:
      x = "rast" + str(i)
      outputs.append(x)
      
## convert to rasters
for i, j, in zip(polygons,outputs):
   arcpy.PolygonToRaster_conversion(i,"hu40_cells",j,"CELL_CENTER","NONE","30")
   
############################################################
####        STEP 3: DO CELL STATISTICS (MAXIMUM)        ####
############################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
      arcpy.mapping.RemoveLayer(df, lyr)

## list rasters
rasters = arcpy.ListRasters()

## set output
output = "C:/Users/scott/Dropbox/MappingData/urbanization2/urb_max.gdb/max40"

## cell statistics
arcpy.gp.CellStatistics_sa(rasters,output,"MAXIMUM","DATA")

###############################################
####      CLEANUP to SAVE DISK SPACE       ####
###############################################

## select all rasters except 1960
#rast_del = []
#for rast in rasters:
#     s = re.findall("\\d+",rast)
#     if s != [u"60"]:
#         rast_del.append(rast)

## delete rasters
for raster in rasters:
   fc_path = os.path.join(env.workspace,raster)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## delete polygons  
for poly in polygons:
   fc_path = os.path.join(env.workspace,poly)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## delete gdb
if arcpy.Exists(env.workspace):
     arcpy.Delete_management(env.workspace)


#######################################################
#######################################################
#### --------------------------------------------- ####
####            1950 REABSORPTION PROCESS          ####
#### --------------------------------------------- ####
#######################################################
#######################################################

## prepare workspace
import arcpy
import os
import re  ## string package

from arcpy import env
#env.workspace = "C:/Users/scott/Dropbox/MappingData/urbanization2"
env.workspace = "D:/MappingData/urbanization2"
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")
env.extent = "MAXOF"

#################################################################################################
####         STEP 1: MOVE FILES OVER TO NEW FILE GEODATABASE AND UPDATE DASYMETRIC ZONES     ####
#################################################################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
     arcpy.mapping.RemoveLayer(df, lyr)

## create file gdb
arcpy.CreateFileGDB_management(env.workspace,"urb50.gdb")


## set lists for feature classes
fc = ["tracts1940/tracts50_das.shp","tracts1940/tracts60_das.shp","tracts1940/tracts70_das.shp",
      "tracts1940/tracts80_das.shp","tracts1940/bg90_das.shp"]

## move rest to geodatabase
for i in fc:
   arcpy.FeatureClassToGeodatabase_conversion(i,"urb50.gdb")

## golf update for 1950
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf50","YEAR < 1951")

## change workspace
#env.workspace = "C:/Users/scott/Dropbox/MappingData/urbanization2/urb50.gdb"
env.workspace = "D:/MappingData/urbanization2/urb50.gdb"

## make list for erase
polygons = arcpy.ListFeatureClasses("*_das")

## cut out new golf courses from das polygons
for poly in polygons:
   output = os.path.join(arcpy.Describe(poly).baseName + "50")
   arcpy.Erase_analysis(poly,"golf50",output)
   
## cleanup
fc = arcpy.ListFeatureClasses("*_das")  # create list of feature classes to delete
for i in fc:
     fc_path = os.path.join(env.workspace,i)
     if arcpy.Exists(fc_path):
        arcpy.Delete_management(fc_path)

######################################################
####        STEP 2: CONVERT TO RASTER CELLS       ####
######################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
      arcpy.mapping.RemoveLayer(df, lyr)

## set list
polygons = arcpy.ListFeatureClasses("*das50")

## add field
for poly in polygons:
     arcpy.AddField_management(poly,"cells30","DOUBLE")
     arcpy.AddField_management(poly,"hu50_cells","DOUBLE")
     
## calculate hu50/30x30m cell
for poly in polygons:
    arcpy.CalculateField_management(poly,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON_9.3")
    arcpy.CalculateField_management(poly,"hu50_cells","!hu50! / !cells30!","PYTHON_9.3")

## raster conversion using hu50_cells
## set output
outputs = []    
for poly in polygons:
   s = re.findall("\\d+",poly)[0:1]  ## returns first two numbers
   t = [int(t) for t in s]
   
   for i in t:
      x = "rast" + str(i)
      outputs.append(x)
      
## convert to rasters
for i, j, in zip(polygons,outputs):
   arcpy.PolygonToRaster_conversion(i,"hu50_cells",j,"CELL_CENTER","NONE","30")

         
############################################################
####        STEP 3: DO CELL STATISTICS (MAXIMUM)        ####
############################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
      arcpy.mapping.RemoveLayer(df, lyr)

## list rasters
rasters = arcpy.ListRasters()

## set output
#output = "C:/Users/scott/Dropbox/MappingData/urbanization2/urb_max.gdb/max50"
output = "D:/MappingData/urbanization2/urb_max.gdb/max50"

## cell statistics
arcpy.gp.CellStatistics_sa(rasters,output,"MAXIMUM","DATA")

###############################################
####      CLEANUP to SAVE DISK SPACE       ####
###############################################

## delete rasters
for raster in rasters:
   fc_path = os.path.join(env.workspace,raster)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## delect those rasters
for rast in rast_del:
   fc_path = os.path.join(env.workspace,rast)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## delete polygons  
for poly in polygons:
   fc_path = os.path.join(env.workspace,poly)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)


#######################################################
#######################################################
#### --------------------------------------------- ####
####            1960 REABSORPTION PROCESS          ####
#### --------------------------------------------- ####
#######################################################
#######################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
      arcpy.mapping.RemoveLayer(df, lyr)

## prepare workspace
import arcpy
import os
import re  ## string package

from arcpy import env
#env.workspace = "C:/Users/scott/Dropbox/MappingData/urbanization2"
env.workspace = "D:/MappingData/urbanization2"
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")
env.extent = "MAXOF"

#################################################################################################
####         STEP 1: MOVE FILES OVER TO NEW FILE GEODATABASE AND UPDATE DASYMETRIC ZONES     ####
#################################################################################################

## create file gdb
arcpy.CreateFileGDB_management(env.workspace,"urb60.gdb")

## set lists for feature classes
fc = ["tracts1940/tracts60_das.shp","tracts1940/tracts70_das.shp",
      "tracts1940/tracts80_das.shp","tracts1940/bg90_das.shp"]

## move rest to geodatabase
arcpy.FeatureClassToGeodatabase_conversion(fc,"urb60.gdb")

## golf update for 1960
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf60","YEAR < 1961")

## change workspace
#env.workspace = "C:/Users/scott/Dropbox/MappingData/urbanization2/urb60.gdb"
env.workspace = "D:/MappingData/urbanization2/urb60.gdb"

## make list for erase
polygons = arcpy.ListFeatureClasses("*_das")

## cut out new golf courses from das polygons
for poly in polygons:
   output = os.path.join(arcpy.Describe(poly).baseName + "60")
   arcpy.Erase_analysis(poly,"golf60",output)
   
## cleanup
fc = arcpy.ListFeatureClasses("*_das")  # create list of feature classes to delete
for i in fc:
     fc_path = os.path.join(env.workspace,i)
     if arcpy.Exists(fc_path):
        arcpy.Delete_management(fc_path)


######################################################
####        STEP 2: CONVERT TO RASTER CELLS       ####
######################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
      arcpy.mapping.RemoveLayer(df, lyr)


## set list
polygons = arcpy.ListFeatureClasses()

## add field
for poly in polygons:
     arcpy.AddField_management(poly,"cells30","DOUBLE")
     arcpy.AddField_management(poly,"hu60_cells","DOUBLE")
     
## calculate hu60/30x30m cell
for poly in polygons:
    arcpy.CalculateField_management(poly,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON_9.3")
    arcpy.CalculateField_management(poly,"hu60_cells","!hu60! / !cells30!","PYTHON_9.3")

## raster conversion using hu60_cells
## set output
outputs = []    
for poly in polygons:
   s = re.findall("\\d+",poly)[0:1]  ## returns first two numbers
   t = [int(t) for t in s]
   
   for i in t:
      x = "rast" + str(i)
      outputs.append(x)

## convert to rasters
for i, j, in zip(polygons,outputs):
   arcpy.PolygonToRaster_conversion(i,"hu60_cells",j,"CELL_CENTER","NONE","30")

         
############################################################
####        STEP 3: DO CELL STATISTICS (MAXIMUM)        ####
############################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
      arcpy.mapping.RemoveLayer(df, lyr)


## list rasters
rasters = arcpy.ListRasters("rast*")

## set output
## campus computers use E drive instead of D drive
output = "E:/MappingData/urbanization2/urb_max.gdb/max60"

## cell statistics
arcpy.gp.CellStatistics_sa(rasters,output,"MAXIMUM","DATA")

###############################################
####      CLEANUP to SAVE DISK SPACE       ####
###############################################

## delete rasters
for raster in rasters:
   fc_path = os.path.join(env.workspace,raster)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## delete polygons  
for poly in polygons:
   fc_path = os.path.join(env.workspace,poly)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## delete gdb
if arcpy.Exists(env.workspace):
     arcpy.Delete_management(env.workspace)


#######################################################
#######################################################
#### --------------------------------------------- ####
####            1970 REABSORPTION PROCESS          ####
#### --------------------------------------------- ####
#######################################################
#######################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
      arcpy.mapping.RemoveLayer(df, lyr)

## prepare workspace
import arcpy
import os
import re  ## string package

from arcpy import env
#env.workspace = "C:/Users/scott/Dropbox/MappingData/urbanization2"
env.workspace = "E:/MappingData/urbanization2"
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")
env.extent = "MAXOF"

#################################################################################################
####         STEP 1: MOVE FILES OVER TO NEW FILE GEODATABASE AND UPDATE DASYMETRIC ZONES     ####
#################################################################################################

## create file gdb
arcpy.CreateFileGDB_management(env.workspace,"urb70.gdb")

## set lists for feature classes
fc = ["tracts1940/tracts70_das.shp","tracts1940/tracts80_das.shp","tracts1940/bg90_das.shp"]

## move rest to geodatabase
arcpy.FeatureClassToGeodatabase_conversion(fc,"urb70.gdb")

## golf update for 1970
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf70","YEAR < 1971")

## change workspace
env.workspace = "E:/MappingData/urbanization2/urb70.gdb"

## make list for erase
polygons = arcpy.ListFeatureClasses("*_das")

## cut out new golf courses from das polygons
for poly in polygons:
   output = os.path.join(arcpy.Describe(poly).baseName + "70")
   arcpy.Erase_analysis(poly,"golf70",output)
   
## cleanup
fc = arcpy.ListFeatureClasses("*_das")  # create list of feature classes to delete
for i in fc:
     fc_path = os.path.join(env.workspace,i)
     if arcpy.Exists(fc_path):
        arcpy.Delete_management(fc_path)


######################################################
####        STEP 2: CONVERT TO RASTER CELLS       ####
######################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
      arcpy.mapping.RemoveLayer(df, lyr)

## set list
polygons = arcpy.ListFeatureClasses()

## add field
for poly in polygons:
     arcpy.AddField_management(poly,"cells30","DOUBLE")
     arcpy.AddField_management(poly,"hu70_cells","DOUBLE")
     
## calculate hu70/30x30m cell
for poly in polygons:
    arcpy.CalculateField_management(poly,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON_9.3")
    arcpy.CalculateField_management(poly,"hu70_cells","!hu70! / !cells30!","PYTHON_9.3")

## raster conversion using hu70_cells
## set output
outputs = []    
for poly in polygons:
   s = re.findall("\\d+",poly)[0:1]  ## returns first two numbers
   t = [int(t) for t in s]
   
   for i in t:
      x = "rast" + str(i)
      outputs.append(x)
      
## convert to rasters
for i, j, in zip(polygons,outputs):
   arcpy.PolygonToRaster_conversion(i,"hu70_cells",j,"CELL_CENTER","NONE","30")

         
############################################################
####        STEP 3: DO CELL STATISTICS (MAXIMUM)        ####
############################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
      arcpy.mapping.RemoveLayer(df, lyr)


## list rasters
rasters = arcpy.ListRasters()

## set output
output = "E:/MappingData/urbanization2/urb_max.gdb/max70"

## cell statistics
arcpy.gp.CellStatistics_sa(rasters,output,"MAXIMUM","DATA")

###############################################
####      CLEANUP to SAVE DISK SPACE       ####
###############################################

## delete rasters
rasters = arcpy.ListRasters()
for raster in rasters:
   fc_path = os.path.join(env.workspace,raster)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## delete polygons  
for poly in polygons:
   fc_path = os.path.join(env.workspace,poly)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## delete gdb
if arcpy.Exists(env.workspace):
     arcpy.Delete_management(env.workspace)


#######################################################
#######################################################
#### --------------------------------------------- ####
####            1980 REABSORPTION PROCESS          ####
#### --------------------------------------------- ####
#######################################################
#######################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
      arcpy.mapping.RemoveLayer(df, lyr)

## prepare workspace
import arcpy
import os
import re  ## string package

from arcpy import env
#env.workspace = "C:/Users/scott/Dropbox/MappingData/urbanization2"
env.workspace = "E:/MappingData/urbanization2"
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")
env.extent = "MAXOF"

#################################################################################################
####         STEP 1: MOVE FILES OVER TO NEW FILE GEODATABASE AND UPDATE DASYMETRIC ZONES     ####
#################################################################################################

## create file gdb
arcpy.CreateFileGDB_management(env.workspace,"urb80.gdb")

## set lists for feature classes
fc = ["tracts1940/tracts80_das.shp","tracts1940/bg90_das.shp"]

## move rest to geodatabase
arcpy.FeatureClassToGeodatabase_conversion(fc,"urb80.gdb")

## golf update for 1980
arcpy.MakeFeatureLayer_management("clip_out/golf_final.shp","golf80","YEAR < 1981")

## change workspace
env.workspace = "E:/MappingData/urbanization2/urb80.gdb"

## make list for erase
polygons = arcpy.ListFeatureClasses("*_das")

## cut out new golf courses from das polygons
for poly in polygons:
   output = os.path.join(arcpy.Describe(poly).baseName + "80")
   arcpy.Erase_analysis(poly,"golf80",output)
   
## cleanup
fc = arcpy.ListFeatureClasses("*_das")  # create list of feature classes to delete
for i in fc:
     fc_path = os.path.join(env.workspace,i)
     if arcpy.Exists(fc_path):
        arcpy.Delete_management(fc_path)


######################################################
####        STEP 2: CONVERT TO RASTER CELLS       ####
######################################################

## clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
    arcpy.mapping.RemoveLayer(df, lyr)

## set list
polygons = arcpy.ListFeatureClasses()

## add field
for poly in polygons:
     arcpy.AddField_management(poly,"cells30","DOUBLE")
     arcpy.AddField_management(poly,"hu80_cells","DOUBLE")
     
## calculate hu80/30x30m cell
for poly in polygons:
    arcpy.CalculateField_management(poly,"cells30","!Shape.Area@SQUAREMETERS! / 900","PYTHON_9.3")
    arcpy.CalculateField_management(poly,"hu80_cells","!hu80! / !cells30!","PYTHON_9.3")

## raster conversion using hu80_cells
## set output
outputs = []    
for poly in polygons:
   s = re.findall("\\d+",poly)[0:1]  ## returns first two numbers
   t = [int(t) for t in s]
   
   for i in t:
      x = "rast" + str(i)
      outputs.append(x)
      
## convert to rasters
for i, j, in zip(polygons,outputs):
   arcpy.PolygonToRaster_conversion(i,"hu80_cells",j,"CELL_CENTER","NONE","30")

         
############################################################
####        STEP 3: DO CELL STATISTICS (MAXIMUM)        ####
############################################################

##  clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
     arcpy.mapping.RemoveLayer(df, lyr)

## list rasters
rasters = arcpy.ListRasters()

## set output
output = "E:/MappingData/urbanization2/urb_max.gdb/max80"

## cell statistics
arcpy.gp.CellStatistics_sa(rasters,output,"MAXIMUM","DATA")

###############################################
####      CLEANUP to SAVE DISK SPACE       ####
###############################################

## delete rasters
for raster in rasters:
   fc_path = os.path.join(env.workspace,raster)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## delete polygons  
for poly in polygons:
   fc_path = os.path.join(env.workspace,poly)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## delete gdb
if arcpy.Exists(env.workspace):
     arcpy.Delete_management(env.workspace)


###################################################################
###################################################################
#### --------------------------------------------------------- ####
####            REABSORB RASTER CELLS INTO 2010 TRACTS         ####
#### --------------------------------------------------------- ####
###################################################################
###################################################################

##  clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
     arcpy.mapping.RemoveLayer(df, lyr)
     
##############################################################
##       PATCH: Dasymetric process with 2010 tracts         ##
##############################################################

env.workspace = "E:/MappingData/urbanization2"

t10 = "tracts1/tracts10.shp"
erase = "clip_out/merge40.shp"

arcpy.Erase_analysis(t10,erase,"urb_max.gdb/t10_das")

##############################################################
####                         RESUME                       ####
##############################################################

## set workspace
env.workspace = "E:/MappingData/urbanization2/urb_max.gdb"

## set lists
rasters = arcpy.ListRasters()
outputs = []    
for rast in rasters:
    s = re.findall("\\d+",rast)
    t = [int(t) for t in s]
    
    for i in t:
       x = "tab" + str(i)
       outputs.append(x)

## problems with above; trying:
for i,j in zip(rasters,outputs):
     arcpy.gp.ZonalStatisticsAsTable_sa("t10_das","GISJOIN",i,j,"DATA","SUM")
     
######################################
####       JOIN and CLEAN         ####
######################################

## join tables prep
tables = arcpy.ListTables()
tracts10 = "t10_das.shp"

## clean up tracts10
arcpy.DeleteField_management(tracts10,["ALAND10","AWATER10"])
#arcpy.AddIndex_management(tracts10,"GISJOIN","","NON_UNIQUE","NON_ASCENDING")

## join
for tab in tables:
   arcpy.AddJoin_management(tracts10,"GISJOIN",tab,"GISJOIN","KEEP_ALL")
   
## save separate
arcpy.FeatureClassToFeatureClass_conversion(tracts10,env.workspace,"t40_80","","","")

## make lists to clean up columns
list1 =  ["_OBJECTID","_ZONE_CODE","_GISJOIN","_COUNT","_AREA"]
list2 = []
for i in tables:
     for j in list1:
         k = i + j
         list2.append(k)

## delete fields
arcpy.DeleteField_management("t40_80",list2)

###################################################################
###################################################################
#### --------------------------------------------------------- ####
####              VECTOR CALCULATIONS ACROSS DECADES           ####
#### --------------------------------------------------------- ####
###################################################################
###################################################################

## prepare workspace
import arcpy
import os
import re  ## string package

from arcpy import env
#env.workspace = "C:/Users/scott/Dropbox/MappingData/urbanization2"
env.workspace = "D:/MappingData/urbanization2"
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")
env.extent = "MAXOF"

######################################################
###        CALCUALATE SQMI FOR EACH DECADE         ###
######################################################

## new workspace
#env.workspace = "E:/MappingData/urbanization2"  ## campus comp
env.workspace = "D:/MappingData/urbanization2"  ## home comp

## new gdb
arcpy.CreateFileGDB_management(env.workspace,"urb40_80.gdb")

## local vars
#merge40 = "clip_out/merge40.shp"
golf = "clip_out/golf_final.shp"
t40_80 = "urb_max.gdb/t40_80"

## move feature classes to new gdb
arcpy.FeatureClassToGeodatabase_conversion([golf,t40_80],"urb40_80.gdb")

## change workspace and t40_80 to new, moved file
env.workspace = "D:/MappingData/urbanization2/urb40_80.gdb"
t40_80 = "t40_80"
golf = "golf_final"

## golf shapes
golfs = []
for i in range(50,90,10):
   x = "golf" + str(i)
   golfs.append(x)
   
equations = ["YEAR < 1951","YEAR < 1961","YEAR < 1971","YEAR < 1981"]

## golf outputs
for i,j in zip(golfs,equations):
   arcpy.MakeFeatureLayer_management(golf,i,j)

## give new sqmi fields
fields = arcpy.ListFields(t40_80,"tab*")
fields = [x.name for x in fields]
new_fields = []
for i in fields:
   s = re.findall("\\d+",i)
   t = [int(t) for t in s]
   for j in t:
      x = "sqmi" + str(j)
      new_fields.append(x)
      
for i in new_fields:
   arcpy.AddField_management(t40_80,i,"DOUBLE")
   
## calculate 1940 sqmi
arcpy.CalculateField_management(t40_80,"sqmi40","!Shape.Area@SQUAREMILES!","PYTHON_9.3")

## calculate the rest
outputs = []
for i in range(50,90,10):
   x = "t40_80_" + str(i)
   outputs.append(x)

## create new erased polygons
for i,j in zip(golfs,outputs):
     arcpy.Erase_analysis(t40_80,i,j)
     
## rename t40_80 to t40_80_40
arcpy.Rename_management("t40_80","t40_80_40")
     
## do calcs and then join to main t40_80 later
new_fields2 = new_fields[1:]
for i,j in zip(outputs,new_fields2):
   arcpy.CalculateField_management(i,j,"!Shape.Area@SQUAREMILES!","PYTHON_9.3")
   
   keep_fields = ["OBJECTID","Shape","GISJOIN",j,"Shape_Length","Shape_Area"]  ## set fields to keep
   fields = arcpy.ListFields(i) ## grab fields
   del_fields = [x.name for x in fields if x.name not in keep_fields]  ## set fields to delete
   arcpy.DeleteField_management(i,del_fields)  ## delete those fields

## join to master: t40_80
for i in outputs:
   arcpy.AddJoin_management(t40_80,"GISJOIN",i,"GISJOIN","KEEP_ALL")

## add in new area sq mi calcs
join_fields = []
for i,j in zip(outputs,new_fields2):  ## conform with joined field names
   x = i + "." + j
   join_fields.append(x)

for i,j in zip(new_fields2,join_fields):
   x = "[" + j + "]"
   arcpy.CalculateField_management("t40_80","GISJOIN",i,x,"VB")
   
## remove all joins
for i in outputs:
   arcpy.RemoveJoin_management("t40_80",i)
   
#########################################################################
###            CREATE NEW TRACTSXX_DAS FILES FOR INTERSECTION         ###
#########################################################################

env.workspace = "D:/MappingData/urbanization2/tracts1940"

polygons = arcpy.ListFeatureClasses("tracts*")
arcpy.FeatureClassToGeodatabase_conversion(polygons,"D:/MappingData/urbanization2/urb40_80.gdb")

## adjust
env.workspace = "D:/MappingData/urbanization2/urb40_80.gdb"
fields =[]
for i in range(40,90,10):
   x = "hu" + str(i)
   fields.append(x)

polygons = arcpy.ListFeatureClasses("tracts*")

## drop fields
for i,j in zip(polygons,fields):
   keep_fields = ["FID","OBJECTID","Shape","GISJOIN",j,"Shape_Length","Shape_Area"]
   del_fields = [x.name for x in arcpy.ListFields(i) if x.name not in keep_fiels]
   if len(del_fields) >0: arcpy.DeleteField_management(i,del_fields)
   
## add and calculate area: sqmi
for i in polygons:
   arcpy.AddField_management(i,"sqmi","DOUBLE")
   arcpy.CalculateField_management(i,"sqmi","!Shape.Area@SQUAREMILES!","PYTHON_9.3")
   
### delete unneeded polygons
del_fc = arcpy.ListFeatureClasses("*das")
for i in del_fc:
   fc_path = os.path.join(env.workspace,i)
   if arcpy.Exists(fc_path):
      arcpy.Delete_management(fc_path)

## change names
old_polygons = arcpy.ListFeatureClasses("*das_1")
for i in old_polygons:
   s = re.findall("\\d+",i)[0:1]
   t = [int(t) for t in s]
   for j in t:
      new_names = "tracts" + str(j) + "_das"
      arcpy.Rename_management(i,new_names)

###################################################################
###################################################################
#### --------------------------------------------------------- ####
####                       DO INTERSECTION                     ####
#### --------------------------------------------------------- ####
###################################################################
###################################################################

##  clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
     arcpy.mapping.RemoveLayer(df, lyr)

## calculate sqmi for polygons in estimates
polygons = arcpy.ListFeatureClasses("t40*")  ## grab all polygons with names starting with "t"
for i in polygons:
   arcpy.AddField_management(i,"sqmi","DOUBLE")
   arcpy.CalculateField_management(i,"sqmi","!Shape.Area@SQUAREMILES!","PYTHON_9.3")
   
##################################################
###           INTERSECTION PROCEDURE           ###
##################################################

polygons1 = arcpy.ListFeatureClasses("t40*")  ## grab 
polygons2 = ["tracts40_das","tracts50_das","tracts60_das","tracts70_das","tracts80_das"]  ## doing manually bc order is messed up
outputs = []
for i in polygons2:
   s = re.findall("\\d+",i)[0:1]
   t = [int(t) for t in s]
   
   for i in t:
      x = "int" + str(i)
      outputs.append(x)

for i,j,k in zip(polygons1,polygons2,outputs):
     input = i + ";" + j
     arcpy.Intersect_analysis(input,k) 

## calculate areas
polygons = outputs
for i in polygons:
   arcpy.AddField_management(i,"sqmi_int","DOUBLE")
   arcpy.CalculateField_management(i,"sqmi_int","!Shape.Area@SQUAREMILES!","PYTHON_9.3")
   

## move to dropbox
for i in polygons:
   output = "C:/Users/scott/Dropbox/MappingData/urbanization2/urb40_80.gdb"
   arcpy.FeatureClassToGeodatabase_conversion(i,output)
   

#############################################
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
#############################################
###                                       ###
###  TRANSITION to R  to calculate areas  ###
###                                       ###
#############################################
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
#############################################

##  clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
     arcpy.mapping.RemoveLayer(df, lyr)

## prepare workspace
import arcpy
import os
import re  ## string package

from arcpy import env
#env.workspace = "C:/Users/scott/Dropbox/MappingData/urbanization2"
env.workspace = "D:/MappingData/urbanization2"
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")
env.extent = "MAXOF"  ## needed for raster analyses
env.qualifiedFieldNames = False  ## good for joins

## create file gdb for 90-19
arcpy.CreateFileGDB_management(env.workspace,"urb90_19.gdb")

## move layers to new gdb
polygons = ["clip_out/golf_final.shp","clip_out/merge40.shp",
            "tracts1/bg00.shp","tracts1/tracts10.shp","tracts1940/bg90_das.shp"]

tables = ["tables/hu00_bg.dbf","tables/hu10_19.dbf"]

## polygons
for i in polygons:
   arcpy.FeatureClassToGeodatabase_conversion(i,"urb90_19.gdb")
   
## tables
for i in tables:
   arcpy.TableToGeodatabase_conversion(i,"urb90_19.gdb")

## change workspace
env.workspace = "D:/MappingData/urbanization2/urb90_19.gdb"

## create golf outputs and run analysis
outputs = ["golf90","golf00"]
equations = ["YEAR < 1991","YEAR < 2001"]
for i,j in zip(outputs,equations):
   arcpy.MakeFeatureLayer_management("golf_final",i,j)
   
## run erase for 90 on bg90_das and tracts10
polygons = ["bg90_das","tracts10"]
outputs = ["bg90_das90","tracts10_das90"]
for i,j in zip(polygons,outputs):
    arcpy.Erase_analysis(i,"golf90",j)
    
## do same for 00 on bg00 and tracts10
arcpy.Merge_management(["merge40","golf00"],"merge00")
polygons = ["bg00","tracts10"]
outputs = ["bg00_das00","tracts10_das00"]
for i,j in zip(polygons,outputs):
   arcpy.Erase_analysis(i,"merge00",j)

## calculate areas
polygons = arcpy.ListFeatureClasses("*das90") + arcpy.ListFeatureClasses("*das00")
for i in polygons:
   arcpy.AddField_management(i,"sqmi","DOUBLE")
   arcpy.CalculateField_management(i,"sqmi","!Shape.Area@SQUAREMILES!","PYTHON_9.3")
   
#############################################
###              INTERSECT                ###
#############################################

##  clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
     arcpy.mapping.RemoveLayer(df, lyr)
     
## intersect 90
arcpy.Intersect_analysis(polygons[0:2],"int90","NO_FID")

## join bg00 data to bg00_das00 to preper for intersect
arcpy.AddIndex_management("bg00_das00","GISJOIN","GISJOIN","NON_UNIQUE")
arcpy.AddJoin_management("bg00_das00","GISJOIN","hu00_bg","GISJOIN")
arcpy.CopyFeatures_management("bg00_das00",env.workspace,"bg00a_das00")
arcpy.DeleteField_management("bg00a_das00",["OID","OBJECTID_1","GISJOIN_1"])
arcpy.Intersect_analysis(["bg00a_das00","tracts10_das00"],"int00","NO_FID")

## intersection calculations
suffix = ["90","00"]
for i in suffix:
   fc = "int" + i
   equation = "!" + "hu" + i + "!" + "*!pcover!"
   arcpy.AddField_management(fc,"sqmi_int","DOUBLE")
   arcpy.AddField_management(fc,"pcover","DOUBLE")
   arcpy.AddField_management(fc,"hu_est","DOUBLE")
   arcpy.CalculateField_management(fc,"sqmi_int","!Shape.Area@SQUAREMILES!","PYTHON_9.3")
   arcpy.CalculateField_management(fc,"pcover","!sqmi_int! / !sqmi!","PYTHON_9.3")
   arcpy.CalculateField_management(fc,"hu_est",equation,"PYTHON_9.3")
   
   ## dissolve
   diss = "diss" + i
   arcpy.Dissolve_management(fc,diss,"GISJOIN_1",[["sqmi_int","SUM"],["hu_est","SUM"]])

## clean up diss90 and diss00
for i in suffix:
   fc = "diss" + i
   new_fields = ["hu" + i, "sqmi" + i]
   old_fields = ["[SUM_hu_est]","[SUM_sqmi_int]"]
   
   for j,k in zip(new_fields,old_fields):
         arcpy.AddField_management(fc,j,"DOUBLE")
         arcpy.CalculateField_management(fc,j,k,"VB")

## delete old fields
for i in suffix:
   fc = "diss" + i
   
   for j in old_fields:
      del_field = re.findall("\\w+",j)
      arcpy.DeleteField_management(fc,del_field)

#####################################################
###         TRACTS 2010 AND 2019  ADN JOIN        ###
#####################################################

## erase golf_final from tracts10_das00 to get sqmi for 2019
arcpy.Erase_analysis("tracts10_das00","golf_final","tracts10_das19")

new_fields = ["sqmi10","sqmi19"]
equations = ["!sqmi!","!Shape.Area@SQUAREMILES!"]
for i,j in zip(new_fields,equations):
   arcpy.AddField_management("tracts10_das19",i,"DOUBLE")
   arcpy.CalculateField_management("tracts10_das19",i,j,"PYTHON_9.3")

## import ham40_80
ham40_80 = "D:/MappingData/urbanization2/tables/ham40_80.dbf"

## join
list1 = [ham40_80,"diss90","diss00","tracts10_das19","hu10_19"]

## add join and export
for i in list1:
   if i in list1[1:3]:
      arcpy.AddJoin_management("tracts10","GISJOIN",i,"GISJOIN_1","KEEP_ALL")
   
   else:
      arcpy.AddJoin_management("tracts10","GISJOIN",i,"GISJOIN","KEEP_ALL")

## export
arcpy.CopyFeatures_management("tracts10","tracts10_fin1")

## clean up tracts10 file
fields = arcpy.ListFields("tracts10_fin1")
#keep_fields = ["FID","Shape","OBJECTID","GISJOIN","Shape_Length","Shape_Area"]
#del_fields = [x.name for x in fields if x.name not in keep_fields]
del_fields = []
for i in fields:
   
#############################################
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
#############################################
###                                       ###
###      TRANSITION to R  to ORGANIZE     ###
###                                       ###
#############################################
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
#############################################

##  clear workspace
mxd = arcpy.mapping.MapDocument("CURRENT")
for df in arcpy.mapping.ListDataFrames(mxd):
  for lyr in arcpy.mapping.ListLayers(mxd, "", df):
     arcpy.mapping.RemoveLayer(df, lyr)
     
## create final gdb:
env.workspace = "D:/MappingData/urbanization2"
arcpy.CreateFileGDB_management(env.workspace,"urb_final.gdb")
arcpy.CopyFeatures_management("urb90_19.gdb/tracts10","urb_final.gdb/tracts10")
arcpy.TableToGeodatabase_conversion("tables/t10_org2.dbf","urb_final.gdb")

## change env.workspace and do table join
env.workspace = "D:/MappingData/urbanization2/urb_final.gdb"
arcpy.AddJoin_management("tracts10","GISJOIN","t10_org2","GISJOIN","KEEP_ALL")
arcpy.CopyFeatures_management("tracts10","tracts10_fin2")
arcpy.DeleteField_management("tracts10_fin2",["OBJECTID_1","GISJOIN_1"])





