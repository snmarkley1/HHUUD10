
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
####          ASSIGNING 1992 NLCD CATEGORIES to 2010 TRACT GEOGRAPHIES           ####
#### --------------------------------------------------------------------------- ####
#####################################################################################
#####################################################################################

### RUN in ArcGIS PRO 2.7

##################################
## PREPARE WORKSPACE            ##
##################################

## Import packages
import arcpy  # need ArcGIS license
from arcpy import env
import os, zipfile, urllib   # for downloading, unzipping files

## Set workspace
base = "D:/HIST_HU_URB"
env.workspace = base

## Set preferences
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")  # coordinate system in use
env.extent = "MAXOF"  # for raster operations
env.qualifiedFieldNames = False  # good for joins

# Create temp folder
arcpy.management.CreateFolder(env.workspace, "temp")
path = os.path.join(env.workspace, "temp")  # create path

# Create GDB
arcpy.management.CreateFileGDB(base, "gis_files","nlcd92.gdb")

# Establish Map
aprx = arcpy.mp.ArcGISProject("CURRENT")


############################################################
## DOWNLOAD/UNZIP 1992 NLCD CATEGORIES                    ##
############################################################

## Create list of URLs--available via the USGS (https://water.usgs.gov/GIS/metadata/usgswrd/XML/nlcde92.xml#stdorder)
urls = ["https://water.usgs.gov/GIS/dsdl/nlcde/nlcde92_1.zip", 
        "https://water.usgs.gov/GIS/dsdl/nlcde/nlcde92_2.zip", 
        "https://water.usgs.gov/GIS/dsdl/nlcde/nlcde92_3.zip", 
        "https://water.usgs.gov/GIS/dsdl/nlcde/nlcde92_4.zip"]
        
## List of output names
outputs = ["nlcd92_1", "nlcd92_2", "nlcd92_3", "nlcd92_4"]

## Run Loop downloading and unzipping raster files
for i, j in zip(urls, outputs):
    zip_path, _ = urllib.request.urlretrieve(i, j)  # retrieve files from URLs
    
    with zipfile.ZipFile(zip_path, "r") as f:  
        f.extractall(path)  # unzip files to temp folder created above
        
        
############################################################
## RECLASSIFY & CONDUCT ZONAL HISTOGRAM                   ##
############################################################

## Change workspace
env.workspace = path

## Grab rasters in list
rasters = ["nlcde92_1/nlcde1.tif", "nlcde92_2/nlcde2.tif", "nlcde92_3/nlcde3.tif", "nlcde92_4/nlcde4.tif"]
outfolder = os.path.join(base, "gis_files", "nlcd92.gdb")

## Reclassify into 3-class Rasters (simplifies following step)
for r in rasters:
    output = os.path.join(outfolder, "rast" + r[15:16] + "_recl")  # make name (e.g.) "rast1_recl"
    arcpy.gp.Reclassify_sa(r, "value", '11 12 1;21 24 2;25 84 3;85 2;86 99 3', output, "NODATA")  # for codes, see below:
    
## 1992 NLCD Codes Specified in Reclassify Step (source: https://water.usgs.gov/GIS/metadata/usgswrd/XML/nlcde92.xml#stdorder):

## ---- Water (1) ---- ##
# 11 - Open Water
# 12 - Perennial Ice/Snow

## ---- "Developed" (2) ---- ##
# 21 - Low Intensity Residential
# 22 - High Intensity Residential
# 23 - Commercial/Industrial/Transportation
# 85 - Urban/Recreational Grasses

## ---- Other (3) ---- ##
# All other numbers thru 99


## Prepare Zonal Histogram
env.workspace = outfolder  # change workspace to gdb just created
rasters = arcpy.ListRasters()  # rasters created above
t10 = os.path.join(base, "gis_files/database1.gdb/t10")  # grab t10 polygon from database1.gdb

## Do Zonal Histogram (output as tables in tables folder)
for r in rasters:
     output = r[:5] + "_zh"  # output in tables folder
     arcpy.sa.ZonalHistogram(t10, "GISJOIN10", r, output, "")  # zonal histogram
        

## DELETE TEMP FOLDER
arcpy.management.Delete(path)    
