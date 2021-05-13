
#########################################################
#### ----------------------------------------------- ####
####             Process NLCD 1992 rasters           ####
#### ----------------------------------------------- ####
#########################################################

## prepare workspace
import arcpy
from arcpy import env
env.workspace = "C:/Users/scott/Dropbox/urb_proj/nlcd92/nlcd.gdb"


## available in folders nlcde92_1, etc.
rasters = ["nlcde1.tif","nlcde2.tif","nlcde3.tif","nlcde4.tif"]

for i in rasters:
     output = i[:4] + i[5:6] + "_recl"
     arcpy.gp.Reclassify_sa(i, 'Value', '11 12 1;21 24 2;25 84 3;85 2;86 99 3', output, 'NODATA')
     
## zonal histogram
rasters = arcpy.ListRasters()

for i in rasters:
     output = i[:5] + "_zh2"
     arcpy.sa.ZonalHistogram("tracts10","GISJOIN",i,output,"")
