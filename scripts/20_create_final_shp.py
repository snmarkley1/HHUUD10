#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
#### --------------------------------------------------------------------------- ####
####                         CREATE FINAL SHAPEFILES                             ####
#### --------------------------------------------------------------------------- ####
#### --------------------------------------------------------------------------- #### 
#####################################################################################
#####################################################################################

# Using Python 3 in ArcGIS Pro 2.7.1

### PREPARE WORKSPACE
import arcpy, os
from arcpy import env

# SET BASE
base = "D:/HHUUD10"

# SET ENV.WORKSPACE
env.workspace = os.path.join(base, "gis_files", "database1.gdb")

# SET ENVIRONMENTS
env.outputCoordinateSystem = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")
env.qualifiedFieldNames = False  ## good for joins


###############################################################################
## STEP 1: CREATE EMPTY TRACT FILE
###############################################################################

## Copy over t10 and rename
arcpy.management.CopyFeatures("t10", "t10_empty")

## Delete unnecessary fields
del_fields = "hu40;hu50;hu60;hu70;hu80;hu90;hu00;hu10;hu19;sqmi"
arcpy.management.DeleteField('t10_empty', del_fields)


###############################################################################
## STEP 2: JOIN w/ HHUUD & HAMMER DATA and EXPORT
###############################################################################

## Grab tables
hhuud = os.path.join(base, "output", "HHUUD.dbf")
#hammer = os.path.join(base, "output", "hammer.dbf")

## Create gdb in folder
arcpy.management.CreateFileGDB(os.path.join(base, "DATA_DOWNLOAD"), "HHUUD10.gdb")
output_gdb = os.path.join(base, "DATA_DOWNLOAD", "HHUUD10.gdb")

## Do join and conversion for HHUUD, clean up, and remove joins
arcpy.management.AddJoin("t10_empty", "GISJOIN", hhuud, "GISJOIN10")
arcpy.conversion.FeatureClassToFeatureClass("t10_empty", output_gdb, "HHUUD10")
arcpy.management.DeleteField("HHUUD10", "GISJOIN;OID_1")

arcpy.management.RemoveJoin('t10_empty', 'HHUUD')


###############################################################################
## Convert to GeoJSON File
###############################################################################

arcpy.conversion.FeaturesToJSON("HHUUD10", 
                                os.path.join(base, "DATA_DOWNLOAD", "HHUUD10.geojson"), 
                                "NOT_FORMATTED", 
                                "NO_Z_VALUES", 
                                "NO_M_VALUES", 
                                "GEOJSON", 
                                "KEEP_INPUT_SR", 
                                "USE_FIELD_NAME")
