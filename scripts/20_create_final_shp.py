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
base = "D:/HIST_HU_URB"

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
hammer = os.path.join(base, "output", "hammer.dbf")

## Do join and conversion for HHUUD, clean up, and remove joins
arcpy.management.AddJoin("t10_empty", "GISJOIN", hhuud, "GISJOIN10")
arcpy.conversion.FeatureClassToFeatureClass("t10_empty", env.workspace, "HHUUD")
arcpy.management.DeleteField("HHUUD", "GISJOIN;OID_1")

arcpy.management.RemoveJoin('t10_empty', 'HHUUD')

## Do join and conversion for HAMMER & clean up
arcpy.management.AddJoin("t10_empty", "GISJOIN", hammer, "GISJOIN10")
arcpy.conversion.FeatureClassToFeatureClass("t10_empty", env.workspace, "HAMMER")
arcpy.management.DeleteField("HAMMER", "GISJOIN;OID_1")





