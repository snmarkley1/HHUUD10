
#####################################################################################
#####################################################################################
#### --------------------------------------------------------------------------- ####
#### --------------------------------------------------------------------------- ####
####                      GENERATING a NEIGHBORS FILE                            ####
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
## GENERATE POLYGON NEIGHBORS TABLE
###############################################################################

### NEIGHBORS ANALYSIS
arcpy.analysis.PolygonNeighbors('t10', 't10_neighbors', 'GISJOIN', 'NO_AREA_OVERLAP', 'BOTH_SIDES', None, 'METERS', 'SQUARE_METERS')


