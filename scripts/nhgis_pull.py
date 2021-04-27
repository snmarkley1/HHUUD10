
## NHGIS IMPORT TEST (PYTHON)

import requests
import json
from pprint import pprint
my_key = "59cba10d8a5da536fc06b59d105a093e6b154940b7a0263ee0891027"
my_headers = {"Authorization": my_key}
url = "https://api.ipums.org/extracts/?product=nhgis&version=v1"
er = """

{
"shapefiles": [
   "us_tract_1940_tl2008",
   "us_tract_1950_tl2008",
   "us_tract_1960_tl2008",
   "us_tract_1970_tl2008",
   "us_tract_1980_tl2008",
   "us_blck_grp_2010_tl2010",
   "us_tract_2010_tl2010"
   ],
 "geographic_extents": ["010"]

}

"""
result = requests.post(url, headers = my_headers, json = json.loads(er))
my_extract_number = result.json()["number"]
print(my_extract_number)

## do data pull
shp_url = "".join(["https://api.ipums.org/extracts/",str(my_extract_number),"?product=nhgis&version=v1"])
r = requests.get(shp_url, headers = my_headers)
extract = r.json()
my_extract_links = extract["download_links"]

# get the file from the URL and write out to a local file 
r = requests.get(my_extract_links["gis_data"], allow_redirects=True, headers=my_headers)
filename = "".join(["nhgis", str(my_extract_number).zfill(4), "_shape.zip"])
open(filename, "wb").write(r.content)


