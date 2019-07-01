# Baltimore Climate Project (summer) Analysis Process Notes

## Data Gathering

**A. Primary data files**
  1. Shapefiles of Census blocks, downloaded direct from [Census.gov](https://www2.census.gov/geo/tiger/TIGER2018/TABBLOCK/). Also community statistical areas, neighborhood statistical areas and zctas.
  2. Black and white TIFs of treecover, courtesy of [Descartes Labs](https://www.descarteslabs.com/) ([G Drive](https://drive.google.com/drive/u/2/folders/17Wg6c1LxFmHYUgQpwv6-G5Z-HWx6O-Y2))
     * 2009 sourced from: [NAIP](https://www.fsa.usda.gov/programs-and-services/aerial-photography/imagery-programs/naip-imagery/)
     * 2017-18 sourced from: NAIP, [Landsat](https://landsat.gsfc.nasa.gov/), Airbus
  3. Neighborhood block groupings, courtesy of City of Baltimore Dept. of Planning

**B. Secondary data files**
  1. Thresholded raster TIFs of treecover calculated from files A.2 (at 63 and 127)
  2. CSVs calculated by Qgis of zonal B.1 statistics, using A.1 zones

**C. Methodology**
  * **ZCTA Notes**
    * There are some differences in the exact boundaries between ZCTA shapes pulled county-by-county and for MD as a whole. All ZCTA groups are from the state-level ZCTA files. ~~except for ZCTA 21222, for which a significant chunk of land area was missing from the file pulled at the state level.~~
    * The Mid-Atlantic Terminal at Dundalk (sometimes included as part of the 21222 ZCTA) is not included.
    * All calculations for included ZCTAs are done over the entire ZCTA, even when the ZCTA expands past the city boundary, with one exception. 21226 is cut off at the city boundary due to limitations the in treecover and heat maps. In addition, a small peninsula in that same ZCTA was removed for ease in computing.
    * 4 ZCTAs ('21208', '21228', '21234', '21236') encroach slightly into Baltimore City, but were not included due to limitiations in treecover and heat maps. 
    * 3 included ZCTAs ('21227', '21222', '21237') only encroach slightly into Baltimore City but were included because the treecover and heat maps encompass them.
    * An insignificant portion of the north end of 21237 is not included on the treecover map and is therefore missing from calculations made for that ZCTA.
    * ZCTA lists for reference:
      * Complete list of Baltimore ZCTAs: '21201', '21202', '21205', '21206', '21207', '21208', '21209', '21210', '21211', '21212', '21213', '21214', '21215', '21216', '21217', '21218', '21222', '21223', '21224', '21225', '21226', '21227', '21228', '21229', '21230', '21231', '21234', '21236', '21237', '21239', '21251'
      * ZCTAs removed from list (for untrimmed): '21208', '21228', '21234', '21236'
      * Complete list of ZCTAs used (for untrimmed): '21201', '21202', '21205', '21206', '21207', '21209', '21210', '21211', '21212', '21213', '21214', '21215', '21216', '21217', '21218', '21222', '21223', '21224', '21225', '21226', '21227', '21229', '21230', '21231', '21237', '21239', '21251'
  * **CSA Notes**
    * Community Statistical Areas are used by The Baltimore Neighborhood Indicators Alliance Vital Signs program.
    * The areas do not include the Johns Hopkins region in roughly the center of the map.
  * **NSA Notes**
    * Target neighborhoods: "Berea", "Broadway East", "Oliver", "Middle East", "Biddle Street","Milton-Montford", "Madison-Eastend", "CARE", "McElderry Park", "Ellwood Park/Monument", "Patterson Place", "Patterson Park Neighborhood", "Baltimore Highlands", "Highlandtown", "Upper Fells Point"
    * Counterpoint neighborhoods: "Butcher's Hill", "Canton", "Washington Hill"
    
  ![Removed ZCTAs](https://github.com/smussenden/2019-baltimore-climate-health-project-data-repo/blob/master/documentation/tree-zcta-baltcity.png?raw=true "Removed ZCTAs")
  
  ![ZCTA Shapefile Overview](https://github.com/smussenden/2019-baltimore-climate-health-project-data-repo/blob/master/documentation/shapefile-overview.png?raw=true "ZCTA Shapefile Overview")
  
  
**D. Maps**
* ALL NEED:
  * Heat (afternoon, median):
  * Treecover (at 63)
    * 2009:
    * 2017-18: 
  *Treecover Change (at 63)
    * Percent change:
    * Percentage point change: 
    
* CSA: https://smussenden.carto.com/viz/8b838637-4661-4fb9-9e42-8083d506e095/public_map 
* NSA: https://smussenden.carto.com/viz/a73ff1be-9c4d-4625-904f-9f2c17f8b435/public_map   
* ZCTA: https://smussenden.carto.com/viz/8f4d2944-0b54-455e-bdeb-007001470bdb/public_map 
* BLOCK: https://smussenden.carto.com/viz/b00d138f-744b-4767-a748-3b8d627e8786/public_map 

ZCTA clipped cleaned outputs must include:
* LIDAR Treecover
* Temp
* EMS

ZCTA extended cleaned outputs must include:
* NAIP Treecover
* Temp
* Hosp


