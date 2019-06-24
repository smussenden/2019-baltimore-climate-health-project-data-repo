# Calculating Zonal Statistics Using LIDAR Vector Files

*Purpose:* Given a vector file of tree cover and using QGIS, how to convert it to a black-and-white raster then calculate mean tree cover in geographic zones.

*Global Notes:* 
* Save layers created at each stage in case QGIS crashes.
* Files needed:
  * Vector file containing tree cover
  * Vector file containing geographic shapes

## 1. Add a column of 1s to vector tree cover file
Output: New column added to vector attribute table.
* Load in vector containing tree cover data.
* Right-click the vector -> Open attribute table
* Click field calculator icon (looks like an abacus):
  * Create new field checked
  * Output field name: "rast_value"
  * Expression: 1
* Toggle editing off to save changes
  
## 2. Create first raster (to demark "yes" data)
Output: Raster file with default name "Rasterized."
* Processing -> Toolbox -> SAGA -> Raster creation tools -> Rasterize
* Rasterize wizard settings: 
  * Shapes: _The vector containing tree cover data_
  * Attribute: "rast_value"
  * Output values: [0] data/no-data
  * Method for multiple values: [0] first
  * Method for lines: [0] thin
  * Method for lines: [0] node
  * Preferred target type: [2] integer (4 byte)
  * Output extent: use layer extent
  * Cellsize: .5
  * Fit: [0] nodes
* Save rasterized file (Right-click -> Export -> as .TIF)

## 3. Create second raster (to demark "no" data)
Output: Raster file with default name "Result."
* Processing -> Toolbox -> SAGA -> Raster tools -> Invert data/no data
* Grid = First raster ("Rasterized")
* Save rasterized file (Right-click -> Export -> as .TIF)

## 4. Create third raster (to convert "no" data into 0s)
Output: Raster file with default name "Calculated."
* Processing -> Toolbox -> SAGA -> Raster calculus -> Raster calculator
* Raster Calculator wizards settings: 
  * Main input layer: 2nd raster ("Rasterized")
  * Additional layers: _0 elements_
  * Formula: 0
  * Resampling method: [3] B-spline interpolation
  * [] Use no-data (unchecked)
  * output data type ~[7] 4 byte floating point number~ [3] unsigned 2 byte integer
* Save rasterized file (Right-click -> Export -> as .TIF)
  
## 5. Prepare third raster for merge
Output: Updates to third raster ("Calculated"), no new layer.
* Double-click third raster ("Calculated") -> symbology (band rendering)
* Render type: paletted/unique values
* Click "Classify"
* Click "OK"
* Save rasterized file (Right-click -> Export -> as .TIF)

## 6. Merge "yes" and "no" rasters
Output: Raster file with default name "Grid."
* Processing -> Toolbox -> SAGA -> Raster Tools -> Mosaic raster layers
* Mosaic Raster Layers wizard settings:
  * Input grids: 1st and 3rd rasters ("Rasterized" and "Calculated")
  * Name: Mosaic
  * Prefered data storage type: [3] 2 byte unsigned
  * Inerpolation: [0] Nearest neighbor
  * Overlapping areas: [1] last
  * Blending distance: 1.0
  * Match: [0] none
  * Output extent: _[Leave blank...]_
  * Cellsize: .5
  * Fit: [0] nodes
* Save rasterized file (Right-click -> Export -> Save features as -> format:.TIF)

## 7. Calculate zonal statistics
Output: Columns added to the vector file's attribute table.
* Processing -> Toolbox -> Raster analysis -> Zonal statistics
* Zonal Statistics wizard settings:
  * Raster layer: 4th raster ("Grid")
  * Raster band: Band 1
  * Vector layer containing zones: a .shp / vector of geometries (such as NSA or block boundaries)
  * Output column prefix _(name matters for use in cleaning file)_: "[last-2-of-year]\_" (e.g. "07\_")
  * Statistics to calculate: mean
* Export the new data (Right click vector -> Export -> Save features as -> format: CSV)
