# Qgis Tutorials

**An assortment of useful Qgis tutorials based on what was needed to complete this project.**

## Contents
* [A. Importing XYZ Tiles and Outputting Them to Raster](#a)
* [B. Clip a Raster Using a Vector](#b)
* [C. Export Rasters and Vectors for Use in Illustrator](#c)
* [D. Calculating Zonal Statistics Using LIDAR Vector Files](#d)

## <a name="a">A. Importing XYZ Tiles and Outputting Them to Raster</a>

*Purpose:* XYZ tiles are linked maps that scale dynamically within Qgis. You may need to trim/clip a map to the dimensions of a city or other area, but an XYZ tile cannot be trimmed directly and must first be converted to a raster.

### 1. Load in the XYZ Tile
* From the file _Browser_ window to the left, right-click _XYZ Tiles_
* Select _New Connection_. Opens a new window.
* In the _Connection Details_ window, paste in a _URL_ of the format "http://example.com/{z}/{x}/{y}.png". You can find XYZ tiles of Google Maps by searching "Google Maps XYZ tiles."
* Click _OK_.
* Verify the XYZ tile has been added to the _Layers_ window (double click it from the file _Browser_ if not). You should see it in the display window.

### 2. Export the Tile as a Raster
* Right-click the XYZ tile layer, choose _Export_ -> _Save As_. Opens a new window.
* In the _Save Raster Layer as..._ window:
  * Select _Raw data_.
  * Uncheck _Create VRT_.
  * Click "_..._" to choose the file name and location.
  * Set the _CRS_ to your _Project CRS_. (This controls map distortion. Set your project CRS from the main menu: _Project_ -> _Properties_ -> _CRS_. For U.S. maps, I recommend using _NAD83 / [Your state here]_.)
  * Choose _Extent_ -> _Current Layer Extent_ (or _Calculate from Layer_ to set the size based on a different layer) to select how much of the map will be exported.
  * _Resolution_ is set in *meters per pixel*. Set _Horizontal_ and _Vertical_ to the same number. I have found 10 to output appropriately high-resolution maps.
  * Click _OK_.

## <a name="b">B. Clip a Raster Using a Vector</a>
* _Raster_ -> _Extraction_ -> _Clip Raster by Mask Layer_
* _Input_: the raster to be clipped
* _Mask_: the vector acting as the "cookie cutter."
* Under _Advanced parameters_, allow it to "[Save to temporary file]," the default. (I found trying to save it to a file here caused the process to fail.)
* Leave checked _Open output file after running algorithm_.
* Click _Run_ and allow the process to complete.
* Once complete, the new, clipped raster layer should populate in the _Layers_ window on the left. Right-click the file and follow the directions in C.2, above. 

## <a name="c">C. Export Rasters and Vectors for Use in Illustrator</a>
1. Adjust the displayed layers and zoom level in the main Qgis display window to be approximately the desired output. Note:
    * Checked layers will be exported even if they do not show because they are beneath other layers. This is useful for outputting multiple layers of a map at the same position and scale, rather than trying to line up individual files later. This convenience and precision should be balanced with filesize, as Illustrator will have to render all the exported layers.
    * The position on the canvas in the next step will not be exactly what it is in this display window and is easier to adjust precisely in Illustrator.
2. _New Print Layout_ (top left, next to the save floppy-disk icon). Opens a new window. 
    * ![New Print Layout](https://github.com/smussenden/2019-baltimore-climate-health-project-data-repo/blob/master/documentation/qgis-directions/qgis-icon-new-print-layout.png?raw=true "New Print Layout")
    * Leave the title box blank and click _OK_. Opens another new window.
3. Set output image size and orientation: Right-click the canvas to choose _Page Properties_.
    * Under _Item Properties_, select a _Size_ and _Orientation_ appropriate to the project. ("A4" the default and is appropriate for many projects. Choose "portrait" or "landscape" depending on whether your project is tall or long.)
4. Click the icon for _"Adds new map to layout"_ (left-hand side, about 1/3 down).
    * ![Add New Map to Layout](https://github.com/smussenden/2019-baltimore-climate-health-project-data-repo/blob/master/documentation/qgis-directions/qgis-icon-add-new-map.png?raw=true "Add New Map to Layout")
5. Click and drag to fill the available canvas with the map layers currently being diplayed by Qgis. Note: 
    * You may realize here the map zoom level is incorrect compared to the size of the canvas, or that the map position causes part of it to be cut off. If this happens, close the window and adjust the map accordingly, then repeat from step 2.
6. Make the following changes to the image _Layout_ and _Properties_ on the right:
    * _Layout_: Check _Always export as vectors_.
    * _Properties_: Un-check _Background_.
7. _Export as PDF_. (Illustrator can read vectors from PDF.) Opens a new window to set file name and location.
    * ![Export as PDF](https://github.com/smussenden/2019-baltimore-climate-health-project-data-repo/blob/master/documentation/qgis-directions/qgis-icon-new-print-layout.png?raw=true "Export as PDF")
    * Choose a name and location for the file. Clicking _Save_ opens a new window with final setting options.
    * Verify _Always export as vectors_ is checked.
    * Check the box for _Disable tiled rater layer exports_. (This prevents some maps from importing into Illustrator as multiple pieces, instead stitching them together into a single image. It does not affect vectors.)
    
## <a name="d">D. Calculating Zonal Statistics Using LIDAR Vector Files</a>

*Purpose:* Given a vector file of tree cover and using QGIS, convert it to a black-and-white raster then calculate mean tree cover in geographic zones.

*Global Notes:* 
* Save layers created at each stage in case QGIS crashes.
* Files needed:
  * Vector file containing tree cover
  * Vector file containing geographic shapes

### 1. Add a column of 1s to vector tree cover file
Output: New column added to vector attribute table.
* Load in vector containing tree cover data.
* Right-click the vector -> Open attribute table
* Click field calculator icon (looks like an abacus):
  * Create new field checked
  * Output field name: "rast_value"
  * Expression: 1
* Toggle editing off to save changes
  
### 2. Create first raster (to demark "yes" data)
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

### 3. Create second raster (to demark "no" data)
Output: Raster file with default name "Result."
* Processing -> Toolbox -> SAGA -> Raster tools -> Invert data/no data
* Grid = First raster ("Rasterized")
* Save rasterized file (Right-click -> Export -> as .TIF)

### 4. Create third raster (to convert "no" data into 0s)
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
  
### 5. Prepare third raster for merge
Output: Updates to third raster ("Calculated"), no new layer.
* Double-click third raster ("Calculated") -> symbology (band rendering)
* Render type: paletted/unique values
* Click "Classify"
* Click "OK"
* Save rasterized file (Right-click -> Export -> as .TIF)

### 6. Merge "yes" and "no" rasters
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

### 7. Calculate zonal statistics
Output: Columns added to the vector file's attribute table.
* Processing -> Toolbox -> Raster analysis -> Zonal statistics
* Zonal Statistics wizard settings:
  * Raster layer: 4th raster ("Grid")
  * Raster band: Band 1
  * Vector layer containing zones: a .shp / vector of geometries (such as NSA or block boundaries)
  * Output column prefix _(name matters for use in cleaning file)_: "[last-2-of-year]\_" (e.g. "07\_")
  * Statistics to calculate: mean
* Export the new data (Right click vector -> Export -> Save features as -> format: CSV)