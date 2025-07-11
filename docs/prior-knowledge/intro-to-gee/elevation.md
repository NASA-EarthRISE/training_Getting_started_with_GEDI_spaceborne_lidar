---
layout: page
title: Retrieving and visualizing elevation data
parent: Introduction to Google Earth Engine
nav_order: 5
---

# Retrieving and visualizing elevation data

This exercise will walk us to the use of the NASADEM elevation dataset. Let’s remember that each product description in the GEE catalog usually brings a small portion of code to exemplify how to use it. We look for the product:

<img align="center" src="../images/intro-gee-images/24_nasadem.png" hspace="15" vspace="10" width="600">

Figure 24. NASA Digital Elevation Model (DEM) listed options

The first product listed must be imported, and change the name to *nasadem*.

<img align="center" src="../images/intro-gee-images/25_nasadem_desc.png" hspace="15" vspace="10" width="600">

Figure 25. NASA DEM description

It’s always important to skim over the dataset product details, such as time availability, spatial scope, significant improvements over other options, and bands.

<img align="center" src="../images/intro-gee-images/26_dem_var.png" hspace="15" vspace="10" width="600">

Figure 26. Product added

```javascript
/**** 	NASA Digital Elevation Model (DEM)  ****/
var elevation = nasadem.select('elevation');
// Set elevation <= 0 as transparent and add to the map.
elevation = elevation.updateMask(elevation.gt(0)).clip(guyana_bou)


// Set elevation visualization properties.
var elevationVis = {
  min: 0,
  max: 2000,
};
/****  	****   	****/

Map.addLayer(elevation, elevationVis, 'Elevation');
```

Now let’s analyze the terrain height at some specific locations. The country of Guyana consists of four main geographic regions: the interior savanna (Rupununi Savannah), the low coastal plain (North Atlantic Ocean), a hilly sand and clay zone across the entire territory, and the highlands with mountain chains. Within the Pakaraima Mountains we find the Mount Roraima peak with 2,772 meters (9,094 ft) of elevation.

<img align="center" src="../images/intro-gee-images/27_pakaraima.png" hspace="15" vspace="10" width="600">

Figure 27. The Pakaraima mountains, Guyana. Source: Wikipedia

```javascript
// Around Pakaraima mountains and Mount Roraima lat + 5.26°, lon -60.76°
var p01 = ee.Geometry.Point(-60.76, 5.26)

Map.addLayer(p01, {}, 'Mount Roraima');
/****  	****   	****/
```

<img align="center" src="../images/intro-gee-images/28_elev_mount.png" hspace="15" vspace="10" width="600">

Figure 28. DEM layer and the location over Pakaraima mountains chain

Black and gray colors correspond to lowlands and plains, while white and bright pixels correspond to highlands and mountain peaks. We are going to use the Inspector tool.  Click on it, and then we click on the point we added in order to see the elevation value we have there.

<img align="center" src="../images/intro-gee-images/29_insp.png" hspace="15" vspace="10" width="600">

Figure 29. Inspector panel.

Hence, the values from all the add layers will be visible for the specified geographical location.

<img align="center" src="../images/intro-gee-images/30_insp_values.png" hspace="15" vspace="10" width="300">

Figure 30. Location values

In the Inspector panel we will see the values for the layers we have added by code, either activated or deactivated. First, we can identify low surface reflectance values from the Sentinel layer. Second, we can observe that the elevation at this location is 885 meters.  This zone harbors the greatest terrain heights like for example the Pakaraima mountains. Let’s do a quick exercise of visualization. The default view of the map visor 

<img align="center" src="../images/intro-gee-images/31_mode.png" hspace="15" vspace="10" width="300">

Figure 31. Visualization mode.

We can make use of the high-resolution images available in a true color setting to visualize the area and try to identify mountainous zones.

<img align="center" src="../images/intro-gee-images/32_basemap.png" hspace="15" vspace="10" width="600">

Figure 32. Visualization using high-resolution basemaps of GEE

We can also use the Map-terrain view to analyze elevation by contour lines

<img align="center" src="../images/intro-gee-images/33_terrain.png" hspace="15" vspace="10" width="300">

Figure 33. Map terrain view

<img align="center" src="../images/intro-gee-images/34_terrain_view.png" hspace="15" vspace="10" width="600">

Figure 34. Terrain view

We can easily recognize our point of reference located at the 900 m contour line.