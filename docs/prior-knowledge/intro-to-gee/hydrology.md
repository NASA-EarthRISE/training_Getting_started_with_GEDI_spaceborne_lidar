---
layout: page
title: Exporting hydrologic datasets
parent: Introduction to Google Earth Engine
nav_order: 6
---

# Exporting hydrologic datasets

In this exercise we learn how to export a dataset with appropriate parametrization in order to be loaded and utilized in another platform.  There exist plenty of water resources data collections available in the GEE catalog, however we will look for river information. The World WildLife Fund (WWF) has developed the HydroSHEDS Free Flowing Rivers Network in partnership with the U.S. Geological Survey, the International Centre for Tropical Agriculture, The Nature Conservancy, and the Center for Environmental Systems Research of the University of Kassel, Germany. This dataset provides hydrographic data at regional and global scales including information of river networks, watershed boundaries, drainage directions, and flow accumulations. Let’s type in the catalog search bar ‘flowing rivers’:

<img align="center" src="../images/intro-gee-images/35rivers.png" hspace="15" vspace="10" width="600">

Figure 35. Looking for flowing rivers datasets

<img align="center" src="../images/intro-gee-images/36_hydrosheds.png" hspace="15" vspace="10" width="600">

Figure 36. HydroSHEDS Free Flowing Rivers Network description.

Let’s import it and name it as rivers, and apply the spatial and temporal filters.  It’s important to notice this information corresponds to February 2000 as it can be read in the description. 

```javascript
/* ***** River information ********* */
var flowingRivers = ee.Image().byte().paint(rivers, 'RIV_ORD', 2)
              	.clip(guyana_bou);

var riverVisParams = {
  min: 1,
  max: 10,
  palette: ['08519c', '3182bd', '6baed6', 'bdd7e7', 'eff3ff']
};

/****  	****   	****/

Map.addLayer(flowingRivers, riverVisParams, 'Free flowing rivers');
Map.addLayer(rivers, null, 'FeatureCollection', false);
```

<img align="center" src="../images/intro-gee-images/37_rivers_clip.png" hspace="15" vspace="10" width="300">

Figure 37. HydroSHEDS clipped layer showing the river network in Guyana.

The layer named as ‘Feature collection’ contains every single item of the dataset such as small tributaries and minor rivers. Now let’s do a zoom at 10 km scale over the capital city. We are able to see the scale of visualization by using the scale bar in the lower right corner as it is circled and pointed out in Figure 38.  Let’s leave only the hydrosheds and true color sentinel layers activated.

<img align="center" src="../images/intro-gee-images/38_river_sem.png" hspace="15" vspace="10" width="600">

Figure 38. Rivers over Sentinel 2A mosaic can show the geographical match of the layers

For now we can deactivate the layer of Landsat and cloud mask by adding a parameter in the last spot of parameters for the function *addLayer*. We can use 0 or false as valid values.

```javascript
Map.addLayer(cloud_mask, {}, 'cloud mask', false);
Map.addLayer(l8_sr_cloud_masked.median(), visual_lan, 'image cloud masked', 0);
```

Now let’s export this dataset using the function *Export*. Something very important is to keep the visor panel large enough for our intended to be exported region or study area. The larger your study region the longer will be the Export request task. One additional step you could do is to draw a polygon of a smaller area of interest within the Guyana territory.  You can check this optional step in the general script link.

```javascript
Export.image.toDrive(
{
  image: flowingRivers,
  description: 'flowingRiversGuyana',
  folder:'training_servir_amazonia',    
  maxPixels:1e13
  }
);
```

We will find all the Export processes in the panel of Tasks.

<img align="center" src="../images/intro-gee-images/39_export_panel.png" hspace="15" vspace="10" width="300">

Figure 39. Export tasks panel.

<img align="center" src="../images/intro-gee-images/40_export_popup.png" hspace="15" vspace="10" width="300">

Figure 40. Export parametrization.


We might find errors like this *“Error: Pixel type not supported: Type\<Long>\. Convert the image to a floating point type or a smaller integer type, for example, using ee.Image.toDouble(). (Error code: 3)”*. This can happen when the datasets come in deprecated, old or very specific number formats.  We just have to apply a cast to our data to the traditional JS number formats like Float or Double. 

```javascript
Export.image.toDrive(
{
  image: ee.Image(flowingRivers).toDouble(),
  description: 'flowingRiversGuyana',
  folder:'training_servir_amazonia',    
  maxPixels:1e13
  }
);
```

In order to check the progress of the Export task, look at the task manager website by click on the link:

<img align="center" src="../images/intro-gee-images/41_taskma.png" hspace="15" vspace="10" width="300">

Figure 41. Task being exported.

<img align="center" src="../images/intro-gee-images/42_task_run.png" hspace="15" vspace="10" width="600">

The task might take around 14 minutes to complete.

Figure 42. Task manager

In order to fund our exported products we have to go to the Google Drive site that belongs to the same gmail account used to log in into GEE

<img align="center" src="../images/intro-gee-images/43_drive.png" hspace="15" vspace="10" width="300">

Figure 43. Google Drive folder that stores the exported products.

We will find one or more geotiff (.tiff) files depending on the size of our study area.

<img align="center" src="../images/intro-gee-images/44_tiff files.png" hspace="15" vspace="10" width="600">

Figure 44. Exported raster into two geotiff files.

Once the exportation has finished we can download the data and upload it in other platforms, for example, QGIS.

<img align="center" src="../images/intro-gee-images/45_qgis.png" hspace="15" vspace="10" width="600">

Figure 45. HydroShed River network raster loaded in QGIS.

Script link: [https://code.earthengine.google.com/1115def3cb8e14a3a772e03ebaa19aa7](https://code.earthengine.google.com/1115def3cb8e14a3a772e03ebaa19aa7).

## Challenge 1: Working with derived water indexes

Create a NDWI index using the Landsat 8 dataset for a period from January  2020 to June 2022 for a specific bioregion of Guyana.

*Hint 1: Use the Docs panel to look for existent scripts that already have done that.*

*Hint 2: Make use of the Cloud-Based Remote Sensing with Google Earth Engine: Fundamentals and Applications book (https://eefabook.org) to find help.*

## Challenge 2: Researching about Earth Engine applications

Develop a short research about the Google Earth Engine Mangrove Mapping Methodology tool.  Try to see how it works, what type of machine learning approach it uses, and analyze part of its code.
