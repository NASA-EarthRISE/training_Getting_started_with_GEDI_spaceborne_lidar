---
layout: page
title: Challenges - SEPAL Sample Design and CEO Project Creation 
parent: "Map Validation and Area Estimation with Collect Earth Online"
nav_order: 7
---

# Challenges

Work on whichever challenges are interesting to you. Collaborate with your colleagues and your instructors. Happy Coding!

## Find more interpretation imagery

See if you can find additional imagery in GEE (or elsewhere) that can help you in your validation point interpretation in CEO.  Add this imagery to our workshop's institution and symbolize it appropriately.

## Create a stratified random sample in SEPAL
Use the SEPAL `Stratified Area Estimator - Design` tool to create a new stratified random sample from the mangrove classification, and export the points into CEO.

## Rerun the analysis with new validation points
Create a new set of sample points in GEE by changing the `seed` value. Load these into a new CEO project and do the interpretation for the new points.  Load the interpretation results into SEPAL and do the `Stratified Area Estimator - Analysis` again.  How have the results changed?  Why is using such a small sample size for validation points not a good idea?  

```javascript
//--------------------------------------------------------------
// Create Stratified Random Sample
//--------------------------------------------------------------

// create stratified random sample from the processed mangrove raster
// values in 'class' property: 0-Not Mangrove,  1-Mangrove
var samplePts = mangrove.stratifiedSample({
  // total # points
  numPoints:20, 
  classBand:'classification', 
  region:mangrove.geometry(), 
  scale:30, 
  projection:'EPSG:4326', 
  seed:1010, 
  classValues:[0,1],
  // # points in each class
  classPoints:[10,10], 
  dropNulls:true, 
  tileScale:2, 
  geometries:true});
print('Sample points:', samplePts);

// Add points to the map
Map.addLayer(samplePts,{},'sample points');
```

# Group Discussions
* Group examination of difficult interpretations
* How could CEO be useful to you?
* What tools/data would be helpful to include in your project?

# General Help
*If you ever need help with your work in CEO a good place to start is with the `Support` and `Blog` pages linked at the top of the CEO website. Tutorials are provided for project creation and interpretation*


