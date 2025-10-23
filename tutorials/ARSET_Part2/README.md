# GEDI LiDAR Footprint Access, Analysis, Visualization, and Download Tutorial

## Quick Start
Option 1 — Run in Google Colab

You can open and execute this tutorial directly in Colab without installing anything locally:
[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/NASA-EarthRISE/training_Getting_started_with_GEDI_spaceborne_lidar/blob/main/tutorials/ARSET_Part2/Select_GEDI_L2B_footprints.ipynb)

## Overview

This tutorial provides an end-to-end workflow for accessing, filtering, visualizing, and exporting GEDI (Global Ecosystem Dynamics Investigation) LiDAR footprint data. It is designed for ecosystem scientists, researchers, and professionals who wish to explore canopy structure metrics and spatial patterns using GEDI datasets.

Using this notebook, you will:

* Search and retrieve GEDI footprints for user-defined Areas of Interest (AOIs)

* Filter GEDI shots based on key canopy metrics (e.g., FHD, PAI)

* Visualize GEDI data interactively in 2D and 3D

* Export analyzed results as CSV or Shapefile formats

* The tutorial is implemented in Python and runs directly within a Jupyter Notebook environment.

## Features

* Data Access: Automated download of GEDI L2A/L2B data subsets for custom AOIs

* Geospatial Processing: AOI selection, geodataframe creation, and coordinate handling

* Analysis Tools: Calculation and comparison of canopy structure metrics across sites

* Visualization: Interactive maps and 3D footprint plots using matplotlib and plotly

* Export: Save processed footprints as .csv or .shp files for GIS or further analysis

##Notebook Structure

1. Installs required Python libraries via pip
2. Imports core dependencies
3. Setup directories
4. Setup and access Harmony API capabilities
5. Establish the study area either by github data hosting or from Google drive.
6. Esablish the temporal period to filter the data by
7. Submit the download request for raw HDF5 files with Harmony
8. Subset the raw files by specific data variables and beams.
9. Explore the raw and subset HDF5 files in colab
10. Convert the HDF5 subset data to a GeoDataFrame
11. Optionally convert the GeoDataFrame for subset data to Shapefile or CSV and export to Google Drive.
12. Explore quality filtering techniques
13. Explore the data with no data values removed
14. Map the difference between quality and non-quality filtered datasets
15. Create a comparative bar plot for counting observations per year for each study area.
16. Optionally save the final filtered dataset as a Shapefile or CSV
17. Plot and explore the final dataset
18. Execute monthly summary violin plots of data by variable for each year of a study area.
19. Create a pairplot show relationships between each subset variable in the dataset. 
20. Generate interactive 3-D visualizations of the data variables for vegetation structure for each study area.
21. Generate a static 3-D visualization of the data variables for vegetation structure for each study area.

## Data Export
`gdf.to_file("Pinyon_GEDIL2B.shp")`
`gdf.to_csv("Pinyon_GEDIL2B.csv")`
