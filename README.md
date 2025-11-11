# Getting Started with GEDI Spaceborne Lidar for Ecosystem Applications — Training Repository

[![DOI](https://zenodo.org/badge/984860390.svg)](https://doi.org/10.5281/zenodo.17353798)

![Last Updated](https://img.shields.io/github/last-commit/NASA-EarthRISE/training_Getting_started_with_GEDI_spaceborne_lidar?label=Last%20Updated&color=2E8B57)

[![Python 3.x](https://img.shields.io/badge/python-3.x-blue.svg)](https://www.python.org/)  
[![R](https://img.shields.io/badge/R-4.x-blue.svg)](https://www.r-project.org/)
[![JavaScript (Google Earth Engine)](https://img.shields.io/badge/JavaScript-Google%20Earth%20Engine-F7DF1E.svg?logo=javascript&logoColor=black)](https://earthengine.google.com/)
[![Google Colab](https://img.shields.io/badge/Google%20Colab-F9AB00.svg?logo=googlecolab&logoColor=white)](https://colab.research.google.com/)

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)  

![Visitor Badge](https://visitor-badge.laobi.icu/badge?page_id=nasa-earthrise.training_Getting_started_with_GEDI_spaceborne_lidar)

## Overview & Purpose
This repository hosts the **training webpage** and **hands-on tutorials** to help users learn the fundamentals of the **Global Ecosystem Dynamics Investigation (GEDI)** spaceborne LiDAR, apply GEDI data to real-world ecosystems, and explore best practices for preparing context-aware, quality-filtered GEDI datasets.

These tutorials are designed for users new to spaceborne lidar (GEDI) who want to:

- Explore GEDI L2B (height and structural metrics)  
- Estimate aboveground biomass using GEDI L4  
- Compare GEDI observations with local, high-resolution lidar data  
- Perform local biomass modeling combining R and Earth Engine (via JavaScript) scripts  

They demonstrate end-to-end workflows: downloading/processing data, visualizing, analysis, and interpretation.


## Citing & Credits
The training is developed by the [NASA EarthRISE](https://github.com/NASA-EarthRISE) team.  
You can also view a live version of the training site here:  
[nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar](https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/)


If you use these tutorials or adapt parts thereof in your research or teaching, please cite: 

Jiménez, S., Mayer, T., Pinto, N., Cooley, S., Healey, S., Christine Evans, Numata, I., Horn, K., West, D., Walker, K., Abramowitz, J., Cruz, S., Martin Arias, V., Pransky, L., Kruskopf, M., Yang, Z., Johnson, L., Fareed, N., d'Oliveira, M., … Billy Ashmall. (2025). NASA-EarthRISE/training_Getting_started_with_GEDI_spaceborne_lidar: v1.0.0 (First-release). Zenodo. https://doi.org/10.5281/zenodo.17353798

---

## Table of Contents

1. [Overview & Purpose](#overview--purpose)  
2. [Citing & Credits](#citing--credits)  
3. [Repository Structure](#repository-structure)  
4. [Getting Started](#getting-started)  
5. [Usage Guidelines & Tips](#usage-guidelines--tips)  
6. [Contribution & Issues](#contribution--issues)

---

## Repository Structure

A few highlights:

- `tutorials/` contains Jupyter notebooks (e.g. `Exploring_Forest_Structure_with_GEDI_L2B.ipynb`, `Exploring_Biomass_with_GEDI_l4.ipynb`) that walk you through using GEDI Level-2 and Level-4 products for forest-structure and biomass analysis with workflows combining **R**, **Python**, and **Google Earth Engine (JavaScript)**.
- `AOIs/` holds geospatial area-of-interest shapefiles or footprints for regional examples.  
- `docs/` and site files power the training webpage.  
- `images/` stores all figures used in the tutorials and web pages.  
- `append_field.txt` is a helper file (e.g. appended field metadata) used by one or more notebooks.

Here is an outline of the main directories and files:

├── AOIs/ ← Areas of interest, boundary files, etc.

├── docs/ ← Documentation or site-generation files

├── images/ ← Images used in tutorials / site

├── tutorials/ ← Python notebooks, R scripts, and Google Earth Engine Scripts / hands-on modules

- Exploring_forest_structure_with_GEDI_L2B.ipynb ← Module 2 L2B vegetation structure access and preparation
   
- Exploring_biomass_with_GEDI_L4.ipynb ← Module 2 L4A & L4B biomass estimations access and preparation
   
- Comparing_GEDI_L2B_with_highres_lidar_sewanee.ipynb ← Module 2 correlate GEDI with airborne lidar
   
- Local_biomass_modelling/ ← Module 4 correlating biomass with local survey data and airborne lidar
   
   - Exercise 1 ← R scripts and supporting datasets for GEDI simulation

   - Exercise 2 ← R script for Random Forest Modelling

   - Exercise 3 ← Google Earth Engine javascript for biomass time series and CCDC land cover maps
   
├── README.md ← This file

├── append_field.txt ← (auxiliary file used in tutorials)


---

## Getting Started

### Take the Course via the Webpage
Take the self-paced course via the webpage and follow along the knowledge checks, external links, supporting information, and guided tutorials for multiple media learning content.

#### **All Modules and Topics Overview**

Overview of topics covered in each module, respective format(s), and any technical requirements are listed below. Additional prerequisites and background information will be detailed within each module. 

#### **Module 1**

**Introduction to Full Waveform Lidar** introduces  lidar data, its physical principles, and its sensitivity to biophysical parameters, and explores several U.S.-based and global applications using different lidar sensors. Exercises will solidify participants’ understanding of waveform lidar capabilities in terrestrial applications. 

**Level:** introductory to intermediate.

| Section | Topics | Format | Requirements |
| :---: | ----- | ----- | ----- |
| **Introduction to GEDI Full Waveform Lidar for Terrestrial Applications** | Fundamentals of Lidar Remote Sensing | Recorded lecture | Video and audio viewing |
| **Introduction to GEDI Full Waveform Lidar for Terrestrial Applications** | Full Waveform Lidar from the GEDI Mission | Recorded lecture | Video and audio viewing |
| **Practical Applications with GEDI Full Waveform Data** | **Tutorial**: *A comprehensive Python notebook for processing and analyzing NASA GEDI L1B (Global Ecosystem Dynamics Investigation Level 1B) full waveform LiDAR data using Google Colab* | Recorded demonstration and self-paced python tutorial | Google account and Drive | NASA EarthData Access Account | Python platform Google Colab notebooks | Github account |

#### **Module 2**

**A Deep Dive into GEDI** details the GEDI mission, its data products, and case study applications. Exploratory scripts provide starting points for participants to evaluate many of GEDI’s data products by incorporating recommended filtering and pre-processing strategies. 

**Level:** introductory to intermediate.

| Section | Topics | Format | Requirements |
| :---: | ----- | ----- | ----- |
| **Lasering in on the GEDI Mission** | Why GEDI?  | Self-paced lecture |  |
| **Lasering in on the GEDI Mission** | Navigating the GEDI Ecosystem | Self-paced lecture |  |
| **Lasering in on the GEDI Mission** | Tools for Navigating GEDI | Self-paced lecture |  |
| **Lasering in on the GEDI Mission** | The Building Block of Analysis | Self-paced lecture |  |
| **Lasering in on the GEDI Mission** | How GEDI Information Ecosystem Studies | Self-paced lecture |  |
| **From Ground to Canopy** | GEDI’s Elevation and Relative Height Metrics Explained | Self-paced lecture |  |
| **From Canopy Layers to Vertical Profiles** | Vegetation Structural Insights From GEDI | Self-paced lecture |  |
| **From Canopy Layers to Vertical Profiles** | **Tutorial**: *Exploring Forest Structure with GEDI L2B in the Southeast* | Self-paced python tutorial | Google account and Drive | NASA EarthData Access Account | Python platform Google Colab notebooks | Github account |
| **From Canopy Layers to Vertical Profiles** | **Tutorial**: *Comparing L2B PAI with high-resolution lidar in the Sewanee Domain, Tennessee* | Self-paced python tutorial | Google account and Drive | NASA EarthData Access Account | Python platform Google Colab notebooks | Github account |
| **Above Ground Biomass with GEDI** | GEDI’s Biomass Estimation Approach | Self-paced lecture |  |
| **Above Ground Biomass with GEDI** | **Tutorial**: *Exploring Biomass with GEDI L4A and L4B in the Southeast* | Self-paced python tutorial | Google account and Drive | NASA EarthData Access Account | Python platform Google Colab notebooks | Github account |

#### **Module 3**

**Biomass Change with OBIWAN** discusses downstream GEDI derived products for carbon and biomass monitoring with Online Biomass Inference using Waveforms and iNventory (OBIWAN) Application Programming Interface (API). Participants will learn how biomass products are derived and their advantages and limitations. This module shows participants how to create dashboards and reporting systems for any geography covering any period from 1985 to the present. 

**Level:** introductory to intermediate.

| Section | Topics | Format | Requirements |
| :---: | ----- | ----- | ----- |
| **From Estimating Biomass with GEDI to Estimating Biomass Change with OBIWAN API** | GEDI mission biomass estimation: theory and products | Recorded lecture | Video and audio viewing |
| **From Estimating Biomass with GEDI to Estimating Biomass Change with OBIWAN API** | OBIWAN: estimating biomass change with GEDI and Landsat time series | Recorded lecture | Video and audio viewing |
| **Customize OBIWAN through its API** | **Tutorial**: Using the OBIWAN API in Alabama | Recorded demonstration of python and Google Earth Engine tutorials | Video and audio viewing | Google account and Drive| Github account| *Optional* Google Earth Engine Account |

#### **Module 4**

**Localized Forest Biomass Estimation** will have advanced applications focused on GEDI derived products for monitoring forest disturbances and carbon dynamics. Dr. Izaya Numata (South Dakota State University) will present methods for forest biomass estimation using field, airborne, and satellite lidar (GEDI) data developed in Acre, Brazil. Participants will understand the advantages, limitations, and considerations for applying such methodology on their own. 

**Level:** Intermediate to advanced.

| Section | Topics | Format | Requirements |
| :---: | ----- | ----- | ----- |
| **Forest Biomass Estimation Using Data from Field, Airborne and Spaceborne Lidar** | Learn to develop local forest biomass model using field and remote sensing data and its application for AGB mapping | Self-paced lecture |  |
| **Calibrating Field, Airborne and Spaceborne Lidar** | **Tutorial: Exercise1)** GEDI waveform simulation with airborne lidar using rGEDI. **Exercise 2\)** Development of local AGB model with field AGB and simulated ALS Relative Height metrics in R. **Exercise 3\)** Above ground biomass change mapping in Google Earth Engine. | Self-paced RStudio and Google Earth Engine tutorials | Github account | RStudio, RTools, rGEDI, and rGEDIsimulator package and program installations | Google Earth Engine Account |



### Follow Along With or Adapt the Tutorials

This training contains modules and tutorials that build throughout the course. Below is a high-level overview:

Module, Focus	Description / Key Outcomes
- Module 1	[Introduction & lidar basics	(Placeholder) Orientation, LiDAR fundamentals, GEDI mission overview](https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/tutorial-applications-of-waveform-lidar)
- Module 2	[Forest structure (L2B)	Work with GEDI L2B products — waveform, footprint metrics, canopy height, etc.](https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/tutorial-vegetation-structure)
- Module 2	[PAI, GEDI-LAS comparisons	Compare GEDI metrics with local LiDAR or field data](https://github.com/NASA-EarthRISE/training_Getting_started_with_GEDI_spaceborne_lidar/blob/main/tutorials/Comparing_GEDI_L2B_with_highres_lidar_sewanee.ipynb)
- Module 2	[Biomass estimation (L4)	Use GEDI Level-4 biomass products, build biomass models](https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/tutorial-biomass)
- Module 3  [Biomass Change with OBIWAN]([https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/module-3-overview](https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/tutorial-obiwan-api))
- Module 4	[Advanced calibration for local biomass](https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/tutorial-calibrate-spaceborne-lidar)

Each notebook includes:
- Background context
- Step-by-step Python code
- Data download and processing instructions
- Visualizations (maps, graphs)
- Exercises or extension ideas
- Be sure to read the narrative around code cells — the explanations are integral to learning.

#### Before you begin EACH TUTORIAL, ensure you have the respective requirements for each tutorial:

- NASA EarthData Access account and login credentials or API token
- A working google colab notebook or jupyter notebook and Google account
- RStudio, Rtools, and respective defined libraries for Module 4 tutorials
- Google Earth Engine account using the Engine Code Editor
- Common geospatial/data libraries (e.g. `numpy`, `pandas`, `geopandas`, `rasterio`, `matplotlib`, etc.) are defined in the tutorials

> Note: The specific versions used in the original tutorials may matter for reproducibility. If version info is available in the notebooks or associated metadata, use those.


We recommend using a virtual environment (venv, conda, etc.) to isolate dependencies.

#### Running the Tutorials

For colab notebooks: 
1. Open a tutorial notebook, e.g. Exploring_forest_structure_with_GEDI_L2B.ipynb.
2. Go cell by cell (or “Run All”) to execute.
3. If prompted for credentials (Earthdata or Earth Engine), follow the instructions in cell comments.
4. Review outputs, figures, logs, intermediate files (if saved).

R + GEE (Local biomass modelling) tutorials, [follow along the prompts from the presentation](https://github.com/NASA-EarthRISE/training_Getting_started_with_GEDI_spaceborne_lidar/blob/main/GEDI_training_Numata.pdf): 
1. In R or RStudio, open the R scripts in Local_biomass_modelling/.
2. Set the working directory appropriately (point to where the .js file and data are).
3. If needed, edit the R script to point to your Python environment (via reticulate) or set paths.
4. Run the R script sequentially. It may call Earth Engine via R or call/submit the GEE JavaScript script.
5. For the JavaScript .js, you may run it in the GEE Code Editor (copy-paste)


## Usage Guidelines & Tips

Data Access
- Many modules download GEDI data (L2 or L4). Ensure your internet connection is stable and that you have sufficient disk space.

Quality Filtering
- GEDI data include quality flags (e.g. sensitivity, beam type, etc. ). Tutorials emphasize applying filters to avoid spurious or low-confidence measurements.

Spatial Subsets / AOIs
- Where possible, focus on modest-sized areas (e.g. tens to a few hundreds of footprints) to speed execution and avoid memory bottlenecks.

Reproducibility
- Document all file paths and data transformations.

Extensibility
- You are encouraged to adapt notebook logic to your own regions (AOIs), combine with other datasets, and extend biomass/height modeling workflows.

Troubleshooting
- If you get dependency conflicts, try isolating in a fresh virtual environment by restarting or disconnect/reconnect the Runs.
- For large raster or geospatial operations, monitor memory use, delete old versions, especially if saved to Drive.
- If data downloads fail, check URLs/availability or replace with alternate mirrors.
- “Module not found” in Python --> Confirm you installed all packages in the active environment

## Contribution & Issues

On the [Discussions Forum](https://github.com/NASA-EarthRISE/training_Getting_started_with_GEDI_spaceborne_lidar/discussions), we welcome contributions, feedback, and requests such as: 
- Filing GitHub issues for bugs, suggestions, broken links, or errata.
- Submitting pull requests with improvements, new modules, or fixes.
- Proposing new case studies or extension notebooks (e.g. for tropical forests, urban forests, change detection), especially via the pre and post questionnaires.
- Updating documentation or enhancing clarity of narrative text.
