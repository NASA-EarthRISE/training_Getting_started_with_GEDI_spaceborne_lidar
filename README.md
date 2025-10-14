# Getting Started with GEDI Spaceborne Lidar for Ecosystem Applications — Training Repository

[![DOI](https://zenodo.org/badge/984860390.svg)](https://doi.org/10.5281/zenodo.17353797)
[![Python 3.x](https://img.shields.io/badge/python-3.x-blue.svg)](https://www.python.org/)  
[![R](https://img.shields.io/badge/R-4.x-blue.svg)](https://www.r-project.org/)
[![JavaScript (Google Earth Engine)](https://img.shields.io/badge/JavaScript-Google%20Earth%20Engine-F7DF1E.svg?logo=javascript&logoColor=black)](https://earthengine.google.com/)
[![Google Colab](https://img.shields.io/badge/Google%20Colab-F9AB00.svg?logo=googlecolab&logoColor=white)](https://colab.research.google.com/)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)  
![Last Updated](https://img.shields.io/github/last-commit/NASA-EarthRISE/training_Getting_started_with_GEDI_spaceborne_lidar?label=Last%20Updated&color=2E8B57)


This repository hosts the **training webpage** and **hands-on tutorials** to help users learn the fundamentals of the **Global Ecosystem Dynamics Investigation (GEDI)** spaceborne LiDAR, apply GEDI data to real-world ecosystems, and explore best practices for preparing context-aware, quality-filtered GEDI datasets.

The training is developed by the [NASA EarthRISE](https://github.com/NASA-EarthRISE) team.  
You can also view a live version of the training site here:  
[nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar](https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/) :contentReference[oaicite:0]{index=0}

## Citing & Credits

If you use these tutorials or adapt parts thereof in your research or teaching, please cite: jimenezSA, Tim Mayer, agoberna, Billy Ashmall, Emil Cherrington, & Christine Evans. (2025). NASA-EarthRISE/training_Getting_started_with_GEDI_spaceborne_lidar: v1.0.0 (First-release). Zenodo. https://doi.org/10.5281/zenodo.17353798

---

## Table of Contents

1. [Repository Structure](#repository-structure)  
2. [Getting Started](#getting-started)  
   1. [Prerequisites](#prerequisites)  
   2. [Installation](#installation)  
   3. [Launching Locally](#launching-locally)  
3. [Tutorial Modules](#tutorial-modules)  
4. [Usage Guidelines & Tips](#usage-guidelines--tips)  
5. [Contribution & Issues](#contribution--issues)  
6. [Citing & Credits](#citing--credits)  
7. [License](#license)

---

## Repository Structure

Here is an outline of the main directories and files:
├── AOIs/ ← Areas of interest, boundary files, etc.
├── docs/ ← Documentation or site-generation files
├── images/ ← Images used in tutorials / site
├── tutorials/ ← Jupyter notebooks / hands-on modules
├── README.md ← This file
├── append_field.txt ← (auxiliary file used in tutorials/site)



A few highlights:

- `tutorials/` contains Jupyter notebooks (e.g. `Exploring_Forest_Structure_with_GEDI_L2B.ipynb`, `Exploring_Biomass_with_GEDI_l4.ipynb`) that walk you through using GEDI Level-2 and Level-4 products for forest-structure and biomass analysis.
- `AOIs/` holds geospatial area-of-interest shapefiles or footprints for regional examples.  
- `docs/` and site files power the training webpage.  
- `images/` stores all figures used in the tutorials and web pages.  
- `append_field.txt` is a helper file (e.g. appended field metadata) used by one or more notebooks.

---

## Getting Started

### Taking the Course
Take the self-paced course via the webpage and follow along the knowledge checks, external links, supporting information, and guided tutorials for multiple media learning content.

### Tutorial Prerequisites

#### Tutorial Modules

The repository is organized into modules that build progressively. Below is a high-level overview:

Module	Focus	Description / Key Outcomes
Module 1	[Introduction & basics	(Placeholder) Orientation, LiDAR fundamentals, GEDI mission overview](https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/tutorial-applications-of-waveform-lidar)
Module 2	[Forest structure (L2B)	Work with GEDI L2B products — waveform, footprint metrics, canopy height, etc.](https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/tutorial-vegetation-structure)
Module 2	[PAI, GEDI-LAS comparisons	Compare GEDI metrics with local LiDAR or field data](https://github.com/NASA-EarthRISE/training_Getting_started_with_GEDI_spaceborne_lidar/blob/main/tutorials/Comparing_GEDI_L2B_with_highres_lidar_sewanee.ipynb)
Module 2	[Biomass estimation (L4)	Use GEDI Level-4 biomass products, build biomass models](https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/tutorial-biomass)
Module 3  [Biomass Change with OBIWAN]([https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/module-3-overview](https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/tutorial-obiwan-api))
Module 4	[Advanced calibration for local biomass](https://nasa-earthrise.github.io/training_Getting_started_with_GEDI_spaceborne_lidar/tutorial-calibrate-spaceborne-lidar)

Each notebook includes:
- Background context
- Step-by-step Python code
- Data download and processing instructions
- Visualizations (maps, graphs)
- Exercises or extension ideas
- Be sure to read the narrative around code cells — the explanations are integral to learning.

#### Before you begin EACH TUTORIAL, ensure you have:

- **Python 3.7+** (or a compatible version)  
- A working Jupyter environment (e.g. JupyterLab or Jupyter Notebook)  
- Common geospatial/data libraries (e.g. `numpy`, `pandas`, `geopandas`, `rasterio`, `matplotlib`, etc.)  
- Internet access (for downloading GEDI data, auxiliary datasets, or dependencies)  

We recommend using a virtual environment (venv, conda, etc.) to isolate dependencies.

#### Installation

1. Clone this repository:

   ```bash
   git clone https://github.com/NASA-EarthRISE/training_Getting_started_with_GEDI_spaceborne_lidar.git
   cd training_Getting_started_with_GEDI_spaceborne_lidar
   ```

2. Create and activate a virtual environment:
```bash
  python3 -m venv venv
  source venv/bin/activate     # On Windows: venv\Scripts\activate
```

3. Install required Python packages.

4. Proceed with the guided notebook


## Usage Guidelines & Tips

Data Access
- Many modules download GEDI data (L2 or L4) or atmospheric, land-cover or topology auxiliary layers (e.g. SRTM, DEMs). Ensure your internet connection is stable and that you have sufficient disk space.

Quality Filtering
- GEDI data include quality flags (e.g. sensitivity, beam status). Tutorials emphasize applying filters to avoid spurious or low-confidence measurements.

Spatial Subsets / AOIs
- Where possible, focus on modest-sized areas (e.g. tens to a few hundreds of footprints) to speed execution and avoid memory bottlenecks.

Reproducibility
- Use fixed seeds in random processes (if introduced) and document all file paths and data transformations.

Extensibility
- You are encouraged to adapt notebook logic to your own regions (AOIs), combine with other datasets, and extend biomass/height modeling workflows.

Troubleshooting
- If you get dependency conflicts, try isolating in a fresh virtual environment.
- For large raster or geospatial operations, monitor memory use.
- If data downloads fail, check URLs/availability or replace with alternate mirrors.

## Contribution & Issues

We welcome contributions, feedback, and requests. You can support this repository by:
- Filing GitHub issues for bugs, suggestions, broken links, or errata.
- Submitting pull requests with improvements, new modules, or fixes.
- Proposing new case studies or extension notebooks (e.g. for tropical forests, urban forests, change detection), especially via the pre and post questionnaires.
- Updating documentation or enhancing clarity of narrative text.

When contributing, please follow GitHub best practices (issue templates, PR reviews, code style consistency).
