---
layout: page
title: Summary
parent: A Deep Dive into GEDI
permalink: /module-2-summary
nav_order: 5
---

# **GEDI Essentials Recap**

Module 2 has taken participants on a comprehensive exploration of the GEDI mission, from its technical design and global ecological significance to the detailed interpretation of its 3-dimensional data products. By unpacking the foundational variables, waveform characteristics, and core vegetation and biomass metrics, this module has equipped users with both the conceptual understanding and practical skills needed to begin working confidently with GEDI data. The hands-on exercises and case studies demonstrated real-world applications in forest monitoring, carbon assessment, biodiversity conservation, and land management, while also highlighting the challenges and opportunities that come with integrating GEDI into broader remote sensing workflows. As participants move forward, they are encouraged to apply these insights, explore GEDI’s capabilities within their own study areas, and build upon the foundational workflows introduced here to set the stage for the more advanced and application-focused modules to come.

# **Answer Key to the Knowledge Checks**

#### **Knowledge Check \#11: True or False?:** 

GEDI’s pre-launch above ground biomass calibration equations were generated from lidar data only. 

* False, generating above ground biomass calibration equations is a function of lidar data and ground plot field inventory datasets that are standardized. The lidar data are used to simulate GEDI-like waveforms and derived metrics using the GEDI simulator (link). These reference lidar data based simulations are then used to calibrate biomass equations with the field biomass reference data. The lidar data can be airborne, drone, or terrestrial laser scanned data so long as it meets the reference data requirements (link cal/val GEDI mission page).

#### **Knowledge Check \#12:** 

* Which dataset (L4A vs L4B) would you use for plot-level forest inventory → L4A  
* Which dataset (L4A vs L4B) would you use for national carbon accounting → L4B

#### **Knowledge Check \#13**

* agbd\_se quantifies the uncertainty of each biomass estimate

#### **Knowledge Check \#14**

* True or False: A dense canopy always means high biomass.  
  * False. Dense tree cover doesn’t always equal high biomass — forest age, disturbance, or degradation can lead to intact canopies with relatively low biomass. That’s why GEDI’s biomass estimates (agbd) are more informative than tree cover alone.

#### **Knowledge Check \#15**

* What makes GEDI unique compared to optical or radar-based biomass datasets?  
  * GEDI uses spaceborne lidar to directly measure forest vertical structure and height, producing globally consistent biomass estimates. Optical and radar data often saturate in dense forests, but GEDI provides a calibrated lidar backbone for global biomass monitoring.

# **References**

Adrah, E., Wong, J. P., & Yin, H. (2025). Integrating GEDI, Sentinel-2, and Sentinel-1 imagery for tree crops mapping. Remote Sensing of Environment, 319, 114644\. [https://doi.org/10.1016/j.rse.2025.114644](https://doi.org/10.1016/j.rse.2025.114644) 

Altarez, R. D. D., Apan, A., & Maraseni, T. (2025). Integrated multi-satellite data and machine learning approach in mapping the successional stages of forest types in a tropical montane forest. Remote Sensing Applications: Society and Environment, 37, 101407\. [https://doi.org/10.1016/j.rsase.2024.101407](https://doi.org/10.1016/j.rsase.2024.101407) 

Aragoneses, E., García, M., Tang, H., & Chuvieco, E. (2025). A multi-sensor approach allows confident mapping of forest canopy fuel load and canopy bulk density to assess wildfire risk at the European scale. Remote Sensing of Environment, 318, 114578\. [https://doi.org/10.1016/j.rse.2024.114578](https://doi.org/10.1016/j.rse.2024.114578) 

Armston, J., Luthcke, S. B., Hofton, M., Kellner, J. R., & Dubayah, R. (2020). Biomass estimation from simulated GEDI, ICESat-2 and NISAR across environmental gradients in Sonoma County, California. Remote Sensing of Environment, 242, 111779\. [https://doi.org/10.1016/j.rse.2020.111779](https://doi.org/10.1016/j.rse.2020.111779) 

Armston, J., Dubayah, R. O., Healey, S. P., Yang, Z., Patterson, P. L., Saarela, S., Stahl, G., Duncanson, L., Kellner, J. R., Pascual, A., & Bruening, J. M. (2023). GEDI L4B Country-level Summaries of Aboveground Biomass (Version 1). ORNL Distributed Active Archive Center. [https://doi.org/10.3334/ORNLDAAC/2321](https://doi.org/10.3334/ORNLDAAC/2321) 

Asner, G. P., et al. (2010). High-resolution mapping of forest carbon stocks in the Colombian Amazon. Biogeosciences, 7(2), 341–361.

Barbosa, F. L. R., Guimarães, R. F., de Carvalho Júnior, O. A., Gomes, R. A. T., de Carvalho, O. L. F., & de Lima, T. P. M. (2022). Estimating the Optimal Threshold for Accuracy Assessment of the Global Ecosystem Dynamics Investigation (GEDI) Data in a Gentle Relief Urban Area. Remote Sensing, 14(15), 3540\. [https://doi.org/10.3390/rs14153540](https://doi.org/10.3390/rs14153540) 

Blair, B., Luthcke, S., Fatoyinbo, L., Abernethy, K., … Zgraggen, C. (2022). Aboveground biomass density models for NASA’s Global Ecosystem Dynamics Investigation (GEDI) lidar mission. Remote Sensing of Environment, 270, 112845\. [https://doi.org/10.1016/j.rse.2021.112845](https://doi.org/10.1016/j.rse.2021.112845) 

Beck, J., Wirt, B., Armston, J., Hofton, M., Luthcke, S., & Tang, H. (2021). GLOBAL Ecosystem Dynamics Investigation (GEDI) Level 2 user guide: For SDPS PGE Version 3 (P003) of GEDI L2A data and SDPS PGE Version 3 (P003) of GEDI L2B data (Version 2.0). NASA Goddard Space Flight Center.

Bonan, G.B. (2008). Forests and climate change: Forcings, feedbacks, and the climate benefits of forests. Science, 320(5882), 1444-1449.

Brunner, I., Herzog, C., Dawes, M.A., Arend, M., & Sperisen, C. (2015). How tree roots respond to drought. Frontiers in Plant Science, 6, 547\.

Bueno, I. T., Silva, C. A., Anderson-Teixeira, K., Magee, L., Zheng, C., Broadbent, E. N., Zambrano, A. M. A., & Johnson, D. J. (2025). Aboveground Biomass and Tree Mortality Revealed Through Multi-Scale LiDAR Analysis. Remote Sensing, 17(5), Article 5\. [https://doi.org/10.3390/rs17050796](https://doi.org/10.3390/rs17050796) 

Burns, P., Hakkenberg, C., & Goetz, S. J. (2024). Gridded GEDI Vegetation Structure Metrics and Biomass Density at Multiple Resolutions (Version 1). ORNL Distributed Active Archive Center. [https://doi.org/10.3334/ORNLDAAC/2339](https://doi.org/10.3334/ORNLDAAC/2339) 

Clark, D.A., Brown, S., Kicklighter, D.W., Chambers, J.Q., Thomlinson, J.R., & Ni, J. (2001). Measuring net primary production in forests: concepts and field methods. Ecological Applications, 11(2), 356-370.  
Combining spaceborne lidar from the Global Ecosystem Dynamics Investigation with local knowledge for monitoring fragmented tropical landscapes: A case study in the forest–agriculture interface of Ucayali, Peru. Ecology and Evolution, 14(8), e70116. [https://doi.org/10.1002/ece3.70116](https://doi.org/10.1002/ece3.70116) 

Cooley, S. S., Pinto, N., Becerra, M., Alvarado, J. W. V., Fahlen, J. C., Rivera, O., Fricker, G. A., Dantas, A.  
Costanza, R., et al. (2017). Twenty years of ecosystem services: How far have we come and how far do we still need to go? Ecosystem Services, 28, 1-16.

De Conto, T., Armston, J., & Dubayah, R. (2024). Characterizing the structural complexity of the Earth’s forests with spaceborne lidar. Nature Communications, 15(1), 8116\. [https://doi.org/10.1038/s41467-024-52468-2](https://doi.org/10.1038/s41467-024-52468-2) 

De Conto, T., Armston, J., \&amp; Dubayah, R. O. (2024). \<i\>GEDI L4C Footprint Level Waveform Structural Complexity Index, Version 2\</i\> (Version 2). ORNL Distributed Active Archive Center. [https://doi.org/10.3334/ORNLDAAC/2338](https://doi.org/10.3334/ORNLDAAC/2338) 

Di Tommaso, S., Wang, S., Vajipey, V., Gorelick, N., Strey, R., & Lobell, D. B. (2023). Annual Field-Scale Maps of Tall and Short Crops at the Global Scale Using GEDI and Sentinel-2. Remote Sensing, 15(17), Article 17\. [https://doi.org/10.3390/rs15174123](https://doi.org/10.3390/rs15174123) 

Dionizio, E. A., Giannini, T. C., Gastauer, M., Barbosa-Silva, R. G., Souza, R. L. F. D., Nunes, S., Ramos, S., Andrino, C. O., & Cavalcante, R. (2025). Aboveground carbon stocks for different forest types in eastern Amazonia. Environmental Research Communications. [https://doi.org/10.1088/2515-7620/adc06a](https://doi.org/10.1088/2515-7620/adc06a) 

Doyle, E. L., Graham, H. A., Boulton, C. A., Lenton, T. M., Feldpausch, T. R., & Cunliffe, A. M. (2025). Evaluating GEDI for quantifying forest structure across a gradient of degradation in Amazonian rainforests. Environmental Research Letters, 20(5), 054016\. [https://doi.org/10.1088/1748-9326/adc752](https://doi.org/10.1088/1748-9326/adc752) 

Dubayah, R. O., Armston, J., Healey, S. P., Bruening, J. M., Patterson, P. L., Kellner, J. R., Duncanson, L., Saarela, S., Ståhl, G., Yang, Z., Tang, H., Blair, J. B., Fatoyinbo, L., Goetz, S., Hancock, S., Hansen, M., Hofton, M., Hurtt, G., & Luthcke, S. (2022). GEDI launches a new era of biomass inference from space. Environmental Research Letters, 17(9), 095001\. [https://doi.org/10.1088/1748-9326/ac8694](https://doi.org/10.1088/1748-9326/ac8694) 

Dubayah, R. O., Armston, J., Healey, S.P., Bruening, J.M., Patterson, P.L., Kellner, J.R., Duncanson, L., Saarela, S., Stahl, G., Yang, Z., Tang, H., Blair, J.B., Fatoyinbo, L., Goetz, S., Hancock, S., Hansen, M., Hofton, M., Hurtt, G., Marselis, S. (2022a). GEDI L4A Footprint Level Aboveground Biomass Density, Version 2.1. ORNL DAAC, Oak Ridge, Tennessee, USA. [https://doi.org/10.3334/ORNLDAAC/2056](https://doi.org/10.3334/ORNLDAAC/2056) 

Dubayah, R. O., Armston, J., Healey, S. P., Yang, Z., Patterson, P. L., Saarela, S., Stahl, G., Duncanson, L., Kellner, J. R., Bruening, J. M., & Pascual, A. (2023). GEDI L4B Gridded Aboveground Biomass Density, Version 2.1 (Version 2.1). ORNL Distributed Active Archive Center. [https://doi.org/10.3334/ORNLDAAC/2299](https://doi.org/10.3334/ORNLDAAC/2299) 

Dubayah, R. O., Armston, J., Healey, S. P., Yang, Z., Patterson, P. L., Saarela, S., Stahl, G., Duncanson, L., & Kellner, J. R. (2022). GEDI L4B Gridded Aboveground Biomass Density, Version 2 (User Guide). Oak Ridge National Laboratory Distributed Active Archive Center. [https://doi.org/10.3334/ORNLDAAC/2017](https://doi.org/10.3334/ORNLDAAC/2017)

Dubayah, R. O., Armston, J., Kellner, J. R., Duncanson, L., Healey, S. P., Patterson, P. L., Hancock, S.,   
Tang, H., Bruening, J., Hofton, M. A., Blair, J. B., & Luthcke, S. B. (2021). GEDI L4A Footprint Level Aboveground Biomass Density, Version 2 (Version 2). ORNL Distributed Active Archive Center. [https://doi.org/10.3334/ORNLDAAC/1986](https://doi.org/10.3334/ORNLDAAC/1986) 

Dubayah, R., Blair, J. B., Goetz, S., Fatoyinbo, L., Hansen, M., Healey, S., Hofton, M., Hurtt, G., Kellner, J., Luthcke, S., Armston, J., Tang, H., Duncanson, L., Hancock, S., Jantz, P., Marselis, S., Patterson, P. L., Qi, W., & Silva, C. (2020). The global ecosystem dynamics investigation: High-resolution laser ranging of the Earth's forests and topography. Science of Remote Sensing, 1, 100002\.

Dubayah, R., Blair, J.B., Goetz, S., Fatoyinbo, L., Hansen, M., Healey, S., Hofton, M., Hurtt, G., Kellner, J., Luthcke, S., Armston, J., Tang, H., Duncanson, L., Hancock, S., Jantz, P., Marselis, S., Patterson, P.L., Qi, W., Silva, C. (2020). The Global Ecosystem Dynamics Investigation: High-resolution laser ranging of the Earth's forests and topography. Science of Remote Sensing, 1, 100002\. [https://doi.org/10.1016/j.srs.2020.100002](https://doi.org/10.1016/j.srs.2020.100002) 

Dubayah, R. O., Duncanson, L., Healey, S. P., Armston, J., Kellner, J. R., & others. (2022). GEDI L4A Footprint Level Aboveground Biomass Density, Version 2 (User Guide). ORNL DAAC. [https://doi.org/10.3334/ORNLDAAC/2055](https://doi.org/10.3334/ORNLDAAC/2055) 

Dubayah, R., Hofton, M., Blair, J., Armston, J., Tang, H., & Luthcke, S. (2021). GEDI L2A Elevation and Height Metrics Data Global Footprint Level V002 \[Data set\]. NASA Land Processes Distributed Active Archive Center. [https://doi.org/10.5067/GEDI/GEDI02\_A.002](https://doi.org/10.5067/GEDI/GEDI02_A.002) 

Dubayah, R.O., S.B. Luthcke, T.J. Sabaka, J.B. Nicholas, S. Preaux, and M.A. Hofton. 2021\. GEDI L3 Gridded Land Surface Metrics, Version 2\. ORNL DAAC, Oak Ridge, Tennessee, USA. [https://doi.org/10.3334/ORNLDAAC/1952](https://doi.org/10.3334/ORNLDAAC/1952) 

Dubayah, R., et al. (2020). The Global Ecosystem Dynamics Investigation: High-resolution laser ranging of the Earth’s forests and topography. Science of Remote Sensing, 1, 100002\.

Dubayah, R.O., S.B. Luthcke, T.J. Sabaka, J.B. Nicholas, S. Preaux, and M.A. Hofton. 2021\. GEDI L3 Gridded Land Surface Metrics, Version 1\. ORNL DAAC, Oak Ridge, Tennessee, USA. [https://doi.org/10.3334/ORNLDAAC/1865](https://doi.org/10.3334/ORNLDAAC/1865) 

Dubayah, R., Tang, H., Armston, J., Luthcke, S., Hofton, M., & Blair, J. (2021). GEDI L2B Canopy Cover and Vertical Profile Metrics Data Global Footprint Level V002 \[Data set\]. NASA Land Processes Distributed Active Archive Center. [https://doi.org/10.5067/GEDI/GEDI02\_B.002](https://doi.org/10.5067/GEDI/GEDI02_B.002) 

Dubayah, R. O., Qi, W., Armston, J., Fatoyinbo, L., Papathanassiou, K., Pardini, M., Stovall, A., Choi, C., & Cazcarra-Bes, V. (2023). Pantropical Forest Height and Biomass from GEDI and TanDEM-X Data Fusion (Version 1). ORNL Distributed Active Archive Center. [https://doi.org/10.3334/ORNLDAAC/2298](https://doi.org/10.3334/ORNLDAAC/2298) 

Duncanson, L., Hancock, S., Armston, J., Kellner, J. R., Cohen, W. B., & Yang, Z. (2019). Statistical properties of hybrid estimators proposed for GEDI—NASA’s global ecosystem dynamics investigation. Environmental Research Letters, 14(6), 065007\. [https://doi.org/10.1088/1748-9326/ab18df](https://doi.org/10.1088/1748-9326/ab18df) 

Duncanson, L., Kellner, J.R., Armston, J., Dubayah, R., Minor, D.M., Hancock, S., Healey, S.P., Patterson, P.L., Saarela, S., Marselis, S. and Silva, C.E., 2022\. Aboveground biomass density models for NASA’s Global Ecosystem Dynamics Investigation (GEDI) lidar mission. Remote Sensing of Environment, 270, p.112845. [https://doi.org/10.1016/j.rse.2021.112845](https://doi.org/10.1016/j.rse.2021.112845) 

Duncanson, L., Kellner, J.R., Armston, J., Dubayah, R., Minor, D.M., Hancock, S., Healey, S.P., Patterson, P.L., Saarela, S., Marselis, S., Silva, C.E., Bruening, J., Goetz, S.J., Tang, H., Hofton, M., Blair, B., Luthcke, S., Fatoyinbo, L., Abernethy, K., et al. (2022). Aboveground biomass density models for NASA's Global Ecosystem Dynamics Investigation (GEDI) lidar mission. Remote Sensing of Environment, 270, 112845\. [https://doi.org/10.1016/j.rse.2021.112845](https://doi.org/10.1016/j.rse.2021.112845) 

Duncanson, L., Neuenschwander, A., Hancock, S., Thomas, N., Fatoyinbo, T., Simard, M., Silva, C. A., Armston, J., Luthcke, S. B., Hofton, M., Kellner, J. R., & Dubayah, R. (2020). Biomass estimation from simulated GEDI, ICESat-2 and NISAR across environmental gradients in Sonoma County, California. Remote Sensing of Environment, 242, 111779. [https://doi.org/10.1016/j.rse.2020.111779](https://doi.org/10.1016/j.rse.2020.111779)

Duncanson, L., Neuenschwander, A., Hancock, S., Thomas, N., Fatoyinbo, T., Simard, M., Silva, C. A., Dwiputra, A., Coops, N. C., & Schwartz, N. B. (2023). GEDI waveform metrics in vegetation mapping—A case study from a heterogeneous tropical forest landscape. Environmental Research Letters, 18(1), 015007\. [https://doi.org/10.1088/1748-9326/acad8d](https://doi.org/10.1088/1748-9326/acad8d) 

Elliott, L. H., Vogeler, J. C., Holbrook, J. D., Barry, B. R., & Vierling, K. T. (2024). Assessing GEDI data fusions to map woodpecker distributions and biodiversity hotspots. Environmental Research Letters, 19(9), 094027\. [https://doi.org/10.1088/1748-9326/ad64eb](https://doi.org/10.1088/1748-9326/ad64eb) 

Evans, C., Carey, L., Guerra, F., Cherrington, E. A., Correa, E., & Quintero, D. (2025). Cloud-Based Solutions for Monitoring Coastal Ecosystems and the Prioritization of Restoration Efforts Across Belize. Remote Sensing, 17(20), 3396\. [https://doi.org/10.3390/rs17203396](https://doi.org/10.3390/rs17203396) 

Fareed, N., Numata, I., Cochrane, M. A., Novoa, S., Tenneson, K., Melo, A. W. F. de, da Silva, S. S., Oliveira, M. V. N. d’, Nicolau, A., & Zutta, B. (2025). Aboveground biomass modeling using simulated Global Ecosystem Dynamics Investigation (GEDI) waveform LiDAR and forest inventories in Amazonian rainforests. Forest Ecology and Management, 578, 122491\. [https://doi.org/10.1016/j.foreco.2024.122491](https://doi.org/10.1016/j.foreco.2024.122491) 

Fayad, I., Baghdadi, N., & Lahssini, K. (2022). An Assessment of the GEDI Lasers’ Capabilities in Detecting Canopy Tops and Their Penetration in a Densely Vegetated, Tropical Area. Remote Sensing, 14(13), Article 13\. [https://doi.org/10.3390/rs14132969](https://doi.org/10.3390/rs14132969) 

Fayad, I., Baghdadi, N., Bailly, J. S., Frappart, F., & Zribi, M. (2020). Analysis of GEDI Elevation Data Accuracy for Inland Waterbodies Altimetry. Remote Sensing, 12(17), Article 17\. [https://doi.org/10.3390/rs12172714](https://doi.org/10.3390/rs12172714) 

Fayad, I., et al. (2021). A CNN-based approach for the estimation of canopy heights and wood volume from GEDI waveforms. Remote Sensing of Environment. Vol. 265\. [https://www.sciencedirect.com/science/article/pii/S0034425721003722](https://www.sciencedirect.com/science/article/pii/S0034425721003722) 

Fayad: Terrain Slope Effect on Forest Height and Wood Volume Estimation from GEDI Data. (2021). Retrieved January 27, 2025, from https://www.mdpi.com/2072-4292/13/11/2136\#

Fu, L., Shu, Q., Yang, Z., Xia, C., Zhang, X., Zhang, Y., Li, Z., & Li, S. (2025). Accuracy assessment of topography and forest canopy height in complex terrain conditions of Southern China using ICESat-2 and GEDI data. Frontiers in Plant Science, 16, 1547688\. [https://doi.org/10.3389/fpls.2025.1547688](https://doi.org/10.3389/fpls.2025.1547688) 

Geremew, T., Zewdie, W., & Pellikka, P. (2023). Ecosystem extent mapping by integrating Landsat 8, PALSAR-2, and GEDI lidar. Applied Geomatics, 15(1), 61–76. [https://doi.org/10.1007/s12518-022-00485-5](https://doi.org/10.1007/s12518-022-00485-5) 

Gillman, L.N., et al. (2015). Latitude, productivity and species richness. Global Ecology and Biogeography, 24(1), 107-117.

Han, H., Xu, K., Zhang, Z., Zhao, P., Jiang, H., & Ding, A. (2024). Impact of GEDI-Derived Forest Vertical Structure Characteristics on the Accuracy Gains in Regional Dominant Tree Species Mapping. IEEE Geoscience and Remote Sensing Letters, 21, 1–5. IEEE Geoscience and Remote Sensing Letters. [https://doi.org/10.1109/LGRS.2024.3489214](https://doi.org/10.1109/LGRS.2024.3489214) 

Healey, S. P., Yang, Z., Patterson, P. L., & Ghazoul, J. (2023). Algorithm Theoretical Basis Document (ATBD) for GEDI Level-4B Gridded Aboveground Biomass Density. ORNL DAAC. Version 2.0. GEDIdaac.ornl.gov

Herold, M., & Johns, T. (2007). Linking requirements with capabilities for deforestation monitoring in the context of the UNFCCC-REDD process. Environmental Research Letters, 2(4), 045025\.

Hoffrén, R., Lamelas, M. T., de la Riva, J., Domingo, D., Montealegre, A. L., García-Martín, A., & Revilla, S. (2023). Assessing GEDI-NASA system for forest fuels classification using machine learning techniques. International Journal of Applied Earth Observation and Geoinformation, 116, 103175\. [https://doi.org/10.1016/j.jag.2022.103175](https://doi.org/10.1016/j.jag.2022.103175) 

Hofton, M. (2019). Algorithm Theoretical Basis Document for GEDI waveform processing: Transmit and receive waveform interpretation for L1A and L2A products (Version 1.0). USGS / LP DAAC. LP DAAC  
[https://doi.org/10.21105/joss.04982](https://doi.org/10.21105/joss.04982) 

Ilangakoon, N., Glenn, N. F., Schneider, F. D., Dashti, H., Hancock, S., Spaete, L., & Goulden, T. (2021). Airborne and Spaceborne Lidar Reveal Trends and Patterns of Functional Diversity in a Semi-Arid Ecosystem. Frontiers in Remote Sensing, 2\. [https://doi.org/10.3389/frsen.2021.743320](https://doi.org/10.3389/frsen.2021.743320) 

Islam, M. D., Di, L., Zhang, C., Yang, R., Qu, J. J., Tong, D., Guo, L., Lin, L., & Pandey, A. (2024). A Decision Rule and Machine Learning-Based Hybrid Approach for Automated Land-Cover Type Local Climate Zones (LCZs) Mapping Using Multi-Source Remote Sensing Data. IEEE Journal of Selected Topics in Applied Earth Observations and Remote Sensing, 17, 8271–8290. [https://doi.org/10.1109/JSTARS.2024.3386389](https://doi.org/10.1109/JSTARS.2024.3386389) 

Jia, D., Wang, C., & Bo, Y. (2025). Evaluation of GEDI for Estimating the Vertical Distribution of PAI in Temperate Forests: A Case Study of the Conterminous United States. IEEE Geoscience and Remote Sensing Letters, 22, 1–5. https://doi.org/10.1109/LGRS.2025.3542874  
Jiapeng Huang & Xiaozhu Yang (2025) Evaluation and improvement of the vertical accuracy of the global open DEM under forest environment, Geocarto International, 40:1, 2453024, DOI: 10.1080/10106049.2025.2453024 

JuanGuerra-Hernández, J., & Pascual, A. (2021). Using GEDI lidar data and airborne laser scanning to assess height growth dynamics in fast-growing species: A showcase in Spain. Forest Ecosystems, 8(1), 14–14. [https://doi.org/10.1186/s40663-021-00291-2](https://doi.org/10.1186/s40663-021-00291-2) 

Kacic, P., Gessner, U., Hakkenberg, C. R., Holzwarth, S., Müller, J., Pierick, K., Seidel, D., Thonfeld, F., Torresani, M., & Kuenzer, C. (2025). Characterizing local forest structural complexity based on multi-platform and \-sensor derived indicators. Ecological Indicators, 170, 113085\. [https://doi.org/10.1016/j.ecolind.2025.113085](https://doi.org/10.1016/j.ecolind.2025.113085) 

Kacic, P., Hirner, A., & Da Ponte, E. (2021). Fusing Sentinel-1 and \-2 to Model GEDI-Derived Vegetation Structure Characteristics in GEE for the Paraguayan Chaco. Remote Sensing, 13(24), Article 24\. [https://doi.org/10.3390/rs13245105](https://doi.org/10.3390/rs13245105) 

Kacic, P., Thonfeld, F., Gessner, U., & Kuenzer, C. (2023). Forest Structure Characterization in Germany: Novel Products and Analysis Based on GEDI, Sentinel-1 and Sentinel-2 Data. Remote Sensing, 15(8), Article 8\. [https://doi.org/10.3390/rs15081969](https://doi.org/10.3390/rs15081969) 

Keawsomsee, S., & Tongleamnak, S. (2024). Assessment of aboveground biomass and carbon stock of rubber plantation using random forest regression with satellite imagery data from Planet NICFI and GEDI data. 2024 28th International Computer Science and Engineering Conference (ICSEC), 1–5. [https://doi.org/10.1109/ICSEC62781.2024.10770717](https://doi.org/10.1109/ICSEC62781.2024.10770717) 

Keith, H., Mackey, B.G., & Lindenmayer, D.B. (2009). Re-evaluation of forest biomass carbon stocks and lessons from the world's most carbon-dense forests. Proceedings of the National Academy of Sciences, 106(28), 11635-11640.

Kellner, J. R., Armston, J., & Duncanson, L. (2021). Algorithm theoretical basis document (ATBD) for GEDI level-4A (L4A) footprint level aboveground biomass density. University of Maryland.

Kellner, J. R., Armston, J., & Duncanson, L. (2022). Algorithm theoretical basis document for GEDI footprint aboveground biomass density (AGBD) \[Article\]. Earth and Space Science, 10, e2022EA002516. [https://doi.org/10.1029/2022EA002516](https://doi.org/10.1029/2022EA002516) 

Kellner, J. R., Armston, J., & Duncanson, L. (2023). Algorithm Theoretical Basis Document for GEDI Footprint Aboveground Biomass Density (L4A). Earth and Space Science, 10, e2022EA002516. [https://doi.org/10.1029/2022EA002516](https://doi.org/10.1029/2022EA002516) 

Kellner, J. R., Armston, J., & Duncanson, L. (2023). Algorithm Theoretical Basis Document for GEDI Footprint Aboveground Biomass Density. Earth and Space Science, 10(4), e2022EA002516. [https://doi.org/10.1029/2022EA002516](https://doi.org/10.1029/2022EA002516) 

Kokalj, Ž., & Mast, J. (2021). Space lidar for archaeology? Reanalyzing GEDI data for detection of ancient Maya buildings. Journal of Archaeological Science: Reports, 36, 102811\. [https://doi.org/10.1016/j.jasrep.2021.102811](https://doi.org/10.1016/j.jasrep.2021.102811) 

Lahssini, K., Baghdadi, N., le Maire, G., & Fayad, I. (2022). Influence of GEDI Acquisition and Processing Parameters on Canopy Height Estimates over Tropical Forests. Remote Sensing, 14(24), Article 24\. [https://doi.org/10.3390/rs14246264](https://doi.org/10.3390/rs14246264) 

Lang, N., Kalischeck, N., Armston, J., Schindler, K., Dubayah, R., Wegner J. D. (2022). Global canopy height regression and uncertainty estimation from GEDI LIDAR waveforms with deep ensembles. Remote Sensing of Environment.https://doi.org/10.1016/j.rse.2021.112760 [https://www.sciencedirect.com/science/article/pii/S0034425721004806](https://www.sciencedirect.com/science/article/pii/S0034425721004806) 

Lefsky, M. A., et al. (2002). Lidar remote sensing for ecosystem studies. BioScience, 52(1), 19–30.

Leite, R. V., Silva, C. A., Broadbent, E. N., Amaral, C. H. do, Liesenberg, V., Almeida, D. R. A. de, Mohan, M., Godinho, S., Cardil, A., Hamamura, C., Faria, B. L. de, Brancalion, P. H. S., Hirsch, A., Marcatti, G. E., Dalla Corte, A. P., Zambrano, A. M. A., Costa, M. B. T. da, Matricardi, E. A. T., Silva, A. L. da, … Klauberg, C. (2022). Large scale multi-layer fuel load characterization in tropical savanna using GEDI spaceborne lidar data. Remote Sensing of Environment, 268, 112764\. [https://doi.org/10.1016/j.rse.2021.112764](https://doi.org/10.1016/j.rse.2021.112764) 

Li, H., Li, X., Kato, T., Inukai, S., & Hiroshima, T. (2025). National-scale calibrated GEDI AGBD models for effective assessment of growth conditions across forest strata. Forest Ecology and Management, 585, 122657\. [https://doi.org/10.1016/j.foreco.2025.122657](https://doi.org/10.1016/j.foreco.2025.122657) 

Li, X., Wessels, K., Armston, J., Hancock, S., Mathieu, R., Main, R., Naidoo, L., Erasmus, B., & Scholes, R. (2023). First validation of GEDI canopy heights in African savannas. Remote Sensing of Environment, 285, 113402\. [https://doi.org/10.1016/j.rse.2022.113402](https://doi.org/10.1016/j.rse.2022.113402) 

Li, Y., Fang, H., Wang, Y., Li, S., Ma, T., Wu, Y., & Tang, H. (2024). Validation of the vertical canopy cover profile products derived from GEDI over selected forest sites. Science of Remote Sensing, 10, 100158\. [https://doi.org/10.1016/j.srs.2024.100158](https://doi.org/10.1016/j.srs.2024.100158) 

Li, Z., Xuan, F., Dong, Y., Huang, X., Liu, H., Zeng, Y., Su, W., Huang, J., & Li, X. (2024). Performance of GEDI data combined with Sentinel-2 images for automatic labelling of wall-to-wall corn mapping. International Journal of Applied Earth Observation and Geoinformation, 127, 103643\. [https://doi.org/10.1016/j.jag.2023.103643](https://doi.org/10.1016/j.jag.2023.103643) 

Lu, D. (2006). The potential and challenge of remote sensing–based biomass estimation. International Journal of Remote Sensing, 27(7), 1297–1328.

Lucas, R., et al. (2015). Mapping forest biomass from radar and optical data. Remote Sensing of Environment, 161, 218–233.

Luthcke, S. B. (2019). Algorithm Theoretical Basis Document for GEDI waveform geolocation: L1B geolocation of waveforms (Version 1.0). USGS / LP DAAC. LP DAAC

Ma, X., Zheng, G., Xu, C., Moskal, L. M., Gong, P., Guo, Q., Huang, H., Li, X., Liang, X., Pang, Y., Wang, C., Xie, H., Yu, B., Zhao, B., & Zhou, Y. (2024). A global product of 150-m urban building height based on spaceborne lidar. Scientific Data, 11(1), 1387\. [https://doi.org/10.1038/s41597-024-04237-5](https://doi.org/10.1038/s41597-024-04237-5) 

Ma, Z., Zhang, S., Camps, A., Park, H., Liu, Q., Tan, P., & Wang, C. (2024). A fast and efficient method to estimate inland water levels using CYGNSS L1 data and DTMs: Application to Floods, lakes and reservoirs monitoring. Journal of Hydrology, 132258\. [https://doi.org/10.1016/j.jhydrol.2024.132258](https://doi.org/10.1016/j.jhydrol.2024.132258) 

Malhi, Y., Baldocchi, D.D., & Jarvis, P.G. (1999). The carbon balance of tropical, temperate and boreal forests. Plant, Cell & Environment, 22(6), 715-740.

Marselis, S. M., Keil, P., Chase, J. M., & Dubayah, R. (2022). The use of GEDI canopy structure for explaining variation in tree species richness in natural forests. Environmental Research Letters, 17(4), 045003\. [https://doi.org/10.1088/1748-9326/ac583f](https://doi.org/10.1088/1748-9326/ac583f) 

McClure, M. M., Tsuyuki, S., & Hiroshima, T. (2024). Integration of Structural Characteristics from GEDI Waveforms for Improved Forest Type Classification. Remote Sensing, 16(24), Article 24\. [https://doi.org/10.3390/rs16244776](https://doi.org/10.3390/rs16244776) 

Netsianda, A., & Mhangara, P. (2025). Aboveground biomass estimation in a grassland ecosystem using Sentinel-2 satellite imagery and machine learning algorithms. Environmental Monitoring and Assessment, 197(2), 138\. [https://doi.org/10.1007/s10661-024-13610-1](https://doi.org/10.1007/s10661-024-13610-1) 

Ngo, Y.-N., Ho Tong Minh, D., Baghdadi, N., & Fayad, I. (2023). Tropical Forest Top Height by GEDI: From Sparse Coverage to Continuous Data. Remote Sensing, 15(4), Article 4\. [https://doi.org/10.3390/rs15040975](https://doi.org/10.3390/rs15040975) 

NOAA Ocean Service. (2024). Coastal Blue Carbon. Retrieved from [https://oceanservice.noaa.gov/ecosystems/coastal-blue-carbon/](https://oceanservice.noaa.gov/ecosystems/coastal-blue-carbon/) 

Oliveira, P. V. C., & Zhang, X. (2025). Upper canopy and understory phenology of Brazilian Amazon forests seen by GEDI lasers. Environmental Research Letters, 20(4), 044015\. [https://doi.org/10.1088/1748-9326/adbfad](https://doi.org/10.1088/1748-9326/adbfad) 

Pan, Y., et al. (2011). A large and persistent carbon sink in the world's forests. Science, 333(6045), 988-993.

Patterson, P. L., Healey, S. P., Ståhl, G., Saarela, S., Holm, S., Andersen, H.-E., Dubayah, R. O., Duncanson, L., Hancock, S., Armston, J., Kellner, J. R., Cohen, W. B., & Yang, Z. (2019). Statistical properties of hybrid estimators proposed for GEDI—NASA’s global ecosystem dynamics investigation. Environmental Research Letters, 14(6), 065007\. [https://doi.org/10.1088/1748-9326/ab18df](https://doi.org/10.1088/1748-9326/ab18df) 

Patterson, P.L., Healey, S.P., Ståhl, G., Saarela, S., Holm, S., Andersen, H.E., Dubayah, R.O., Duncanson,   
L., Hancock, S., Armston, J., Kellner, J.R., Cohen, W.B., Yang, Z. (2019). Statistical properties of hybrid estimators proposed for GEDI—NASA's global ecosystem dynamics investigation. Environmental Research Letters, 14(6), 065007\. [https://doi.org/10.1088/1748-9326/ab18df](https://doi.org/10.1088/1748-9326/ab18df) 

Pettorelli, N., et al. (2005). Using the satellite-derived NDVI to assess ecological responses to environmental change. Trends in Ecology & Evolution, 20(9), 503-510.

Potapov, P., Li, X., Hernandez-Serna, A., Turubanova, S., Tyukavina, A., Hansen, M. C., Tang, H., Nguyen, Q. H. (2021). Tropical Forest Canopy Structure and Change Assessment Using Landsat, GEDI, and Airborne LIDAR Data. IGARSS. [https://ieeexplore.ieee.org/document/9554814](https://ieeexplore.ieee.org/document/9554814) 

Potapov, P., Li, X., Hernandez-Serna, A., Tyukavina, A., Hansen M. C., Kommareddy, A., Pickens, A., Turubanova, S., Tang, H., Edibaldo Silva, C., Armston, J., Dubayah, R., Blair, J. B., Hofton, M. (2021) Mapping global forest canopy height through integration of GEDI and Landsat data. Remote Sensing of Environment 253\.[https://doi.org/10.1016/j.rse.2020.112165](https://doi.org/10.1016/j.rse.2020.112165) 

Potapov, P., Li, X., Hernandez-Serna, A., Tyukavina, A., Hansen, M. C., Kommareddy, A., Pickens, A., Turubanova, S., Tang, H., Silva, C. E., Armston, J., Dubayah, R., Blair, J. B., & Hofton, M. (2021). Mapping global forest canopy height through integration of GEDI and Landsat data. Remote Sensing of Environment, 253, 112165\.

Putzenlechner, B., Bevern ,Felix, Koal ,Philipp, Grieger ,Simon, Kappas ,Martin, Koukal ,Tatjana, Löw, Markus, & and Filipponi, F. (2024). Accuracy assessment of LAI, PAI and FCOVER from Sentinel-2 and GEDI for monitoring forests and their disturbance in Central Germany. European Journal of Remote Sensing, 57(1), 2422323\. [https://doi.org/10.1080/22797254.2024.2422323](https://doi.org/10.1080/22797254.2024.2422323) 

Qi, W., & Silva, C. (2020). The Global Ecosystem Dynamics Investigation: High-resolution laser ranging of the Earth’s forests and topography. Science of Remote Sensing, 1, 100002\. [https://doi.org/10.1016/j.srs.2020.100002](https://doi.org/10.1016/j.srs.2020.100002) 

Qi, W., Armston, J., Choi, C., Stovall, A., Saarela, S., Pardini, M., Fatoyinbo, L., Papathanasiou, K., & Dubayah, R. (2023). Mapping Large-Scale Pantropical Forest Canopy Height by Integrating GEDI Lidar and TanDEM-X InSAR Data. In Review. [https://doi.org/10.21203/rs.3.rs-3306982/v1](https://doi.org/10.21203/rs.3.rs-3306982/v1) 

R. D. L. R., Aguilar-Amuchastegui, N., Reygadas, Y., Gan, J., DeFries, R., & Menge, D. N. L. (2024).  
Ren, C., Jiang, H., Xi, Y., Liu, P., & Li, H. (2023). Quantifying Temperate Forest Diversity by Integrating GEDI LiDAR and Multi-Temporal Sentinel-2 Imagery. Remote Sensing, 15(2), Article 2\. [https://doi.org/10.3390/rs15020375](https://doi.org/10.3390/rs15020375) 

Roy: The impact of geolocation uncertainty on GEDI tropical forest canopy height estimation and change monitoring—ScienceDirect. (n.d.). Retrieved January 27, 2025, from [https://www.sciencedirect.com/science/article/pii/S2666017221000110](https://www.sciencedirect.com/science/article/pii/S2666017221000110) 

Ryan, M.G., & Yoder, B.J. (1997). Hydraulic limits to tree height and tree growth. Bioscience, 47(4), 235-242.

Saatchi, S. S., et al. (2011). Benchmark map of forest carbon stocks in tropical regions across three continents. PNAS, 108(24), 9899–9904.

Saatchi, S. S., & Favrichon, S. (2023). Global Vegetation Height Metrics from GEDI and ICESat2 (Version 1). ORNL Distributed Active Archive Center. [https://doi.org/10.3334/ORNLDAAC/2294](https://doi.org/10.3334/ORNLDAAC/2294) 

Sandamali, J., & Narine, L. L. (2025). A data-driven, cloud-based approach for forest aboveground biomass mapping using GEDI and other earth observation data: An ecoregion-specific Investigation across the state of Alabama, USA. Geocarto International, 40(1), 2465446\. [https://doi.org/10.1080/10106049.2025.2465446](https://doi.org/10.1080/10106049.2025.2465446) 

Scheffer, M., et al. (2009). Early-warning signals for critical transitions. Nature, 461(7260), 53-59.  
Schneider F. D., Ferraz, A., Hancock, S., Duncanson, L I., Dubayah, R. O., Pavlick, R. P., Schimel, D. S. (2020). Towards mapping the diversity of canopy structure from space with GEDI. Remote Sensing of the Environment. 15\. [https://doi.org/10.1088/1748-9326/ab9e99](https://doi.org/10.1088/1748-9326/ab9e99) 

Seyrek, E. C., Narin, O. G., & Uysal, M. (2025). Forest canopy cover estimation with machine learning using GEDI and Landsat data in the Western Marmara Region, Türkiye. Earth Science Informatics, 18(2), 230\. [https://doi.org/10.1007/s12145-025-01747-7](https://doi.org/10.1007/s12145-025-01747-7) 

Shean, D., Swinski, J. p., Smith, B., Sutterley, T., Henderson, S., Ugarte, C., Lidwa, E., & Neumann, T. (2023). SlideRule: Enabling rapid, scalable, open science for the NASA ICESat-2 mission and beyond. Journal of Open Source Software, 8(81), 4982\.

Shendryk, Y. (2022). Fusing GEDI with earth observation data for large area aboveground biomass mapping. International Journal of Applied Earth Observation and Geoinformation, 115, 103108\. [https://doi.org/10.1016/j.jag.2022.103108](https://doi.org/10.1016/j.jag.2022.103108) 

Tang, H. Armston, J. (2019, December 6). Algorithm Theoretical Basis Document for GEDI L2B: Footprint canopy cover and vertical profile metrics (Version 1.0). USGS / LP DAAC. LP DAAC

Tsao, A., Nzewi, I., Jayeoba, A., Ayogu, U., & Lobell, D. B. (2023). Canopy Height Mapping for Plantations in Nigeria Using GEDI, Landsat, and Sentinel-2. Remote Sensing, 15(21), Article 21\. [https://doi.org/10.3390/rs15215162](https://doi.org/10.3390/rs15215162) 

Urbazaev, M., Hess, L. L., Hancock, S., Sato, L. Y., Ometto, J. P., Thiel, C., Dubois, C., Heckel, K., Urban, M., Adam, M., & Schmullius, C. (2022). Assessment of terrain elevation estimates from ICESat-2 and GEDI spaceborne LiDAR missions across different land cover and forest types. Science of Remote Sensing, 6, 100067\. [https://doi.org/10.1016/j.srs.2022.100067](https://doi.org/10.1016/j.srs.2022.100067) 

Wang, C., Elmore, A. J., Numata, I., Cochrane, M. A., Lei, S., Hakkenberg, C. R., Li, Y., Zhao, Y., & Tian, Y. (2022). A Framework for Improving Wall-to-Wall Canopy Height Mapping by Integrating GEDI LiDAR. Remote Sensing, 14(15), 3618\. [https://doi.org/10.3390/rs14153618](https://doi.org/10.3390/rs14153618) 

Wang, C., Elmore, A. J., Numata, I., Cochrane, M. A., Shaogang, L., Huang, J., Zhao, Y., Li, Yuanyuan. (2022). Factors affecting relative height and ground elevation estimations of GEDI among forest types across the conterminous USA. GIScience & Remote Sensing. Vol 59, Issue 1\. [https://doi.org/10.1080/15481603.2022.2085354](https://doi.org/10.1080/15481603.2022.2085354) 

Wang, C., Jia, D., Lei, S., Numata, I., & Tian, L. (2023). Accuracy Assessment and Impact Factor Analysis of GEDI Leaf Area Index Product in Temperate Forest. Remote Sensing, 15(6), Article 6\. [https://doi.org/10.3390/rs15061535](https://doi.org/10.3390/rs15061535) 

Wang, Y., Fang, H., Li, Y., Li, S., & Tang, H. (2025). Validation of the vertical plant area index profile product derived from GEDI over global forest sites. Agricultural and Forest Meteorology, 371, 110612\. [https://doi.org/10.1016/j.agrformet.2025.110612](https://doi.org/10.1016/j.agrformet.2025.110612) 

Wang, Z., Cai, H., & Yang, X. (2024). A new method for mapping vegetation structure parameters in forested areas using GEDI data. Ecological Indicators, 164, 112157\. [https://doi.org/10.1016/j.ecolind.2024.112157](https://doi.org/10.1016/j.ecolind.2024.112157) 

Woodgate, W., Phinn, S., Devereux, T., & Aryal, R. R. (2025). Bushfire recovery at a long-term tall eucalypt flux site through the lens of a satellite: Combining multi-scale data for structural-functional insight. Remote Sensing of Environment, 317, 114530\. [https://doi.org/10.1016/j.rse.2024.114530](https://doi.org/10.1016/j.rse.2024.114530) 

Xi, Y., Tian, Q., Zhang, W., Zhang, Z., Tong, X., Brandt, M., & Fensholt, R. (2022). Quantifying understory vegetation density using multi-temporal Sentinel-2 and GEDI LiDAR data. GIScience & Remote Sensing, 59(1), 2068–2083. [https://doi.org/10.1080/15481603.2022.2148338](https://doi.org/10.1080/15481603.2022.2148338) 

Xu, M., et al. (2013). Variation in carbon storage and its distribution by stand age and forest type in boreal and temperate forests in northeastern China. PLoS One, 8(8), e72201.

Zhang, Q., Zhang, G., Zhang, Y., Xiao, X., You, N., Li, Z., Tang, H., Yang, T., Di, Y., & Dong, J. (2024). Coupling GEDI LiDAR and Optical Satellite for Revealing Large-Scale Maize Lodging in Northeast China. Earth’s Future, 12(1), e2023EF003590. [https://doi.org/10.1029/2023EF003590](https://doi.org/10.1029/2023EF003590) 

Ziegler, A., Heisig, J., Ludwig, M., Reudenbach, C., Meyer, H., & Nauss, T. (2023). Using GEDI as training data for an ongoing mapping of landscape-scale dynamics of the plant area index. Environmental Research Letters, 18(7), 075003\. [https://doi.org/10.1088/1748-9326/acde8f](https://doi.org/10.1088/1748-9326/acde8f) 

Zurqani, H. A. (2025). A multi-source approach combining GEDI LiDAR, satellite data, and machine learning algorithms for estimating forest aboveground biomass on Google Earth engine platform. Ecological Informatics, 103052\. [https://doi.org/10.1016/j.ecoinf.2025.103052](https://doi.org/10.1016/j.ecoinf.2025.103052)