# FL_Patch_Reef_Residency

This repository holds code, data, and other files related to the following manuscript:

Panzetta, S.W.S., Stuart, C.E., & Green, S.J. (In Revision). Biogeophysical drivers of abundance for 
habitat-shifting reef fishes on stepping-stone coral patch reefs. *Coral Reefs*.

In this project, we investigated how habitat structure, biotic community composition, and environmental conditions present on patch reefs off the Florida Keys, USA, influenced the abundances of two habitat-shifting fish species — gray snapper (*Lutjanus griseus*) and bluestriped grunt (*Haemulon sciurus*). We partitioned sub-adult *L. griseus* and *H. sciurus* abundance records into calibration and validation subsets. We then fit zero-inflated negative binomial regressions and evaluated predictive accuracy and covariate contributions. When allowing a ±5 variation from observed fish counts, the fitted *L. griseus* and *H. sciurus* models achieved predictive accuracies of 83.78% and 64.86%, respectively. The two species showed similar responses to various factors, exhibiting higher abundances on small, spatially isolated patch reefs hosting predators and fringed by seagrass. Additionally, patch reefs with elevated summer salinities and warmer winter temperatures harboured significantly higher abundances of the focal species. Depth, slope, and bathymetric position index also influenced sub-adult abundance in the same direction, though to varying statistical magnitudes, with both species showing a preference for shallower patch reefs located along steep slopes and in dips or valleys within the seascape. Our findings highlight key reef features that could support migrating reef fish, informing habitat protection and enhancement strategies.

Order of Analysis:
1. *Create_Reef_Distance_Raster.R* - Creates a 'distance to reef' raster that will be used in conjunction with other spatial-environmental rasters developed in Stuart, C.E.; Wedding, L.M.; Pittman, S.J.; Green, S.J. (2021). Habitat suitability modeling to inform seascape connectivity conservation and management. Diversity, 13, 465. [https://doi.org/10.3390/d13100465](https://doi.org/10.3390/d13100465). 
2. *Prepare_Patch_Reef_Data.py* - Utilizes ArcGIS Pro 3.1.0 and ArcPy to create and consolidate a suite of spatial-environmental predictors at patch reefs where fish surveys were conducted.
3. *Add_Predators_Check_Multicollinearity.R* - Calculates the abundance and density of predators on patch reefs, and incorporates these metrics alongside other predictors. This script performs correlation and multicollinearity checks using Pearson correlation coefficients (r) and variance inflation factors (VIF), followed by cleaning and preparing the training and testing data subsets.
4. *Count_Regressions.R* - Fits preliminary Poisson regressions to the training subsets, checks for overdispersion, and re-fits zero-inflated negative binomial regressions. It makes predictions on the testing subsets, calculates model fit and predictive accuracy metrics, and generates diagnostic plots.
5. Bivariate_Plots.R - Plots the relationship between each biogeophysical predictor and the abundance of sub-adult gray snapper and bluestriped grunt on patch reefs.

You are welcome to use and borrow from this code base in accordance with the license below. Please contact Sofia Panzetta (@panzetta@ualberta.ca) if you have questions about this work.

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
