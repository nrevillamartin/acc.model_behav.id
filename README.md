# Behaviour identification and time-activity budget estimation of the threatened little bustard using accelerometry 


This is the repository for the following paper:

Revilla-Martín, N., Silva, J. P., Mougeot F., Morales M. B., Marques A. T., Mañosa S., Giralt D., Bretagnolle V., Bota G., Arroyo B., and Bravo C. *Behaviour identification and time budget estimation of the threatened Little Bustard using accelerometry.*


In this work, we applied machine learning methods to accelerometry data to identify and classify behaviours in the little bustard (Tetrax tetrax), a species declining due to habitat loss and degradation. 
Using recordings of four captive individuals, we fitted models to classify key behaviours: standing, lying, vigilance, locomotion, foraging and male courtship, with separate models for each sex due to behavioural differences. 

We tested different sampling frequencies, balancing methods, and data-splitting approaches to inspect interindividual variation and the effect of sample size.

Here we present the scripts for an end-to-end analysis:

  1.	Preparation of Ornitela accelerometer data to be visualised in UvaBits annotation tool (script 1)
  2.	Windowing data (script 2) and prepare tables (script 3)
  3.	Extract features (script 4)
  4.	Train random forest models (random-split and LOIO models) (script 4)
  5.	Calculate time-activity budgets of free ranging individuals (scripts 5, 6 and 7)


