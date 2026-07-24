# Research-in-Applied-Economics-Dissertation
Final-year Econometrics Dissertation for Charles Feng

VG250_KRS.xxx (.dbf, .prj, .shp, .shx) contain shapefiles for the output of arcgis maps of German counties

analysis_germany (2).R contains the main code of the dissertation, which takes the formatted and produced dataset, kreis_full_dataset.csv, and estimates the primary models via OLS regression
additional placebo checks are done with kreis_placebo_dataset.csv, also in this R code.

maps_germany.py takes the kreis shapefiles, and merges them with the kreis data in the two aforementioned kreis datasets, and produces fully formatted heatmaps of the intensity of various economic and social variables across the German counties (output, immigration, etc.)

Finally, for the appendix, partyRILEdata.ipynb uses the manifesto project API to download party and ideological data for the selected group of German parties, outputting formatted tables.
