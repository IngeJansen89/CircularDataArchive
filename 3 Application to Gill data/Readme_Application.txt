./Raw data 
Raw data from "Gill, J. & Hangartner, D. (2010). Circular Data in Political Science and How to Handle It. Political Analysis, 18, 316-336".
Retrieved from https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/15875, May 2nd 2017 by Inge Jansen.
Original data manipulations can be obtaind through the same link. 

#Data manipulations and Analysis
All steps in the Analysis of the Gill & Harngartner data are described in 
the R-file "Germandata Analysis.R"

Note that the data were analysed with the same Ccode as the simulation study (../2 Simulation study/Ccodes_and_scripts).
The full model was analysed with extra iterations. To this end line 40 and 41 of "Regression_generic.cpp" were adjusted to read:
int    iters   = 20000;
int    burnin  = 15000;
 

To save space output files are not included in this archive.