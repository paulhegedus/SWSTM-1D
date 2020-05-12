# Soil Water and Solute Transport Model - 1D (SWSTM1D)
One-dimensional water and solute transport model. object oriented using R6 library

requires exe file, sourceCode file, classes folder, and modules folder.

sourceCode checks that all required elements of SWSTM1D present and available. contains R6 classes that are not module specific.

exe file contains the execution script for running SWSTM1D. first runs source code to check model environment for required elements. then takes the inputs from inputs folder or directly from user in script to set up sim model. outputter class makes output folder (or can write to gui). for each time step all modules executed and outputter saves t level plots. after all time steps outputter saves final plots

Q: inputs folder holds script to make inputs or just holds the input files?
