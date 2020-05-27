## SWSTM1D - Soil Water and Solute Transport Model 1D
## Paul Hegedus
## May 2020

## Description:
## Script for loading and implementing the 1D soil transport simulation mod. 
## This is the main .exe script in the 1D soil transport sim model. will not 
## run without proper components in working directory.
## see flowcharts for overview  ... etc. (modSchema vX)
##
## Location of modules and the model scripts can be different from inputs/
## outputs in order to make it easier to run experiments and save inputs/
## outputs in different folders on your computer without having to copy the
## model script everywhere inputs and outputs are located.
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# User Inputs ---------------------------
# Need to provide the path to model files location (where 'modules' is)
modPath <- "/Users/PaulBriggs/Box/Hegedus/Dissertation/Chapter3/swstm1D/dev/Code"
# Need to provide the path to location of 'inputs' folder; 
# outputs put here (this does not need to be same as model)
ioPath <- "/Users/PaulBriggs/Box/Hegedus/Dissertation/Chapter3/swstm1D/dev/Test"
tInName <- "tIn_dat"
zInName <- "zIn_dat"
# Need to provide the string of module names
modsIn <- c("DrainModuleFC") 
# Source Code ---------------------------
source(paste0(modPath, "/SWSTM1D_sourceCode.R"))
# Check Model Dependencies ---------------------------
CheckForModelReqs(modPath,ioPath)
# Initialize Model ---------------------------
swstm1d <- SWSTM1D$new(
  modPath = modPath,
  ioPath = ioPath,
  tInName = tInName,
  zInName = zInName,
  modsIn = modsIn
)
# SetUp Model ---------------------------
swstm1d$SetUp() 
# Execute Model Simulation ---------------------------
swstm1d$Execute() 
# Save Model Simulation Outputs ---------------------------
swstm1d$Output() 
