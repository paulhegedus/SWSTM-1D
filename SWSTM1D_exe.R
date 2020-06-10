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
modPath <- "/Users/PaulBriggs/Box/Hegedus/Dissertation/Chapter3/SWSTM1D/dev/SWSTM-1D"
# Need to provide the path to location of 'inputs' folder; 
# outputs put here (this does not need to be same as model)
ioPath <- "/Users/PaulBriggs/Box/Hegedus/Dissertation/Chapter3/SWSTM1D/dev/SWSTM-1D"
# Filename of t or z inputs and the time step intervals to output data
tInName <- "tIn_dat"
zInName <- "zIn_dat"
# Need to provide the string of module names
mods_select <- c("DrainModuleFC",
                 "RootModule_Dist", #  RootModule_Length
                 "TranspModule_PET_noLim") #  TranspModule_AET
mods_data_loc <- c("DrainModuleFC_in",
                   "RootModule_root_depths",
                   "TranspModule_ET",
                   "TranspModule_wp") #  
# Need to provide string of outputter names
op_select <- c("SWSTM1D_OP",
               "DrainModuleFC_OP") 
# TODO: Handle NULL for no outputters
# Intervals to output from each outputter. Must be same
# order as 'op_select'. First element is for 'z' outputs,
# second element is for 't' outputs.
op_ints <- list(
  c(1,1), # SWSTM1D_OP
  c(1,1) # DrainModuleFC_OP
)

# Source Code ---------------------------
source(paste0(modPath, "/SWSTM1D_sourceCode.R"))

# Check Model Requirements/Inputs ---------------------------
CheckForModelReqs(modPath,ioPath)

# Initialize Model ---------------------------
pc <- proc.time()

swstm1d <- SWSTM1D$new(
  modPath = modPath,
  ioPath = ioPath,
  tInName = tInName,
  zInName = zInName,
  mods_select = mods_select,
  op_select = op_select,
  mods_data_loc = mods_data_loc,
  op_ints = op_ints
)

# SetUp Model ---------------------------
swstm1d$SetUp() 

# Execute Model Simulation ---------------------------
swstm1d$Execute() 

# Save Model Simulation Outputs ---------------------------
swstm1d$Output() %>%
  invisible()

proc.time() - pc
