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
mod_path <- "/Users/PaulBriggs/Box/Hegedus/Dissertation/Chapter3/SWSTM1D/dev/SWSTM-1D"
# Need to provide the path to location of 'inputs' folder; 
# outputs put here (this does not need to be same as model)
io_path <- "/Users/PaulBriggs/Box/Hegedus/Dissertation/Chapter3/SWSTM1D/dev/SWSTM-1D"
# Filename of t or z inputs and the time step intervals to output data
t_dat_name <- "tIn_dat"
z_dat_name <- "zIn_dat"
# Module names
mods_select <- c("DrainModuleFC",
                 "RootModule_Dist", # OR  RootModule_Length
                 "ET_Partition_T", # Partition all ET to T
                 "TranspModule_PET_noLim") # OR TranspModule_AET
mods_data_loc <- list("DrainModuleFC_in",
                      "RootModule_root_depths",
                      "ET_inputs", # AET or PET, partitioned b/w E and T
                      "TranspModule_wp") # NA w/ AET
# Outputter names
op_select <- NULL #c("SWSTM1D_OP",
            #   "DrainModuleFC_OP") 
# TODO: Handle NULL for no outputters
# Intervals to output from each outputter. Must be same
# order as 'op_select'. First element is for 'z' outputs,
# second element is for 't' outputs.
op_ints <- list(
  c(1,1), # SWSTM1D_OP
  c(1,1) # DrainModuleFC_OP
)

# Source Code ---------------------------
source(paste0(mod_path, "/SWSTM1D_sourceCode.R"))

# Check Model Requirements/Inputs ---------------------------
checkForModelReqs(mod_path,io_path)

# Initialize Model ---------------------------
pc <- proc.time()

swstm1d <- SWSTM1D$new(
  mod_path = mod_path,
  io_path = io_path,
  t_dat_name = t_dat_name,
  z_dat_name = z_dat_name,
  mods_select = mods_select,
  op_select = op_select,
  mods_data_loc = mods_data_loc,
  op_ints = op_ints
)

# SetUp Model ---------------------------
swstm1d$setUp() 

# Execute Model Simulation ---------------------------
swstm1d$execute() 

# Save Model Simulation Outputs ---------------------------
swstm1d$output() %>%
  invisible()

proc.time() - pc
