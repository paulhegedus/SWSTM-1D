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
mod_path <- 
  "/Users/PaulBriggs/Box/Hegedus/Dissertation/Chapter3/SWSTM1D/dev/SWSTM-1D"

# Need to provide the path to location of 'inputs' folder; 
# outputs put here (this does not need to be same as model)
io_path <- 
  "/Users/PaulBriggs/Box/Hegedus/Dissertation/Chapter3/SWSTM1D/dev/SWSTM-1D"

# Filename of t or z inputs and the time step intervals to output data
t_dat_name <- "tIn_dat"
z_dat_name <- "zIn_dat"

# Module names
module_list <- list(
  list(module = "DrainModuleFC",
       z_dat = "fc_dat"),
  list(module = "RootModule_Dist",
       t_dat = "RootModule_root_depths",
       crop = "wheat"),
  # list(module = "ET_Partition_E", # use explicit even for just E 
  #      t_dat = "ET_inputs"),
  # list(module = "ET_Partition_T", # use explicit even for just T
  #     t_dat = "ET_inputs"),
  list(module = "ET_Partition_ET_explicit", # E and T explicit
      t_dat = "ET_inputs_explicit"), # pre partitioned E and T
  # list(module = "TranspModule_AT")
  list(module = "TranspModule_PT_noLim",
       z_dat = "wp_data"),
  # list(module = "EvapModule_AE",
  #      max_evap_depth = 5)#,
  list(module = "EvapModule_PE",
       z_dat = "wp_data",
       max_evap_depth = 5) # wp_data not needed if passed in w/transp mod
)
# Outputter names
outputter_list <-  list(  # NULL
 list(op = "SWSTM1D_OP",
      t_int = 1, # time intervals to print t level data
      z_int = 1), # time intervals to print z level data
 list(op = "SWSTM1D_Figs",
      t_int = 1,
      z_int = 1)
 # list(op = "Precip_Figs"),
 # list(op = "DrainModule_Figs",
 #      op_ints = 1), # intervals for plotting?
 # list(op = "TranspModule_Figs",
 #      op_ints = 1),
 # list(op = "EvapModule_Figs",
 #      op_ints = 1)
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
  module_list = module_list,
  outputter_list = outputter_list
)

# SetUp Model ---------------------------
swstm1d$setUp() 

# Execute Model Simulation ---------------------------
swstm1d$execute() 

# Save Model Simulation Outputs ---------------------------
swstm1d$output() %>% 
  invisible()

proc.time() - pc
