## 1D Water and Solute Transport Model
## Paul Hegedus
##
## Description:
## see flowcharts for overview  ... etc.
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## set working directory to script location (Rstudio version)
setwd(
  dirname(
    rstudioapi::getActiveDocumentContext()$path
  )
) 
# base r approach
#setwd(getSrcDirectory()[1])
# rob's method for parent dir
## to the parent directory of script
#scr_dir <- dirname(sys.frame(1)$ofile) # <- error: not that many frames on the stack
#parent <- sub("/[^/]+$", "", scr_dir)
#setwd(parent)

## packages and source code
source(paste0(getwd(),"/1DtransportMod_sourceCode_heg.R"))

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### Set Up Inputs
## time level inputs
# these will include the timesteps across the simulation period
# with relevant parms. the only required parameter in this table
# is time (the bare min. scenario isjust drainage of water in the 
# soil, nothing else)
sim_length <- 10
t_in <- data.frame(
  time = 1:sim_length,
  prec = rep(1,sim_length)
)
## depth level inputs
# provide the inputs specific to each layer for the initial conditions
# t=0. the min. requirements are depth of layer (depth), field capacity (fc),
# and volumetric water content (vwc). Additional parameters can be included 
# for different modules
num_layers <- 5
z_in <- data.frame(
  time = rep(0,num_layers),
  depth = rep(1,num_layers),
  fc = rep(0.1,num_layers),
  vwc = rep(0.5,num_layers)
)
## modules
modules <- c("DrainModuleFC")

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### Execute Simulation
soilMod1d <- SoilMod1D$
  # initialize model with t and z level tables & string of module names
  new(t_in,z_in,modules)$
  # run the model
  simRunFun()

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### Visualize Outputs
soilMod1d$soilModData$plot_dpXt() # deep perc vs time
soilMod1d$soilModData$plot_pXt() # precip vs time 

#soilMod1d$soilModData$t_dat # time level data outputs 
#soilMod1d$soilModData$z_dat # depth level data outputs

for(i in 0:sim_length){
  print(soilMod1d$soilModData$plot_vwcXz(i)) # vwc vs depth over time
  i=Sys.sleep(2)
}





