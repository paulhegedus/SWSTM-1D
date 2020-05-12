## Input Generator
##
## Script for making the input tables required by SWSTM1D
## User needs to amend values below to reflect their options.
##
## Note that this includes examples of module specific inputs that are not
## required in all cases.
## 
## This is the script that 'superusers' can use to change inputs using
## scripts or any user can use to find more info on columns required for 
## the model

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## set working directory to script location (Rstudio version)
#setwd(
#  dirname(
#    rstudioapi::getActiveDocumentContext()$path
#  )
#) 
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Standard Inputs ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## general inputs
sim_length <- 10 # simulation length
num_layers <- 5 # number of soil depth increments

## time level inputs
# these will include the timesteps across the simulation period
# with relevant parms. the only required parameter in this table
# is time (the bare min. scenario isjust drainage of water in the 
# soil, nothing else)
t_in <- data.frame(
  time = 1:sim_length, # time starts at 1
  prec = rep(0,sim_length) # precips for each time step of sim
)
write.csv(t_in,paste0(getwd(),"/tIn_dat.csv"),row.names=FALSE)

## depth level inputs
# provide the inputs specific to each layer for the initial conditions
# t=0. the min. requirements are depth of layer (depth), field capacity (fc),
# and volumetric water content (vwc). Additional parameters can be included 
# for different modules
z_in <- data.frame(
  time = rep(0,num_layers), # always initial conditions
  depth = rep(1,num_layers), # depth of each increment
  fc = rep(0.1,num_layers), # field capacities across each increment
  vwc = rep(0.5,num_layers) # vwc of each increment
)
write.csv(z_in,paste0(getwd(),"/zIn_dat.csv"),row.names=FALSE)

## modules
modules <- data.frame(
  moduleNames = c("DrainModuleFC") # user provides vector of module names
)
write.csv(modules,paste0(getwd(),"/moduleSlctIn_dat.csv"),row.names=FALSE)

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Module X?? ####
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# example of inputs for specific modules??




##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
rm(sim_length,num_layers,t_in,z_in,modules)


