# 1D transport mod sandbox
## the SoilMod1D class ...
## 04/04/2020: takes all the inputs needed to generate the simulation
## the fields required are all of the necessary user inputs while the 
## methods execute the function. the run model method chains together
## sub methods based on user inputs. model assumes 1m2 surface area.
## all of the vec's could be passed in as columns from a table (csv) in future

SoilMod1D <- R6Class( 
  classname = "SoilMod1D",
  public = list( # start public fields and methods
    #### required parms ####
    ## t level parms  ##
    # max amount of time unit to run
    sim_length = NULL, 
    # length of time steps... restricting uneven time steps
    time_disc = NULL, 
    # unit of time, mainly for plotting later, also for fluxes (mass/volume per time per area)
    time_unit = NULL, 
    
    ## z level parms @ t=0 ##
    # vector of layer depths, not restricting layers of uneven depths
    layer_depths = NULL,
    # unit for depth, i.e. cm, mm, in (water unit lengths need to match : prec_unit, ET, etc.)
    depth_unit = NULL, 
    # must be same length as layer_depths
    vwc0 = NULL,
    # must be same length as layer_depths
    fc = NULL, 
    ## will need to figure out a way to add z level inputs for multiple timesteps (i.e. allow 
    ## insertion of mineralization by depth for all time steps)
    
    #### modules ####
    drain_mod = NULL,
    
    #### parms provided or to be calculated, not required ####
    # if these parms are not provided and their associated module is not selected (=FALSE)
    # these parms are left null and their associated module is skipped. if they are left null 
    # and their associated module is selected (=TRUE) than they are derived. 
    # precipitation and fertilization do not have a module selection. they are either provided
    # or are not. some others may be added to this
    
    ## t level parms ##
    # must be same length as sim_length/time_step = 1 obs for each time step
    prec_vec = NULL, # private? b/c put in table eventually?
    # must match depth unit (or be converted)
    prec_unit = NULL, 
    
    # !!! add more when new modules made !!!
    # evap, transp, roots, fert, dep, vol, dn, net_min, etc.
    # min/max air_temp - for soil min (and/or snow module. 
    # if user provides temps AND selects snow mod, precip will auto correct to snowmelt + 
    # prec. snowpack modeled across time b/f sim to determine snowmelt and prec per time step) 
    
    ## z level parms ##
    # need to think about how to make this work 
    # i.e. net_min, denit, etc. that you may want to 
    # manually add at each depth for specific time steps
    
    #### public outputs ####
    # output t level data
    t_out = NULL,
    # output z level data
    z_out = NULL,
    
    #### functions ####
    ## initialize function
    # !!! need to add arguments when new modules are made !!!
    initialize = function(
      # required parms
      sim_length,
      time_disc,
      time_unit,
      layer_depths,
      depth_unit,
      vwc0,
      fc,
      # modules
      drain_mod=TRUE, # placeholder for other module options 
      # optional parms or calculated (when x_mod=T) parms
      prec_vec=NULL, # no calc
      prec_unit=NULL 
      
    ){
      #### required parms #####
      stopifnot(is.numeric(sim_length))
      self$sim_length <- sim_length
      
      stopifnot(is.numeric(time_disc))
      self$time_disc <- time_disc
      
      stopifnot(is.character(time_unit))
      self$time_unit <- time_unit
      
      stopifnot(is.numeric(layer_depths))
      self$layer_depths <- layer_depths
      
      stopifnot(is.character(depth_unit))
      self$depth_unit <- depth_unit
      
      stopifnot(is.numeric(vwc0),
                length(vwc0)==length(layer_depths))
      self$vwc0 <- vwc0
      
      stopifnot(is.numeric(fc),
                length(fc)==length(layer_depths))
      self$fc <- fc
      
      private$tot_depth <- sum(layer_depths)
      private$layers <- length(layer_depths)
      private$time_steps <- sim_length/time_disc
      
      #### optional parms & modules ####
      self$drain_mod <- drain_mod
      
      if(!is.null(prec_vec)){
        stopifnot(as.numeric(prec_vec),
                  length(prec_vec)==private$time_steps)
        self$prec_vec <- prec_vec
        
        # check if prec in same units as depth, change error to conversion eventually
        stopifnot(prec_unit==depth_unit) 
        self$prec_unit <- depth_unit # may not be needed in long run
      }else{
        # want to set prec to 0 for all time steps if not provided
        self$prec_vec <- rep(0,
                             private$time_steps)
        self$prec_unit <- depth_unit
      }
      
      # !!! add new modules here !!!
      #  i.e. ET, solute transport stuff...
      
    }, # end initialize function
    run_soil_mod = function(){
      ## setup_output_dat
      # this is t_out (time step) and z_out (time*depth)
      private$setup_output_dat()
      
      ## set up initial soil profile @ t = 0 (list of soil layers)
      private$setup_soil_profile()
      
      ## for each time step run each module that is on
      # z_out, t_out, and the soil profile will be updated from 
      # modules during each iteration
      for(i in 1:nrow(self$t_out)){   
        if(self$drain_mod){
          private$drain_mod() # change to calc
          private$update_profile_post_drain() # change to update
          # etc. 
        }
        
        
        
        # !!! add new modules here !!!
        #  i.e. ET, solute transport stuff...
        
      } # end sim
      
      return(invisible(self))
    }
    
    ## more functions etc.
    
    
  ), # end public
  private = list(
    #### derived parms ####
    # sum(layer_depths)
    tot_depth = NULL,
    # length(layer_depths)
    layers = NULL,
    # sim_length/time_step = total number of time steps
    time_steps = NULL, 
    
    #### generate objects ####
    soil_profile = NULL,
    
    #### function ####
    ## function for setting up the t and z level output tables
    # !!! when you build more modules you need to update this statement !!!
    setup_output_dat = function(){
      ## create t level intial table
      self$t_out <- data.frame(
        time=seq(1:self$sim_length,
                 self$time_disc),
        prec=self$prec_vec)
      ## create z level intial table
      # !!! when you build more modules you need to update this statement !!!
      self$z_out <- data.frame(
        time=rep(0,private$layers),
        layer=seq(1:private$layers),
        topDepth=NA,
        botDepth=sumPrevFun(self$layer_depths),
        depth=self$layer_depths,
        fc=self$fc,
        vwc=self$vwc0
      )
      self$z_out$topDepth <- self$z_out$botDepth - self$z_out$depth
      
      # example of how to add cols with new modules
      if(self$drain_mod){
        self$t_out$wBot <- NA
        self$t_out$wRunOff <- NA
        
        self$z_out$wTop <- NA
        self$z_out$wBot <- NA
      }
      
      # !!! add more conditionals for new modules here !!!
      # i.e. ET, solute stuff, etc.
      
      return(invisible(self))
    }, # end setup_output_dat
    
    ## function for setting up the soil profile list before each timestep
    setup_soil_profile = function(){
      private$soil_profile <- apply(self$z_out,1,list) %>%
        lapply(as.data.frame) %>%
        lapply(t) %>%
        lapply('row.names<-',NULL) %>%
        lapply(as.data.frame) %>%
        `names<-`(1:private$layers)
      
      private$soil_profile <- private$soil_profile %>%
        lapply(initSoilLayers)
      
      return(invisible(private))
    }, # end setup_soil_profile
    
    ## drain module
    drain_module = function(){
      # function creates drain layers from soil profile
      # for each layer, the drain function is performed 
      # and z_out.wTop & z_out.wBot updated
      #drainProfile <- private$soil_profile
      
      # soil profile updated with newly calculated vwc 
      return(invisible(self))
    }
  ) # end private
) # end SoilMod1D class

SoilLayer <- R6Class(
  classname="SoilLayer",
  public=list(
    depth=NULL,
    fc=NULL,
    vwc=NULL,
    
    initialize = function(depth,fc,vwc){
      stopifnot(is.numeric(depth),
                length(depth)==1)
      self$depth <- depth
      stopifnot(is.numeric(fc),
                length(fc)==1)
      self$fc <- fc
      stopifnot(is.numeric(vwc),
                length(vwc)==1)
      self$vwc <- vwc
    }
  )
)

## need a constructor to get from current soil_profile to drain layer w/out repeating
## initialization
DrainLayer <- R6Class(
  classname="DrainLayer",
  inherit = SoilLayer,
  public=list(
    w_top = NULL,
    w_bot = NULL,
    
    initialize = function(){
      stopifnot(is.numeric(depth),
                length(depth)==1)
      self$depth <- depth
      stopifnot(is.numeric(fc),
                length(fc)==1)
      self$fc <- fc
      stopifnot(is.numeric(vwc),
                length(vwc)==1)
      self$vwc <- vwc
    }
  )
)

#### Functions ####
# takes a vector of numbers and iteratively adds 
# the prev number
sumPrevFun <- function(vec){
  stopifnot(is.numeric(vec),
            length(vec)>1) 
  for(i in 2:length(vec)){ 
    vec[i] <- vec[i] + vec[i-1]
  }
  return(vec)
}

# function for initializing soil layers based on 
# the generation of 
initSoilLayers <- function(df){
  soilLayer <- SoilLayer$new(
    depth=df$depth,
    fc=df$fc,
    vwc=df$vwc
  )
  return(soilLayer)
}











## Module (Abstract Class)
Module <- R6Class(
  classname = "Module",
  public = list(
    initialize = function(){
      stop("You cannot instantiate an abstract class!")
    },
    setup = function(){
      stop("This is the abstract class")
    },
    calculate = function(){
      stop("This is the abstract class")
    },
    update_output = function(){
      stop("This is the abstract class")
    }
  )
)


DrainModuleFC <- R6Class(
  classname="DrainModuleFC",
  inherit = Module,
  public = list(
    v_top = NULL, 
    v_bot = NULL,
    #vwc0 = NULL,
    vwc = NULL,
    fc = NULL,
    depth = NULL,
    initialize = function(v_top=0,vwc,fc,depth=1){ # vwc0,
      self$v_top <- v_top
      self$vwc <- vwc
      #self$vwc0 <- vwc0
      self$fc <- fc
      self$depth <- depth # assumed 1 if not provided (see args)
    },
    setup = function(){
      stop("I have not been created yet")
    },
    calculate = function(){
      #vwc_temp <- self$vwc0 + self$v_top
      vwc_temp <- self$vwc + self$v_top
      if(vwc_temp > self$fc){
        self$v_bot <- vwc_temp - self$fc * self$depth
        self$vwc <- self$vwc - self$v_bot
      }else{
        self$v_bot <- 0
      }
      return(invisible(self))
    }
  )
)

#testModule <- Module$new()
tm <- DrainModuleFC$new(
  v_top=0,
  vwc=0.25,
  fc=0.3,
  depth=1
)



SoilModData <- R6Class(
  "SoilModData",
  public = list(
    # t level data
    t_dat = NULL,
    # z level data
    z_dat = NULL,
    # soil profile
    soil_profile = NULL,
    ## initialize
    initialize = function(t_dat,z_dat){
      stopifnot(is.data.frame(t_dat))
      stopifnot(is.data.frame(z_dat))
      
      
      
    }
    ## functions
    
    
  )
)

SoilModParms <- R6Class(
  "SoilModParms",
  public = list(
    ## required t level parms ##
    # max amount of time unit to run
    sim_length = NULL, 
    # length of time steps... restricting uneven time steps
    time_disc = NULL, 
    # unit of time, mainly for plotting later, also for fluxes (mass/volume per time per area)
    time_unit = NULL, 
    
    ## z level parms @ t=0 ##
    # vector of layer depths, not restricting layers of uneven depths
    layer_depths = NULL,
    # unit for depth, i.e. cm, mm, in (water unit lengths need to match : prec_unit, ET, etc.)
    depth_unit = NULL, 
    # must be same length as layer_depths
    vwc0 = NULL,
    # must be same length as layer_depths
    fc = NULL, 
    
    ## optional t level parms ##
    # must be same length as sim_length/time_step = 1 obs for each time step
    prec_vec = NULL, # private? b/c put in table eventually?
    # must match depth unit (or be converted)
    prec_unit = NULL, 
    
  )
)



SoilMod1D <- R6Class( 
  classname = "SoilMod1D",
  public = list( # start public fields and methods
    #### required parms ####
    ## t level parms  ##
    # unit of time, mainly for plotting later, also for fluxes (mass/volume per time per area)
    time_unit = NULL, 
    ## z level parms @ t=0 ##
    # unit for depth and water etc., i.e. cm, mm, in (water unit lengths need to match : prec_unit, ET, etc.)
    length_unit = NULL, 
    
    #### model options ####
    
    
    #### public outputs ####
    # output t level data
    t_dat = NULL,
    # output z level data
    z_dat = NULL,
    
    #### functions ####
    ## initialize function
    # !!! need to add arguments when new modules are made !!!
    initialize = function(
      # required parms
      time_unit,
      depth_unit,
      vwc0,
      fc,
      # modules
      drain_mod=TRUE, # placeholder for other module options 
      # optional parms or calculated (when x_mod=T) parms
      prec_vec=NULL, # no calc
      prec_unit=NULL 
      
    ){
      #### required parms #####
      stopifnot(is.numeric(sim_length))
      self$sim_length <- sim_length
      
      stopifnot(is.numeric(time_disc))
      self$time_disc <- time_disc
      
      stopifnot(is.character(time_unit))
      self$time_unit <- time_unit
      
      stopifnot(is.numeric(layer_depths))
      self$layer_depths <- layer_depths
      
      stopifnot(is.character(depth_unit))
      self$depth_unit <- depth_unit
      
      stopifnot(is.numeric(vwc0),
                length(vwc0)==length(layer_depths))
      self$vwc0 <- vwc0
      
      stopifnot(is.numeric(fc),
                length(fc)==length(layer_depths))
      self$fc <- fc
      
      private$tot_depth <- sum(layer_depths)
      private$layers <- length(layer_depths)
      private$time_steps <- sim_length/time_disc
      
      #### optional parms & modules ####
      self$drain_mod <- drain_mod
      
      if(!is.null(prec_vec)){
        stopifnot(as.numeric(prec_vec),
                  length(prec_vec)==private$time_steps)
        self$prec_vec <- prec_vec
        
        # check if prec in same units as depth, change error to conversion eventually
        stopifnot(prec_unit==depth_unit) 
        self$prec_unit <- depth_unit # may not be needed in long run
      }else{
        # want to set prec to 0 for all time steps if not provided
        self$prec_vec <- rep(0,
                             private$time_steps)
        self$prec_unit <- depth_unit
      }
      
      # !!! add new modules here !!!
      #  i.e. ET, solute transport stuff...
      
    }, # end initialize function
    run_soil_mod = function(){
      ## setup_output_dat
      # this is t_out (time step) and z_out (time*depth)
      private$setup_output_dat()
      
      ## set up initial soil profile @ t = 0 (list of soil layers)
      private$setup_soil_profile()
      
      ## for each time step run each module that is on
      # z_out, t_out, and the soil profile will be updated from 
      # modules during each iteration
      for(i in 1:nrow(self$t_out)){   
        if(self$drain_mod){
          private$drain_mod() # change to calc
          private$update_profile_post_drain() # change to update
          # etc. 
        }
        
        
        
        # !!! add new modules here !!!
        #  i.e. ET, solute transport stuff...
        
      } # end sim
      
      return(invisible(self))
    }
    
    ## more functions etc.
    
    
  ), # end public
  private = list(
    #### derived parms ####
    # sum(layer_depths)
    tot_depth = NULL,
    # length(layer_depths)
    layers = NULL,
    # sim_length/time_step = total number of time steps
    time_steps = NULL, 
    
    #### generate objects ####
    soil_profile = NULL,
    
    #### function ####
    ## function for setting up the t and z level output tables
    # !!! when you build more modules you need to update this statement !!!
    setup_output_dat = function(){
      ## create t level intial table
      self$t_out <- data.frame(
        time=seq(1:self$sim_length,
                 self$time_disc),
        prec=self$prec_vec)
      ## create z level intial table
      # !!! when you build more modules you need to update this statement !!!
      self$z_out <- data.frame(
        time=rep(0,private$layers),
        layer=seq(1:private$layers),
        topDepth=NA,
        botDepth=sumPrevFun(self$layer_depths),
        depth=self$layer_depths,
        fc=self$fc,
        vwc=self$vwc0
      )
      self$z_out$topDepth <- self$z_out$botDepth - self$z_out$depth
      
      # example of how to add cols with new modules
      if(self$drain_mod){
        self$t_out$wBot <- NA
        self$t_out$wRunOff <- NA
        
        self$z_out$wTop <- NA
        self$z_out$wBot <- NA
      }
      
      # !!! add more conditionals for new modules here !!!
      # i.e. ET, solute stuff, etc.
      
      return(invisible(self))
    }, # end setup_output_dat
    
    ## function for setting up the soil profile list before each timestep
    setup_soil_profile = function(){
      private$soil_profile <- apply(self$z_out,1,list) %>%
        lapply(as.data.frame) %>%
        lapply(t) %>%
        lapply('row.names<-',NULL) %>%
        lapply(as.data.frame) %>%
        `names<-`(1:private$layers)
      
      private$soil_profile <- private$soil_profile %>%
        lapply(initSoilLayers)
      
      return(invisible(private))
    }, # end setup_soil_profile
    
    ## drain module
    drain_module = function(){
      # function creates drain layers from soil profile
      # for each layer, the drain function is performed 
      # and z_out.wTop & z_out.wBot updated
      #drainProfile <- private$soil_profile
      
      # soil profile updated with newly calculated vwc 
      return(invisible(self))
    }
  ) # end private
) # end SoilMod1D class

SoilLayer <- R6Class(
  classname="SoilLayer",
  public=list(
    depth=NULL,
    fc=NULL,
    vwc=NULL,
    
    initialize = function(depth,fc,vwc){
      stopifnot(is.numeric(depth),
                length(depth)==1)
      self$depth <- depth
      stopifnot(is.numeric(fc),
                length(fc)==1)
      self$fc <- fc
      stopifnot(is.numeric(vwc),
                length(vwc)==1)
      self$vwc <- vwc
    }
  )
)

## need a constructor to get from current soil_profile to drain layer w/out repeating
## initialization
DrainLayer <- R6Class(
  classname="DrainLayer",
  inherit = SoilLayer,
  public=list(
    w_top = NULL,
    w_bot = NULL,
    
    initialize = function(){
      stopifnot(is.numeric(depth),
                length(depth)==1)
      self$depth <- depth
      stopifnot(is.numeric(fc),
                length(fc)==1)
      self$fc <- fc
      stopifnot(is.numeric(vwc),
                length(vwc)==1)
      self$vwc <- vwc
    }
  )
)

#### Functions ####
# takes a vector of numbers and iteratively adds 
# the prev number
sumPrevFun <- function(vec){
  stopifnot(is.numeric(vec),
            length(vec)>1) 
  for(i in 2:length(vec)){ 
    vec[i] <- vec[i] + vec[i-1]
  }
  return(vec)
}

# function for initializing soil layers based on 
# the generation of 
initSoilLayers <- function(df){
  soilLayer <- SoilLayer$new(
    depth=df$depth,
    fc=df$fc,
    vwc=df$vwc
  )
  return(soilLayer)
}




#### Initialize ####
## user can either make initial tables here or import a 
## csv in the proper format and pass into the model?

# test no prec, just drain
soilModV1 <- SoilMod1D$new(
  sim_length = 1,
  time_disc = 1,
  time_unit = "day",
  layer_depths = c(1,1,1),
  depth_unit = "cm",
  vwc0 = c(0.25,0.25,0.25),
  fc = c(0.2,0.2,0.2)
)

# test with prec


# test over time just drain


# test over time with prec

#### Run Soil Model ####
soilModV1$run_soil_mod()

