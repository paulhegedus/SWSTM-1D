## Title: SWSTM1D_OP Outputter
## 
## Interface/Abstraction: This object follows the "outputters" 
## interface consisting of the methods; Write_z and Write_t
## Write_z: write depth (z) level data at each timestep (t)
## Write_t: writes t level data at each timestep (t)
## runOutput: does something
## closeCon: closes any open connections
##
## Description: This class is the general outputter for the SWSTM-1D model.
## The Outputter class generator makes a outputter object that saves
## data. Plot output will be generated later.

## Inputs: soilModData
## Methods: Write_z, Write_t
##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# File OutPutter Class Generator ---------------------------
## Outputter that saves data from swstm1d simulation
SWSTM1D_OP <- R6Class(
  "SWSTM1D_OP",
  public = list(
    soilModData = NULL,
    z_con = NULL,
    t_con = NULL,
    z_int = NULL,
    t_int = NULL,
    
    initialize = function(soilModData, op_list) {
      stopifnot(
        exists("t_dat", soilModData),
        exists("z_dat", soilModData),
        exists("io_path", soilModData)
      ) 
      self$soilModData <- soilModData
      self$z_int <- op_list$z_int
      self$t_int <- op_list$t_int
      
      # Write initial z and t level info & open connection
      z_dat <- do.call(
        rbind.data.frame,
        lapply(self$soilModData$soilProfile$soil_layers, as.data.frame)
      )
      z_dat$time <- 0
      fwrite(z_dat, paste0(self$soilModData$io_path, "/outputs/z_dat.csv"))
      
      t_dat <- self$soilModData$t_dat[1, ]
      t_dat[1, ] <- NA
      t_dat[1, "time"] <- 0
      fwrite(t_dat, paste0(self$soilModData$io_path, "/outputs/t_dat.csv"))
      
      self$z_con <- file(description = paste0(self$soilModData$io_path, 
                                             "/outputs/z_dat.csv"),
                        open = "a")
      self$t_con <- file(description = paste0(self$soilModData$io_path, 
                                             "/outputs/t_dat.csv"),
                        open = "a")
    },
    writeZ = function(t) {
      op <- ifelse(t > self$t_int,
                   t / self$t_int,
                   self$t_int / t)
      if (op == as.integer(op)) {
        z_dat <- do.call(
          rbind.data.frame,
          lapply(self$soilModData$soilProfile$soil_layers, as.data.frame)
        )
        z_dat$time <- t
        write.table(z_dat, 
                    self$z_con, 
                    row.names = FALSE, 
                    col.names = FALSE, 
                    sep = ",")
      }
    },
    writeT = function(t) {
      op <- ifelse(t > self$z_int,
                   t / self$z_int,
                   self$z_int / t)
      if (op == as.integer(op)) {
        t_dat <- self$soilModData$t_dat[t, ] %>% 
          as.data.frame()
        write.table(t_dat, 
                    self$t_con, 
                    row.names = FALSE, 
                    col.names = FALSE, 
                    sep = ",")
      }
    },
    runOutput = function() {},
    closeCon = function() {
      close(self$z_con)
      close(self$t_con)
    }
  )
  #private = list()
)







