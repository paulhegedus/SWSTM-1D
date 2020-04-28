## DATABASE CREATION SCRIPT
## Paul Hegedus - 20190808
##
## The databse is created in pgadmin and schemas and tables made here.
## This script creates the PostgreSQL database structure. Includes
## schema creation and tables within schemas. 
## 
##*******************************************************************************
## The schemas correspond to the farmer name and the type of data
## stored within. "_r" corresponds to raw data, while "_a" corresponds
## to data that has been collected, cleaned, and aggregated with 
## covariate data. "_r" schemas contain tables for yield "yld", protein
## "pro", as-applied N "aa_n", and as-applied seeding rate "aa_sr". "_a"
## schemas contain tables for protein and yield for each year that contain
## the yield/protein, N and/or seeding, and remotely sensed covariate data.
##
## NOTE: CREATE SHCEMAS in PGADMIN (will create tables below)
##
## The schema "all_farms" holds general and coarse farm level information.
## This includes a table that holds field names and their boundaries and a table
## with farms (farmers with spread out land have multiple) and their boundaries. 
## 
##
##*******************************************************************************
library("RPostgreSQL")

##*******************************************************************************
## connect to database
db <- dbConnect(dbDriver("PostgreSQL"), user="postgres", password="pbh210220",   #  Ph210220 on PC              
                dbname="OFPE_Test", host="localhost",port="5433")               #   5432 on PC
dbGetQuery(db,"CREATE EXTENSION postgis;
           CREATE EXTENSION postgis_raster;") ## enable postgis in the database    #   if db just created
##*******************************************************************************
## DATABASE CREATION
##*******************************************************************************
#dbSendQuery(db,"CREATE DATABASE 'OFPE_Test'
#  WITH 
#  OWNER = postgres
#  ENCODING = 'UTF8'
#  CONNECTION LIMIT = -1;")

##-------------------------------------------------------------------------------
## build schemas
##-------------------------------------------------------------------------------
farmers <- c("bailey","broyles","loewen","merja","norgaard","oconnor","quinn","vandyke","wood")
farmSchemes <- unlist(lapply(farmers,rep,2))

schemas <- rep(NA,(length(farmers)*2)+1)
for(i in 1:length(schemas)){
  if(i==1){
    schemas[i] <- "all_farms"
  }else{
    if(grepl(farmSchemes[i-1],schemas[i-1])){
      schemas[i] <- paste0(farmSchemes[i-1],"_a")
    }else{
      schemas[i] <- paste0(farmSchemes[i-1],"_r")
    }
  }
}

for(i in 1:length(schemas)){
  dbGetQuery(db,paste0("CREATE SCHEMA ",schemas[i]))
}

##-------------------------------------------------------------------------------
## "all_farms.farmers" tables
##-------------------------------------------------------------------------------
## farmers table
## create table to hold info on farmers
dbGetQuery(db,"CREATE TABLE all_farms.farmers (
           farmeridx INTEGER NOT NULL,
           farmer VARCHAR(100) NOT NULL,
           PRIMARY KEY (farmeridx)
           )")
##-------------------------------------------------------------------------------
## "all_farms.farms" tables
##-------------------------------------------------------------------------------
## create table to hold farm information
dbGetQuery(db,"CREATE TABLE all_farms.farms (
           farmidx INTEGER NOT NULL,
           farm VARCHAR(100) NOT NULL,
           farmeridx INTEGER REFERENCES all_farms.farmers(farmeridx),
           PRIMARY KEY (farmidx)
          )")
dbGetQuery(db,"ALTER TABLE all_farms.farms ADD COLUMN geom geometry")
##-------------------------------------------------------------------------------
## "all_farms.fields" tables
##-------------------------------------------------------------------------------
## fields table
dbGetQuery(db,"CREATE TABLE all_farms.fields (
          wfid INTEGER NOT NULL,
          fieldidx INTEGER NOT NULL,
          farmidx INTEGER REFERENCES all_farms.farms(farmidx),
          farmeridx INTEGER REFERENCES all_farms.farmers(farmeridx),
          fieldname VARCHAR(100) NOT NULL,
          PRIMARY KEY (fieldidx,wfid)
          )")
dbGetQuery(db,"ALTER TABLE all_farms.fields ADD COLUMN geom geometry")
##-------------------------------------------------------------------------------
## "farmer_x" tables
##-------------------------------------------------------------------------------
## these tables will be generated and uploaded in the data upload scripts. The
## script will check if table exists and create new table if it doesn't. Otherwise
## will append table where data is not redundant.
##

##*******************************************************************************
dbDisconnect(db)
##*******************************************************************************



