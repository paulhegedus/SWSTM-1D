##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## PLOT FXNS ####
## need to go into specific modules OG in soilModData class

## plotting functions
plot_vwcXz = function(t){
  stopifnot(any(grepl("vwc",names(self$zDat))))
  pd <- subset(self$zDat,
               self$zDat$time==t)
  for(i in 1:nrow(pd)){
    pd$z_labels[i] <- 
      ifelse(i==1,
             paste0("0 - ",
                    pd$z[i]),
             paste0(pd$z[i-1],
                    " - ",
                    pd$z[i]))
  }
  pd$z_labels <- as.factor(pd$z_labels) %>%
    fct_rev()
  p <- ggplot(pd,
              aes(x=z_labels,y=vwc)) +
    geom_bar(stat="identity",
             color="darkblue",
             fill="darkblue") +
    scale_y_continuous(limits=c(0,1), 
                       breaks=seq(0,1,0.1)) +
    labs(y="Volumetric Water Content",x="Depth") +
    coord_flip() + 
    theme_classic() +
    ggtitle(paste0("Time: ",t))
  ## TODO: add in depth units
  print(p)
},
plot_dpXt = function(){
  stopifnot(any(grepl("deepPerc",names(self$tDat))))
  pd <- self$tDat
  ymax <- RoundTo(max(pd$deepPerc),1,ceiling)
  ystep <- -(ymax-0)/10
  xstep <- (max(pd$time)-0)/10
  
  p <- ggplot(pd,
              aes(x=time,
                  y=deepPerc)) +
    geom_bar(stat="identity",
             color="white",
             fill="darkblue") +
    scale_y_reverse(limits=c(ymax,0),
                    labels=seq(ymax,0,ystep),
                    breaks=seq(ymax,0,ystep)) +
    scale_x_continuous(position = "top",
                       limits=c(0.5,max(pd$time)+0.5),
                       breaks = seq(1,max(pd$time),xstep)) +
    labs(y="Deep Percolation",x="Time Step") +
    theme_classic() 
  ## TODO: add in depth & time units
  print(p)
},
plot_pXt = function(){
  stopifnot(any(grepl("prec",names(self$tDat))))
  pd <- self$tDat
  ymax <- RoundTo(max(pd$prec),1,ceiling)
  ystep <- -(ymax-0)/10
  xstep <- (max(pd$time)-0)/10
  
  p <- ggplot(pd,
              aes(x=time,
                  y=prec)) +
    geom_bar(stat="identity",
             color="white",
             fill="blue") +
    scale_y_reverse(limits=c(ymax,0),
                    labels=seq(ymax,0,ystep),
                    breaks=seq(ymax,0,ystep)) +
    scale_x_continuous(position = "top",
                       limits=c(0.5,max(pd$time)+0.5),
                       breaks = seq(1,max(pd$time),xstep)) +
    labs(y="Precipitation",x="Time Step") +
    theme_classic() 
  ## TODO: add in depth & time units
  print(p)
}














##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### SANDBOX ####
## mutability tests
Foo <- R6Class(
  "Foo",
  public = list(
    x=NULL,
    initialize = function(x){
      self$x = x
    }
  )
)
foo <- Foo$new(5)
foo$x
FooBarNoFlds <- R6Class(
  classname = "FooBarNoFlds",
  public = list(
    initialize = function(){},
    funfun = function(foo){
      foo$x <- foo$x + 1
    }
  )
)
FooBarNoFlds$new()$funfun(foo)
foo$x

## i thought this should work
foo2 <- Foo$new(5)
FooBarWithFlds <- R6Class(
  classname = "FooBarWithFlds",
  public = list(
    x = NULL,
    initialize = function(x){
      self$x <- x
    },
    funfun = function(){
      self$x <- self$x + 1
    }
  )
)
FooBarWithFlds$new(foo2$x)$funfun()
foo2$x
# ^ not passing entire class in so makes a copy?

## this works
foo3 <- Foo$new(5)
FooBarWithFlds <- R6Class(
  classname = "FooBarWithFlds",
  public = list(
    y = NULL,
    initialize = function(y){
      self$y <- y
    },
    funfun = function(){
      self$y$x <- self$y$x + 1
    }
  )
)
FooBarWithFlds$new(foo3)$funfun()
foo3


