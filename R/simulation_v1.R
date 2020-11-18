#' Simulation analysis
#'
#'


# github version of PropCIs package are prefered than the CRAN version
# because a numerical instability is reported on the CRAN version of PropCIs
# (the issue was fixed for the github version)

#' require(magrittr)
#' require(plyr)
#' require(devtools)
#'
#' install_github('shearer/PropCIs')
#' require(PropCIs)
#'
#'
#' my.simulation <- function( # define a function named 'my.simulation'. The syntax is function_name <- function(...). The arguments are decribed in "...".
#'   my.test,
#'   list.args.my.test,
#'   i.from = 500,
#'   i.to = 2000,
#'   i.by = 500
#'   ){
#'   for(  # 'for' syntax; for(i in sequence){what to be itterated}
#'     i in seq(from = i.from,to = i.to,by = i.by)
#'     ){
#'     j = 1
#'     print(i)
#'     while(j < i+1){
#'       result_i <- do.call( # "do.call" function is called with arbitarary R function and its arguments (as a list-class object).
#'         my.test,
#'         list.args.my.test
#'         )
#'       result_i$simsize <- i
#'       result_i$trial   <- j
#'       if( !("df.result" %in% ls() )) df.result <- result_i
#'       if( "df.result" %in% ls())     df.result <- rbind(df.result,result_i)
#'       j <- j + 1
#'     }
#'   }
#'   return(df.result)
#' }
#'
#'
#' eg.my.test <- function(p1=0.9,n1=200,p2=0.5,n2=200,conf.level=0.025,delta=0.1){
#'   sim.dat <- rbinom(n = n1,size = 1,prob = p1)
#'   CL <- try(
#'     PropCIs::diffscoreci(n1, sum(sim.dat), n2, n2*p2,conf.level = conf.level)
#'     )
#'   if(class(CL)=='try-error') success <- FALSE
#'   if(class(CL)!='try-error') success <- CL[[1]][2] < delta
#'   df.result <- data.frame(
#'     p1=p1,
#'     n1=n1,
#'     p2=p2,
#'     n2=n2,
#'     UCL = CL[[1]][1],
#'     LCL = CL[[1]][2],
#'     conf.level=conf.level,
#'     delta=delta,
#'     success=success
#'     )
#'   return(df.result)
#'   }
#'
#' eg.list.args.my.test <-   # prepare a list of arguments to be passed to a function will be called in my.simulation function.
#'   list(
#'     p1=0.5,
#'     n1=30,
#'     p2=0.5,
#'     n2=30,
#'     conf.level=0.025,
#'     delta=0.1
#'     )
#'
#' eg.simulation  <-
#'   my.simulation(
#'     eg.my.test,
#'     eg.list.args.my.test,
#'     i.from = 500,
#'     i.to = 5000,
#'     i.by = 500
#'     )
#'
#' eg.simulation %>%
#'   ddply(
#'     .(n1,simsize),
#'     function(D){
#'       return(sum(D$success)-1/unique(D$simsize))
#'     }
#'   )
#'
#'
#'
#' # eg.my.test2 <- function(n=NULL, mean=0.9,sd=200,conf.level=0.025,delta=0.1){
#' #   sim.dat <- rexp(n = 200, rate = 0.25)
#' #   CL <- try(
#' #     (n1, sum(sim.dat), n2, n2*p2,conf.level = conf.level)
#' #   )
#' #   if(class(CL)=='try-error') success <- FALSE
#' #   if(class(CL)!='try-error') success <- CL[[1]][2] < delta
#' #   df.result <- data.frame(
#' #     p1=p1,
#' #     n1=n1,
#' #     p2=p2,
#' #     n2=n2,
#' #     UCL = CL[[1]][1],
#' #     LCL = CL[[1]][2],
#' #     conf.level=conf.level,
#' #     delta=delta,
#' #     success=success
#' #   )
#' #   return(df.result)
#' # }
#'
#' eg.list.args.my.test <-   # prepare a list of arguments to be passed to a function will be called in my.simulation function.
#'   list(
#'     p1=0.5,
#'     n1=30,
#'     p2=0.5,
#'     n2=30,
#'     conf.level=0.025,
#'     delta=0.1
#'   )
#'
#' eg.simulation  <-
#'   my.simulation(
#'     eg.my.test,
#'     eg.list.args.my.test,
#'     i.from = 500,
#'     i.to = 5000,
#'     i.by = 500
#'   )
#'
#' eg.simulation %>%
#'   ddply(
#'     .(n1,simsize),
#'     function(D){
#'       return(sum(D$success)-1/unique(D$simsize))
#'     }
#'   )
#'
#'
#' #' For NHCAP study.
#' #' PI: Dr.Takazono
#' #' 2020/08/21
#' #'
#'
#' my.test_binomCI <- function(p=0.875,n=80,conf.level=0.025,delta=0.10){
#'   sim.dat <- rbinom(n = n,size = 1,prob = p)
#'   CL_upper <-
#'     GenBinomApps::clopper.pearson.ci(
#'       k = sum(sim.dat), n=n, alpha = conf.level, CI = "upper"
#'       )$Upper.limit
#'   CL_lower <-
#'     GenBinomApps::clopper.pearson.ci(
#'       k = sum(sim.dat), n=n, alpha = conf.level, CI = "lower"
#'       )$Lower.limit
#'   success <- CL_upper-CL_lower < delta
#'   df.result <- data.frame(
#'     p=p,
#'     n=n,
#'     obs.p = sum(sim.dat)/n,
#'     LCL = CL_lower,
#'     UCL = CL_upper,
#'     obs.diff=CL_upper-CL_lower,
#'     conf.level=conf.level,
#'     delta=delta,
#'     success=success
#'     )
#'   return(df.result)
#'   }
#'
#' list.args.my.test_binomCI <-   # prepare a list of arguments to be passed to a function will be called in my.simulation function.
#'   list(
#'     p=0.97,
#'     n=80,
#'     conf.level=0.05,
#'     delta=0.10
#'     )
#'
#' res.simulation.my.test_binomCI  <-
#'   my.simulation(
#'     my.test_binomCI,
#'     list.args.my.test_binomCI,
#'     i.from = 500,
#'     i.to = 5000,
#'     i.by = 500
#'   )
#'
#' res.simulation.my.test_binomCI %>%
#'   ddply(
#'     .(n,simsize),
#'     function(D){
#'       return((sum(D$success)-1)/unique(D$simsize))
#'     }
#'   )
#'
#'
#' # NHCAP -------------------------------------------------------------------
#'
#' install.packages("binom")
#' install.packages("binomSamSize")
#'
#' require("binom")
#' require("binomSamSize")
#'
#' binomSamSize::ciss.binom(
#'   p0 = 0.875,
#'   d = 0.10,
#'   alpha = 0.05,
#'   ci.fun =  binom.confint,
#'   method="exact"
#'   )
