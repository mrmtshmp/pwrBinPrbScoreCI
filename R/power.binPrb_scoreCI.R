#' Power analysis for a test of "H0: binom-prb. CI > delta" via simulation data.
#'
#' @import strMNCI
#' @import PropCIs
#' @import magrittr
#' @import plyr
#' @import dplyr
#' @importFrom stats rbinom
#' @importFrom stats uniroot
#'
#' @param Ref.Prp  A numer. vector of bin. prb. for the reference group.
#' @param Test.Prp  A numer. vector of bin. prb. for the test group.
#' @param non.inf.bound Delta (numeric)
#' @param SampleSize.Ref Sample size for the reference group.
#' @param SampleSize.Test Sample size for the test group.
#' @param SS.prop.Ref A numer. vector of proportions (>0, <1) in the respective strata. If NULL, the sample size will be equally divided for each strata.
#' @param SS.prop.Test A numer. vector of proportions (>0, <1) in the respective strata. If NULL, the sample size will be equally divided for each strata.
#' @param adj.prp.min NULL
#' @param adj.prp.max NULL
#' @param n.adj
#' @param conf.level
#' @param itt 500
#' @param ci.method "strMNCI::scoreint.strat.MN" or "PropCIs::diffscoreci".
#' @param str The number of strata.
#'
#' @references Eqs.27 and 28 in "Comparative analysis of two rates", MIETTINEN AND NURMINEN, Stat. Med.(1985)
#'
#' @export
#'
power_via_scoreCIs <-
  function(
    Ref.Prp  = c(rep(0.400, 8), rep(0.200, 8)),
    Test.Prp = c(rep(0.400, 8), rep(0.200, 8)),
    non.inf.bound=0.15,
    SampleSize.Ref=176,
    SampleSize.Test=176,
    SS.prop.Ref = c( rep(0.8*1/8, 8), rep(0.2*1/8, 8)),
    SS.prop.Test= c( rep(0.8*1/8, 8), rep(0.2*1/8, 8)),
    adj.prp.min=NULL, adj.prp.max=NULL,n.adj=30*0.8/0.2,
    conf.level=0.95,
    itt=500,
    ci.method =
      "strMNCI::scoreint.strat.MN",
    str=16
    ){

    if(length(Ref.Prp)>1 &
        length(Ref.Prp)!=str)
      stop("Wrong length in argument 'Ref.Prp'")

    if(length(Test.Prp)>1 &
        length(Test.Prp)!=str)
      stop("Wrong length in argument 'Test.Prp'")

    if(!is.null(SS.prop.Ref) &
        length(Ref.Prp)!=length(SS.prop.Ref))
      stop("Wrong length in argument 'SS.prop.Ref'")

    if(!is.null(SS.prop.Test) &
        length(Test.Prp)!=length(SS.prop.Test))
      stop("Wrong length in argument 'SS.prop.Test'")


    if(str>1 & length(Ref.Prp)==1) Ref.Prp  = rep(Ref.Prp,str)
    if(str>1 & length(Test.Prp)==1) Test.Prp  = rep(Test.Prp,str)

    if(is.null(SS.prop.Ref)) SS.prop.Ref=rep(1/str,str)
    if(is.null(SS.prop.Test)) SS.prop.Test=rep(1/str,str)

    if(is.null(adj.prp.min)) adj.prp.min <- 1-n.adj/str * min(SS.prop.Ref,SS.prop.Test)
    if(is.null(adj.prp.max)) adj.prp.max <- 1+n.adj/str * min(SS.prop.Ref,SS.prop.Test)

    vec.RefPrd <-
      apply(
        matrix(
          c(SampleSize.Ref*SS.prop.Ref, Ref.Prp),
          ncol = str, byrow = TRUE
          ),
        2,
        FUN = function(vec){
          r.adj <-
            try(
              uniroot(
                f = function(x)
                  round(vec[1]) -
                  x * vec[1],
                interval = c(adj.prp.min, adj.prp.max)
                )
              )
          if(class(r.adj)=="try-error")
            stop(
              "Set wider uniroot(tolerence=) via adj.prp.min, adj.prp.max or choose other SS.prop values"
              )
          vec[1] <- as.integer(vec[1] * r.adj$root)
          res <-
            rbinom(
              n = itt,
              size = vec[1],
              prob = vec[2]
              )
          return(list(res, r.adj$root))
          }
        )
    RefPrd.r.adj <- laply(vec.RefPrd,function(L) L[[2]])
    vec.RefPrd <-  laply(vec.RefPrd,function(L) L[[1]]) %>% t()

    vec.TestPrd <-
      apply(
        matrix(
          c(
            SampleSize.Test *
              SS.prop.Test,
            Test.Prp
            ),
          ncol = str,
          byrow = TRUE
          ),
        2,
        FUN = function(vec){
          r.adj <-
            try(
              uniroot(
                f = function(x)
                  round(vec[1]) -
                  x * vec[1],
                interval = c(adj.prp.min, adj.prp.max)
                )
              )
          if(class(r.adj)=="try-error")
            stop(
              "Set wider uniroot(tolerence=) via adj.prp.min, adj.prp.max or choose other SS.prop values"
              )
          vec[1] <-
            as.integer(
              vec[1] * r.adj$root
              )
          res <- rbinom(
            n = itt,
            size = vec[1],
            prob = vec[2]
            )
          return(list(res, r.adj$root))
          }
        )
    TestPrd.r.adj <- laply(vec.TestPrd,function(L) L[[2]])
    vec.TestPrd <- laply(vec.TestPrd,function(L) L[[1]]) %>% t()

    # long data
    simdat <-
      vec.RefPrd %>%
      matrix(byrow = T, ncol = 1, dimnames = list(c(),c("y1"))) %>%
      bind_cols(
        "itt"=rep(1:itt, length=itt*str),
        "n1"=rep(SampleSize.Ref*SS.prop.Ref*RefPrd.r.adj, each=itt)
        ) %>%
      bind_cols(
        vec.TestPrd %>%
          matrix(byrow = T, ncol = 1, dimnames = list(c(),c("y2"))) %>%
          bind_cols(
            "itt"=rep(1:itt, length=itt*str),
            "n2"=rep(SampleSize.Test*SS.prop.Test*TestPrd.r.adj, each=itt)
            )
        )


  df.res.diffscoreci <- data.frame(
    matrix(
      vector(),
      itt,
      3,
      dimnames=list(
        c(),
        c("point.est","LCI95", "UCI95")
        )
      ),
    stringsAsFactors=F
    )

  # CI of probability in the simulation sample data.
  if(
    ci.method ==
      "PropCIs::diffscoreci"
    ){
    if(str>1) stop("Wrong method for str>1 is specified")
    for(i in 1:itt){
      res.scoreci  <-
        PropCIs::diffscoreci(
        x1 = simdat[i,"y1"], n1 = SampleSize.Ref,
        x2 = simdat[i,"y2"], n2 = SampleSize.Test,
        conf.level
        )
      df.res.diffscoreci[i,] <-
        c(Ref.Prp,
          res.scoreci[[1]][1],
          res.scoreci[[1]][2]
        )
      }
    }

  if(
    ci.method ==
      "strMNCI::scoreint.strat.MN"
    ){
    res.scoreci  <-
      simdat %>%
      as.data.frame() %>%
      ddply(
        .progress = "text",
        .(itt...2),
        .fun = function(M){
          strMNCI::scoreint.strat.MN(
            M[,c("y1","n1","y2","n2")], weight = "MN",
            conflev = 0.95
            )
          }
        )
      df.res.diffscoreci$point.est <- res.scoreci[,"V1"]
      df.res.diffscoreci$LCI95 <- res.scoreci[,"V2"]
      df.res.diffscoreci$UCI95 <- res.scoreci[,"V3"]
      }

  # Power calculation ------------
  res.diffscoreci_2 <- df.res.diffscoreci %>%
    mutate(
      Ref.Prp = point.est,
      Ratio = round(
        UCI95/ point.est,
        2),
      non.inf.bound   = non.inf.bound
      ) %>%
    mutate(
      "OKorNot" = ifelse(
        UCI95 <
          non.inf.bound,
        1,
        0
        )
      )
  return(
    list(
      "Power"=sum(
        res.diffscoreci_2$OKorNot
        ) / itt,
      "Sample size (adjusted)" = simdat,
      "diffscoreci" = res.diffscoreci_2
      )
    )
}

