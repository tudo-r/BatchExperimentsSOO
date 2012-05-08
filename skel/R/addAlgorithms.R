#' Add "L-BFGS-B from \code{\link{optim}}.
#' @param reg [\code{\link{ExperimentRegistrySOO}}]\cr
#'   Experiment registry.
#' @return Nothing.
#' @export
addAlgorithmBFGS = function(reg) {
  # FIXME: stop criterion mist so
  checkArg(reg, "ExperimentRegistrySOO")
  addAlgorithm(reg, id="bfgs", fun=function(static, dynamic, ...) {
    control = list(maxit=.Machine$integer.max)
    res = budgetWrapper({
      optim(method="L-BFGS-B", par=dynamic$start, fn=dynamic$test.fun, 
             lower=dynamic$low, upper=dynamic$upp, control=control)
    }, dynamic$test.fun, function(res) res$par, function(res) res$value)
    makeResultSOO(dynamic$test.fun, dynamic$opt, res$par, res$value, static$prec.levels)
  })
}

#' Add "L-BFGS-B from \code{\link[minqa]{bobyqa}}.
#' @param reg [\code{\link{ExperimentRegistrySOO}}]\cr
#'   Experiment registry.
#' @return Nothing.
#' @export
addAlgorithmBOBYQA = function(reg) {
  # FIXME: stop criterion mist so
  checkArg(reg, "ExperimentRegistrySOO")
  addAlgorithm(reg, id="bobyqa", fun=function(static, dynamic, ...) {
    requirePackages("minqa", "addAlgorithmBOBYQA")
    control = list(iprint=0, maxfun=dynamic$budget, ...)
    res = budgetWrapper({
      bobyqa(par=dynamic$start, fn=dynamic$test.fun, 
            lower=dynamic$low, upper=dynamic$upp, control=control)
    }, dynamic$test.fun, function(res) res$par, function(res) res$fval)
    makeResultSOO(dynamic$test.fun, dynamic$opt, res$par, res$value, static$prec.levels)
  })
}


#' Add \code{\link[cmaes]{cma_es}}.
#' @param reg [\code{\link{ExperimentRegistrySOO}}]\cr
#'   Experiment registry.
#' @return Nothing.
#' @export
addAlgorithmCMAES = function(reg) {
  checkArg(reg, "ExperimentRegistrySOO")
  addAlgorithm(reg, id="cmaes", fun=function(static, dynamic, ...) {
    requirePackages("cmaes", "addAlgorithmCMAES")
    control = list(maxit=Inf, stopfitness=dynamic$stopfitn, ...)
    #FIXME
    #control$lambda <- 2*control$mu
    res = budgetWrapper({
      cma_es(dynamic$start, dynamic$test.fun, 
        lower=dynamic$low, upper=dynamic$upp, control=control)
    }, dynamic$test.fun, function(res) res$par, function(res) res$value)
    makeResultSOO(dynamic$test.fun, dynamic$opt, res$par, res$value, static$prec.levels)
  })
}

#' Add \code{\link[DEoptim]{DEoptim}}.
#' @param reg [\code{\link{ExperimentRegistrySOO}}]\cr
#'   Experiment registry.
#' @return Nothing.
#' @export
addAlgorithmDEoptim = function(reg) {
  checkArg(reg, "ExperimentRegistrySOO")
  addAlgorithm(reg, id="deoptim", fun=function(static, dynamic, ...) {
    requirePackages("DEoptim", "addAlgorithmDEoptim")
    control <- DEoptim.control(itermax=1000, VTR=dynamic$stopfitn, ...)
    res = budgetWrapper({
      res = DEoptim(dynamic$test.fun, lower=dynamic$low, dynamic$upp, control=control)
    }, dynamic$test.fun, function(res) res$optim$bestmem, function(res) res$optim$bestval)
    print(str(res))
    makeResultSOO(dynamic$test.fun, dynamic$opt,  res$par, res$value, static$prec.levels)
  })
}

#' Add \code{\link[GenSA]{GenSA}}.
#' @param reg [\code{\link{ExperimentRegistrySOO}}]\cr
#'   Experiment registry.
#' @return Nothing.
#' @export
addAlgorithmGenSA = function(reg) {
  checkArg(reg, "ExperimentRegistrySOO")
  addAlgorithm(reg, id="gensa", fun=function(static, dynamic, ...) {
    requirePackages("GenSA", "addAlgorithmGenSA")
    control <- list(maxit=.Machine$integer.max, threshold.stop=dynamic$stopfitn, ...)
    res = budgetWrapper({
      res = GenSA(dynamic$start, dynamic$low, dynamic$upp, dynamic$test.fun, control=control)
    }, dynamic$test.fun, function(res) res$par, function(res) res$val)
    makeResultSOO(dynamic$test.fun, dynamic$opt,  res$par, res$value, static$prec.levels)
  })
}

#' Add \code{\link[pso]{psoptim}}.
#' @param reg [\code{\link{ExperimentRegistrySOO}}]\cr
#'   Experiment registry.
#' @return Nothing.
#' @export
addAlgorithmPSO = function(reg) {
  checkArg(reg, "ExperimentRegistrySOO")
  addAlgorithm(reg, id="pso", fun=function(static, dynamic, ...) {
    requirePackages("pso", "addAlgorithmPSO")
    control <- list(maxit=Inf, abstol=dynamic$stopfitn, ...)
    res = budgetWrapper({
      res = psoptim(dynamic$start, dynamic$test.fun, lower=dynamic$low, upper=dynamic$upp, control=control)
    }, dynamic$test.fun, function(res) res$par, function(res) res$value)
    makeResultSOO(dynamic$test.fun, dynamic$opt,  res$par, res$value, static$prec.levels)
  })
}


#' Add \code{\link[rbga]{genalg}}.
#' @param reg [\code{\link{ExperimentRegistrySOO}}]\cr
#'   Experiment registry.
#' @return Nothing.
#' @export
#FIXME: iters doof, aber sonst frisst er extrem viel speicher?
addAlgorithmRBGA = function(reg) {
  checkArg(reg, "ExperimentRegistrySOO")
  addAlgorithm(reg, id="rbga", fun=function(static, dynamic, ...) {
    requirePackages("genalg", "addAlgorithmRBGA")
    res = budgetWrapper({
      res = rbga(stringMin=dynamic$low, stringMax=dynamic$upp, evalFunc=dynamic$test.fun, iters=dynamic$budget, ...)
    }, dynamic$test.fun, function(res) res$par, function(res) res$value)
    makeResultSOO(dynamic$test.fun, dynamic$opt,  res$par, res$value, static$prec.levels)
  })
}


