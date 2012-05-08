#FIXME was passiert mit der seed bei problem mit params, das ist eine alllg frage!


#' Add test function from soobench package as a problem for single-objective optimization.
#'
#' @param reg [\code{\link{ExperimentRegistrySOO}}]\cr
#'   Experiment registry.
#' @param test.gen [\code{function(dimensions)}]\cr
#'   Test function generator.
#' @param dims [\code{integer}]\cr
#'   Vector of decision space dimensions for test function.
#' @param prec.levels [\code{numeric}]\cr
#'   Vector of desired target precision levels.  
#'   Number of functions evaluations will be recorded when a level is reached
#'   for the first time.
#' @param budget [\code{integer(1)} | \code{function(dim)}]\cr
#'   Number of allowed functions evaluations.
#'   Can be a function of test function dimension.
#' @param seed [\code{numeric}]\cr
#'   Problem seed. See \code{\link{addProblem}}.
#'   Allows the synchronization of start values for optimizers for a test function.
#'   Default is \code{NULL}
#' @return [\code{\link{Design}}]. Problem design with dimensions as exhaustive parameter.
#' @export
addProblemSOO = function(reg, test.gen, dims, prec.levels, budget, seed=NULL) {
  checkArg(reg, "ExperimentRegistrySOO")
  checkArg(test.gen, formals="dimensions")
  dims = convertIntegers(dims)
  checkArg(dims, "integer", na.ok=FALSE)
  checkArg(prec.levels, "numeric", na.ok=FALSE)
  budget = convertInteger(budget)
  if (is.integer(budget) && length(budget) == 1)
    b=budget; budget = function(dim) b
  checkArg(budget, formals="dim")
  dim = dims[1]
  dummy = test.gen(dim)
  # FIXME: id so mist
  id = strsplit(function_id(dummy), sprintf("-%id", dim))[[1]][1]
  static = list(id=id, test.gen = test.gen, dims=dims, 
    prec.levels=prec.levels, budget.fun=budget)
  addProblem(reg, id, seed=seed, static=static, dynamic = function(static, dim) {
    # FIXME remove and ::: everywhere
    source("skel/R/makeResultSOO.R")
    source("skel/R/setBudgetException.R")
    budget = static$budget.fun(dim)
    test.fun = static$test.gen(dim)
    test.fun = soobench:::record_target_values_function(test.fun)
    test.fun = setBudgetException(test.fun, budget)
    low = lower_bounds(test.fun)
    upp = upper_bounds(test.fun)
    opt = global_minimum(test.fun)
    start = drop(random_parameters(1L, test.fun))
    stopfitn = opt$val + min(prec.levels)
    list(test.fun=test.fun, low=low, upp=upp, opt=opt, start=start, 
      budget=budget, stopfitn=stopfitn)
  })
  makeDesign(id, exhaustive=list(dim=dims))
}  


