#' Set a budget of function evaluations for a test function.
#'
#' If this is exceeded, an exception is thrown so the optimizer stops.
#' Note that this implies we will not receive the usual return value
#' structure from the optimizer and have to create the relevant stuff
#' ourself!
#' @param test.fun [\code{\link{record_target_values_function}}]\cr
#'   Test function.
#' @param budget [\code{integer(1)}]\cr
#'   Budget of allowed function evaluations.
#' @return [\code{\link{record_target_values_function}}].
#' @export
setBudgetException = function(test.fun, budget) {
  force(test.fun)
  checkArg(test.fun, "record_target_values_function")
  budget = convertInteger(budget)
  checkArg(budget, "integer", len=1, na.ok=FALSE)
  test.fun2 = function(x, ...) {
    n = if (is.matrix(x)) ncol(x) else 1L
    if (length(soobench:::target_values(test.fun)) + n > budget)
      stop("Budget exceeded!")
    test.fun(x, ...)
  }
  attributes(test.fun2) = attributes(test.fun)
  return(test.fun2)
}


#' Wrapper to call an optimizer with restricted budget.
#'
#' For test functions modified with \code{\link{setBudgetException}}.
#' Handles the exception and contructs a standardized result 
#' object.
#' @param expr [\code{expression}]\cr
#'   Expression that calls the optimizer on the test function.
#' @param test.fun [\code{\link{record_target_values_function}}]\cr
#'   Test function.
#' @param get.par [\code{function(res)}]\cr
#'   Getter for solution vector in optimizer result structure.
#' @param get.value [\code{function(res)}]\cr
#'   Getter for solution target value in optimizer result structure.
#' @return [\code{list}]. Has elements \code{par} and \code{value}.
#'   If budget was exhausted, \code{par} contains NAs and \code{value} is the minimum
#'   of all visited target values.
#' @export 
budgetWrapper = function(expr, test.fun, get.par, get.value) {
  checkArg(test.fun, "record_target_values_function")
  checkArg(get.par, formals="res")
  checkArg(get.value, formals="res")
  tryCatch({
    suppressAll({
      res = eval(expr)
    })
    list(par = get.par(res), value=get.value(res))
  }, error=function(e) {
    if (e$message == "Budget exceeded!") {
      y = soobench:::target_values(environment(test.fun)$test.fun)
      list(par=as.numeric(rep(NA, number_of_parameters(test.fun))), value=min(y))
    } else {
      stop(e)
    }
  })
}