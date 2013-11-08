#' Create a result list for single-objective optimization in standard form.
#' @param test.fun [\code{\link{record_target_values_function}}]\cr
#'   Test function.
#' @param opt [\code{list}]\cr
#'   Global minimum.
#' @param par [\code{numeric}]\cr
#'   Solution vector.
#' @param value [\code{numeric(1)}]\cr
#'   Solution target value.
#' @param prec.levels [\code{numeric}]\cr
#'   Vector of desired target precision levels.  
#' @return [\code{list}].
#'   \item{fevals [\code{integer(1)}]}{Number of function evaluations.}
#'   \item{par [\code{numeric}]}{Solution vector.}
#'   \item{value [\code{numeric(1)}]}{Solution target value.}
#'   \item{gap [\code{integer(1)}]}{\code{value} - \code{opt$value}.}
#'   \item{fht [\code{numeric}]}{First hitting times for \code{prec.levels}.}
#'   \item{opt [\code{list}]}{Global minimum.}
#' @export
makeResultSOO = function(test.fun, opt, par, value, prec.levels) {
  checkArg(test.fun, "record_target_values_function")
  checkArg(opt, "list")
  # can be NA because of budget
  checkArg(par, "numeric", na.ok=TRUE)
  checkArg(value, "numeric", len=1, na.ok=FALSE)
  checkArg(prec.levels, "numeric", na.ok=FALSE)
  # FIXME: very ugly hack, look for envir calls in code
  y = soobench:::target_values(environment(test.fun)$test.fun)
  target.levels = opt$value + prec.levels
  fevals = length(y)
  fht = soobench:::first_hitting_times(y, target.levels)
  list(fevals=fevals, par=par, value=value, gap=value-opt$value, fht=fht, opt=opt)
}