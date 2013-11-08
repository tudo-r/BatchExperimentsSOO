#' Reduce results into standard data.frame
#' @param reg [\code{\link{ExperimentRegistrySOO}}]\cr
#'   Experiment registry.
#' @return [\code{data.frame}]. Contains the following columns: 
#'   \item{...}{Everything implied by \code{\link{reduceResultsSimple}}.}
#'   \item{opt [\code{list}]}{Global minimum target value.}
#'   \item{value [\code{numeric(1)}]}{Solution target value.}
#'   \item{gap [\code{integer(1)}]}{\code{value} - \code{opt}.}
#'   \item{fevals [\code{integer(1)}]}{Number of function evaluations.}
#'   \item{fht_<i> [\code{integer(1)}]}{First hitting time for each precision level.}
#' @export
reduceResultsSOO = function(reg) {
  checkArg(reg, "ExperimentRegistrySOO")
  reduceResultsSimple(reg, fun=function(job,res) {
    r1 = list(opt=res$opt$val, value=res$value, gap=res$gap, fevals=res$fevals)
    r2 = as.list(res$fht)
    names(r2) = sprintf("fht_%i", 1:length(r2))
    c(r1, r2)
  })
}  