#' Construct registry for single-objective optimization benchmark experiments.
#'
#' BatchExperimentsSOO will always be added to the registry packages. 
#' @param ... See [\code{\link{ExperimentRegistry}}].
#' @return [\code{\link{ExperimentRegistrySOO}}].
#' @export
#' @aliases ExperimentRegistrySOO
makeExperimentRegistrySOO = function(...) {
  args = list(...)
  ps = "BatchExperimentsSOO"
  packages = args$packages
  if (!is.null(packages)) {
    checkArg(packages, "character")
    ps = union(ps, packages)
  }
  args$packages = ps
  reg = do.call(makeExperimentRegistry, args)
  class(reg) = c(class(reg), "ExperimentRegistrySOO")
  return(reg)
}