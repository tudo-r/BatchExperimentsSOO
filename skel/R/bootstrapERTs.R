#' Boostrap expected runtime (ERT) values from first-hitting times.
#'
#' Bootstrapped from first-hitting times 
#' for a precisions level nmeasured in function evaulations.
#'
#' @param runtimes [\code{numeric}]\cr
#'   Vector of solution target values.
#' @param succ [\code{numeric}]\cr
#'   First-hitting times.
#' @param target [\code{numeric(1)}]\cr
#'   Target value to reach.
#' @param n [\code{integer(1)} | \code{function(dim)}]\cr
#'   Number of ERTs to return.
#' @return [\code{\link{numeric}}]. 
#' @export
bootstrapERTs = function(runtimes, succ, n) {
  checkArg(runtimes, "numeric", na.ok=FALSE)
  m = length(runtimes)
  checkArg(succ, "logical", na.ok=FALSE, len=m)
  n = convertInteger(n)
  checkArg(n, "integer", len=1, na.ok=FALSE)
  if (!any(succ))
    return(rep(Inf, n))
  erts = numeric(n)
  for (i in 1:n) {
    samps = integer(0)
    while(TRUE) {
      # boostrap some runs
      inds = sample(1:m, replace=TRUE, 10000)
      samps = c(samps, inds)
      # hit the target in the new runs?
      if (any(succ[inds]))
        break
    }
    # index where we first hit the target val
    j = which(succ[samps])[1]
    erts[i] = sum(runtimes[samps][1:j])
  }
  return(erts)
} 


bootstrapERTsDDPLY = function(data, params.flat, prec.levels, i, n.erts) {
  fht.str = sprintf("fht_%i", i)
  fht.strs = sprintf("fht_%i", seq_along(prec.levels))
  cols = setdiff(colnames(data), c(fht.strs, "fevals"))
  prec.level = prec.levels[[i]]
  data = ddply(data, c("dim", "prob", "algo", params.flat), function(d) {
    fht = d[[fht.str]]
    succ = !is.na(fht)
    runtimes = ifelse(succ, fht, d$fevals)
    ert = bootstrapERTs(runtimes, succ, n.erts)
    d = d[1:n.erts, cols]
    d$ert = ert
    d
  }, .progress="text") 
}
