
rename = function(data, probs, algos) {
  f = function(x, y) {
    if (is.factor(x)) {
      was.factor = TRUE 
      x = as.character(x)
    } else {
      was.factor = FALSE
    }
    for (s in names(y)) {
      j = which(x == s)
      x[j] = y[[s]]
    }
    if (was.factor) as.factor(x) else x
  }
  data$prob = f(data$prob, probs)
  return(data)
}


# fixme: wir brauchen sowas wie eine experiment id. alos wasa nur wdh worden ist
s = function(data) {
  dims = sort(unique(data$dim))
  n.dims = length(dims)
  probs = sort(unique(data$prob))
  n.probs = length(probs)
  algos = sort(unique(data$algo))
  n.algos = length(algos)
  # FIXME:
  repls = max(data$repl)
  messagef("Dims: %i\n%s", n.dims, collapse(dims, sep=", "))
  messagef("Problems: %i\n%s", n.probs, collapse(probs, sep=", "))
  messagef("Algorithms: %i\n%s", n.algos, collapse(algos, sep=", "))
  messagef("Replications: %s", collapse(repls, sep=", "))
  
  #params.flat = c()
  #mean.perf <- ddply(data2, c("prob", "algo", params.flat), summarize,
  #  mean_gap=mean(gap),
  #  mean_ert=mean(ert),
  #  sd_ert=sd(ert)
  #)  
  cns = colnames(data)
  fht.strs = cns[str_detect(cns, "fht")]
  for (pp in probs) {
    messagef("Problem: %s", pp)
    data2 = subset(data, prob==pp)
    e = ddply(data2, c("dim"), function(d) {
      colSums(!is.na(d[,fht.strs]))
    })
    print(e)
  }    
}

probs = sprintf("bbob_%02i", 1:24)
names(probs) = sprintf("bbob_%i", 1:24)
dd = rename(data, probs=probs)

#s(dd)

prec.levels = c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5)

dd = bootstrapERTsDDPLY(dd, c(), prec.levels=prec.levels, 1, 20)


#getSolved