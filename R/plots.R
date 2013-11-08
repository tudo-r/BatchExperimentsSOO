# FIXME welche params für einen algo?
plotCompareAlgorithms = function(data, dim, probs=as.character(unique(data$prob)), algos=as.character(unique(data$algo)), 
                                 measure="ert", scale_y, format, prefix="compare-algos", suffix="") {
  
  checkArg(data, "data.frame")
  if (missing(dim) && length(unique(data$dim)) == 1)
    dim = data$dim[1]
  else
    checkArg(dim, choices=unique(data$dim))
  checkArg(probs, subset=as.character(unique(data$prob)))
  checkArg(algos, subset=as.character(unique(data$algo)))
  if (!missing(scale_y))
    checkArg(scale_y, "function")
  if (!missing(format))
    checkArg(format, choices=c("pdf", "png"))
  checkArg(prefix, "character", len=1, na.ok=FALSE)
  checkArg(suffix, "character", len=1, na.ok=FALSE)
  
  data = subset(data, data$dim == dim & prob %in% probs & algo %in% algos)
  if (!missing(format)) {
    fn = sprintf("%s%s.%s", prefix, suffix, format)
    do.call(format, list(fn))
  }
  p = ggplot(data, aes_string(x="algo", y=measure)) +
    geom_boxplot() +
    facet_wrap(~prob, scales="free_y")
  if (!missing(scale_y))
    p = p + scale_y()
  print(p)
  if (!missing(format))
    dev.off()
}


#library(ggplot2)
#source("skel/R/bootstrapERTs.R")

# FIXME what happens with inf / nas in plot?


#data2 = subset(data, prob != "ackley" & algo != "rbga")
#data2 = data
#data2 = bootstrapERTsDDPLY(data2, c(), prec.levels, 1, n.erts=10)

#plotCompareAlgorithms(data2, scale_y=scale_y_log10)