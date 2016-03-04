#' correct copynumber by gc percent and mappability
#' @param bed A data.table
#' @return A data.table
#' @export
cor.cn <- function (bed, map.min = 0.9, samplesize = 50000,
                   verbose = T, gc.cor = T, map.cor = T)
{
  message("run cor.cn for ", unique(bed$gid))

  if (length(bed$reads) == 0 | length(bed$gc) == 0 | length(bed$map) == 0){
    stop("Missing one of required columns: reads, gc, map")
  } else{
    message(str(bed))
  }

  bed = bed[reads > 0 & gc > 0]

  reads.range = bed[, quantile(reads, prob = c(0.025, 1-0.025), na.rm = TRUE)]
  gc.range = bed[, quantile(gc, prob = c(0.025, 1 - 0.025), na.rm = TRUE)]

  bed[, ideal:=(map >= map.min &
                reads >= reads.range[1] & reads <= reads.range[2] &
                chr <= 22 &
                gc >= gc.range[1] & gc <= gc.range[2] &
                pn == 0)]

  m.gc.rough = bed[ideal==T, loess(reads ~ gc, span = 0.03)]

  gc.values = seq(0, 1, by = 0.001)
  m.gc.final = loess(predict(m.gc.rough, gc.values) ~ gc.values, span = 0.3)

  bed[, cor.gc := reads/predict(m.gc.final, gc)]

  cor.gc.range = quantile(bed[ideal==T, cor.gc], prob = c(0.025, 1 - 0.025), na.rm = T)
  m.map = bed[cor.gc > cor.gc.range[1] & cor.gc < cor.gc.range[2],
              approxfun(lowess(map, cor.gc))]
  bed[, cor.map := cor.gc/m.map(map)]

  bed[, copy := map.cor*cor.map + (!map.cor)*cor.gc][, copy:=copy/mean(copy,na.rm=T)*2]
  
  bed
}
