#' source ~/.bashrc, run bash command line
#' @examples
#' sh("ls ~/")
#' @export
sh = function(.cmd){ system(sprintf("echo 'source ~/.bashrc; %s'|bash", .cmd)) }

#' read bedfile
#' @param .bedfile A tsv file
#' @return A data.table
#' @export
read.bed = function(.bedfile) {
  message("read bedfile: ", .bedfile)
  bed = fread(.bedfile, header = F) %>%
    setnames(c("V1","V2","V3","V4","V5","V6","V7","V8"),
             c("chr","start","end","gc","map","pn","reads","gid"))
  bed$gid = as.character(bed$gid)
  return(bed)
}



