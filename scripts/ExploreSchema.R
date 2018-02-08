find.schema <- function(x, y ){
  rs.start <- x[ , .I[PosRootMotion == y]]
  rs.extend <- rs.start + 1
  print(entropy(table(x$PosRootMotion[rs.extend]), unit = "log2"))
  print(table(x$PosRootMotion[rs.extend]))
  print(sum(table(x$PosRootMotion[rs.extend]))/sum(nrow(x)))
  print(
    round(table(x$PosRootMotion[rs.extend])/sum(table(x$PosRootMotion[rs.extend])),2)
  )
  pie(table(x$PosRootMotion[rs.extend]), main = paste("Given Step by", y, "Likelihood of Next Chord"))
}

