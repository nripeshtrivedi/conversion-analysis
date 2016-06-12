all_numbers<-{4, 8,  15,  26,  38,  42,  46,  55,  68,  83,  86,  89,  96,  98,  99, 102, 113, 117, 123, 128, 131, 132, 133, 134,
,136, 137, 138, 139, 142, 146, 149, 150, 156, 158, 159, 162, 164, 165}
cptfn <- function(data, pen) {
  ans <- cpt.mean(data, test.stat="CUSUM", method = "SegNeigh", penalty = "Manual", pen.value = pen)
  length(cpts(ans)) +1
}
elbowplot1 <- unlist(lapply(c(1:1000), function(p) cptfn(data = count , pen = p)))
plot(elbowplot1)
layout(matrix(c(1,2,3,4,5),5,1,byrow=TRUE), widths=c(3,4), heights=c(2,6),respect=TRUE)
par(mfrow = c(10, 2))
par(mar=c(5,4,1,2))