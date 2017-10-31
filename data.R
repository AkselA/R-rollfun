
### stereo
library(tuneR)
stereo <- readMP3(paste0("~/Documents/R/data/DR-test/",
                          "ATribeCalledQuest_TheKillingSeason.mp3"))

l <- stereo@left
r <- stereo@right
stereo <- data.frame(l, r)
lnz <- which(l != 0)
rnz <- which(r != 0)

stereo <- stereo[min(c(lnz, rnz)):max(c(lnz, rnz)),]
stereo <- stereo[201:22250, ]
rownames(stereo) <- NULL
# matplot(stereo, type="l", lty=1)

### xyz
set.seed(1)
l <- 300
per <- rep(c(1:9, 2:-6, -5:0, 9:1, -5:5, -3:8, 5:-4), length.out=l)/5
per <- per + rnorm(l)/5
x <- rnorm(l) + ema(per, 11)
x <- x * seq(0.1, 10, length.out=l)^(ema(x, 20)*2)
xyz <- x
# plot(xyz, type="l")

### xyz2
xyz2 <- log(abs(xyz - 1))
xyz2 <- rolliter(xyz2, 9, 9, TRUE) - xyz2/5
xyz2 <- fade(xyz2, fin=c(0.5, 40, 20), fout=c(0.4, 20, 10))
xyz2 <- xyz2/max(abs(xyz2))
# plot(xyz2, col="#FF0000FF", lwd=1.5, type="l", pch=16)

### remove intermediate files
rm(l, r, lnz, rnz, per, x)
