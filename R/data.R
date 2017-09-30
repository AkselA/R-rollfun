
# # ### audio
# library(tuneR)
# audio <- readMP3("~/Documents/R/data/DR-test/ATribeCalledQuest_TheKillingSeason.mp3")

# l <- audio@left
# r <- audio@right
# audio <- data.frame(l, r)
# lnz <- which(l != 0)
# rnz <- which(r != 0)

# audio <- audio[min(c(lnz, rnz)):max(c(lnz, rnz)),]
# audio <- audio[200:9200, ]
# # matplot(audio, type="l", lty=1)

# ### xyz
# set.seed(1)
# l <- 270
# per <- rep(c(1:9, 2:-6, -5:0, 9:1, -5:5, -3:8, 5:-4), length.out=l)/5
# per <- per + rnorm(l)/5
# x <- rnorm(l) + ema(per, 11)
# x <- x * seq(0.1, 10, length.out=l)^(ema(x, 20)*2)
# xyz <- x
# # plot(xyz, type="l")

# ### xyz2
# fin <- 30
# fout <- 20
# fade <- rolliter(c(rep(0.25, fin), 
                   # rep(1, l-fin-fout), 
                   # rep(0.2, fout)), 
                   # 15, 5, TRUE, FALSE)
# xyz2 <- log(abs(xyz - 1)) * fade
# xyz2 <- rolliter(xyz2, 9, 9, TRUE) + xyz2/5
# # plot(xyz2)

# ### remove intermediate files
# rm(l, r, lnz, rnz, per, x, fin, fout, fade)

