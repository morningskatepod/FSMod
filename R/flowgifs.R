library(diagram)
library(animation)

flow <- t(flow)

for(i in seq(1,500,25)) {
png(filename=paste('PNGs/',i-1,".png", sep = ''))
par(mar = c(2,2,2,2),mfrow = c(1,1))
names = c('calves','1st Lact','1st Dry','Lactating','Dry','2nd Lact','3rd Dry',
          '3rd Lact','2nd Dry')
M <- matrix(nrow = 9, ncol = 9, byrow = TRUE, data = 0)
size <- flow[i,]/5000
M[2, 1] <- M[3, 2] <- M[6, 3] <- M[8, 9] <- M[9, 6] <-
  M[7,8] <- M[4,7] <- M[5,4] <- M[1, 4] <- M[4,5] <- "flow"
plotmat(M, pos = c(3, 3, 3), curve = 0, name = names, lwd = 1, box.size = size,
        box.lwd = 2, cex.txt = 0.8, box.type = "circle", box.prop = 0.5,txt.yadj = -2.5)
dev.off()
}
