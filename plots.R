

load("results_men.RData")

range=rbind(c(0,0.15),c(0.1,0.4),c(0,0.2),c(0,0.08),c(0.15,0.6),c(0,0.15),c(0,0.2),c(0,0.3))
cause = c("INF", "NEOP", "LUNG", "END", "CIRC", "RESP", "DIG", "EXT")
countries = c("CAN", "USA", "JPN", "AUT", "BEL", "DNK", "FIN", "FRA", "GRE", "HUN", "ICE",
              "IRL", "ITA", "NL", "NOR", "POL", "SPA", "SWE", "SWI", "UK", "AUS", "NZL")

pdf("causes-men.pdf",16,9)
par(mfrow=c(2,4))
for(i in 1:c){
  matplot(start.year:end.year, datarray.smooth[,i,], type="n", ylim=range[i,], main=cause[i], ylab="", xlab="",
          bty="n", axes=F, lwd=3, cex.main = 2.5)
  grid()
  axis(1, at=c(1970,1985,2000,2015), cex.axis=2)
  axis(2, cex.axis=2)
  matlines(start.year:end.year, datarray.smooth[,i,], lty=1, col="lightgrey", lwd=1)
  matlines(start.year:end.year, m.simplex[,i], lty=1, col=hcl.colors(c)[i], lwd=5)
}
dev.off()


pdf("PC1-men.pdf",16,9)
par(mfrow=c(2,4))
for(j in 1:c){
  matplot(start.year:end.year,m.simplex[,j], main=cause[j], type="n", ylim=range[j,],
          xlab="", ylab="", bty="n", axes=F, cex.main=2.5)
  grid()
  axis(1, at=c(1970,1985,2000,2015), cex.axis=2)
  axis(2, cex.axis=2)
  matlines(start.year:end.year,m.simplex[,j], lty=1, col=hcl.colors(c)[j], lwd=5)
  points(start.year:end.year,m_big_pc1_plus.simplex[,j],pch="+", col=hcl.colors(c)[j], cex=1.5)
  points(start.year:end.year,m_big_pc1_minus.simplex[,j],pch="-", col=hcl.colors(c)[j], cex=1.5)
}
dev.off()

pdf("PC2-men.pdf",16,9)
par(mfrow=c(2,4))
for(j in 1:c){
  matplot(start.year:end.year,m.simplex[,j], main=cause[j], type="n", ylim=range[j,],
          xlab="", ylab="", bty="n", axes=F, cex.main=2.5)
  grid()
  axis(1, at=c(1970,1985,2000,2015), cex.axis=2)
  axis(2, cex.axis=2)
  matlines(start.year:end.year,m.simplex[,j], lty=1, col=hcl.colors(c)[j], lwd=5)
  points(start.year:end.year,m_big_pc2_plus.simplex[,j],pch="+", col=hcl.colors(c)[j], cex=1.5)
  points(start.year:end.year,m_big_pc2_minus.simplex[,j],pch="-", col=hcl.colors(c)[j], cex=1.5)
}
dev.off()

pdf("PC3-men.pdf",16,9)
par(mfrow=c(2,4))
for(j in 1:c){
  matplot(start.year:end.year,m.simplex[,j], main=cause[j], type="n", ylim=range[j,],
          xlab="", ylab="", bty="n", axes=F, cex.main=2.5)
  grid()
  axis(1, at=c(1970,1985,2000,2015), cex.axis=2)
  axis(2, cex.axis=2)
  matlines(start.year:end.year,m.simplex[,j], lty=1, col=hcl.colors(c)[j], lwd=5)
  points(start.year:end.year,m_big_pc3_plus.simplex[,j],pch="+", col=hcl.colors(c)[j], cex=1.5)
  points(start.year:end.year,m_big_pc3_minus.simplex[,j],pch="-", col=hcl.colors(c)[j], cex=1.5)
}
dev.off()

pdf("PC4-men.pdf",16,9)
par(mfrow=c(2,4))
for(j in 1:c){
  matplot(start.year:end.year,m.simplex[,j], main=cause[j], type="n", ylim=range[j,],
          xlab="", ylab="", bty="n", axes=F, cex.main=2.5)
  grid()
  axis(1, at=c(1970,1985,2000,2015), cex.axis=2)
  axis(2, cex.axis=2)
  matlines(start.year:end.year,m.simplex[,j], lty=1, col=hcl.colors(c)[j], lwd=5)
  points(start.year:end.year,m_big_pc4_plus.simplex[,j],pch="+", col=hcl.colors(c)[j], cex=1.5)
  points(start.year:end.year,m_big_pc4_minus.simplex[,j],pch="-", col=hcl.colors(c)[j], cex=1.5)
}
dev.off()

pdf("clus-men.pdf", width = 10, height = 10)
plot(sc.pc1,sc.pc2, col=1:n, pch=0, cex=0, xlim=range(sc.pc1)+c(-2,2), cex.axis=1.5, cex.lab=1.5,
     ylim=range(sc.pc1)+c(-2,2), xlab="PC1", ylab="PC2")
grid()
title("Men", cex.main=2)
points(cntrs[,c(1,2),1], col=c(4,2,3,1,"Orange"), cex=1.5, pch=19)
cl[which(cl==5)] <- "Orange"
text(sc.pc1,sc.pc2,countries,col=cl, cex=2)
abline(h=0, col="grey", lty=2)
abline(v=0, col="grey", lty=2)
dev.off()

pdf("groups-men.pdf", 16, 9)
par(mfrow=c(2,4))
for(i in 1:c){
  matplot(start.year:end.year, datarray.smooth[,i,], type="n", 
          ylim=range[i,], main=cause[i], ylab="", xlab="",
          bty="n", axes=F, cex.main=2.5)
  grid()
  axis(1, at=c(1970,1985,2000,2015), cex.axis=2)
  axis(2, cex.axis=2)
  matlines(start.year:end.year, g1[,i], lty=1, col=1, lwd=3)
  matlines(start.year:end.year, g2[,i], lty=1, col=2, lwd=3)
  matlines(start.year:end.year, g3[,i], lty=1, col=3, lwd=3)
  matlines(start.year:end.year, g4[,i], lty=1, col=4, lwd=3)
  matlines(start.year:end.year, g5[,i], lty=1, col="Orange", lwd=3)
}
dev.off()


rm(list=ls())

load("results_women.RData")

range=rbind(c(0,0.1),c(0.25,0.6),c(0,0.2),c(0,0.1),c(0.1,0.55),c(0,0.15),c(0,0.2),c(0,0.2))

cause = c("INF", "NEOP", "LUNG", "END", "CIRC", "RESP", "DIG", "EXT")
countries = c("CAN", "USA", "JPN", "AUT", "BEL", "DNK", "FIN", "FRA", "GRE", "HUN", "ICE",
              "IRL", "ITA", "NL", "NOR", "POL", "SPA", "SWE", "SWI", "UK", "AUS", "NZL")

pdf("causes-women.pdf",16,9)
par(mfrow=c(2,4))
for(i in 1:c){
  matplot(start.year:end.year, datarray.smooth[,i,], type="n", ylim=range[i,], main=cause[i], ylab="", xlab="",
          bty="n", axes=F, lwd=3, cex.main = 2.5)
  grid()
  axis(1, at=c(1970,1985,2000,2015), cex.axis=2)
  axis(2, cex.axis=2)
  matlines(start.year:end.year, datarray.smooth[,i,], lty=1, col="lightgrey", lwd=1)
  matlines(start.year:end.year, m.simplex[,i], lty=1, col=hcl.colors(c)[i], lwd=5)
}
dev.off()


pdf("PC1-women.pdf",16,9)
par(mfrow=c(2,4))
for(j in 1:c){
  matplot(start.year:end.year,m.simplex[,j], main=cause[j], type="n", ylim=range[j,],
          xlab="", ylab="", bty="n", axes=F, cex.main=2.5)
  grid()
  axis(1, at=c(1970,1985,2000,2015), cex.axis=2)
  axis(2, cex.axis=2)
  matlines(start.year:end.year,m.simplex[,j], lty=1, col=hcl.colors(c)[j], lwd=5)
  points(start.year:end.year,m_big_pc1_plus.simplex[,j],pch="+", col=hcl.colors(c)[j], cex=1.5)
  points(start.year:end.year,m_big_pc1_minus.simplex[,j],pch="-", col=hcl.colors(c)[j], cex=1.5)
}
dev.off()

pdf("PC2-women.pdf",16,9)
par(mfrow=c(2,4))
for(j in 1:c){
  matplot(start.year:end.year,m.simplex[,j], main=cause[j], type="n", ylim=range[j,],
          xlab="", ylab="", bty="n", axes=F, cex.main=2.5)
  grid()
  axis(1, at=c(1970,1985,2000,2015), cex.axis=2)
  axis(2, cex.axis=2)
  matlines(start.year:end.year,m.simplex[,j], lty=1, col=hcl.colors(c)[j], lwd=5)
  points(start.year:end.year,m_big_pc2_plus.simplex[,j],pch="+", col=hcl.colors(c)[j], cex=1.5)
  points(start.year:end.year,m_big_pc2_minus.simplex[,j],pch="-", col=hcl.colors(c)[j], cex=1.5)
}
dev.off()

pdf("PC3-women.pdf",16,9)
par(mfrow=c(2,4))
for(j in 1:c){
  matplot(start.year:end.year,m.simplex[,j], main=cause[j], type="n", ylim=range[j,],
          xlab="", ylab="", bty="n", axes=F, cex.main=2.5)
  grid()
  axis(1, at=c(1970,1985,2000,2015), cex.axis=2)
  axis(2, cex.axis=2)
  matlines(start.year:end.year,m.simplex[,j], lty=1, col=hcl.colors(c)[j], lwd=5)
  points(start.year:end.year,m_big_pc3_plus.simplex[,j],pch="+", col=hcl.colors(c)[j], cex=1.5)
  points(start.year:end.year,m_big_pc3_minus.simplex[,j],pch="-", col=hcl.colors(c)[j], cex=1.5)
}
dev.off()

pdf("PC4-women.pdf",16,9)
par(mfrow=c(2,4))
for(j in 1:c){
  matplot(start.year:end.year,m.simplex[,j], main=cause[j], type="n", ylim=range[j,],
          xlab="", ylab="", bty="n", axes=F, cex.main=2.5)
  grid()
  axis(1, at=c(1970,1985,2000,2015), cex.axis=2)
  axis(2, cex.axis=2)
  matlines(start.year:end.year,m.simplex[,j], lty=1, col=hcl.colors(c)[j], lwd=5)
  points(start.year:end.year,m_big_pc4_plus.simplex[,j],pch="+", col=hcl.colors(c)[j], cex=1.5)
  points(start.year:end.year,m_big_pc4_minus.simplex[,j],pch="-", col=hcl.colors(c)[j], cex=1.5)
}
dev.off()

pdf("clus-women.pdf", width = 10, height = 10)
plot(sc.pc1,sc.pc2, col=1:n, pch=0, cex=0, xlim=range(sc.pc1)+c(-2,2), cex.axis=1.5, cex.lab=1.5,
     ylim=range(sc.pc1)+c(-2,2), xlab="PC1", ylab="PC2")
grid()
title("Women", cex.main=2)
points(cntrs[,c(1,2),1], col=c("Orange",4,2,1,6,3), cex=1.5, pch=19)
cl2 = c(4,4,3,"Orange",6,4,"Orange",6,"Orange","Orange",1,2,3,4,1,"Orange",3,1,6,2,4,2)
text(sc.pc1,sc.pc2,countries,col=cl2, cex=2)
abline(h=0, col="grey", lty=2)
abline(v=0, col="grey", lty=2)
dev.off()

pdf("groups-women.pdf", 16, 9)
par(mfrow=c(2,4))
for(i in 1:c){
  matplot(start.year:end.year, datarray.smooth[,i,], type="n", 
          ylim=range[i,], main=cause[i], ylab="", xlab="",
          bty="n", axes=F, cex.main=2.5)
  grid()
  axis(1, at=c(1970,1985,2000,2015), cex.axis=1.5)
  axis(2, cex.axis=1.5, las=1)
  matlines(start.year:end.year, g1[,i], lty=1, col="Orange", lwd=3)
  matlines(start.year:end.year, g2[,i], lty=1, col=4, lwd=3)
  matlines(start.year:end.year, g3[,i], lty=1, col=3, lwd=3)
  matlines(start.year:end.year, g4[,i], lty=1, col=2, lwd=3)
  matlines(start.year:end.year, g5[,i], lty=1, col=6, lwd=3)
  matlines(start.year:end.year, g6[,i], lty=1, col=1, lwd=3)
}
dev.off()

rm(list=ls())

