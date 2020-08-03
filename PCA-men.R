
# Sources + packages ------------------------------------------------------

source("functions.R")
library(compositions)
library(kernlab)
library(cluster)

# Import external data ----------------------------------------------------

dat1 <- read.csv("ICD/ICD7_comp.csv")
dat2 <- read.csv("ICD/ICD8_comp.csv")
dat3 <- read.csv("ICD/ICD9_comp.csv")
dat4 <- read.csv("ICD/ICD10_comp.csv")

ID <- c(5020,4010,4020,2090,4050,4070,4080,4140,4150,4160,4170,4180,3160,4190,
        4210,4220,4230,4240,5150,4280,4290,4300,4308,2450)
names <- c("Australia","Austria","Belgium","Canada","Denmark","Finland","France",
           "Greece","Hungary","Iceland","Ireland","Italy","Japan","Luxembourg",
           "Netherlands","Norway","Poland","Portugal","New Zealand","Spain",
           "Sweden","Switzerland","United Kingdom","United States of America")

dat <- rbind(dat1,dat2,dat3,dat4)
dat$Prop <- dat$Counts/dat$AllCauses
nations = unique(dat$Country)
n = length(nations)
years = unique(dat$Year)
y = length(years)
causes = unique(dat$CauseM)
c = length(causes)
sex = unique(dat$Sex)
s = length(sex)

begin=c()
for(i in 1:n){
  begin[i] = min(dat[dat$Country==nations[i],3])
}
start.year = max(begin[-24])
end.year = 2015
TT = end.year-start.year+1


###############################################################################
##########################                        #############################
###########                         MEN                          ##############
#########################                         #############################
###############################################################################

# Preproc ---------------------------------------------------------------------

# We remove other causes and infant mortality
c=8
# Organize data in a list
dati <- list()
for(i in 1:n){
  dati[[i]] = matrix(NA,TT,c)
  for(j in 1:c){
    for(k in 1:TT){
      v = dat[dat$Country==nations[i]&dat$Year==start.year-1+k&dat$Sex==1&dat$CauseM==j,]
      v = v[v$Age=="40-44"|v$Age=="45-49"|v$Age=="50-54"|v$Age=="55-59"|v$Age=="60-64",]
      dati[[i]][k,j] <- if(length(v)>0) sum(v[,7]) else NA
    }
  }
  print(i)
}

cause <- c("Infect", "Neop", "Lung", "Endoc", "Circ", "Resp", "Digest", "Extern")
stati = names[match(nations,ID)]  # ordered names of the countries

X11()
par(mfrow=c(4,6), mar=c(2,2,2,2))
for(i in 1:n){
  matplot(start.year:end.year, dati[[i]], type="l", main= stati[i], ylab="", lty=1, 
          col=hcl.colors(c), lwd=2, xlab="")
}
# from this plot we can see that Portugal and Luxembourg have too many missing data,
# they will be removed from the analysis. Other missing values can be spotted, as 
# written in the paper (sec. 2.1)

dati = dati[-c(17,24)]
stati = stati[-c(17,24)]
n=n-2

# Imputation for Greece ---------------------------------------------------

# Greece exhibits zero cases of lung cancer for the first two years. We
# recode them as NAs and we impute the values following Kraus (2015). 
# These data were recorded as general neoplasms (see series) so we subtract
# them from cause 2, without altering the total.

dati[[9]]
dati[[9]][1:2,3] <- NA
dati[[9]][,3] <-  reg.smooth(diag(TT),matrix(dati[[9]][,3],TT,1),r=10)$beta
miss=1:2
miss.nation=rep(F,n)
miss.nation[9] = T
x = t(sapply(dati,function(x) x[,3]))
x.train = x[!miss.nation,-miss]
y.train = x[!miss.nation,miss]
x.test = x[miss.nation,-miss]
y.test = x[miss.nation,miss]
rec.ridge = ridge.recon.fit(x.train,y.train,info=F,plot.coefnorm=F,
                            plot.selection=F,selection="gcv")
y.test = rec.ridge$a%*%x.test
plot(dati[[9]][,3], pch=19, ylim=c(0, max(dati[[9]][,3], na.rm=T)))
lines(dati[[9]][,3])
points(miss, y.test, pch=19, col=2)
dati[[9]][miss,3] <- y.test
dati[[9]][miss,2] <- dati[[9]][miss,2]-y.test 


# replace other zeros with NAs
for(i in 1:n){
  zer <- which(dati[[i]]==0)
  dati[[i]][zer] <- NA
  print(zer)
}
# see NAs
for(i in 1:n){
  zer <- which(is.na(dati[[i]]))
  print(zer)
}


# Smoothing ---------------------------------------------------------------

dat.smooth = lapply(dati, function(x) apply(x,2, function(y) reg.smooth(diag(TT),matrix(y,TT,1),type="gen.ridge",r=10)$beta))

# plot of smoothed data with missingness
X11()
par(mfrow=c(4,6), mar=c(2,2,2,2))
for(i in 1:n){
  matplot(start.year:end.year, dat.smooth[[i]], type="l", main= stati[i], ylab="", lty=1, col=hcl.colors(c), lwd=2, xlab="")
}

# Imputation --------------------------------------------------------------

# We repeat the process we applied to Greece for all countries with missing
# data. First for Poland (we have 2 consecutive missing years) and then 
# Iceland (too wiggly due to zero-inflation).
imput = dat.smooth
for(i in 1:n){
  for(j in 1:c){
    miss=which(is.na(imput[[i]][,j]))
    miss.nation=sapply(imput, function(x) anyNA(x[,j]))
    if(length(miss)>1){ 
      x = t(sapply(imput,function(x) x[,j]))
      x.train = x[!miss.nation,-miss]
      y.train = x[!miss.nation,miss]
      x.test = x[i,-miss]
      y.test = x[i,miss]
      rec.ridge = ridge.recon.fit(x.train,y.train,info=F,plot.coefnorm=F,
                                  plot.selection=F,selection="gcv")
      y.test = rec.ridge$a%*%x.test
      plot(imput[[i]][,j], pch=19, ylim=range(imput[[i]][,j], na.rm=T),
           main = paste(stati[i],cause[j]))
      lines(imput[[i]][,j])
      points(miss, y.test, pch=19, col=2)
      imput[[i]][miss,j] <- y.test
    }
  }
}
dat.smooth = imput

# check for NAs
for(i in 1:n){
  zer <- which(is.na(dat.smooth[[i]]))
  print(zer)
}
# There are still some isolated NAs that cannot directly be attacked by Kraus
# (2015). We artificially induce missing values before and after the missing years.
for(i in 1:n){
  zer <- which(is.na(dat.smooth[[i]]))
  if(length(zer)>0){
    for(j in 1:length(zer)){
      dat.smooth[[i]][zer[j]-1] = NA
      dat.smooth[[i]][zer[j]+1] = NA
    }
  }
}
dat.smooth[[22]] <- matrix(dat.smooth[[22]][-457], 57, 8)
for(i in 1:n){
  zer <- which(is.na(dat.smooth[[i]]))
  print(zer)
}

# then we applied again the method.
imput = dat.smooth
for(i in 1:n){
  for(j in 1:c){
    miss=which(is.na(imput[[i]][,j]))
    miss.nation=sapply(imput, function(x) anyNA(x[,j]))
    if(length(miss)>1){ 
      x = t(sapply(imput,function(x) x[,j]))
      x.train = x[!miss.nation,-miss]
      y.train = x[!miss.nation,miss]
      x.test = x[i,-miss]
      y.test = x[i,miss]
      rec.ridge = ridge.recon.fit(x.train,y.train,info=F,plot.coefnorm=F,
                                  plot.selection=F,selection="gcv")
      y.test = rec.ridge$a%*%x.test
      plot(imput[[i]][,j], pch=19, ylim=range(imput[[i]][,j], na.rm=T),
           main = paste(stati[i],cause[j]))
      lines(imput[[i]][,j])
      points(miss, y.test, pch=19, col=2)
      imput[[i]][miss,j] <- y.test
    }
  }
}
dat.smooth = imput


# data transformation -----------------------------------------------------

# We move from absolute to relative deaths 
dat.smooth = lapply(dat.smooth, function(x) t(apply(x,1, function(y) y/sum(y))))

# Additional smoothing for Iceland (still too wiggly)
dat.smooth[[11]] =  apply(dat.smooth[[11]],2, function(y) reg.smooth(diag(TT),matrix(y,TT,1),
                                                                     type="gen.ridge",r=10)$beta)

# plot of smoothed data without missingness
X11()
par(mfrow=c(4,6), mar=c(2,2,2,2))
for(i in 1:n){
  matplot(start.year:end.year, dat.smooth[[i]], type="l", main= stati[i], ylab="", lty=1, 
          col=hcl.colors(c), lwd=2, xlab="")
}

# We consider also the clr transformed data
dat.smooth.log = list()
for(i in 1:n){
  dat.smooth.log [[i]] <- t(apply(t(apply(t(dat.smooth[[i]]),1,log)),2, function(x) x-mean(x)))
}

# Finally we convert lists into arrays
datarray.smooth = array(c(TT,c,n), data=unlist(dat.smooth, recursive=F))
datarray.smooth.log = array(c(TT,c,n), data=unlist(dat.smooth.log, recursive=F))

# Mean --------------------------------------------------------------------

# mean is computed on clr functions and then back-transfomed, as explained in
# the paper, sec. 3.2
m=matrix(0, TT,c)
for(i in 1:TT){
  m[i,] = apply(t(sapply(dat.smooth.log, function(x) x[i,])),2,mean, na.rm=T)
}
m.simplex <- t(apply(t(apply(t(m),1,exp)),2, function(x) x/sum(x)))

# Covariance --------------------------------------------------------------

# covariance is computed on clr functions, as explained in the paper, sec. 3.2
C = matrix(NA,c*TT,c*TT)
for(i in 1:c){
  for(j in 1:c){
    C[(TT*(i-1)+1):(TT*i),(TT*(j-1)+1):(TT*j)] <- cov(t(datarray.smooth.log [,i,]),
                                                      t(datarray.smooth.log [,j,]))
  }
}

# PCA ---------------------------------------------------------------------

# fPCA is based on eigendecomposition of the covariance operator
spec = eigen(C)
expl = spec$values[1:4]/sum(spec$values)  # percentage of explained variance of each component

# We then extract the first 4 components...
pc1=matrix(NA,TT,c)
for(i in 1:c){
  pc1[,i] = spec$vectors[(TT*(i-1)+1):(TT*i),1]
}
pc2=matrix(NA,TT,c)
for(i in 1:c){
  pc2[,i] = spec$vectors[(TT*(i-1)+1):(TT*i),2]
}
pc3=matrix(NA,TT,c)
for(i in 1:c){
  pc3[,i] = spec$vectors[(TT*(i-1)+1):(TT*i),3]
}
pc4=matrix(NA,TT,c)
for(i in 1:c){
  pc4[,i] = spec$vectors[(TT*(i-1)+1):(TT*i),4]
}

# ...and we back-transform them to the functional simplex, 
# see sec. 3.2 of the paper
pc1.simplex <- t(apply(t(apply(pc1,1,exp)),1, function(x) x/sum(x)))
pc2.simplex<- t(apply(t(apply(pc2,1,exp)),1, function(x) x/sum(x)))
pc3.simplex<- t(apply(t(apply(pc3,1,exp)),1, function(x) x/sum(x)))
pc4.simplex<- t(apply(t(apply(pc4,1,exp)),1, function(x) x/sum(x)))

# For graphical purposes, we need the following quantities
big_pc1.simplex <- t(apply(t(apply(sqrt(spec$values[1])*pc1,1,exp)),1, function(x) x/sum(x)))
big_pc2.simplex <- t(apply(t(apply(sqrt(spec$values[2])*pc2,1,exp)),1, function(x) x/sum(x)))
big_pc3.simplex <- t(apply(t(apply(sqrt(spec$values[3])*pc3,1,exp)),1, function(x) x/sum(x)))
big_pc4.simplex <- t(apply(t(apply(sqrt(spec$values[4])*pc4,1,exp)),1, function(x) x/sum(x)))

# Mean +/- each component
m_pc1_plus.simplex = t(apply(m.simplex*pc1.simplex, 1, function(x) x/sum(x)))
m_pc1_minus.simplex = t(apply(m.simplex/pc1.simplex, 1, function(x) x/sum(x)))
m_big_pc1_plus.simplex = t(apply(m.simplex*big_pc1.simplex, 1, function(x) x/sum(x)))
m_big_pc1_minus.simplex = t(apply(m.simplex/big_pc1.simplex, 1, function(x) x/sum(x)))
m_pc2_plus.simplex = t(apply(m.simplex*pc2.simplex, 1, function(x) x/sum(x)))
m_pc2_minus.simplex = t(apply(m.simplex/pc2.simplex, 1, function(x) x/sum(x)))
m_big_pc2_plus.simplex = t(apply(m.simplex*big_pc2.simplex, 1, function(x) x/sum(x)))
m_big_pc2_minus.simplex = t(apply(m.simplex/big_pc2.simplex, 1, function(x) x/sum(x)))
m_pc3_plus.simplex = t(apply(m.simplex*pc3.simplex, 1, function(x) x/sum(x)))
m_pc3_minus.simplex = t(apply(m.simplex/pc3.simplex, 1, function(x) x/sum(x)))
m_big_pc3_plus.simplex = t(apply(m.simplex*big_pc3.simplex, 1, function(x) x/sum(x)))
m_big_pc3_minus.simplex = t(apply(m.simplex/big_pc3.simplex, 1, function(x) x/sum(x)))
m_pc4_plus.simplex = t(apply(m.simplex*pc4.simplex, 1, function(x) x/sum(x)))
m_pc4_minus.simplex = t(apply(m.simplex/pc4.simplex, 1, function(x) x/sum(x)))
m_big_pc4_plus.simplex = t(apply(m.simplex*big_pc4.simplex, 1, function(x) x/sum(x)))
m_big_pc4_minus.simplex = t(apply(m.simplex/big_pc4.simplex, 1, function(x) x/sum(x)))

# Scores ------------------------------------------------------------------

# We compute scores as inner product in the simplex between each observed 
# function and each component
sc.pc1 = sapply(dat.smooth, function(x) inner.simplex(x/m.simplex,pc1.simplex))
sc.pc2 = sapply(dat.smooth, function(x) inner.simplex(x/m.simplex,pc2.simplex))
sc.pc3 = sapply(dat.smooth, function(x) inner.simplex(x/m.simplex,pc3.simplex))
sc.pc4 = sapply(dat.smooth, function(x) inner.simplex(x/m.simplex,pc4.simplex))

# this is equivalent to the inner product in the transformed space
sc.pc1.bis = sapply(dat.smooth.log, function(x) sum(diag((t(x-m))%*%pc1)))      

# projected data
data.proj = cbind(sc.pc1, sc.pc2, sc.pc3, sc.pc4)
row.names(data.proj) <-  stati    # projected data

# Clustering --------------------------------------------------------------

# We compute the spectral cluster algorithm for different values of G, as 
# discussed in the paper in sec. 3.3
cl.spec <- array(NA, c(9,n,1000))
clus <- matrix(NA,9,n)
for(l in 2:10){
  set.seed(1)
  spec.centr <- array(NA, c(l,dim(data.proj)[2],1000))
  for(i in 1:1000){
    cc4 <- specc(data.proj, l)
    cl.spec[l-1,,i] <- match(cc4@.Data, order(cc4@withinss))
    spec.centr[,,i] <- cc4@centers
  }
  # We retain the most common partition using majority vote
  clus[l-1,] <- apply(cl.spec[l-1,,],1,function(x) which.max(tabulate(x))) 
  print(l)
}

# Silhouette index
si =c()
for(l in 2:10){
  si[l-1] = mean(silhouette(clus[l-1,], dist(data.proj))[,3])
}
si      # we consider 5 groups

# Clustering
set.seed(1)
cl.spec <- matrix(NA,n,1000)
spec.centr <- array(NA, c(5,dim(data.proj)[2],1000))
for(i in 1:1000){
  cc4 <- specc(data.proj, 5)
  cl.spec[,i] <- match(cc4@.Data, order(cc4@withinss))
  spec.centr[,,i] <- cc4@centers
}
cl <-  apply(cl.spec,1,function(x) which.max(tabulate(x))) 

# Centroids
mat <- c()
for(i in 1:1000) mat[i]=sum(cl.spec[,i]==cl) 
cntrs <- spec.centr[,,which(mat==22)]

# Functional centroids
g1 = m+cntrs[1,1,1]*(pc1)+cntrs[1,2,1]*(pc2)+cntrs[1,3,1]*(pc3)+cntrs[1,4,1]*(pc4)
g1 <- t(apply(t(apply(t(g1),1,exp)),2, function(x) x/sum(x)))
g3 = m+cntrs[2,1,1]*(pc1)+cntrs[2,2,1]*(pc2)+cntrs[2,3,1]*(pc3)+cntrs[2,4,1]*(pc4)
g3 <- t(apply(t(apply(t(g3),1,exp)),2, function(x) x/sum(x)))
g4 = m+cntrs[3,1,1]*(pc1)+cntrs[3,2,1]*(pc2)+cntrs[3,3,1]*(pc3)+cntrs[3,4,1]*(pc4)
g4 <- t(apply(t(apply(t(g4),1,exp)),2, function(x) x/sum(x)))
g2 = m+cntrs[4,1,1]*(pc1)+cntrs[4,2,1]*(pc2)+cntrs[4,3,1]*(pc3)+cntrs[4,4,1]*(pc4)
g2 <- t(apply(t(apply(t(g2),1,exp)),2, function(x) x/sum(x)))
g5 = m+cntrs[5,1,1]*(pc1)+cntrs[5,2,1]*(pc2)+cntrs[5,3,1]*(pc3)+cntrs[5,4,1]*(pc4)
g5 <- t(apply(t(apply(t(g5),1,exp)),2, function(x) x/sum(x)))


save.image(file="results_men.RData")
