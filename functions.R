
reg.smooth <- function(x,y,type="gen.ridge",r){   # performs smoothing via SVD 
  not.na = !is.na(y)                  
  x=x[not.na,not.na]
  y=y[not.na]
  omega=matrix(0,ncol=length(y), nrow=length(y))
  for(i in 1:(length(y)-1)){
    omega[i,i] <- 1
    omega[i,i+1] <- -1              
  }
  omega=t(omega)%*%omega
  sing = svd(x)
  inner = t(sing$u)%*%y
  filter = pen(x,y,r,type,omega=omega)
  A = sing$v%*%filter%*%diag(1/sing$d)%*%t(sing$u)
  beta.est = rep(NA, length(y))
  beta.est[not.na] = matrix(A%*%y, nrow=1)
  S=x%*%A
  df = sum(S^2)
  df.bis=sum(diag(filter%*%t(filter)))
  out=list(beta.est,df,df.bis,S)
  names(out) = c("beta","df","df.bis","smoothing.matrix")
  return(out)
}

pen <- function(x,y,r,type,omega=NA){   
  n = nrow(x)
  p=ncol(x)
  if(type=="ols"){
    out=diag(min(n,p))                                                 # ols
  }
  if(type=="pcr"){ 
    out = diag(min(n,p))
    out[floor(r):min(n,p),floor(r):min(n,p)] <- 0                      # pcr
  }     
  if(type=="ridge"){
    out = solve(diag(svd(x)$d^2)+r*diag(min(n,p)))%*%diag(svd(x)$d^2)  # ridge
  }
  if(type=="gen.ridge"){
    w=t(svd(x)$v)%*%omega%*%svd(x)$v
    out = solve(diag(svd(x)$d^2)+r*w)%*%diag(svd(x)$d^2)               # generalized ridge
  }  
  return(out)
}

ridge.recon.fit = function(XO,XM,ROO,RMO,RMM,muO,muM,n,nalpha=100,alpha,selection="gcv",
                           rss.complete=TRUE,
                           info.path=0,plot.coefnorm=FALSE,plot.coefnorm.xvar="alpha",
                           plot.selection=FALSE)
{
  if (xor(missing(XO),missing(XO))) stop("provide both XO and XM or none of them")
  if (!missing(XO)) {
    n = NROW(XO)
    R = make.var.pos.semidef(var(cbind(XO,XM),use="pairwise.complete.obs")*(n-1)/n)
    dimO = NCOL(XO)
    dimM = NCOL(XM)
    ROO = R[1:dimO,1:dimO]
    RMO = R[(dimO+1):(dimO+dimM),1:dimO]
    RMM = R[(dimO+1):(dimO+dimM),(dimO+1):(dimO+dimM)]
    muO = colMeans(XO,na.rm=TRUE)
    muM = colMeans(XM,na.rm=TRUE)
  } else {
    if (missing(n)) stop("provide n")
    dimO = NCOL(RMO)
    dimM = NROW(RMO)
  }
  ROO.eigen = eigen(ROO,symmetric=TRUE)
  p = sum(ROO.eigen$values>.Machine$double.eps^.5)
  ROO.eigenval = ROO.eigen$values[1:p]
  ROO.eigenfun = ROO.eigen$vectors[,1:p,drop=FALSE]
  RMO.times.ROO.eigenfun = RMO%*%ROO.eigenfun
  tr.RMM = sum(diag(RMM))
  if (missing(alpha)) {
    # alpha = exp(seq(log(df.to.alpha(1,ROO.eigenval)),log(df.to.alpha(min(n/2,p)-1,ROO.eigenval)),len=nalpha))
    #alpha.max = df.to.alpha(1,ROO.eigenval)
    df.min = 1
    df.max = min(floor(n),p)-1
    # range for alpha grid from inequalities in https://stats.stackexchange.com/questions/32246/implementing-ridge-regression-selecting-an-intelligent-grid-for-alpha
    alpha.max = mean(ROO.eigenval)*(p-df.min)/df.min # upper bound for alpha corresponding to df.min
    alpha.min.1 = mean(ROO.eigenval^(-1))^(-1)*(p-df.max)/df.max # lower bound for alpha corresponding to df.max
    alpha.min.2 = df.to.alpha(df.max,ROO.eigenval)
    alpha.min = max(alpha.min.1,alpha.min.2)
    alpha = exp(seq(log(alpha.max),log(alpha.min),len=nalpha))
  } else {
    nalpha = length(alpha)
  }
  if (rss.complete) { # quantities for computing rss from complete curves
    complete = complete.cases(XO)&complete.cases(XM)
    if (sum(complete)==0) stop("no complete cases")
    if (sum(complete)<10) warning("less than 10 complete cases, selection might be unreliable")
    tr.RMM.complete = sum(apply(XM[complete,],2,var))
    RMO.complete = cov(XM[complete,],XO[complete,])
    ROO.complete = var(XO[complete,])
  }
  path = list()
  path.summary = matrix(0,nalpha,7)
  path.coefnorm = matrix(0,nalpha,dimO)
  colnames(path.summary) = c("alpha","df","rss","pctrss","gcv","aic","cp")
  rss0 = .5*tr.RMM
  for (i in 1:nalpha) {
    fit1 = ridge.recon.fit1(ROO.eigenval,ROO.eigenfun,RMO.times.ROO.eigenfun,tr.RMM,alpha=alpha[i])
    if (rss.complete) { # compute rss from complete curves
      fit1$rss = 0.5*(tr.RMM.complete - 2*sum(fit1$a*RMO.complete) + sum(crossprod(fit1$a)*ROO.complete))
    }
    fit1$pctrss = 1-fit1$rss/rss0
    fit1$gcv = fit1$rss/((1-fit1$df/n)^2)
    fit1$aic = n*log(fit1$rss)+2*fit1$df
    path[[i]] = fit1
    path.summary[i,-7] = c(alpha[i],fit1$df,fit1$rss,fit1$pctrss,fit1$gcv,fit1$aic)
    path.coefnorm[i,] = sqrt(colSums(fit1$a^2))
  }
  path.summary[,"cp"] = n*path.summary[,"rss"]/path.summary[nalpha,"rss"] + 2*path.summary[,"df"]
  index.selected = which.min(path.summary[,selection])
  if (info.path>0) {
    print(path.summary)
  }
  if (plot.selection) {
    plot(path.summary[,c("alpha",selection)],type="o",log="x")
    abline(v=alpha[index.selected],col=2)
  }
  if (plot.coefnorm) {
    switch(plot.coefnorm.xvar,
           alpha = {matplot(alpha,path.coefnorm,xlab="alpha",ylab="coefnorm",log="x",type="l",lty=1)
             abline(v=alpha[index.selected],col=2)},
           totalnorm  = {matplot(sqrt(rowSums(path.coefnorm)^2),path.coefnorm,xlab="totalcoefnorm",ylab="coefnorm",type="l",lty=1)
             abline(v=sum(path.coefnorm[index.selected,]),col=2)},
           pctrss = {matplot(path.summary[,"pctrss"],path.coefnorm,xlab="pctrss",ylab="coefnorm",type="l",lty=1)
             abline(v=path.summary[index.selected,"pctrss"],col=2)}
    )
  }
  c(path[[index.selected]],list(muO=muO,muM=muM,selection=selection,index.selected=index.selected,
                                path.summary=path.summary,path.coefnorm=path.coefnorm,path=path))
}

ridge.recon.fit1 = function(ROO.eigenval,ROO.eigenfun,RMO.times.ROO.eigenfun,tr.RMM,alpha)
{
  # a = RMO.times.ROO.eigenfun%*%(t(ROO.eigenfun)/(ROO.eigenval+alpha))
  # rss = 0.5*(tr.RMM - 2*sum(a*RMO) + sum(crossprod(a)*ROO))
  a = RMO.times.ROO.eigenfun/rep(ROO.eigenval+alpha,each=NROW(RMO.times.ROO.eigenfun))
  rss = 0.5*(tr.RMM - 2*sum(a*RMO.times.ROO.eigenfun) + sum(colSums(a^2)*ROO.eigenval))
  a = tcrossprod(a,ROO.eigenfun)
  list(a=a,rss=rss,df=sum(ROO.eigenval/(ROO.eigenval+alpha)),alpha=alpha)
}

make.var.pos.semidef = function(R)
{
  eig.R = eigen(R,symmetric=TRUE)
  w = which(eig.R$values>.Machine$double.eps^.5)
  R = eig.R$vectors[,w]%*%(eig.R$values[w]*t(eig.R$vectors[,w]))
  R
}

df.alpha.eq = function(alpha,deg.fr,eigenval)
{
  sum(eigenval/(eigenval+alpha)) - deg.fr
}

df.to.alpha = function(deg.fr,eigenval)
{
  p = length(eigenval)
  uniroot(df.alpha.eq,c(mean(eigenval^(-1))^(-1)*(p-deg.fr)/deg.fr,mean(eigenval)*(p-deg.fr)/deg.fr),deg.fr=deg.fr,eigenval=eigenval)$root
}

inner.simplex <- function(x,y){
  inner = c()
  for(i in 1:dim(x)[1]){
    inner[i] <- acomp(x[i,])%*%acomp(y[i,]) 
  }
  return(sum(inner))
}
