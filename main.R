# Clean variables
rm(list=ls())

### 1. Apply KDE

## Import database, declare variables
library('mlbench')
N<-400
Ntst<-50 #per class
p<-mlbench.spirals(N,1,0.08)

# index c1 and c2
ic1<-which(p[[2]]==1)
ic2<-which(p[[2]]==2)

# x = attributes from samples
xall<-as.matrix(p[[1]])
xc1<-xall[ic1,]
xc2<-xall[ic2,]

N1<-dim(xc1)[1]
N2<-dim(xc2)[1]

y1<-matrix(-1,nrow = N1,ncol=1)
y2<-matrix(1,nrow = N2,ncol=1)
y<-rbind(y1,y2)

xseq1<-sample(N1)
xseq2<-sample(N2)

## Separe training and test samples

# Test Samples
xtst1<-xc1[xseq1[1:Ntst],]
xtst2<-xc2[xseq2[1:Ntst],]

ytst1<-as.matrix(y1[xseq1[1:Ntst]])
ytst2<-as.matrix(y2[xseq2[1:Ntst]])

Ntst1<-nrow(xtst1)
Ntst2<-nrow(xtst2)


# Training Samples
xt1<-xc1[xseq1[(Ntst+1):N1],]
xt2<-xc2[xseq2[(Ntst+1):N2],]

yt1<-as.matrix(y1[xseq1[(Ntst+1):N1]])
yt2<-as.matrix(y2[xseq2[(Ntst+1):N2]])

xt<-rbind(xt1,xt2)
yt<-rbind(yt1,yt2)

Nt1<-nrow(xt1)
Nt2<-nrow(xt2)
Nt<-Nt1+Nt2

# The whole set: training and test together
xtall<-rbind(xtst1,xtst2,xt1,xt2)
ytall<-rbind(ytst1,ytst2,yt1,yt2)

## Plots
myXlim<-myYlim<-c(-1,1)
plot(xt1,col='blue',xlim = myXlim, ylim = myYlim, xlab = '', ylab = '')
par(new=T)
plot(xtst1,col='green',xlim = myXlim, ylim = myYlim, xlab = '', ylab = '')
par(new=T)
plot(xt2,col='red',xlim = myXlim, ylim = myYlim, xlab = '', ylab = '')
par(new=T)
plot(xtst2,col='orange',xlim = myXlim, ylim = myYlim, xlab = '', ylab = '')

## Find best h to test samples (avoiding overfitting)

# Create full distance matrix
distAll <- as.matrix(dist(xtall))

# Extract only test vs training
distExt <- distAll[1:(2*Ntst),(2*Ntst+1):N]

# Same for labels:
labelAll <- ytall %*% t(ytall)
labelExt <- labelAll[1:(2*Ntst),(2*Ntst+1):N]

# Loop Setup
hseq<-seq(0.01,1,0.01)
perc_err_TST<-matrix(nrow = length(hseq))

# Loop for getting best h
for (i in 1:length(hseq))
{
  h<-hseq[i]
  K<-exp(-0.5*(distExt/h)^2)
  
  # if yerr[x]<0, sample was misclassified
  yerr<-colSums(t(labelExt*K))
  perc_err_TST[i]<-sum(1*(yerr<0))/(2*Ntst)
}

hBest<-hseq[which.min(perc_err_TST)]

# Percentual test error plot
myYlim<-c(0,0.5)
myXlim<-c(0,1)
plot(hseq,perc_err_TST, type='l', col = 'black',xlim = myXlim, ylim = myYlim, xlab = '', ylab = '')

### 2. Take "trespassers" out

## Iterate over all samples, remove misclassified ones

distFullTrain <- distAll[,(2*Ntst+1):N]
labelFullTrain <- labelAll[,(2*Ntst+1):N]

h<-hBest
K<-exp(-0.5*(distFullTrain/h)^2)

# (if yerr[x]<0, sample was misclassified)
yerr<-colSums(t(labelFullTrain*K))
badIndexes<-which(yerr<0)

### 3. Iterate over border, marking selected ones

## Define KDE function

myKDEprob = function(p, X, h){
  n<-length(X[,1])
  allData<-rbind(p,X)
  allDist<-as.matrix(dist(allData))
  pDist<-allDist[(2:(n+1)),1]
  K<-exp(-0.5*(pDist/h)^2)
  result<-sum(K)
  # result<-(1/(2*pi)^(1/2))*(result/n*h)
  return(result)
}

## Map area and calculate probabilities in KDE

xrange<-yrange<-seq(-1,1,0.01)
kdeMapC1<-matrix(nrow = length(xrange), ncol = length(yrange))
kdeMapC2<-matrix(nrow = length(xrange), ncol = length(yrange))

for (i in 1:length(xrange)){
  for (j in 1:length(yrange)){
    p<-c(xrange[i],yrange[j])
    kdeMapC1[i,j]<-myKDEprob(p,xt1,hBest)
    kdeMapC2[i,j]<-myKDEprob(p,xt2,hBest)
  }
}

# C1 marked by -1, C2 marked by 1
kdeTotal<-kdeMapC2-kdeMapC1
classMap<-2*(kdeTotal>0)-1

## Plots
myXlim<-myYlim<-c(-1,1)
# contour(xrange, yrange, classMap, xlim = myXlim, ylim = myYlim)
# par(new=T)
plot(xt1,col='blue',xlim = myXlim, ylim = myYlim, xlab = '', ylab = '')
par(new=T)
plot(xtst1,col='green',xlim = myXlim, ylim = myYlim, xlab = '', ylab = '')
par(new=T)
plot(xt2,col='red',xlim = myXlim, ylim = myYlim, xlab = '', ylab = '')
par(new=T)
plot(xtst2,col='orange',xlim = myXlim, ylim = myYlim, xlab = '', ylab = '')

# getting and plotting the contour points
contourPoints<-contourLines(xrange, yrange, classMap)[[1]]
par(new=T)
plot(contourPoints[[1]],col='black',xlim = myXlim, ylim = myYlim, xlab = '', ylab = '')

### 4. Select only marked ones to new KDE

## Iterate over samples and copy marked ones (???)


### 5. Run new KDE with only selected samples as training


