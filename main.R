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
Ntst<-Ntst1+Ntst2

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


### 2. Take "trespassers" out

## Iterate over all samples, remove misclassified ones


### 3. Iterate over border, marking selected ones



### 4. Select only marked ones to new KDE

## Iterate over samples and copy marked ones (???)


### 5. Run new KDE with only selected samples as training


