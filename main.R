# Clean variables
rm(list<-ls())

### 1. Apply KDE

## Import database, declare variables


## Separe training and test samples


## Find best h to test samples (avoiding overfitting)


### 2. Take "trespassers" out

## Iterate over all samples, remove misclassified ones


### 3. Iterate over border, marking selected ones



### 4. Select only marked ones to new KDE

## Iterate over samples and copy marked ones (???)


### 5. Run new KDE with only selected samples as training


