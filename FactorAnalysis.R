
# AUFGABE 3 #
# Install and load the required libraries ----------------------------------------
pacman::p_load(reshape2, ggplot2, corrplot, psych,
               gplots, RColorBrewer, EFAtools, 
               lavaan, semPlot, semTools)

urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment2/main/indivData.csv'
indivData <-read.csv(urlfile)
urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment2/main/data.eval.csv'
data.eval <-read.csv(urlfile)

head(data.eval)
summary(data.eval)
dim(data.eval) #1401 25

# Aufgabe 3 - Factor Analysis

# Rescaling data - Standardize data with mean = 0 and standard deviation = 1
#data.sc <- data.eval
#data.sc[, -10] <- scale(data.sc[, -10])
#head(data.sc)

summary(data.sc)

# Investigate the Correlation Matrix - wieso Fehlermeldung mit 'x' muss numerisch sein? HIER FUNKTIONIERT ES BEI MIR NICHT, BITTE 1x testen
corrplot(cor(data.sc[, -10]),
         method = "number", 
         type = "upper",
         order = "hclust")

# Alternative with Circles instead of numbers
corrplot(cor(data.sc[, -10]), type = "upper", order = "hclust")

# Test KMO Criterion - check if higher than 0.5? --> if yes than data suitable for factor analysis
KMO(cor(data.sc[, -10]))

# Screeplot: Eigenvalues vs. number of factors
plot(eigen(cor(data.sc[, -10]))$values, 
     type = "o",                 
     xlab = "Number of factors",   
     ylab = "Eigenvalues",
     pch = 16)                        
abline(h = 1, col = "grey")

# Example Maximum Likelihood method for factor extraction
ml.unrotated = fa(data.sc[, -10], 
                  fm = "ml",            
                  nfactors = 2,         # here testing with different factors
                  rotate = "none",      # rotation in next step
                  scores ='regression') 

ml.unrotated

#orthagonal rotation - rotate values from above - insert values from Assignment 2 (target: correlation more extreme)
ml.rotated = fa(data.sc[, -10], 
                fm = "ml",            
                nfactors = 2,         
                rotate = "varimax",   
                scores ='regression') 

ml.rotated

# heatmaps for loading visualization
heatmap.2(ml.rotated$loadings,
          col = brewer.pal(9, "Greens"), 
          trace="none", key = FALSE , dend = "none",
          Colv = FALSE , cexCol = 1.2)
