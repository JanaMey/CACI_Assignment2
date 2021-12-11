# # AUFGABE 3 #
# Install and load the required libraries ----------------------------------------
pacman::p_load(reshape2, ggplot2, corrplot, psych,
               gplots, RColorBrewer, EFAtools, 
               lavaan, semPlot, semTools)

# load data from git--------------------------------------------------------------
urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment2/main/indivData.csv'
indivData <-read.csv(urlfile)
urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment2/main/dataEvalScale.csv'
data.eval <-read.csv(urlfile)

head(data.eval)
names(data.eval)
summary(data.eval)
dim(data.eval) #1401 25

# Aufgabe 3 - Factor Analysis-----------------------------------------------------

#copy data
data.sc <- data.eval

#Corrplot with Circles
corrplot(cor(data.sc[, -c(1,2,3,4,25)]),
         #method = "number",
         #insig = 'blank',
         type = "lower",
         tl.cex = 0.75,
         tl.col = "black")
#addCoef.col ='grey28')
#number.digits = 1,
#number.cex = 0.65)

#Corrplot without Circles
corrplot(cor(data.sc[, -c(1,2,3,4,25)]),
         method = "number",
         #insig = 'blank',
         type = "lower",
         tl.cex = 0.75,
         tl.col = "black",
         addCoef.col ='grey28',
         number.digits = 1,
         number.cex = 0.65)

# Test KMO Criterion - check if higher than 0.5? --> if yes than data suitable for factor analysis
KMO(cor(data.sc[, -c(1,2,3,4,25)])) #0.874 suitable

# Screeplot: Eigenvalues vs. number of factors --> 4 values higher than 1 -> use 4 factors
plot(eigen(cor(data.sc[, -c(1,2,3,4,25)]))$values, 
     type = "o",                 
     xlab = "Number of factors",   
     ylab = "Eigenvalues",
     pch = 16)                        
abline(h = 1, col = "grey")


# Example Maximum Likelihood method for factor extraction
ml.unrotated = fa(data.sc[, -c(1,2,3,4,25)], 
                  fm = "ml",            
                  nfactors = 4,         # here testing with different factors --> 4 bc eigenvalues > 1
                  rotate = "none",      # rotation in next step
                  scores ='regression') 

ml.unrotated
#results - cumulativeVar 47% - 

#orthagonal rotation - rotate values from above - insert values from Assignment 2 (target: correlation more extreme)
ml.rotated = fa(data.sc[, -c(1,2,3,4,25)], 
                fm = "ml",            
                nfactors = 4,         
                rotate = "varimax",   
                scores ='regression') 

ml.rotated

#orthagonal rotation - allow for correlation between factors
ml2.rotated = fa(data.sc[, -c(1,2,3,4,25)], 
                fm = "ml",            
                nfactors = 4,         
                rotate = "oblimin",   
                scores ='regression') 

ml2.rotated 


# heatmaps for loading visualization
#Checken ob rotated bessere Lösung ist!!!
png("Heatmap for Loading visualization.png", width=300, height = 300)
heatmap.2(ml.rotated$loadings,
          col = brewer.pal(9, "Greens"), 
          trace="none", key = FALSE , dend = "none",
          Colv = FALSE , cexCol = 1.2)
dev.off()

# Visual representation of factor loadings
fa.diagram(ml.rotated, main = "Rotated Factor Loadings")
fa.diagram(ml2.rotated, main = "Rotated Factor Loadings with CORR")
fa.diagram(ml.unrotated, main = "Unrotated Factor Loading")



