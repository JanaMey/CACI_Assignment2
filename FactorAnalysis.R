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

#rename variables
names(data.eval)[9] <- "vibrant nightlife"
names(data.eval)[10] <- "delicious food"
names(data.eval)[11] <- "easy to get around"
names(data.eval)[12] <- "good shopping"
names(data.eval)[13] <- "cultural events"
names(data.eval)[14] <- "interesting museums"
names(data.eval)[18] <- "too touristic"
names(data.eval)[24] <- "english-speaker-friendly"


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

# Correlation among Cities
# Aggregate the data
City.mean <- aggregate(.~City, data = data.sc[, -c(1:3)], mean) 
rownames(City.mean) <- City.mean[, 1] # use brand for the row names
City.mean <- City.mean[, -1] # remove brand name column

# Heatmap
heatmap.2(as.matrix(City.mean), # input should be a matrix
          col = brewer.pal(9, "GnBu"), 
          # turn off some default options not to clutter the plot
          # try commenting them step by step to see what happens otherwise
          trace = "none", 
          key = FALSE, 
          dend = "none")

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
                  nfactors = 3,         # here testing with different factors --> 4 bc eigenvalues > 1
                  rotate = "none",      # rotation in next step
                  scores ='regression') 

ml.unrotated
#results - cumulativeVar 47% - 

#orthagonal rotation - rotate values from above - insert values from Assignment 2 (target: correlation more extreme)
ml.rotated = fa(data.sc[, -c(1,2,3,4,25)], 
                fm = "ml",            
                nfactors = 3,         
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


#perceptual mapping
scores.rotated <- data.frame(ml.rotated$scores)
scores.rotated$City <- data.sc$City
head(scores.rotated)
mean_fa <- aggregate(.~City, data = scores.rotated, mean)

ggplot(data = mean_fa, aes(x = ML1, y = ML2)) +
  geom_point() + 
  geom_vline(xintercept = 0, color = "grey50") +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_text(aes(label = City, hjust = 0.5, vjust = 1.3)) +
  labs(x = "Touristic Attraction", y = "Friendly Ambience") +
  theme_classic()

ggplot(data = mean_fa, aes(x = ML1, y = ML3)) +
  geom_point() + 
  geom_vline(xintercept = 0, color = "grey50") +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_text(aes(label = City, hjust = 0.5, vjust = 1.3)) +
  labs(x = "Touristic Attraction", y = "Cultural Experience") +
  theme_classic()

#ggplot(data = mean_fa, aes(x = ML4, y = ML3)) +
  geom_point() + 
  geom_vline(xintercept = 0, color = "grey50") +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_text(aes(label = City, hjust = 0.5, vjust = 1.3)) +
  labs(x = "Food and Price", y = "Cultural Experience") +
  theme_classic()

#Problem: We have two dimensions but three factors!!!

#####################################################################################################
#Comparison with MDS
# compute mean distances
City.dist = dist.i = NULL # initialize (creates an empty list)
for(i in unique(data.sc$id_unique)){
  
  # To check what is done for each iteration, uncomment the line below
  # and run step-by-step
  # i = unique(attrEval$id_new)[1] # fix the i index
  
  # subset the data for each respondent i
  data.sc.i = data.sc[data.sc$id_unique == i, -c(1, 2)]
  data.sc.i = data.sc.i[order(data.sc.i$City), ] # sort by station
  rownames(data.sc.i) <- data.sc.i$City
  
  dist.i[[i]] <- dist(data.sc.i[,-1], method = "euclidean")
  City.dist[[i]] <- as.matrix(dist.i[[i]])
  dist.i[[i]] <- as.dist(dist.i[[i]])
  
  # save as a data frame
  City.dist[[i]] <- data.frame(City.dist[[i]])
  
  # add individual counter (id)
  City.dist[[i]]$id_unique <- i
  City.dist[[i]]$City <- rownames(data.sc.i)
}

# combine the list into one data frame
City.dist <- do.call(rbind, City.dist)
dim(City.dist)
head(City.dist)

...



