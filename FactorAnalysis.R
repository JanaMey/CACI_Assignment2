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
         type = "upper",
         tl.cex = 1.2,
         tl.col = "black")
#addCoef.col ='grey28')
#number.digits = 1,
#number.cex = 0.65)

# #Corrplot without Circles
# corrplot(cor(data.sc[, -c(1,2,3,4,25)]),
#          method = "number",
#          #insig = 'blank',
#          type = "upper",
#          tl.cex = 1.2,
#          tl.col = "black",
#          addCoef.col ='grey28',
#          number.digits = 1,
#          number.cex = 0.9)

# Test KMO Criterion - check if higher than 0.5? --> if yes than data suitable for factor analysis
KMO(cor(data.sc[, -c(1,2,3,4,25)])) #0.874 suitable

# Correlation among Cities
# Aggregate the data
City.mean <- aggregate(.~City, data = data.sc[, -c(1:3)], mean) 
rownames(City.mean) <- City.mean[, 1] # use brand for the row names
City.mean <- City.mean[, -1] # remove brand name column

# # Heatmap
# heatmap.2(as.matrix(City.mean), # input should be a matrix
#           col = brewer.pal(9, "GnBu"), 
#           # turn off some default options not to clutter the plot
#           # try commenting them step by step to see what happens otherwise
#           trace = "none", 
#           key = FALSE, 
#           dend = "none")

# Screeplot: Eigenvalues vs. number of factors --> 4 values higher than 1 -> use 4 factors
plot(eigen(cor(data.sc[, -c(1,2,3,4,25)]))$values, 
     type = "o",                 
     xlab = "Number of factors", 
     ylab = "Eigenvalues",
     cex.lab=1.5,
     cex.axis=1.5,
     pch = 16)                        
abline(h = 1, col = "grey")

#cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5

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
# png("Heatmap for Loading visualization.png", width=300, height = 300)
# heatmap.2(ml.rotated$loadings,
#           col = brewer.pal(9, "Greens"), 
#           trace="none", key = FALSE , dend = "none",
#           Colv = FALSE , cexCol = 1.2)
# dev.off()

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
  geom_text(aes(label = City, hjust = 0.5, vjust = 1.3), size = 4.5) +
  labs(x = "Amusement trip", y = "Relaxing Trip") +
  xlim(-1.3, 1.3)+
  theme_classic(base_size = 17)
ggsave(file="FA_PercMap.png", width=8, height=8, dpi=600)  

ggplot(data = mean_fa, aes(x = ML1, y = ML3)) +
  geom_point() + 
  geom_vline(xintercept = 0, color = "grey50") +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_text(aes(label = City, hjust = 0.5, vjust = 1.3), size = 4.5) +
  labs(x = "Amusement trip", y = "Culture Trip") +
  xlim(-1.3, 1.3)+
  theme_classic(base_size = 17)
ggsave(file="FA_PercMap_Culture.png", width=8, height=8, dpi=600)  

#----------------------------------------Preference mapping--------------------------------------
#FA_selected => citys and 2 Factors Points in the map
fa.selected<- mean_fa[,-3] #nur City,ML1,ML2
fa.selected$type <- "point"
fa.selected
#Column für Segment Travel with hinzufügen
#subset of Single
single <- subset(indivData, indivData$PartnershipStatus== "single")
length(unique(single$id_unique)) # Anzahl Ids in Single 105
idListSingle <- unique(single$id_unique) # Liste mit den ids aus long.data

# subset of in a relationship.
couple <- subset(indivData, indivData$PartnershipStatus== "in a relationship.")
length(unique(couple$id_unique)) # Anzahl Ids in bachelor 134
idListCouple <- unique(couple$id_unique) # Liste mit den ids aus long.data

data.eval$PartnershipStatus <- ifelse(data.eval$id_unique %in% idListSingle, "Single",
                                      ifelse(data.eval$id_unique %in% idListCouple, "In Relationship", "Married"))

summary(couple$Avg_Budget)
summary(single$Avg_Budget)
summary(couple$Avg_Budget)

## Add Factors M1-M3 to data.eval as new columns
data.eval <- merge(data.eval, mean_fa, by = "City")
head(data.eval)

#For PLOT 2D: ML1,ML2
# Single #
profit.1 <- lm(Pref ~ -1 + ML1 + ML2,
               data = data.eval[data.eval$PartnershipStatus == "Single",])
param <- data.frame(t(coef(profit.1)))
param$City <- "Single"
# reorder the columns
param <- param[, c("City", "ML1", "ML2")]
param$type <- "vector_relationship"
param
# combine with fa.selected
fa.selected <- rbind(fa.selected, param)
rownames(mds.selected) <- NULL # overwrite the rownames

# # Couple # 
profit.2 <- lm(Pref ~ -1 + ML1 + ML2,
               data = data.eval[data.eval$PartnershipStatus == "In Relationship",])
param <- data.frame(t(coef(profit.2)))
param$City <- "In Relationship"
# reorder the columns
param <- param[, c("City", "ML1", "ML2")]
param$type <- "vector_relationship"
# combine with fa.selected
fa.selected <- rbind(fa.selected, param)
rownames(mds.selected) <- NULL # overwrite the rownames

# Other #
profit.3 <- lm(Pref ~ -1 + ML1 + ML2,
               data = data.eval[data.eval$PartnershipStatus == "Married",])
param <- data.frame(t(coef(profit.3)))
param$City <- "Married"
# reorder the columns
param <- param[, c("City", "ML1", "ML2")]
param$type <- "vector_relationship"
# combine with fa.selected
fa.selected <- rbind(fa.selected, param)
rownames(mds.selected) <- NULL # overwrite the rownames

#-------------#
# Plot1
ggplot(data = subset(fa.selected, type == "point"), 
       aes(x = ML1, y = ML2)) +
  geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
  geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
  geom_point() +
  # Add text labels using ggrepel package
  geom_label_repel(aes(label = City),
                   size          = 5,
                   box.padding   = 0.8,
                   point.padding = 0.5) +
  # Add Vectors 
  geom_segment(data = subset(fa.selected, type == "vector_relationship"),
               aes(x = -ML1, y = -ML2, xend = ML1*2, yend = ML2*2),
               #col = "midnightblue",
               colour=c("darkred","darkgreen","darkblue"),
               arrow = arrow(length = unit(0.7, "cm")),size = 1.0) +
  # Add vector labels
  geom_text(data = subset(fa.selected, type == "vector_relationship"),
            aes(label = City), 
            #col = "midnightblue",
            colour=c("darkred","darkgreen","darkblue"),
            size = 5,
            hjust = 0, vjust =3) +
  labs(x = "Amusement Trip", y = "Relaxing Trip") +
  xlim(-1.3, 1.3)+
  theme_classic(base_size = 17)
ggsave(file="FA_vectoren.png", width=8, height=8, dpi=900)
getwd()

##########################################
#For PLOT 2D: ML1,ML3

#FA_selected => citys and 2 Factors Points in the map
fa.selected<- mean_fa[,-4] #nur City,ML1,ML2
fa.selected$type <- "point"
fa.selected
#Column für Segment Travel with hinzufügen
#subset of Single
single <- subset(indivData, indivData$PartnershipStatus== "single")
length(unique(single$id_unique)) # Anzahl Ids in Single 105
idListSingle <- unique(single$id_unique) # Liste mit den ids aus long.data

# subset of in a relationship.

## Add Factors M1-M3 to data.eval as new columns
data.eval <- merge(data.eval, mean_fa, by = "City")
head(data.eval)

# Single #
profit.1 <- lm(Pref ~ -1 + ML1 + ML3,
               data = data.eval[data.eval$PartnershipStatus == "Single",])
param <- data.frame(t(coef(profit.1)))
param$City <- "Single"
# reorder the columns
param <- param[, c("City", "ML1", "ML3")]
param$type <- "vector_relationship"
param
# combine with fa.selected
fa.selected <- rbind(fa.selected, param)
rownames(mds.selected) <- NULL # overwrite the rownames

# # Couple # 
profit.2 <- lm(Pref ~ -1 + ML1 + ML3,
               data = data.eval[data.eval$PartnershipStatus == "In Relationship",])
param <- data.frame(t(coef(profit.2)))
param$City <- "In Relationship"
# reorder the columns
param <- param[, c("City", "ML1", "ML3")]
param$type <- "vector_relationship"
# combine with fa.selected
fa.selected <- rbind(fa.selected, param)
rownames(mds.selected) <- NULL # overwrite the rownames

# Other #
profit.3 <- lm(Pref ~ -1 + ML1 + ML3,
               data = data.eval[data.eval$PartnershipStatus == "Married",])
param <- data.frame(t(coef(profit.3)))
param$City <- "Married"
# reorder the columns
param <- param[, c("City", "ML1", "ML3")]
param$type <- "vector_relationship"
# combine with fa.selected
fa.selected <- rbind(fa.selected, param)
rownames(mds.selected) <- NULL # overwrite the rownames

#-------------#
# Plot2
ggplot(data = subset(fa.selected, type == "point"), 
       aes(x = ML1, y = ML3)) +
  geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
  geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
  geom_point() +
  # Add text labels using ggrepel package
  geom_label_repel(aes(label = City),
                   size          = 5,
                   box.padding   = 0.8,
                   point.padding = 0.5) +
  # Add Vectors 
  geom_segment(data = subset(fa.selected, type == "vector_relationship"),
               aes(x = -ML1, y = -ML3, xend = ML1*2, yend = ML3*2),
               colour=c("darkred","darkgreen","darkblue"),
               arrow = arrow(length = unit(0.7, "cm")),size = 1.0) +
  # Add vector labels
  geom_text(data = subset(fa.selected, type == "vector_relationship"),
            aes(label = City), 
            colour=c("darkred","darkgreen","darkblue"),
            size = 5,
            hjust = -0.3, vjust =1.7) +
  labs(x = "Amusement Trip", y = "Culture Trip") +
  xlim(-1.3, 1.3)+
  theme_classic(base_size = 17)
ggsave(file="FA_vectoren_culture.png", width=8, height=8, dpi=900)
getwd()





######################################################################################################
# #Comparison with MDS
# # compute mean distances
# City.dist = dist.i = NULL # initialize (creates an empty list)
# for(i in unique(data.sc$id_unique)){
#   
#   # To check what is done for each iteration, uncomment the line below
#   # and run step-by-step
#   i = unique(data.sc$id_unique)[1] # fix the i index
#   
#   # subset the data for each respondent i
#   data.sc.i = data.sc[data.sc$id_unique == i, -c(1, 2, 3)]
#   data.sc.i = data.sc.i[order(data.sc.i$City), ] # sort by station
#   rownames(data.sc.i) <- data.sc.i$City
#   
#   dist.i[[i]] <- dist(data.sc.i[,-1], method = "euclidean")
#   City.dist[[i]] <- as.matrix(dist.i[[i]])
#   dist.i[[i]] <- as.dist(dist.i[[i]])
#   
#   # save as a data frame
#   City.dist[[i]] <- data.frame(City.dist[[i]])
#   
#   # add individual counter (id)
#   City.dist[[i]]$id_unique <- i
#   City.dist[[i]]$City <- rownames(data.sc.i)
# }
# 
# # combine the list into one data frame
# City.dist <- do.call(rbind, City.dist)
# dim(City.dist)
# head(City.dist)
# 
# # Compute mean dissimilarity between Cities across respondents
# dist.mean <- aggregate(City.dist[, -c(7,8)], 
#                        by = list(City = City.dist$City), 
#                        mean)
# 
# rownames(dist.mean) <- dist.mean$City
# dist.mean = as.matrix(dist.mean[, -1])
# dist.mean
# 
# # Metric MDS - interval transformation
# mds <- smacofSym(dist.mean, 
#                  ndim = 2, 
#                  type = "interval")
# 
# mds.conf = data.frame(mds$conf)
# mds.conf$City = rownames(mds.conf)
# 
# # Property fitting
# # Add coordinates to attrEval (original dataset)
# data.sc <- merge(data.sc, mds.conf, by = "City")
# head(data.sc)
# 
# 
# # Vector Model 
# #profit.vector <- lm(cbind(friendly, historical, affordable, trendy, `vibrant nightlife`,
#                           `delicious food`, `easy to get around`, `good shopping`, 
#                           `cultural events`, `interesting museums`, clean, green, international,
#                           `too touristic`, fun, noisy, romantic, safe, beautiful,
#                           `english-speaker-friendly`, Pref) 
#                     ~ -1 + D1 + D2, data = data.sc)
# 
# profit.vector <- lm(cbind(Pref) 
#                     ~ -1 + D1 + D2, data = data.sc)
# 
# param <- data.frame(t(coef(profit.vector)))
# param$City <- rownames(param)
# 
# # reorder the columns
# param <- param[, c("City", "D1", "D2")]
# param$type <- "vector"
# 
# # combine with mds.selected
# mds.conf$type <- "point"
# mds.conf <- rbind(mds.conf, param)
# rownames(mds.conf) <- NULL # overwrite the rownames
# 
# # Plot
# ggplot(data = subset(mds.conf, type == "point"), 
#        aes(x = D1, y = D2)) +
#   geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
#   geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
#   geom_point() +
#   # Add text labels using ggrepel package
#   geom_label_repel(aes(label = City),
#                    size          = 2,
#                    box.padding   = 0.8,
#                    point.padding = 0.5) +
#   # Add vectors for attributes
#   geom_segment(data = subset(mds.conf, type == "vector"),
#                aes(x = 0, y = 0, xend = D1, yend = D2),
#                col = "darkblue",
#                arrow = arrow(length = unit(0.5, "cm"))) +
#   # Add vector labels
#   geom_text(data = subset(mds.conf, type == "vector"),
#             aes(label = City), 
#             col = "darkblue",
#             hjust = -0.5, vjust = 1) +
#   labs(x = "Dimension 1", y = "Dimension 2") +
#   theme_bw()
# 
# #HIER STIMMT ETWAS NOCH NICHT.. WARUM NUR 6 STÄDTE?
# 
# 
# 
# 
# 
# ### Vector Model mit Segmenten Single vs Relationshipt vs Other ###
# # Column für Segment Travel with hinzufügen
# # subset of Single
# single <- subset(indivData, indivData$PartnershipStatus== "single") 
# length(unique(single$id_unique)) # Anzahl Ids in Single 105
# idListSingle <- unique(single$id_unique) # Liste mit den ids aus long.data
# 
# # subset of in a relationship.
# couple <- subset(indivData, indivData$PartnershipStatus== "in a relationship.")
# length(unique(couple$id_unique)) # Anzahl Ids in bachelor 134
# idListCouple <- unique(couple$id_unique) # Liste mit den ids aus long.data
# 
# data.sc$PartnershipStatus <- ifelse(data.sc$id_unique %in% idListSingle, "Single", 
#                                     ifelse(data.sc$id_unique %in% idListCouple, "In Relationship", "Other"))
# 
# # Single # 
# profit.1 <- lm(Pref ~ -1 + D1 + D2, 
#                data = data.sc[data.sc$PartnershipStatus == "Single",])
# param <- data.frame(t(coef(profit.1)))
# param$City <- "Single"
# # reorder the columns
# param <- param[, c("City", "D1", "D2")]
# param$type <- "vector_relationship"
# # combine with mds.selected
# mds.conf <- rbind(mds.conf, param)
# rownames(mds.conf) <- NULL # overwrite the rownames
# 
# # Couple # 
# profit.2 <- lm(Pref ~ -1 + D1 + D2, 
#                data = data.sc[data.sc$PartnershipStatus == "In Relationship",])
# param <- data.frame(t(coef(profit.2)))
# param$City <- "In Relationship"
# # reorder the columns
# param <- param[, c("City", "D1", "D2")]
# param$type <- "vector_relationship"
# # combine with mds.selected
# mds.conf <- rbind(mds.conf, param)
# rownames(mds.conf) <- NULL # overwrite the rownames
# 
# # Other # 
# profit.3 <- lm(Pref ~ -1 + D1 + D2, 
#                data = data.sc[data.sc$PartnershipStatus == "Other",])
# param <- data.frame(t(coef(profit.3)))
# param$City <- "Other"
# # reorder the columns
# param <- param[, c("City", "D1", "D2")]
# param$type <- "vector_relationship"
# # combine with mds.selected
# mds.conf <- rbind(mds.conf, param)
# rownames(mds.conf) <- NULL # overwrite the rownames
# 
# # Plot
# ggplot(data = subset(mds.conf, type == "point"), 
#        aes(x = D1, y = D2)) +
#   geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
#   geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
#   geom_point() +
#   # Add text labels using ggrepel package
#   geom_label_repel(aes(label = City),
#                    size          = 2,
#                    box.padding   = 0.8,
#                    point.padding = 0.5) +
#   # Add Vectors for attributes
#   geom_segment(data = subset(mds.conf, type == "vector_relationship"),
#                aes(x = -D1, y = -D2, xend = D1*3, yend = D2*3),
#                col = "turquoise4",
#                arrow = arrow(length = unit(0.5, "cm"))) +
#   # Add vector labels
#   geom_text(data = subset(mds.conf, type == "vector_relationship"),
#             aes(label = City), 
#             col = "turquoise4",
#             size = 5,
#             hjust = -0.5, vjust = 1) +
#   labs(x = "Dimension 1", y = "Dimension 2") +
#   theme_bw()
# 
# #Aber sind das jetzt die Preferences??
