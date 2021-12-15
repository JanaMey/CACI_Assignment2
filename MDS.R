# Install and load the required libraries ----------------------------------------
pacman::p_load(reshape2, ggplot2, dplyr, stringr, corrplot,smacof,ggrepel,ggforce,MASS, reshape2)

urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment2/main/indivData.csv'
indivData <-read.csv(urlfile)
urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment2/main/dataEvalScale.csv'
data.eval <-read.csv(urlfile)

summary(data.eval) #mean=0
head(data.eval)
dim(data.eval) # 1401 25

str(indivData)
head(indivData)
dim(indivData) # 258 43

# #rename variables
# names(data.eval)[9] <- "vibrant nightlife"
# names(data.eval)[10] <- "delicious food"
# names(data.eval)[11] <- "easy to get around"
# names(data.eval)[12] <- "good shopping"
# names(data.eval)[13] <- "cultural events"
# names(data.eval)[14] <- "interesting museums"
# names(data.eval)[18] <- "too touristic"
# names(data.eval)[24] <- "english-speaker-friendly"
########################################################################
########################################################################
# Ab hier Aufgabe 2 MDS
# Distribution of distance is skewed
# Log-transform the variable
#data.eval[, -c(1, 2, 3, 4)] <- log(data.eval[, -c(1, 2, 3, 4)])

# Derive Proximity Measure =====================================================
# Compute Euclidean distance measure (aggregate-level)
# Compute dissimilarity on mean attribute evaluations --------------------------
cities.mean <- aggregate(data.eval[, -c(1, 2, 3, 4, 25)], # 25: without preferences
                         by = list(city = data.eval$City), 
                         FUN = mean)
rownames(cities.mean) <- cities.mean$city

# Compute distance on mean attribute evaluations
dist.onmean <- dist(cities.mean[, -1], method = "euclidean",
                    diag = TRUE, upper = TRUE)
dist.onmean

# Also save dist.onmean as matrix
dist.onmean <- as.matrix(dist.onmean)

# Compare the resulting dissimilarity matrices --------------------------------
round(dist.onmean, 2)

# # Fix the code
# corrplot(dist.onmean,
#          is.corr = FALSE,
#          method = "color",
#          type = "upper",
#          number.cex = 1,
#          tl.cex = 1,
#          tl.col = "black",
#          addCoef.col ='black',
#          col= colorRampPalette(c("grey80", "grey0"))(5)) 

# corrplot shows all number and the background fades.
corrplot(cor(dist.onmean),
         method = "number", # besser mit method = "color" aber da brauchen wir einen cutoff. Wie geht das?
         #insig = 'blank',
         type = "upper",
         tl.cex = 1.2,
         tl.col = "black",
         addCoef.col ='grey28',
         number.digits = 1,
         number.cex = 0.9)
# save as width:1000 height:800

# Let's define a vector of different mds methods
mds_method <- c("ratio", "interval", "ordinal")

# Let's define a vector of different dissimilarity methods
# points of aggregation
dist_method <- c("dist.onmean")

# Fix the number of dimensions
k = 2

# Let's use a for loop
mds = stress = NULL # initialize empty lists

for(d in dist_method){
  
  # Uncomment the line below to check step-by-step
  # d = dist_method[1]
  
  mds.d = stress.d = NULL # initialize empty lists
  
  # save the distance matrix as a temporary object
  temp.dist <- get(d)
  
  for(m in mds_method){
    
    # Uncomment the line below to check step-by-step
    # m = mds_method[1]
    
    # use cmscale() for absolute and smacof for the rest
    temp <- mds(temp.dist, ndim = k, type = m)
    # mds() is the same as smacofSym()
      
    temp.coord <- temp$conf     # save the coordinates
    temp.stress <- temp$stress  # save the Stress-1 value
    
    # save as a data frame with required additional info
    temp.coord <- data.frame(City = rownames(temp.coord), # station names
                             mds_method = m,  # mds method
                             dist_method = d, # dissimilarity measure
                             temp.coord)
    # set the column names
    colnames(temp.coord) <- c("City", "mds_method", "dist_method",
                              "dim1", "dim2")
    
    # save the coordinates as m element of mds.d list
    mds.d[[m]] <- temp.coord 
    
    # save the Stress-1 as m element of stress.d list
    stress.d[[m]] <- data.frame(mds_method = m,
                                dist_method = d,
                                stress_1 = temp.stress)
    
  }
  
  # combine row-wise in one data frame and save as d elements of
  # mds and stress lists
  mds[[d]] <- do.call(rbind, mds.d)
  stress[[d]] <- do.call(rbind, stress.d)
}


# combine row-wise in one data frame
mds <- do.call(rbind, mds)
rownames(mds) <- NULL
mds

stress <- do.call(rbind, stress)
rownames(stress) <- NULL
stress

# Plot the solutions ----------------------------------------------------------
# Overwrite mds_method as a factor with levels as in the vector we defined
# This is going to help to visualize the plots in the order we want
mds$mds_method <- factor(mds$mds_method, levels = mds_method)
# 
# For distance method: distances on mean evals
# ggplot(data = subset(mds, dist_method == "dist.onmean"), 
#        aes(x = dim1, y = dim2)) +
#   geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
#   geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
#   geom_point() +
#   # Add text labels using ggrepel package
#   geom_label_repel(aes(label = City),
#                    size          = 2,
#                    box.padding   = 0.8,
#                    point.padding = 0.5) +
#   facet_wrap(.~mds_method, scales = "free", nrow = 1) +
#   labs(x = "Dimension 1", y = "Dimension 2") +
#   theme_bw()


# Property Fitting ============================================================
# Joint Space of TV Stations and Attributes 
# Selected solution
mds.selected <- subset(mds, dist_method == "dist.onmean" & 
                         mds_method == "interval")
mds.selected <- mds.selected[, c("City", "dim1", "dim2")]
mds.selected

# Add coordinates to attrEval (original dataset)
data.eval <- merge(data.eval, mds.selected, by = "City")
head(data.eval)

# Vector Model ----------------------------------------------------------------
# Example for attributes informative and exciting
# we suppress the intercept, so that the vectors go through the origin
profit.vector <- lm(cbind(affordable,romantic,historical,safe,clean,noisy,trendy,international,friendly,fun,green)#,interesting.museums,easy.to.get.around,delicious.food,too.touristic,beautiful,vibrant.nightlife,good.shopping,english.speaker.friendly,cultural.events 
                    ~ -1 + dim1 + dim2, data = data.eval)

colnames(data.eval[5:25])#all attributes
#profit.vector <- lm(cbind() # ein Beispiel mit friendly und historical. Kann beides ausgetauscht werden.
#                    ~ -1 + dim1 + dim2, data = data.eval)

summary(profit.vector) 
param <- data.frame(t(coef(profit.vector)))
param$City <- rownames(param) #attribute names

# reorder the columns
param <- param[, c("City", "dim1", "dim2")]
param$type <- "vector"

# combine with mds.selected
mds.selected$type <- "point"
mds.selected <- rbind(mds.selected, param)
rownames(mds.selected) <- NULL # overwrite the rownames


# Plot
# only cities
ggplot(data = subset(mds.selected, type == "point"),
       aes(x = dim1, y = dim2)) +
  geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
  geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
  geom_point() +
  ylim(-1, 1) +
  # Add text labels using ggrepel package
  geom_label_repel(aes(label = City),
                   size = 7,
                   #hjust = 0,         # horizontal adjustment of the position
                   vjust = 1) +        # vertical adjustment of the positio
  labs(x = "Dimension 1", y = "Dimension 2") + #x = "Comfortable", y = "Exciting"
  theme_bw(base_size = 21)+
  ggsave(file="MDS.png", width=8, height=8, dpi=600)
    
# Plot
ggplot(data = subset(mds.selected, type == "point"), 
      aes(x = dim1, y = dim2)) +
  geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
  geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
  geom_point() +
  # Add text labels using ggrepel package
  geom_label_repel(aes(label = City),
                   size  = 7,
                   vjust = 1,
                   col = 'grey27')+
                  #box.padding   = 0.8,
                  # point.padding = 0.5) +
  # Add vectors for attributes
  geom_segment(data = subset(mds.selected, type == "vector"),
                aes(x = 0, y = 0, xend = dim1, yend = dim2),
                col = "midnightblue",
                arrow = arrow(length = unit(0.5, "cm"))) +
  # Add vector labels
  geom_text(data = subset(mds.selected, type == "vector"),
            aes(label = City), 
            col = "midnightblue",
            size = 5.5, #5.5
            hjust = 1.0, vjust = 1.5) +
  labs(x = "Dimension 1", y = "Dimension 2") +
  theme_bw(base_size = 21)
  ggsave(file="MDS_vectoren.png", width=8, height=8, dpi=1000)   # width=8 besser
  getwd()
    
### Vector Model mit Segmenten Bachelor, Master, Other ###
# Column für Segment Occupation hinzufügen
# subset of bachelor students
# bachelor <- subset(indivData, indivData$Occupation=="Bachelor student")
# length(unique(bachelor$id_unique)) # Anzahl Ids in bachelor 64
# idListBachelor <- unique(bachelor$id_unique) # Liste mit den ids aus long.data
# 
# # subset of master students
# master <- subset(indivData, indivData$Occupation=="Master student")
# length(unique(master$id_unique)) # Anzahl Ids in bachelor 128
# idListMaster <- unique(master$id_unique) # Liste mit den ids aus long.data
# 
# data.eval$Occupation <- ifelse(data.eval$id_unique %in% idListBachelor, "Bachelor", 
#                                ifelse(data.eval$id_unique %in% idListMaster, "Master", "Other"))
# 
# # Bachelor # 
# profit.1 <- lm(Pref ~ -1 + dim1 + dim2, 
#                data = data.eval[data.eval$Occupation == "Bachelor",])
# param <- data.frame(t(coef(profit.1)))
# param$City <- "Bachlor"
# # reorder the columns
# param <- param[, c("City", "dim1", "dim2")]
# param$type <- "vector_occupation"
# # combine with mds.selected
# mds.selected <- rbind(mds.selected, param)
# rownames(mds.selected) <- NULL # overwrite the rownames
# 
# # Master # 
# profit.2 <- lm(Pref ~ -1 + dim1 + dim2, 
#                data = data.eval[data.eval$Occupation == "Master",])
# param <- data.frame(t(coef(profit.2)))
# param$City <- "Master"
# # reorder the columns
# param <- param[, c("City", "dim1", "dim2")]
# param$type <- "vector_occupation"
# # combine with mds.selected
# mds.selected <- rbind(mds.selected, param)
# rownames(mds.selected) <- NULL # overwrite the rownames
# 
# # Other # 
# profit.3 <- lm(Pref ~ -1 + dim1 + dim2, 
#                data = data.eval[data.eval$Occupation == "Other",])
# param <- data.frame(t(coef(profit.3)))
# param$City <- "Other"
# # reorder the columns
# param <- param[, c("City", "dim1", "dim2")]
# param$type <- "vector_occupation"
# # combine with mds.selected
# mds.selected <- rbind(mds.selected, param)
# rownames(mds.selected) <- NULL # overwrite the rownames
# 
# # Plot
# 
# ggplot(data = subset(mds.selected, type == "point"), 
#        aes(x = dim1, y = dim2)) +
#   geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
#   geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
#   geom_point() +
#   # Add text labels using ggrepel package
#   geom_label_repel(aes(label = City),
#                    size          = 2,
#                    box.padding   = 0.8,
#                    point.padding = 0.5) +
#   # Add Vectors for attributes
#   geom_segment(data = subset(mds.selected, type == "vector_occupation"),
#                aes(x = -dim1, y = -dim2, xend = dim1, yend = dim2),
#                col = "midnightblue",
#                arrow = arrow(length = unit(0.5, "cm"))) +
#   # Add vector labels
#   geom_text(data = subset(mds.selected, type == "vector_occupation"),
#             aes(label = City), 
#             col = "midnightblue",
#             size = 5,
#             hjust = -0.5, vjust = 1) +
#   labs(x = "Dimension 1", y = "Dimension 2") +
#   theme_bw(base_size = 21)
# #ggsave(file="MDS_vectoren_segmente.png", width=8, height=8, dpi=800)   # width=8 besser


### Vector Model mit Segmenten Single vs Relationshipt vs Other ###
# Column für Segment Travel with hinzufügen
# subset of Single
single <- subset(indivData, indivData$PartnershipStatus== "single") 
length(unique(single$id_unique)) # Anzahl Ids in Single 105
idListSingle <- unique(single$id_unique) # Liste mit den ids aus long.data

# subset of in a relationship.
couple <- subset(indivData, indivData$PartnershipStatus== "in a relationship.")
length(unique(couple$id_unique)) # Anzahl Ids in bachelor 134
idListCouple <- unique(couple$id_unique) # Liste mit den ids aus long.data

data.eval$PartnershipStatus <- ifelse(data.eval$id_unique %in% idListSingle, "Single", 
                               ifelse(data.eval$id_unique %in% idListCouple, "In Relationship", "Married"))

# Single # 
profit.1 <- lm(Pref ~ -1 + dim1 + dim2, 
               data = data.eval[data.eval$PartnershipStatus == "Single",])
param <- data.frame(t(coef(profit.1)))
param$City <- "Single"
# reorder the columns
param <- param[, c("City", "dim1", "dim2")]
param$type <- "vector_relationship"
# combine with mds.selected
mds.selected <- rbind(mds.selected, param)
rownames(mds.selected) <- NULL # overwrite the rownames

# Couple # 
profit.2 <- lm(Pref ~ -1 + dim1 + dim2, 
               data = data.eval[data.eval$PartnershipStatus == "In Relationship",])
param <- data.frame(t(coef(profit.2)))
param$City <- "In Relationship"
# reorder the columns
param <- param[, c("City", "dim1", "dim2")]
param$type <- "vector_relationship"
# combine with mds.selected
mds.selected <- rbind(mds.selected, param)
rownames(mds.selected) <- NULL # overwrite the rownames

# Married # 
profit.3 <- lm(Pref ~ -1 + dim1 + dim2, 
               data = data.eval[data.eval$PartnershipStatus == "Married",])
param <- data.frame(t(coef(profit.3)))
param$City <- "Married"
# reorder the columns
param <- param[, c("City", "dim1", "dim2")]
param$type <- "vector_relationship"
# combine with mds.selected
mds.selected <- rbind(mds.selected, param)
rownames(mds.selected) <- NULL # overwrite the rownames

# Plot
ggplot(data = subset(mds.selected, type == "point"), 
       aes(x = dim1, y = dim2)) +
  geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
  geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
  geom_point() +
  # Add text labels using ggrepel package
  geom_label_repel(aes(label = City),
                   size          = 7, #2,
                   vjust = 1,
                   col = 'grey27')+
                   #box.padding   = 0.8,
                   #point.padding = 0.5) +
  # Add Vectors for attributes
  geom_segment(data = subset(mds.selected, type == "vector_relationship"),
               aes(x = -dim1, y = -dim2, xend = dim1*2, yend = dim2*2),
               col = "midnightblue",
               arrow = arrow(length = unit(0.7, "cm"))) +
  # Add vector labels
  geom_text(data = subset(mds.selected, type == "vector_relationship"),
            aes(label = City), 
            col = "midnightblue",
            size = 6, #5,
            hjust = 0.5, vjust = 1.2) +# hjust = -0.5, vjust = 1) +
  labs(x = "Dimension 1", y = "Dimension 2") +
  theme_bw(base_size = 21)
ggsave(file="MDS_vectoren_PartnershipStatus.png", width=10, height=8, dpi=800)   # width=8 besser

# ### Vector Model mit Segmenten Gender ###
# # Column für Segment Gender with hinzufügen
# # subset of Single
# female <- subset(indivData, indivData$Gender== "Female")
# length(unique(female$id_unique)) # Anzahl Ids in Single 148
# idListFemale <- unique(female$id_unique) # Liste mit den ids aus long.data
# 
# # subset of Male
# male <- subset(indivData, indivData$Gender== "Male")
# length(unique(male$id_unique)) # Anzahl Ids in bachelor 110
# idListMale <- unique(male$id_unique) # Liste mit den ids aus long.data
# 
# data.eval$Gender <- ifelse(data.eval$id_unique %in% idListFemale, "Female", "Male")
# 
# 
# # Male #
# profit.1 <- lm(Pref ~ -1 + dim1 + dim2,
#                data = data.eval[data.eval$Gender == "Male",])
# param <- data.frame(t(coef(profit.1)))
# param$City <- "Male"
# # reorder the columns
# param <- param[, c("City", "dim1", "dim2")]
# param$type <- "vector_gender"
# # combine with mds.selected
# mds.selected <- rbind(mds.selected, param)
# rownames(mds.selected) <- NULL # overwrite the rownames
# 
# # Female #
# profit.2 <- lm(Pref ~ -1 + dim1 + dim2,
#                data = data.eval[data.eval$Gender == "Female",])
# param <- data.frame(t(coef(profit.2)))
# param$City <- "Female"
# # reorder the columns
# param <- param[, c("City", "dim1", "dim2")]
# param$type <- "vector_gender"
# # combine with mds.selected
# mds.selected <- rbind(mds.selected, param)
# rownames(mds.selected) <- NULL # overwrite the rownames
# 
# # Plot
# ggplot(data = subset(mds.selected, type == "point"),
#        aes(x = dim1, y = dim2)) +
#   geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
#   geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
#   geom_point() +
#   # Add text labels using ggrepel package
#   geom_label_repel(aes(label = City),
#                    size          = 2,
#                    box.padding   = 0.8,
#                    point.padding = 0.5) +
#   # Add Vectors for attributes
#   geom_segment(data = subset(mds.selected, type == "vector_gender"),
#                aes(x = -dim1, y = -dim2, xend = dim1*1, yend = dim2*1),
#                col = "turquoise4",
#                arrow = arrow(length = unit(0.5, "cm"))) +
#   # Add vector labels
#   geom_text(data = subset(mds.selected, type == "vector_gender"),
#             aes(label = City),
#             col = "turquoise4",
#             size = 5,
#             hjust = -0.5, vjust = 1) +
#   labs(x = "Dimension 1", y = "Dimension 2") +
#   theme_bw()
# # #ggsave(file="MDS_vectoren_gender.png", width=8, height=8, dpi=800)   # width=8 besser
# 
#   
# Ideal-point Model -----------------------------------------------------------
# Example for attributes informative and exciting
# Add the quadratic term
data.eval$q <- data.eval$dim1^2 + data.eval$dim2^2
profit.ideal <- lm(cbind(friendly, historical) 
                   ~ dim1 + dim2 + q, data = data.eval)

summary(profit.ideal) 
param <- data.frame(t(coef(profit.ideal)))[, -1]
param$City <- rownames(param)

# Corrected!!! Was missing before, and now the computation of 
# ideal points is correct
param$dim1 <- -param$dim1/2*param$q
param$dim2 <- -param$dim2/2*param$q

# reorder the columns
param <- param[, c("City", "dim1", "dim2")]
param$type <- "ideal"


# combine with mds.selected
mds.selected <- rbind(mds.selected, param)
rownames(mds.selected) <- NULL # overwrite the rownames


# Plot
ggplot(data = subset(mds.selected, type != "vector"), 
       aes(x = dim1, y = dim2, col = type)) +
  geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
  geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
  geom_point(show.legend = FALSE) +
  # Add text labels using ggrepel package
  geom_label_repel(aes(label = City),
                   size          = 2,
                   box.padding   = 0.8,
                   point.padding = 0.5,
                   show.legend = FALSE) +
  # Add circular contours
  geom_mark_circle(data = subset(mds.selected, type == "ideal" & City == "friendly"), 
                   aes(fill = City), expand = 0.1, show.legend = FALSE) +
  geom_mark_circle(data = subset(mds.selected, type == "ideal" & City == "friendly"),
                   aes(fill = City), expand = 0.2, show.legend = FALSE) +
  geom_mark_circle(data = subset(mds.selected, type == "ideal" & City == "friendly"),
                   aes(fill = City), expand = 0.4, show.legend = FALSE) +
  scale_color_manual(values = c("darkblue", "black")) +
  scale_fill_manual(values = c("white", "white")) +
  labs(x = "Dimension 1", y = "Dimension 2") +
  theme_bw()

# Joint Mapping of Perceptions and Preferences ================================
# Let's assume we have preference ratings of each TV channel by a respondent
# sort original data frame
data.eval <- data.eval[order(data.eval$id_unique, data.eval$City), ]
head(data.eval, 8)
head(data.eval[data.eval$id_unique == 1, ], 8)

# add preferences for id_new = 1
# high preference for ARD and ZDF
data.eval$Pref <- ifelse(data.eval$id_unique == 1, c(5, 4, 2, 1, 1, 3, 4, 5), NA)
data.eval[data.eval$id_unique == 1, ]


# Property fitting (what model to use: ideal or vector?)
# As preference ratings should be interpreted as "the more, the better",
# we will opt for the vector model
profit.1 <- lm(Pref ~ -1 + dim1 + dim2, 
               data = data.eval[data.eval$id_unique == 1, ])
summary(profit.1)

param <- data.frame(t(coef(profit.1)))
param$City <- "id1"

# reorder the columns
param <- param[, c("City", "dim1", "dim2")]
param$type <- "vector_pref"

# combine with mds.selected
mds.selected <- rbind(mds.selected, param)
rownames(mds.selected) <- NULL # overwrite the rownames


# Plot
ggplot(data = subset(mds.selected, type == "point"), 
       aes(x = dim1, y = dim2)) +
  geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
  geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
  geom_point() +
  # Add text labels using ggrepel package
  geom_label_repel(aes(label = City),
                   size          = 2,
                   box.padding   = 0.8,
                   point.padding = 0.5) +
  # Add Vectors for attributes
  geom_segment(data = subset(mds.selected, type == "vector_pref"),
               aes(x = -dim1, y = -dim2, xend = dim1, yend = dim2),
               col = "darkblue",
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Add vector labels
  geom_text(data = subset(mds.selected, type == "vector_pref"),
            aes(label = City), 
            col = "darkblue",
            hjust = -0.5, vjust = 1) +
  labs(x = "Dimension 1", y = "Dimension 2") +
  theme_bw()


# Note that the better the model fits (higher Rsquare), the larger  
# the scale of the parameter and, hence, the end of the arrow will be.
# For more visually pleasing figure, we can simply rescale the
# estimates in the vector-model
# Here, let's try rescaling down
original <- mds.selected[mds.selected$City == "id1", 
                         c("dim1", "dim2")]

rescaled <- original * 0.5


# overwrite the values with the rescaled values
mds.selected[mds.selected$City == "id1", 
             c("dim1", "dim2")] <- rescaled


# Plot again
ggplot(data = subset(mds.selected, type == "point"), 
       aes(x = dim1, y = dim2)) +
  geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
  geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
  geom_point() +
  # Add text labels using ggrepel package
  geom_label_repel(aes(label = City),
                   size          = 2,
                   box.padding   = 0.8,
                   point.padding = 0.5) +
  # Add vectors for attributes
  geom_segment(data = subset(mds.selected, type == "vector_pref"),
               aes(x = -dim1, y = -dim2, xend = dim1, yend = dim2),
               col = "darkblue",
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Add vector labels
  geom_text(data = subset(mds.selected, type == "vector_pref"),
            aes(label = City), 
            col = "darkblue",
            hjust = -0.5, vjust = 1) +
  labs(x = "Dimension 1", y = "Dimension 2") +
  theme_bw()
