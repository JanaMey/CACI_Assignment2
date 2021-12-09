# Install and load the required libraries ----------------------------------------
pacman::p_load(reshape2, ggplot2, dplyr, stringr, corrplot,smacof,ggrepel,ggforce,MASS, reshape2)

urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment2/main/indivData.csv'
indivData <-read.csv(urlfile)
urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment2/main/dataEvalScale.csv'
data.eval <-read.csv(urlfile)

summary(data.eval) #mean=0
#head(data.eval[,-25]) #without preferences
head(data.eval)
dim(data.eval) # 1401 25

str(indivData)
head(indivData)
dim(indivData) # 258 43

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
         tl.cex = 0.75,
         tl.col = "black",
         addCoef.col ='grey28',
         number.digits = 1,
         number.cex = 0.65)


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

# For distance method: distances on mean evals
ggplot(data = subset(mds, dist_method == "dist.onmean"), 
       aes(x = dim1, y = dim2)) +
  geom_vline(xintercept = 0, col = "grey50", linetype = "dotted") +
  geom_hline(yintercept = 0, col = "grey50", linetype = "dotted") +
  geom_point() +
  # Add text labels using ggrepel package
  geom_label_repel(aes(label = City),
                   size          = 2,
                   box.padding   = 0.8,
                   point.padding = 0.5) +
  facet_wrap(.~mds_method, scales = "free", nrow = 1) +
  labs(x = "Dimension 1", y = "Dimension 2") +
  theme_bw()


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
profit.vector <- lm(cbind(affordable,friendly, international,romantic,historical, fun, noisy, safe, trendy, green)
                    ~ -1 + dim1 + dim2, data = data.eval)

colnames(data.eval[5:24])#all attributes
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
  theme_bw(base_size = 21)
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
                   vjust = 1)+
                  #box.padding   = 0.8,
                  # point.padding = 0.5) +
  # Add vectors for attributes
  geom_segment(data = subset(mds.selected, type == "vector"),
                aes(x = 0, y = 0, xend = dim1, yend = dim2),
                col = "royalblue4",
                arrow = arrow(length = unit(0.5, "cm"))) +
  # Add vector labels
  geom_text(data = subset(mds.selected, type == "vector"),
            aes(label = City), 
            col = "royalblue4",
            size = 5,
            hjust = 1.2, vjust = 1.4) +
  labs(x = "Dimension 1", y = "Dimension 2") +
  theme_bw(base_size = 21)
  ggsave(file="MDS_vectoren.png", width=8, height=8, dpi=600)  
    
    

  
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
