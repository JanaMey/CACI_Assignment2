# Install and load the required libraries ----------------------------------------
pacman::p_load(reshape2, ggplot2, dplyr, stringr, corrplot)

# Load required dataset ----------------------------------------------------------
#data.wide <- read.csv("QuestionaireData_CityTrips.csv")# unn?tig, wir laden es von git mit urlfile
urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment2/main/QuestionaireData_CityTrips.csv?token=AIBBFR5OGL'
data.wide <-read.csv(urlfile)
str(data.wide)
head(data.wide)
dim(data.wide) # 266 462

# Create a new unique ID index ===================================================
# The ID index resets for each Sample. Therefore, we need to create a unique id 
# index
data.wide$id_unique <- 1:nrow(data.wide)
head(data.wide)

# Reshaping data from wide to long format ========================================
# Separate attribute evaluations (here, I am going to use dplyr package, 
# the same can be accomplished in other ways working with base R)
# However, dplyr has some nice functions, which allow us to select variables
# with similar names with much less lines of code

# First let's separate attribute evaluations and preference ratings --------------
# which are on individual-city (only 6 cities in the sample) level
names(data.wide)

# We want to select all columns that have _Att and Pref in the names and 
# save this object
data.long <- data.wide %>% 
  select(c("Sample", "ID", "id_unique"), # we still need the respondent index
         contains("_Att"),  # any column that contains _Att in the name
         starts_with("Pref_")) # any column that starts with Pref_              

head(data.long)
dim(data.long) # 266 402 ====>>>>> ICH BEKOMME 423???
str(data.long)

# Reshape to long format
data.long <- melt(data.long, id.vars = c("Sample", "ID", "id_unique"))
head(data.long)
dim(data.long)

# Let's delete the NAs
is.na(data.long)
data.long <- data.long[!is.na(data.long$value), ]
dim(data.long)


# Now we have both attribute evaluations of each city and preference rating stacked
# under each other
# Let's create a variable that differentiates whether it is attribute eval.
# or preference rating
data.long$type <- ifelse(str_detect(data.long$variable, "_Att"), "Attribute", "Pref")
head(data.long)

data.long$id_unique
# Now we want to create a column City which is the 1st part of the variable
# string (before _) if it's type = "Attribute" and 2nd part (after _) if
# it's type = "Pref"
# For this we will use a useful function strsplit(), which 
# splits a string by the provided delimiter (here, underscore _)
# Let's run this part separately for the 1st row (observation)
strsplit(data.long[1, ]$variable, "_") # not working as variable is a factor
str(data.long)

# so we need to convert it to a character class and then apply strsplit()
strsplit(as.character(data.long[1, ]$variable), "_")

# Now we want to select only the 1st element. 
# We can do this using sapply()
sapply(strsplit(as.character(data.long[1, ]$variable), "_"), `[`, 1)

# if we wanted to choose the 2nd element, then
sapply(strsplit(as.character(data.long[1, ]$variable), "_"), `[`, 2)

# Ok, now let's apply it to each row and use ifelse() by column type
data.long$City <- ifelse(data.long$type == "Attribute",
                         sapply(strsplit(as.character(data.long$variable), "_"), `[`, 1),
                         sapply(strsplit(as.character(data.long$variable), "_"), `[`, 2))
head(data.long)
unique(data.long$City)

# Now let's create column Attribute, which is the index for attributes now contained
# in column variable if type = "Attribute" and let's set the value to "Pref" if type = "Pref
data.long$Attribute <- ifelse(data.long$type == "Attribute",
                              sapply(strsplit(as.character(data.long$variable), "_"), `[`, 2),
                              "Pref")
head(data.long)
unique(data.long$Attribute)

# Attribute column is a character. Let's save it as a factor and
# assign meaningful labels from the documentation file
# Define the vector of labels
attLabels <- c("friendly", "historical", "affordable", "trendy",
               "vibrant nightlife", "delicious food", "easy-to-get-around",
               "good shopping", "cultural events", "interesting museums",
               "clean", "green", "international", "too touristic",
               "fun", "noisy", "romantic", "safe", "beautiful", 
               "english-speaker-friendly")
length(attLabels) # should be 20


# Set Attribute variable as a factor, define levels from 1 to 20, and 
# define the labels 
data.long$Attribute <- factor(data.long$Attribute, levels = c(paste0("Att", 1:20), "Pref"),
                              labels = c(attLabels, "Pref"))
head(data.long)

# Let's reshape to wide format 
# so that we have one column for each attribute and one column for Pref
data.long <- dcast(data.long, 
                   Sample + ID + id_unique + City ~ Attribute, 
                   value.var = "value")
head(data.long)
dim(data.long) # 1590   25

# Are these correct dimensions?
# Each respondent was supposed to evaluate 6 cities. #
length(unique(data.long$id_unique)) # 266
266 * 6 # 1596 

# How come?
# There are still NAs in the data, these are the real missing values:
# item non-response: respondents didn't give an answer to this questions.

# We can save this data frame separately as a csv and load and work with it later
#write.csv(data.long, file = "data_long.csv", row.names = FALSE)
head(data.long)

# Separate individual-level variables ============================================
indivData <- data.wide %>% select(!contains("Pref") & !contains("_Att"))
head(indivData)
dim(indivData)

# Some checks
unique(data.long$City)
length(unique(data.long$City)) # 20


# We can save this data frame separately as a csv and load and work with it later
#write.csv(indivData, file = "indivData.csv", row.names = FALSE)


#---------------------------------------------------------------------------------------------------------------
#Exercise 1
#Missing Values
any(is.na(data.long)) #True: Still Item Nonresponse NAs
is.na(data.long)
summary(data.long)
names(data.long)
head(data.long)
str(data.long)

data.eval <- data.long #copy the data

####################################################################################################
#ALTERNATIVE 1 (alle IDs rauswerfen, machen wir aber nicht: Gehe zu ALTERNATIVE 2)
#missing.ids <- unique(subset(data.eval, is.na(friendly & historical &affordable
                                              # &trendy  &`vibrant nightlife`&`delicious food`
                                              # &`easy-to-get-around`&`good shopping`&`cultural events`
                                              # &`interesting museums`&`too touristic`
                                              # &`english-speaker-friendly`&clean &green &international  
                                              # &fun &noisy &romantic &safe &beautiful))$id_unique)
# missing.ids
#length(missing.ids) #60 IDs haben mind. 1 NA in den Evaluationen bei mind. einer Stadt

#data.pref <- data.long #Was ist mit den Preferences?
#missing.ids2 <- unique(subset(data.pref, is.na(friendly & historical &affordable
                                               # &trendy  &`vibrant nightlife`&`delicious food`
                                               # &`easy-to-get-around`&`good shopping`&`cultural events`
                                               # &`interesting museums`&`too touristic`
                                               # &`english-speaker-friendly`&clean &green &international  
                                               # &fun &noisy &romantic &safe &beautiful&Pref))$id_unique)
#length(missing.ids2) #72 IDs haben mind. 1 NA in den Evaluationen UND Preferences
#ABER es reicht, wenn wir nur die Evaluationen rausstreichen, genügt, wenn die vollständig sind.

#wie sehen die Daten mit Missing Values aus:
#missings.data.eval <- subset(data.eval, id_unique %in% missing.ids)
#missings.data.eval[order(missings.data.eval$id_unique), ]
#item non response. All survey non responses are already out. 
#die meisten haben bei einer Stadt alles nicht ausgefüllt, einige nur Preferences nicht ausgefüllt

#die IDs mit NAs in Evaluations entfernen
#data.eval <- subset(data.eval, !id_unique %in% missing.ids) #dann wird alles von der ID weggemacht
#dim(data.eval) #1236 25 -> 59 IDs entfernt -> 354 Rows entfernt
#summary(data.eval)
###################################################################################################

#ALTERNATIVE 2
#Wir werfen alle Rows raus, die NAs enthalte.
data.eval <- na.omit(data.eval)
any(is.na(data.eval)) #false
dim(data.eval) #1431 25 -> von 1596 sind 165 rausgefallen
length(unique(data.eval$id_unique)) #wir haben noch 264 von 266 IDs


###################################################################################################
########################################################################
#Sample description sociodemographics and City attributes and preferences
indivData <- data.wide %>% select(!contains("Pref") & !contains("_Att"))
summary(indivData)
dim(indivData) #266 43
head(indivData)
str(indivData)
#indivData <- melt(indivData, id.vars = c("Sample", "ID", "id_unique"))


##Delete the NAs (survey non respondents)
any(is.na(indivData)) #True
indivData <- indivData[!is.na(indivData$Age), ] # deleted 4 ids #
length(unique(indivData$id_unique)) #262
any(is.na(indivData)) #False
dim(indivData) #262 43

##Rel.ship
# "" rauswerfen
unique(indivData$PartnershipStatus)
length(unique(indivData$PartnershipStatus))#5
indivData<-subset(indivData, PartnershipStatus!= "")# delete 2 ids #
length(unique(indivData$PartnershipStatus)) # test ok: 4
dim(indivData) #260 43
#don't trust no hoe zu singles
indivData$PartnershipStatus[indivData$PartnershipStatus == "don't trust no hoe"] <- "single"
unique(indivData$PartnershipStatus)#test ok
dim(indivData) #260 43 ok

##AGE: Minderjaehriege entfernen (Outlier)
min(indivData$Age)
max(indivData$Age)
length(unique(indivData)) # get number of age levels for number of bins 43
sort(indivData$Age) #1 11-Jähriger und 1 88-Jähriger
subset(indivData, indivData$Age=="11") #ID 140
subset(indivData, indivData$Age=="88") #ID 211
#plots
#plot(indivData$Age) #hauptsächlich zwischen 20 und 30 Jahre alt
#boxplot(indivData$Age) #die 40-60 Jährigen aber drin lassen
ggplot(data = indivData, aes(x = Age)) + #fill: variable for differencing ('target)
  geom_histogram(bins = 43, col = "white", fill ="turquoise4") + # position dodge: next to each other
  labs(x = "Age", y = "Frequency") +
 # grid(TRUE)+
  scale_x_continuous(breaks = seq(0, 90, by = 5)) +
  theme_classic(base_size = 10)# change size of text
  ggsave(file="age_ditribution.png", width=8, height=3, dpi=600) 
#Outlier eleminieren
indivData <- subset(indivData, indivData$Age>15 & indivData$Age<81)
dim(indivData) #Jetzt nur noch 258

#ID Abgleich: Schnittmenge ermitteln 
#nur IDs in indivData lassen, die auch in data.eval vorkommen
length(unique(data.eval$id_unique)) # Anzahl Ids in eval.data 264
length(unique(indivData$id_unique)) # Anzahl Ids in individual.data 258
idList<- unique(data.eval$id_unique) # Liste mit den ids aus long.data
indivData <- subset(indivData, indivData$id_unique %in% idList)# anpassung der Ids
length(unique(indivData$id_unique))# 257
#"umgekehrte Richtung" zu Sicherheit: nur IDs in data.eval lassen die auch in IndivData vorkommen
length(unique(data.eval$id_unique)) # Anzahl Ids in eval.data 264
length(unique(indivData$id_unique)) # Anzahl Ids in individual.data 257
idList<- unique(indivData$id_unique) # Liste mit den ids aus long.data
data.eval <- subset(data.eval, data.eval$id_unique %in% idList)# anpassung der Ids
length(unique(data.eval$id_unique)) # wir haben noch 257 von 266 IDs

dim(indivData) #257  43
dim(data.eval) #1395   25
#JETZT KOENNEN WIR MIT DIESEN 257 IDs WEITER ARBEITEN! :)

#Mean for Evaluations
#merge to even longer format
data.longer <- melt(data.eval, id.vars = c("id_unique", "ID", "Sample", "City"),
                    variable.name = "attribute")
head(data.longer)
dataMean <- aggregate(data.longer[, "value"], na.rm=TRUE, 
                      by = list(attribute = data.longer$attribute), 
                      FUN = mean) #oder median
dataMean

# #Outliers: => besser outliers bei befragten-merkmalen anweden z.B. Age um Minderj?hriege oder sehr alte auszuschlie?en
# ggplot(data = data.longer, aes(x = attribute, y = value)) +
#   geom_boxplot() +
#   facet_wrap(attribute~., ncol = 7) + #to plot in 7 columns
#   stat_summary(fun = mean, geom = "point", color = "darkred") +
#   labs(y = "") +
#   theme_classic()
# ggsave("Boxplot all Attributes.png", device="png", width = 16, height = 3)
#Attribute aufteilen in zwei Plots, übersichtlicher. But How?
#Prefs muss raus, da andere Skalierung als die Evals!
#removed 38 rows



# #Corrplot for Correlations
# corrplot(cor(data.eval[, -c(1,2,3,4)]),
#          method = "number", 
#          type = "upper",
#          number.cex = 0.8,
#          tl.cex = 0.9)  
# 
# #Barplot pro Sample. ähnliche Verteilung
# ggplot(data = data.longer, aes(y = attribute, x = value)) +
#   geom_bar(stat = "summary", fun = "mean") + # to plot the mean
#   geom_vline(xintercept = 3, linetype = "dashed") +
#   facet_wrap(Sample~.) +
#   scale_x_continuous(limits = c(0, 5), breaks = c(0:5)) +
#   labs(x = "", y = "") +
#   theme_bw()
# #removed 515 rows ?!
# 
# 
ggplot(data = data.longer, aes(y = City, x = value)) +
  geom_bar(stat = "summary", fun = "mean") +
  geom_vline(data = dataMean, aes(xintercept = x),
             linetype = "dashed") +
  facet_wrap(attribute~.) +
  scale_x_continuous(limits = c(0, 5), breaks = c(0:5)) +
  labs(x = "", y = "") +
  theme_bw()
# #Pref muss raus, weil Pref von 1-7 geht, die Evals von 1-5!!! #and removed 515 rows ?!
# 
# ggplot(data = data.longer, aes(y = City, x = value)) +
#   geom_bar(stat = "summary", fun = "mean") + 
#   geom_vline(data = dataMean, aes(xintercept = x),
#              linetype = "dashed") +
#   facet_wrap(attribute~.) +   
#   scale_x_continuous(limits = c(0, 5), breaks = c(0:5)) +
#   labs(x = "", y = "") +
#   theme_bw()

#save Plots and Tables for respondent characteristics
#Age
summary(indivData$Age) #average 25 years old, 18 Minimum, 63 Maximum
#png("Age.png", width=300, height=400) #Variante 1
#plot(indivData$Age)
#dev.off()
#png("Age2.png", width=300, height=400) #Varante 2
# hist(indivData$Age,
#      main="",
#      xlab="Age",
#      ylab="Frequency")
# dev.off()

#Gender
indivData[,"Gender"] <- as.factor(indivData[,"Gender"])
#png("Gender.png", width=300, height=450)
plot(indivData$Gender)
#dev.off()
summary(indivData$Gender) #148 Female, 110 Male
148/258

#Occupation
indivData[,"Occupation"] <- as.factor(indivData[,"Occupation"])
summary(indivData$Occupation) #128 Master student, 64 Bachelor student, 50 Employed, 6 Self Employed,
#5 PhD student,..
prop.table(table(indivData$Occupation))

#Number of Trips
summary(indivData$Number_of_Trips) #Mean 3 -> 2-3 times

table(indivData$Number_of_Trips, indivData$PartnershipStatus)

#Nationality
indivData[,"Nationality"] <- as.factor(indivData[,"Nationality"])
summary(indivData$Nationality) #119 German, 37 Russian, 25 Bulgarian, 9 British, 9 Taiwanese
#5 Hungarian, 4 Spanish, Chinese and Danish, 3 American,..
length(unique(indivData$Nationality)) #41 Nationalitäten
prop.table(table(indivData$Nationality))

#Partnership
indivData[,"PartnershipStatus"] <- as.factor(indivData[,"PartnershipStatus"])
summary(indivData$PartnershipStatus) #134 relationship, 105 single,, 17 married,..
prop.table(table(indivData$PartnershipStatus))
#CurrentCity
indivData[,"CurrentCity"] <- as.factor(indivData[,"CurrentCity"])
summary(indivData$CurrentCity) #123 Berlin most 
123/258
length(unique(indivData$CurrentCity)) #76

#Average Budget
summary(indivData$Avg_Budget) #2.039 mean


#city preferences and favourires???
#preferences
average_pref <- round(by(data.eval$Pref,data.eval$City, mean),1)
average_pref<- as.list(average_pref)
average_pref <- do.call(rbind,average_pref)
average_pref <- as.data.frame(average_pref)
#average_pref <- cbind(city = rownames(average_pref),average_pref)
#rownames(average_pref) <- 1:nrow(average_pref)# new index
average_pref

#Travel destination: Where have you been?
colnames(indivData[,4:23]) # only city names
df = data.frame()# new dataframe
for (i in colnames(indivData[,4:23])){
  city = i
  sum = sum(indivData[[i]])
  df = rbind(df, data.frame(city,sum))
}
df$city<-as.character(df$city)
df <- df[order(df$city),] #order alphabetically
df <- cbind(df, average_pref)
df <- df[order(as.integer(df$sum),decreasing = FALSE), ] # sort 
#write.csv(df, "favourite_cities.csv")

#Barplot df
ggplot(df, aes(x=reorder(city,-sum), y=sum)) + 
  geom_bar(stat = "identity", fill='turquoise4') +
  geom_text(aes(label=V1)) +
  labs(x = "", y = "Frequency of Visits") +
  theme_classic(base_size = 11) + # change size of text
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
ggsave(file="city_ditribution.png", width=8, height=4, dpi=600)  

#--------------------------------------------------alter code f�r travel destination
# #Travel destination: Where have you been?
# #sum(indivData$Berlin) # sum over column
# colnames(indivData[,4:23]) # only city names
# df = data.frame()# new dataframe
# for (i in colnames(indivData[,4:23])){
#   city = i
#   sum = sum(indivData[[i]])
#   df = rbind(df, data.frame(city,sum))
# }
# #df <- df[order(as.integer(df$sum),decreasing = FALSE), ] # sort 
# df
# 
# #Barplot df
# ggplot(df, aes(x=reorder(city,-sum), y=sum)) + 
#   geom_bar(stat = "identity", fill='turquoise4') +
#   labs(x = "", y = "Frequency of Visits") +
#   theme_classic(base_size = 11) + # change size of text
#   theme(axis.text.x=element_text(angle = 45, hjust = 1))
#   ggsave(file="city_ditribution.png", width=8, height=4, dpi=600)  

# #Travel destination: Where have you been?
# Berlin_count <- count(subset(indivData, indivData$Berlin=="1"))      #230 von 258 waren in Berlin 
# 230/258 #89.15%
# Paris_count <- count(subset(indivData, indivData$Paris=="1"))       #166
# 166/258 #64.34%
# London_count <- count(subset(indivData, indivData$London=="1"))      #170
# 170/258 #65.89%
# Barcelona_count <- count(subset(indivData, indivData$Barcelona=="1"))   #145
# Madrid_count <- count(subset(indivData, indivData$Madrid=="1"))      #57
# Rome_count <- count(subset(indivData, indivData$Rome=="1"))        #118
# Stockholm_count <- count(subset(indivData, indivData$Stockholm=="1"))   #67
# Amsterdam_count <- count(subset(indivData, indivData$Amsterdam=="1"))   #143
# Prague_count <- count(subset(indivData, indivData$Prague=="1"))      #154
# 154/258 #59.69%
# Budapest_count <- count(subset(indivData, indivData$Budapest=="1"))    #93
# Lisbon_count <- count(subset(indivData, indivData$Lisbon=="1"))      #46
# Brussels_count <- count(subset(indivData, indivData$Brussels=="1"))    #82
# Vienna_count <- count(subset(indivData, indivData$Vienna=="1"))      #135
# StPetersburg_count <- count(subset(indivData, indivData$StPetersburg=="1"))#55
# Krakow_count <- count(subset(indivData, indivData$Krakow=="1"))      #45
# Riga_count <- count(subset(indivData, indivData$Riga=="1"))        #37
# Istanbul_count <- count(subset(indivData, indivData$Istanbul=="1"))    #59
# Geneva_count <- count(subset(indivData, indivData$Geneva=="1"))      #35
# Athens_count <- count(subset(indivData, indivData$Athens=="1"))      #48
# Dublin_count <- count(subset(indivData, indivData$Dublin=="1"))      #43
# 
# names(indivData)
# 
# ### HILFE: die 20 Städte in einem neuen Dataframe darstellen -> Plot, in dem zu sehen ist, welche Stadt ###
# ### am häufigsten besucht wurde:
# list_Cities <- list(v1=indivData[1,4:23], v2=c(Berlin_count, Paris_count,London_count,Barcelona_count,
#                                                Madrid_count,Rome_count,Stockholm_count,Amsterdam_count,
#                                                Prague_count,Budapest_count, Lisbon_count,Brussels_count,
#                                                Vienna_count,StPetersburg_count, Krakow_count,Riga_count,
#                                                Istanbul_count,Geneva_count, Athens_count,Dublin_count))
# 
# 
# #d <- data.frame(id=c("id1","id2","id3","id4","id15","id6","id7","id8","id9","id10","id11","id12","id13",
# #                     "id14","id15","id16","id17","id18", "id19", "id20"),
# #          x=c(Berlin_count,Paris_count,London_count,Barcelona_count,
# #              Madrid_count,Rome_count,Stockholm_count,Amsterdam_count,
# #              Prague_count,Budapest_count, Lisbon_count,Brussels_count,
# #              Vienna_count,StPetersburg_count, Krakow_count,Riga_count,
# #              Istanbul_count,Geneva_count, Athens_count,Dublin_count),
# #          y=indivData[1,4:23])
# #funktioniert noch nicht..

#-------------------------------------------------------------------------------------------

#Purpose
count(subset(indivData, indivData$Purpose1=="1")) #54 Visit family
54/258 #20.93%
count(subset(indivData, indivData$Purpose2=="1")) #131 Visit friends
131/258 #50.77%
count(subset(indivData, indivData$Purpose3=="1")) #63 Partying
63/258 #24.42%
count(subset(indivData, indivData$Purpose4=="1")) #232 Exploring a new city
232/258 #89.92%
count(subset(indivData, indivData$Purpose5=="1")) #112 Visiting various cultural events
112/258 #43.41%
count(subset(indivData, indivData$Purpose_Other=="1")) #0

#With whom
count(subset(indivData, indivData$With_Whom_1=="1")) #112 with family
count(subset(indivData, indivData$With_Whom_2=="1")) #216 with friends
count(subset(indivData, indivData$With_Whom_3=="1")) #146 with a partner
count(subset(indivData, indivData$With_Whom_4=="1")) #16 with collegues
count(subset(indivData, indivData$With_Whom_5=="1")) #69 by yourslelf


###################################################################
# Am Ende von Task 1 Datensätze für die anderen Aufgaben speichern.
data.eval[, -c(1, 2, 3, 4)] <- scale(data.eval[, -c(1, 2, 3, 4)])
summary(data.eval)
#mainPath <- file.path("C:/Users/Lilli/Google Drive/2021CACI/Assignment02/CACI_Assignment2")
#setwd(mainPath)
#getwd()
write.csv(data.eval, file = "dataEvalScale.csv", row.names = FALSE)
write.csv(indivData, file = "indivData.csv", row.names = FALSE)

