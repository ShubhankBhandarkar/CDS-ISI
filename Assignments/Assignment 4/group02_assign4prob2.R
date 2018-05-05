# Group 2
# Demographic clusters
# Number of Optimal Clusters = 6
# Major clustering parameters : Gender, Height, Weight (Height & Weight are highly correlated to gender) 
# Gender is found to be a deciding factor in preferences and interests 
library(dplyr)
columns = read.csv('columns.csv')
responses = read.csv('responses.csv')
responses_clone = responses
responses = as_tibble(responses_clone)
ncol(responses)
str(responses)
levels(responses)

# Replacing missing data with the mode for categorical variables
# The responses are scaled on a 1-5 scale
#================================================================================================================

print(responses, n=20)
factors = c(names(which(sapply(responses_clone, class) =='factor'))) #Smoking, Alcohol, Punctuality, Lying, Internet.usage, 
# Lef-Right handed, Education, Only child, Village - Town, House/ Block of Flats, Gender

# Smoking
table(responses$Smoking) # 1 -Current Smoker, 2 -Former Smoker, 3 -never smokes, 4 -tried smoking
responses$Smoking = (as.numeric(responses$Smoking) -1) * 5 # 5-> Current Smoker
responses$Smoking[which(responses$Smoking == 0)] = 20 #Empty -> Mode
responses$Smoking[which(responses$Smoking == 15)] = 1 # 1->Never Smoked
responses$Smoking[which(responses$Smoking == 20)] = 3 # 3-> Tried smoking
responses$Smoking[which(responses$Smoking == 10)] = 4 # 4-> Former smoker
# Creating new features for each type
responses$Never_smoked = ifelse(responses$Smoking == 1,1,0)
responses$tried_smoking = ifelse(responses$Smoking == 3,1,0)
responses$fomer_smoker = ifelse(responses$Smoking == 4,1,0)
responses$current_smoker = ifelse(responses$Smoking == 5,1,0)

# Alcohol
table(responses$Alcohol) # 1 - drink a lot,2 - Never, 3 - Social Drinker
responses$Alcohol = (as.numeric(responses$Alcohol) - 1) *5 # 5-> drinks a lot
responses$Alcohol[which(responses$Alcohol == 0)] = 15 #Empty -> Mode
responses$Alcohol[which(responses$Alcohol == 10)] = 1 #1 -> Never
responses$Alcohol[which(responses$Alcohol == 15)] = 3 #3 -> Social Drinker
# Creating new features for each type
responses$never_drank = ifelse(responses$Alcohol == 1,1,0)
responses$social_drinker = ifelse(responses$Alcohol == 3,1,0)
responses$drunkard = ifelse(responses$Alcohol == 5,1,0)
sum(responses$drunkard)

# Punctuality
table(responses$Punctuality) # 1 - Always on time,2 - Often early, 3 - running late 
responses$Punctuality = (as.numeric(responses$Punctuality) - 1)*5 # 5 - Always on time
responses$Punctuality[which(responses$Punctuality == 0)] = 5 #Empty -> Mode
responses$Punctuality[which(responses$Punctuality == 15)] = 1 #1 -> Running Late
responses$Punctuality[which(responses$Punctuality == 10)] = 4 #4 -> Often early
# Creating new features for each type
responses$runs_late = ifelse(responses$Punctuality == 1,1,0)
responses$often_early = ifelse(responses$Punctuality == 4,1,0)
responses$on_time = ifelse(responses$Punctuality == 5,1,0)
sum(responses$on_time)

# Lying
table(responses$Lying) # 1 - Everytime it suits me, 2 - Never, 3 - To avoid hurting, Sometimes 
responses$Lying = (as.numeric(responses$Lying) - 1)*6
responses$Lying[which(responses$Lying == 0)] = 24 #Empty -> Mode
responses$Lying[which(responses$Lying == 6)] = 5 #5 -> Everytime it suits me
responses$Lying[which(responses$Lying == 12)] = 1 #1 -> Never
responses$Lying[which(responses$Lying == 18)] = 2 #2 -> Only to avoid hurting someone
responses$Lying[which(responses$Lying == 24)] = 4 #4 -> Sometimes
# Creating new features for each type
responses$never_lie = ifelse(responses$Lying == 1,1,0)
responses$avoid_hurt = ifelse(responses$Lying == 2,1,0)
responses$lie_sometimes = ifelse(responses$Lying == 4,1,0)
responses$lie_mostly = ifelse(responses$Lying == 5,1,0)
sum(responses$lie_mostly)

# Internet Usage
table(responses$Internet.usage) # 0 - Few hours a dy,1 - Less than an hour, 2 - most of the day,3 -No time 
responses$Internet.usage = (as.numeric(responses$Internet.usage) - 1)*6
responses$Internet.usage[which(responses$Internet.usage == 0)] = 4 #4 -> Few hours
responses$Internet.usage[which(responses$Internet.usage == 6)] = 2 #2 -> Less than an hour
responses$Internet.usage[which(responses$Internet.usage == 12)] = 5 #5 -> Most of the day
responses$Internet.usage[which(responses$Internet.usage == 18)] = 1 #1 -> No time
# Creating new features for each type
responses$no_internet = ifelse(responses$Internet.usage == 1,1,0)
responses$internet_lesshour = ifelse(responses$Internet.usage == 2,1,0)
responses$inter_fewhour = ifelse(responses$Internet.usage == 4,1,0)
responses$inter_most = ifelse(responses$Internet.usage == 5,1,0)
sum(responses$inter_fewhour)


# Left - Right Handed
table(responses$Left...right.handed) # 1 - Left, 2 - Right 
responses$Left...right.handed = (as.numeric(responses$Left...right.handed) - 1)*5 # 5 - Left handed
responses$Left...right.handed[which(responses$Left...right.handed == 0)] = 10 #Empty -> Mode
responses$Left...right.handed[which(responses$Left...right.handed == 10)] = 1 #1 -> Right Handed
# Creating new features for each type
responses$right_hand = ifelse(responses$Left...right.handed == 1,1,0)
responses$left_hand = ifelse(responses$Left...right.handed == 5,1,0)
sum(responses$right_hand)

# Education
table(responses$Education) # 1 - drink a lot,2 - Never, 3 - Social Drinker 
responses$Education = (as.numeric(responses$Education) - 1)*7
responses$Education[which(responses$Education == 0)] = 42 #Empty -> Mode
responses$Education[which(responses$Education == 21)] = 6 #6 -> Doctorate Degree
responses$Education[which(responses$Education == 14)] = 1 #1 -> Currently primary school
responses$Education[which(responses$Education == 28)] = 5 #5 -> Masters degree
responses$Education[which(responses$Education == 7)] = 4 #4 -> Bachelor degree
responses$Education[which(responses$Education == 42)] = 3 #3 -> Secondary school
responses$Education[which(responses$Education == 35)] = 2 #2 -> Primary school
# Creating new features for each type
responses$current_primary = ifelse(responses$Education == 1,1,0)
responses$primary_school = ifelse(responses$Education == 2,1,0)
responses$second_school = ifelse(responses$Education == 3,1,0)
responses$bachelor_degree = ifelse(responses$Education == 4,1,0)
responses$master_degree = ifelse(responses$Education == 5,1,0)
responses$docctorate = ifelse(responses$Education == 6,1,0)
sum(responses$docctorate)


# Only Child
table(responses$Only.child) # 1 - drink a lot,2 - Never, 3 - Social Drinker 
responses$Only.child = (as.numeric(responses$Only.child) - 1)*5 #5 -> No
responses$Only.child[which(responses$Only.child == 0)] = 5 #Empty -> Mode
responses$Only.child[which(responses$Only.child == 10)] = 1 #1 -> Yes
# Creating new features for each type
responses$only_child_yes = ifelse(responses$Only.child == 1,1,0)

# Village/ town
table(responses$Village...town) # 1 - drink a lot,2 - Never, 3 - Social Drinker 
responses$Village...town = (as.numeric(responses$Village...town) - 1) *5 #5 -> City
responses$Village...town[which(responses$Village...town == 0)] = 5 #Empty -> Mode
responses$Village...town[which(responses$Village...town == 10)] = 1 #1 -> Village
# Creating new features for each type
responses$Village = ifelse(responses$Village...town == 1,1,0)

# House / Block of Flats
table(responses$House...block.of.flats) # 1 - drink a lot,2 - Never, 3 - Social Drinker 
responses$House...block.of.flats = (as.numeric(responses$House...block.of.flats) - 1) * 5 #5 -> Flat
responses$House...block.of.flats[which(responses$House...block.of.flats == 0)] = 5 #Empty -> Mode
responses$House...block.of.flats[which(responses$House...block.of.flats == 10)] = 1 #1 -> House/Bungalow
# Creating new features for each type
responses$house_type = ifelse(responses$House...block.of.flats == 1,1,0)

# Gender
table(responses$Gender)
responses$Gender = (as.numeric(responses$Gender) - 1)*5 #5-> Female
responses$Gender[which(responses$Gender == 0)] = 5 #Empty -> Mode
responses$Gender[which(responses$Gender == 10)] = 1 #1 -> Male
# Creating new features for each type
responses$gender_male = ifelse(responses$Gender == 1,1,0)

# Drop Factor variables
responses = responses[, !colnames(responses) %in% factors]
#================================================================================================================
# Impute missing values
#================================================================================================================
library(DMwR)
responses_complete = data.frame(responses)
responses_complete = knnImputation(responses_complete, k = 5)

#=================================================================================================================
# Applying K-means
#================================================================================================================
#write.csv(responses_complete, file = 'responses_complete.csv')
library(doSNOW)
library(doParallel)

start_time = Sys.time()
cl = makeCluster(3, type = 'SOCK')
registerDoSNOW(cl)

kmeanfit = foreach(i = 1:25) %dopar%{
  kfit = kmeans(responses_complete, centers = i, nstart = 20)
}
stopCluster(cl)
Sys.time() - start_time

withinSS = sapply(kmeanfit, function(result){result$tot.withinss})
betweenSS = sapply(kmeanfit, function(result){result$betweenss})
clusters = sapply(kmeanfit, function(result){result$cluster})
clusters[,6]
kfit = kmeans(responses_complete, centers = 7, nstart = 20)
plot(1:25, withinSS, type = 'b', col = 'red')
points(1:25, betweenSS, type = 'b')
# Optimal number of clusters = 6
# The decrease in withinSS from 6 centres to 7 is very low. This suggests that there might be actually 
# 6 types of personalities

#=================================================================================================================
library(Rtsne)
tsne_fit = Rtsne(responses_complete, perplexity = 30, theta = 0.5, dims = 2, pca = FALSE)
plot(tsne_fit$Y, col = responses$gender_male+1)
# Both genders can be seen separated from each other.   
#=================================================================================================================
# Looking for effects of Gender in behaviours/preferences
# ==============================================================================================================
# Consulted Group 10 for this part for the visualization
library(ggplot2)
movies = responses_complete %>% select(Movies:Action)

##applying PCA on movies
movie_fit1 <- prcomp(movies, scale. = TRUE)
pcData1 <- data.frame(movie_fit1$x)
group1 = responses_complete$gender_male

vPCs2 <- movie_fit1$rotation[, 1:7] %>% as.data.frame()
multiple2 <- min( 
  (max(pcData1[,"PC1"]) - min(pcData1[,"PC1"]))/(max(vPCs2[,"PC1"])-min(vPCs2[,"PC1"])), 
  (max(pcData1[,"PC2"]) - min(pcData1[,"PC2"]))/(max(vPCs2[,"PC2"])-min(vPCs2[,"PC2"])) 
)

#Visualizing the effect of gender 
ggplot(pcData1, aes(x=PC1, y=PC2)) + 
  geom_point(aes(colour=group1))   + 
  coord_equal() + 
  geom_text(data=vPCs2, 
            aes(x = movie_fit1$rotation[, "PC1"]*multiple2*0.82, 
                y = movie_fit1$rotation[,"PC2"]*multiple2*0.82, 
                label=rownames(movie_fit1$rotation)), 
            size = 2, vjust=1, color="black") +
  geom_segment(data=vPCs2, 
               aes(x = 0, 
                   y = 0,
                   xend = movie_fit1$rotation[,"PC1"]*multiple2*0.8, 
                   yend = movie_fit1$rotation[,"PC2"]*multiple2*0.8), 
               arrow = arrow(length = unit(.2, 'cm')), 
               color = "grey30")
# Men -> 1, Women -> 0
# A clear distinction can be seen in the separation of the types of movies each gender prefers
# Example: Men prefer Action, Sci-fi, War movies, while women seems to like Romantic, Fairy tales more.
#================================================================================================================

# Conclusion : Gender is plays a mojor role in clustering

# Fittig a tree to see the other factors influencing clustering
library(rpart)
library(rpart.plot)

responses_complete$cluster = clusters[,6]
tree1 = rpart(cluster ~., data = responses_complete, minbucket = 10, method = "class")
rpart.plot(tree1, type = 4)

# Height and weight are seen to have a significant effect on the clustering
cor(responses_complete$Height, responses_complete$gender_male)
cor(responses_complete$Weight, responses_complete$gender_male)
# Height & weight are highly correlated to gender