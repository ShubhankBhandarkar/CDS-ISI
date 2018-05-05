# Package used to read JSON file - jsonlite
# The fromJSON() function was used to store the data in a dataframe
# Number of Cuisine clusters : 17 (From the Kmeans withiSS curve obtained)
# The elboe was obtained on 17 clusters (The difference in withiss fromm 17 to 18 was very low)
library(jsonlite)
library(quanteda)
library(dplyr)
library(doSNOW)

df = fromJSON('cuisine.json.txt', flatten =  TRUE)

# cLeaning the data
# ==================================================================================================
df$ingredients = gsub(' ', "_",df$ingredients)
df$ingredients = tolower(df$ingredients)
length(which(!complete.cases(df$ingredients))) # No blank rows

ingred_tokens = tokenize(df$ingredients, what = 'word', remove_numbers = FALSE, verbose = TRUE,
                         remove_punct = TRUE)

ingred_dfm = dfm(ingred_tokens, tolower = FALSE)
ingred_token_df = as.data.frame(ingred_dfm)
ingred_token_df = as_tibble(ingred_token_df[,-1]) #remove the letter c

# ==============================================================================================================
# Removing sparse terms
# ==============================================================================================================
ingred_no = colSums(ingred_token_df)
ingred_token_df = ingred_token_df[,-12] # Removing Salt which has a very high occurence
length(which(ingred_no > 5))
ingred = ingred_token_df[, which(ingred_no>5)]
ingred_no = colSums(ingred)
ncol(ingred)
summary(ingred_no)

#===========================================================================================================
# CLustering on the basis of ingredients
# K-means
library(doSNOW)
ingred_400 = ingred_token_df[, which(ingred_no>400)]
dim(ingred_350)
ingred_no = colSums(ingred_token_df)
length(which(ingred_no>350))
ingred_350 = ingred[, which(ingred_no>350)]
ingred_400 = ingred[, which(ingred_no>400)]


# Centers = 1:25
start_time = Sys.time()
cl = makeCluster(7, type = "SOCK")
registerDoSNOW(cl)

kmfitpar = foreach(i = 1:25) %dopar%{
  kmeanfit = kmeans(ingred_400, centers = i, nstart = 20)
}
stopCluster(cl)
Time_taken = Sys.time() - start_time  


withinSS = sapply(kmfitpar, function(result){result$tot.withinss})
betweenSS = sapply(kmfitpar, function(result){result$betweenss})

plot(1:25, withinSS, col = 'red', type = 'b')
points(1:25, betweenSS, type = 'b')


#=================================================================================================================
#t-SNE
library(Rtsne)
cuisineTSNE <- Rtsne(ingred, check_duplicates = FALSE, 
                     pca = FALSE, perplexity = 30, theta = 0.5, dims = 2)

# Applying kmeans on tSNE
start_time = Sys.time()
cl = makeCluster(7, type = "SOCK")
registerDoSNOW(cl)

kmfit_tsne = foreach(i = 1:25) %dopar%{
  kmeanfit = kmeans(cuisineTSNE$Y, centers = i, nstart = 20)
}
stopCluster(cl)
Time_taken = Sys.time() - start_time  

withinSS = sapply(kmfit_tsne, function(result){result$tot.withinss})
betweenSS = sapply(kmfit_tsne, function(result){result$betweenss})

plot(1:25, withinSS, type = 'b')

# The number of clusters was found to be 14 by observing the elbow of the plot

