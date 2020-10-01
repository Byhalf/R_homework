source("load-mov-data.R")
library(data.table)
library('dendextend')

setDT(examples)
setDT(votes)
setDT(movies)
setDT(users)


################ exercice 1 ##################

#conditional entropy
#H(Y|X)=H(Y,X)-H(X)=H(Y)-I(X;Y)
#I(X;Y)=H(X)+H(Y)-H(X,Y)

p.y  = table(examples$occupation)/nrow(examples)
H.Y = - sum(p.y * log2(p.y))
p.x = table(votes$rating)/nrow(examples)
H.X = - sum(p.x * log2(p.x))

ct <- table(votes$rating,examples$occupation)
p.xy = ct/ nrow(examples)

H.XY = - sum(p.xy * log2(p.xy))
H.conditYX = H.XY - H.X

I.XY = H.X + H.Y - H.XY
verif = H.Y - I.XY
#print(verif == H.conditYX)

print(paste(c("entropy of H(OCCUPATION|RATING) = ",H.conditYX)))

#################  exercice 2 #################
ans <- votes[, .(.N), by = .(movie)]
#ans = ans[order(-N)]

rated_67 = ans[N==67]

movies_id67 = rated_67[,-2]


movies_nbrs = movies[,c("movie","title")]
rated_67_titles <-merge(movies_nbrs,movies_id67,by="movie")

#examples.67 <- examples[examples$movie %in% rated_67_titles$movie]
examples.67 <-merge(examples,rated_67_titles,by="movie")
#regexp to format the labels
rated_67_titles$title = gsub("[(].*[)]","",rated_67_titles$title)
rated_67_titles$title = gsub("(.*?\\s.*?\\s)","\\1\n",rated_67_titles$title)


means <- tapply(examples.67$rating,examples.67$movie,mean)
par(mar=c(10,3,4,3), las=2)

boxplot(examples.67$rating ~ examples.67$movie,main="Movies rated 67 times",
names=rated_67_titles$title)
points(means, col="red", pch=16)

################  Exercice 3 ##############
#a

a = table(votes$user,votes$rating)
a = as.data.frame.matrix(a) 

names(a) <- c("ONE","TWO","THREE","FOUR","FIVE")
fr_rating = a/rowSums(a)


fr_rating = round(fr_rating,2)
users = cbind(users,fr_rating)

names(users)
#b
d <- users[,c("age","ONE","TWO","THREE","FOUR","FIVE")]

hc.average <- hclust(dist(d), method = "average")
hcda <- as.dendrogram(hc.average)
head(hcda)
#c
hc <- cutree(hcda, k=20)
head(hc)
#hc

#d
#compute the number of users in each cluster
nbr_users = vector()
for (i in 1:20) {
    nbr_users <- c(nbr_users,nrow(d[hc==i,]))
}

#compute the average age of users in each cluster
avr_age_users = vector()
for (i in 1:20) {
    if(nrow(d[hc==i,])>0){
        avr_age_users <- c(avr_age_users,sum(d[hc==i,age])/nrow(d[hc==i,]))
    }else{
        avr_age_users <- c(avr_age_users,0)
    }
 }

#numbers of duplicates
nbr_duplicates = vector()
for(i in 1:20){
      nbr_duplicates <- c(nbr_duplicates, sum(duplicated(users[hc==i,c(2,6:10)])))
}


res <- data.frame(cbind(1:20,nbr_users, avr_age_users,nbr_duplicates))
names(res) <- c("cluster","nbr of users","avr age of users","nbr of duplicates")
res