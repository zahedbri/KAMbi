library(tidyverse) 
library(data.table)
library(skimr)
library(sqldf)
library(dplyr)
library(corrplot)
library(pastecs)
library(psych)
library(cluster)
library(RColorBrewer)
library(ggpubr)
library(factoextra)
library(Hmisc)
library(fpc)

####################################### IMPORTING DATA #######################################

setwd("")

working_competence <- read.csv(file='ib_delo_kompetenca.txt', header = TRUE, sep="#")

area <- read.csv(file='ib_podrocje.txt', header = TRUE, sep = "#")

questions <-read.csv(file = 'ib_vprasanje.txt', header=TRUE, sep="#")

answers <- read.csv(file = 'ib_odgovor.txt', header=TRUE, sep="#")

links_competences <- read.csv(file = 'ib_poklic_povezave.txt', header=TRUE, sep="#")

candidate_profession <- read.csv(file = 'ib_kandidat_poklic.txt', header=TRUE, sep="#")

profession_characteristics <- read.csv(file = 'ib_poklic_znacilnosti.txt', header=TRUE, sep="#")

candidate <- read.csv(file = 'ib_kandidat.txt', header=TRUE, sep="#")

profession <- read.csv(file='ib_poklic.txt', header = TRUE, sep = '#')


####################################### QUICK ANALYSIS #######################################

links_competences <- sqldf('select * from links_competences order by ID_POKLIC asc')

any(is.na(candidate))

sqldf('select count(distinct ID_KANDIDAT) from candidate')

sqldf('select count(distinct USER_IP) from candidate')

any(is.na(candidate_profession))

colnames(answers)
head(answers)
tail(answers)

glimpse(answers)
glimpse(candidate_profession)

####################################### INITIALIZATION OF VARIABLES FOR STATSTICAL ANALYSIS #######################################

nRows <- dim(candidate)[1]
nColumns <- dim(questions)[1]
nQuestions <- dim(questions)[1]
nAreas <- dim(area)[1]

area_inite_index <- c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,44,47,50,53,56,58,62,65,68,71,74,77)
area_finite_index <- c(3,6,9,12,15,18,21,24,27,30,33,36,39,43,46,49,52,55,57,61,64,67,70,73,76,78)

vector_answers  <- as.numeric(unlist(sqldf("select CASE
WHEN ODGOVOR = 'NE' THEN 0.0
WHEN ODGOVOR = 'NIKOLI' THEN 0.0
WHEN ODGOVOR = 'DA' THEN 1.0
WHEN ODGOVOR = 'VEDNO' THEN 1.0
WHEN ODGOVOR = 'NAJPOGOSTEJE' THEN 0.75
WHEN ODGOVOR = 'VČASIH DA/VČASIH NE' THEN 0.50
WHEN ODGOVOR = 'REDKO' THEN 0.25
END AS tocke
from answers order by ID_KANDIDAT,ID_VPRASANJE")))

names(vector_answers) <- NULL

# Answers

matrix_answers <- t(matrix(vector_answers,
                        nrow = nColumns,
                        ncol = nRows))
head(matrix_answers)

options(max.print = 10000)
any(is.na(matrix_answers))
glimpse(matrix_answers)

# Velocity

vector_velocity <- as.numeric(unlist(sqldf("select CAS from answers order by ID_KANDIDAT,ID_VPRASANJE")))
names(vector_velocity) <- NULL
matrix_velocity <- t(matrix(vector_velocity,
                        nrow = nColumns,
                        ncol = nRows))

# Fields

unique(area) 

f_area <- function(answers, area_inite_index, area_finite_index) {
  result <- replicate(nAreas,0)
  for(j in 1:nAreas) {
    result[j] <- sum(answers[area_inite_index[j]:area_finite_index[j]])
  }
  return (result)
}

work_characteristic <- area$OPIS[1:13]
competences <- area$OPIS[14:26]

matrix_fields <- matrix (0L, nrow = nRows, ncol = nAreas)
colnames(matrix_fields) <- seq(1, nAreas)

for(j in 1:nRows) {
  matrix_fields[j,] <- f_area(
    matrix_answers[j,1:nQuestions],
    area_inite_index,area_finite_index)
}

any(is.na(matrix_fields))
glimpse(matrix_fields)

####################################### DESCRIPTIVE STATISTIC #######################################


# Answers
mean(matrix_answers[,1:39])
mean(matrix_answers[,40:78])

par(mfrow = c(1,1))

hist(matrix_answers[,40:78], col = "red",
     main = "Frequency of answers for the first group of questions",
     xlab = "DA/NE", breaks = 2, labels = TRUE, freq=TRUE)

hist(matrix_answers[,40:78], col = "red",
     main = "Frequency of answers for the second group of questions",
     xlab = "Never / Rarely / Sometimes yes - Sometimes no / Often / Always", breaks = 5, labels = TRUE, freq=TRUE)

summary(matrix_answers)
stat.desc(matrix_answers)

# Velocity
describe(matrix_velocity)
par(mfrow = c(3,3))

for(i in 1:9) {
  hist(matrix_answers[,i], col="red",
       main=questions$BESEDILOVPR[i],
       xlab = "NO / YES", breaks=2,
       labels = TRUE, freq=TRUE)
}

for(i in 10:18) {
  hist(matrix_answers[,i], col="red",
       main=questions$BESEDILOVPR[i],
       xlab = "NO / YES", breaks=2,
       labels = TRUE, freq=TRUE)
}

for(i in 19:27) {
  hist(matrix_answers[,i], col="red",
       main=questions$BESEDILOVPR[i],
       xlab = "NO / YES", breaks=2,
       labels = TRUE, freq=TRUE)
}

for(i in 28:36) {
  hist(matrix_answers[,i], col="red",
       main=questions$BESEDILOVPR[i],
       xlab = "NO / YES", breaks=2,
       labels = TRUE, freq=TRUE)
}

for(i in 37:39) {
  hist(matrix_answers[,i], col="red",
       main=questions$BESEDILOVPR[i],
       xlab = "NO / YES", breaks=2,
       labels = TRUE, freq=TRUE)
}

par(mfrow = c(3,3))

for(i in 40:48) {
  hist(matrix_answers[,i], col="red",
       main=questions$BESEDILOVPR[i],
       xlab = "Never / Rarely / Sometimes yes - Sometimes no / Often / Always", breaks=5,
       labels = TRUE, freq=TRUE)
}

for(i in 49:57) {
  hist(matrix_answers[,i], col="red",
       main=questions$BESEDILOVPR[i],
       xlab = "Never / Rarely / Sometimes yes - Sometimes no / Often / Always", breaks=5,
       labels = TRUE, freq=TRUE)
}

for(i in 58:66) {
  hist(matrix_answers[,i], col="red",
       main=questions$BESEDILOVPR[i],
       xlab = "Never / Rarely / Sometimes yes - Sometimes no / Often / Always", breaks=5,
       labels = TRUE, freq=TRUE)
}

for(i in 67:75) {
  hist(matrix_answers[,i], col="red",
       main=questions$BESEDILOVPR[i],
       xlab = "Never / Rarely / Sometimes yes - Sometimes no / Often / Always", breaks=5,
       labels = TRUE, freq=TRUE)
}

for(i in 76:78) {
  hist(matrix_answers[,i], col="red",
       main=questions$BESEDILOVPR[i],
       xlab = "Never / Rarely / Sometimes yes - Sometimes no / Often / Always", breaks=5,
       labels = TRUE, freq=TRUE)
}

# Velocity
mean(matrix_velocity[,1:39])
mean(matrix_velocity[,40:78])

summary(matrix_velocity)
stat.desc(matrix_velocity)
describe(matrix_velocity)

par(mfrow = c(3,3))

for(i in 1:9) {
  hist(matrix_velocity[,i], col="orange",
       main=questions$BESEDILOVPR[i],
       xlim = c(0,30),
       breaks=max(matrix_velocity[,i]),
       labels = TRUE, freq=TRUE)
}

for(i in 10:18) {
  hist(matrix_velocity[,i], col="orange",
       main=questions$BESEDILOVPR[i],
       xlim = c(0,30),
       breaks=max(matrix_velocity[,i]),
       labels = TRUE, freq=TRUE)
}

for(i in 19:27) {
  hist(matrix_velocity[,i], col="orange",
       main=questions$BESEDILOVPR[i],
       xlim = c(0,30),
       breaks=max(matrix_velocity[,i]),
       labels = TRUE, freq=TRUE)
}

for(i in 28:36) {
  hist(matrix_velocity[,i], col="orange",
       main=questions$BESEDILOVPR[i],
       xlim = c(0,30),
       breaks=max(matrix_velocity[,i]),
       labels = TRUE, freq=TRUE)
}

for(i in 37:39) {
  hist(matrix_velocity[,i], col="orange",
       main=questions$BESEDILOVPR[i],
       xlim = c(0,30),
       breaks=max(matrix_velocity[,i]),
       labels = TRUE, freq=TRUE)
}

par(mfrow = c(3,3))

for(i in 40:48) {
  hist(matrix_velocity[,i], col="orange",
       main=questions$BESEDILOVPR[i],
       xlim = c(0,30),
       breaks=max(matrix_velocity[,i]),
       labels = TRUE, freq=TRUE)
}

for(i in 49:57) {
  hist(matrix_velocity[,i], col="orange",
       main=questions$BESEDILOVPR[i],
       xlim = c(0,30),
       breaks=max(matrix_velocity[,i]),
       labels = TRUE, freq=TRUE)
}

for(i in 58:66) {
  hist(matrix_velocity[,i], col="orange",
       main=questions$BESEDILOVPR[i],
       xlim = c(0,30),
       breaks=max(matrix_velocity[,i]),
       labels = TRUE, freq=TRUE)
}

for(i in 67:75) {
  hist(matrix_velocity[,i], col="orange",
       main=questions$BESEDILOVPR[i],
       xlim = c(0,30),
       breaks=max(matrix_velocity[,i]),
       labels = TRUE, freq=TRUE)
}

for(i in 76:78) {
  hist(matrix_velocity[,i], col="orange",
       main=questions$BESEDILOVPR[i],
       xlim = c(0,30),
       breaks=max(matrix_velocity[,i]),
       labels = TRUE, freq=TRUE)
}

# Fields

mean(matrix_fields)
stat.desc(matrix_fields)
describe(matrix_fields)

par(mfrow = c(3,3))

for(i in 1:9) {
  hist(
    matrix_fields[,i], col = "orange",
    main=area$OPIS[i],
    xlim = c(0,4),
    breaks=8,
    labels = TRUE, freq=TRUE)
}

for(i in 10:18) {
  hist(
    matrix_fields[,i], col = "orange",
    main=area$OPIS[i],
    xlim = c(0,4),
    breaks=8,
    labels = TRUE, freq=TRUE)
}

for(i in 19:26) {
  hist(
    matrix_fields[,i], col = "orange",
    main=area$OPIS[i],
    xlim = c(0,4),
    breaks=8,
    labels = TRUE, freq=TRUE)
}

####################################### RESEARCH #######################################

# Similarity

f_similarity <- function(length,vec1,vec2) {
  result <- 0
  for(j in 1:length) {
    if (vec1[j] == vec2[j] ) {
      result <- result+1}
  }
  return (result)
}

similarity_matrix <- matrix(0L, nrow = nRows, ncol = nRows)

for(i in 1:nRows) {
  for(j in 1:nRows){  
    similarity_matrix[i,j] = f_similarity(nQuestions, as.vector(matrix_answers[i,1:nQuestions]),as.vector(matrix_answers[j,1:nQuestions]))/nQuestions
  }
}

par(mfrow = c(1,1))
plot(similarity_matrix)

heatmap (similarity_matrix,
         scale="column",
         xlab="Candidate",
         ylab="",
         main="Similarity matrix",
         col =  colorRampPalette(brewer.pal(8, "Reds"))(25))

legend(x="bottomright", legend=c("min", "avg", "strong", "max"),
       fill=colorRampPalette(brewer.pal(8, "Reds"))(4))

heatmap(similarity_matrix[1:100,1:100],
        scale="column",
        xlab="Candidate",
        ylab="",
        main="Similarity matrix 100x100",
        col =  colorRampPalette(brewer.pal(8, "Reds"))(25))

legend(x="bottomright", legend=c("min", "avg", "strong", "max"),
       fill=colorRampPalette(brewer.pal(8, "Reds"))(4))


# Corelation

cor_answers <- rcorr(matrix_answers)
cor_fields <- rcorr(matrix_fields)

par(mfrow = c(1,1))
corrplot(cor_answers$r, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45,
         main = "Corelation between nswers")

corrplot(cor_answers$P, type = "upper", order = "hclust",
         p.mat = cor_answers$P, sig.level = 0.01, insig = "blank",
         main = "The significance of the connection between answers")

corrplot(cor_fields$r, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45,
         main = "Corelation between fields")

corrplot(cor_fields$P, type = "upper", order = "hclust",
         p.mat = cor_fields$P, sig.level = 0.01, insig = "blank",
         main = "The significance of the connection between fields")

options(warn = -1)
set.seed(8808,"Mersenne")

# Cluster analysis

gap_answers <- clusGap(matrix_answers, FUN = kmeans, K.max = 10, B=100)
print(gap_answers, method = "firstmax")
# execute in RConsole
# fviz_gap_stat(gap_answers)
plot(gap_answers)

# Elbow
set.seed(123)
k.max<-15
wss<-sapply(1:k.max, function(k){kmeans(matrix_answers, k, nstart=50,iter.max = 15 )$tot.withinss})
plot (1:k.max, wss,
      type="b", pch = 19, frame= FALSE,
      xlab = "Number of clusters",
      ylab = "Total")

cluster_answers <- kmeans(matrix_answers, 2)
print(cluster_answers)
plotcluster(matrix_answers, cluster_answers$cluster)
# fviz_cluster(cluster_answers, data = matrix_answers,
#              palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
#              geom = "point",
#              ellipse.type = "convex",
#              ggtheme = theme_bw())


gap_fields <- clusGap(matrix_fields, FUN = kmeans, K.max = 10, B = 100)
print(gap_fields, method = "firstmax")

#fviz_gap_stat(gap_fields)
cluster_fiuelds <- kmeans(matrix_fields, 2)

#fviz_cluster(cluster_fiuelds, data = matrix_fields, main = "Clusters of fields")
plotcluster(matrix_fields, cluster_fiuelds$cluster)

####################################### DATA CLEANSING #######################################

low_limit <- 380
high_limit <- 624

valid_surveys <- subset(matrix_answers , rowSums(matrix_velocity[,1:78]) > low_limit & rowSums(matrix_velocity[,1:78])<high_limit)
length(valid_surveys)

length(valid_surveys)

# CLUSTER 2
gap_answers2 <- clusGap(valid_surveys, FUN = kmeans, K.max = 10, B=100)
print(gap_answers2, method = "firstmax")
# for RConsole
# fviz_gap_stat(gap_answers)
plot(gap_answers)

cluster_answers2 <- kmeans(valid_surveys, 2)
print(cluster_answers2)
plotcluster(valid_surveys, cluster_answers2$cluster)

# fviz_cluster(cluster_answers2, data = valid_surveys,
#              palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
#              geom = "point",
#              ellipse.type = "convex",
#              ggtheme = theme_bw())

