setwd("C:/Users/Choi Sung Wook/Desktop")
data = read.table("ex31.txt", header=T)
View(data)

d2 = dist(data, method="euclidean")
d2
d1 = dist(data, method="manhattan")
d1
?dist

#1. 단일연결법=최단연결법
single = hclust(dist(data, method="manhattan"),
                method="single")
plclust(single) # 군집화 과정을 그래프화(Dendrogram)
plclust(single, hang=-1) # 위 과정을 밑으로 쭉 내려서

cutree(single,2) # 군집 몇 개로 나눌지 지정 - id 부여

#2. 완전연결법=최장연결법
complete = hclust(dist(data, method="manhattan"),
                  method="complete")
plclust(complete)
plclust(complete, hang=-1)
cutree(complete,2)
cutree(complete,3)

#3. 평균연결법
average = hclust(dist(data, method="manhattan"),
                 method="average")
plclust(average)
cutree(average,3)

#4. 중심연결
centroid = hclust(dist(data, method="manhattan"),
                  method="centroid")
plclust(centroid)

# 분할분석

library(cluster)
diana = diana(data, metric="manhattan")
plot(diana)
cutree(diana,3)

# K-means clustering

new_data = data.matrix(data, rownames.force=NA) # Convert a dataframe to a numeric matrix) 
average = hclust(dist(new_data), method="average")
initial = tapply(new_data, 
                 list(rep(cutree(average,2), ncol(new_data)), col(new_data)),
                 mean)
kmeans(new_data, initial, algorithm="MacQueen")

