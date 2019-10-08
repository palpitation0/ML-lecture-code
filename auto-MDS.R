library(xlsx)
data = read.xlsx("C:/Users/Choi Sung Wook/Desktop/대학원/머신러닝 dataset/mva/auto.xlsx", 1)
head(data)

X = data[,-1]
autoName = data[,1]

# scaling
Z = scale(X, center=TRUE, scale=TRUE)
maxX = apply(X, 2, max)
minX = apply(X, 2, min)
z01x = scale(X, center=minX, scale=maxX-minX)

# distance matrix
z01x.dist = dist(z01x, method="euclidean")
z01x.dist = as.matrix(z01x.dist)
colnames(z01x.dist) = autoName
rownames(z01x.dist) = autoName

# MDS 분석 적용
mds1 = cmdscale(z01x.dist, k=2)
plot(mds1[,1], mds1[,2], type="n", 
     xlab="", ylab="", main="cmdscale(Auto)")
text(mds1[,1], mds1[,2], rownames(z01x.dist), cex=0.9)
abline(h=0, v=0, lty=3)
# 해석 : 대략 배기량 급별로 군집화됨 (예외 Credos)

install.packages("smacof")
library(smacof)
mds2 = smacofSym(z01x.dist, ndim=2)
plot(mds2$conf[,1], mds2$conf[,2], type="n")
text(mds2$conf[,1], mds2$conf[,2], rownames(z01x.dist), cex=0.9)
abline(h=0, v=0, lty=3)
attributes(mds2)
mds2$stress

# 스크리 그림 그리기
mds2.1 = smacofSym(z01x.dist, ndim=1)
mds2.2 = smacofSym(z01x.dist, ndim=2)
mds2.3 = smacofSym(z01x.dist, ndim=3)
mds2.4 = smacofSym(z01x.dist, ndim=4)
stress.value = c(mds2.1$stress, mds2.2$stress,
                 mds2.3$stress, mds2.4$stress)
plot(stress.value, type="l")
points(stress.value, cex=0.9)
# 해석 : 2차원으로만 해도 stress 값 충분히 낮춰진다

# 적합도 진단 그림 그리기
plot(mds2$confdist, mds2$delta)
# x 축은 관측된 거리, y 축은 2차원 좌표상에 그려진 거리.
# 잘 적합되었다면 관측점들이 45도 대각선상에 나타남.

# Non-metric MDS : library(MASS) 의 isoMDS 함수 이용
