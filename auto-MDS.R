library(xlsx)
data = read.xlsx("C:/Users/Choi Sung Wook/Desktop/���п�/�ӽŷ��� dataset/mva/auto.xlsx", 1)
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

# MDS �м� ����
mds1 = cmdscale(z01x.dist, k=2)
plot(mds1[,1], mds1[,2], type="n", 
     xlab="", ylab="", main="cmdscale(Auto)")
text(mds1[,1], mds1[,2], rownames(z01x.dist), cex=0.9)
abline(h=0, v=0, lty=3)
# �ؼ� : �뷫 ��ⷮ �޺��� ����ȭ�� (���� Credos)

install.packages("smacof")
library(smacof)
mds2 = smacofSym(z01x.dist, ndim=2)
plot(mds2$conf[,1], mds2$conf[,2], type="n")
text(mds2$conf[,1], mds2$conf[,2], rownames(z01x.dist), cex=0.9)
abline(h=0, v=0, lty=3)
attributes(mds2)
mds2$stress

# ��ũ�� �׸� �׸���
mds2.1 = smacofSym(z01x.dist, ndim=1)
mds2.2 = smacofSym(z01x.dist, ndim=2)
mds2.3 = smacofSym(z01x.dist, ndim=3)
mds2.4 = smacofSym(z01x.dist, ndim=4)
stress.value = c(mds2.1$stress, mds2.2$stress,
                 mds2.3$stress, mds2.4$stress)
plot(stress.value, type="l")
points(stress.value, cex=0.9)
# �ؼ� : 2�������θ� �ص� stress �� ����� ��������

# ���յ� ���� �׸� �׸���
plot(mds2$confdist, mds2$delta)
# x ���� ������ �Ÿ�, y ���� 2���� ��ǥ�� �׷��� �Ÿ�.
# �� ���յǾ��ٸ� ���������� 45�� �밢���� ��Ÿ��.

# Non-metric MDS : library(MASS) �� isoMDS �Լ� �̿�