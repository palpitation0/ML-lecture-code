install.packages("HSAUR")
library(HSAUR)
data(heptathlon)
head(heptathlon)
summary(heptathlon)
boxplot(heptathlon)

attach(heptathlon)
hurdles = max(hurdles) - hurdles
run200m = max(run200m) - run200m
run800m = max(run800m) - run800m
# 달리기는 기록 짧을수록 우수한 성적임을 반영 

library(stats)
hep.data = heptathlon[,-8]
hep.pca = princomp(hep.data, cor=T, scores=T)
names(hep.pca)
hep.pca
summary(hep.pca)
# 첫 번째 주성분이 63.72%, 두번째가 17.06% 의
# 분산비율 나타내며, 둘 더하면 80.77% 설명됨

eig.val = hep.pca$sdev^2
eig.val
# 각 주성분의 표준편차 제곱하여 고유값 나옴. 
# 제2성분이 1.19, 제3이 0.52 -> 유효성분=2개

screeplot(hep.pca, type="lines")
hep.pca$loadings[, 1:2]
# 주성분 계수는 loadings 이용해서 확인한다.
# |계수| : 제1성분-제2성분 'javelin' 에서 차이

hep.pca$scores[,1:2]
# 각 선수에 대한 주성분 점수-->군집화도 가능
biplot(hep.pca, cex=0.7)
#각 개체의 관찰값을 주성분 점수로 하고,
#주성분계수를 동시에 나타내어 이들의 관계를 도식화
#거리나 바향 가까울수록 변수들 상관성 높음.

