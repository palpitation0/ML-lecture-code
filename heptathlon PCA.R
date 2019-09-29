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
# �޸���� ��� ª������ ����� �������� �ݿ� 

library(stats)
hep.data = heptathlon[,-8]
hep.pca = princomp(hep.data, cor=T, scores=T)
names(hep.pca)
hep.pca
summary(hep.pca)
# ù ��° �ּ����� 63.72%, �ι�°�� 17.06% ��
# �л���� ��Ÿ����, �� ���ϸ� 80.77% ������

eig.val = hep.pca$sdev^2
eig.val
# �� �ּ����� ǥ������ �����Ͽ� ������ ����. 
# ��2������ 1.19, ��3�� 0.52 -> ��ȿ����=2��

screeplot(hep.pca, type="lines")
hep.pca$loadings[, 1:2]
# �ּ��� ����� loadings �̿��ؼ� Ȯ���Ѵ�.
# |���| : ��1����-��2���� 'javelin' ���� ����

hep.pca$scores[,1:2]
# �� ������ ���� �ּ��� ����-->����ȭ�� ����
biplot(hep.pca, cex=0.7)
#�� ��ü�� �������� �ּ��� ������ �ϰ�,
#�ּ��а���� ���ÿ� ��Ÿ���� �̵��� ���踦 ����ȭ
#�Ÿ��� ���� �������� ������ ����� ����.
