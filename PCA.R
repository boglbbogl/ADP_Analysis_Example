#PCA(Principal Component Analysis)
mt <- mtcars
cov(mt) # 공분산행렬
mt_cor <- cor(mt) # 상관행렬
eigen(mt_cor) # 고유값
mt_pca <- princomp(mt, cor = T) # princomp(), cor = 상관행렬
mt_pca <- prcomp(mt, center= T, scale. = T) # prcomp(표준화), center=중앙(0), scale=분산(1)
summary(mt_pca)
loadings(mt_pca) # 주성분의 로딩 벡터
mt_pca$scores # 각 관측치를 주성분들로 표현한 값
plot(mt_pca, type="line") # Screeplot
abline(h=1, lty=3, col="red") # line 추가
biplot(mt_pca) # biplot

us <- USArrests
us_pca <- princomp(us, cor=T)
princomp(~., data=us, cor=T) # Fomula 형식
us_pca <- prcomp(us, center = T, scale. = T)
summary(us_pca)
plot(us_pca, type="line")
abline(h=0.8, lty=2, col=2)
biplot(us_pca)

head(iris, 1)
iris <- iris[,c(1:4)]
iris_pca <- princomp(iris, cor = T)
summary(iris_pca)
loadings(iris_pca)
plot(iris_pca, type="line")
abline(h=0.5, col="red")
iris_pca$scores
biplot(iris_pca)
