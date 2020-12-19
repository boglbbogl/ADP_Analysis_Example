#R version upgrade
library(installr)
check.for.updates.R()
install.R()

#회귀분석
data(mtcars)
head(mtcars, 3)
reg <- lm(mpg~., data=mtcars) # 회귀분석
reg1 <- step(reg, direction='both') # 변수선택법 '단계적선택법'
reg1
summary(reg1)
step(lm(mpg~., data=mtcars),
     scope=c(lower=~1),
     direction='both') # 회귀분석, 변수선택법
library(lsr)
standardCoefs(reg1) # 표준화계수
library(car)
vif(reg1) # 다중공선성
residualPlots(reg1) # 잔차plot
outlierTest(reg1) # 이상치
qqPlot(reg1, main="Q-Q plot") # 정규성 가정
durbinWatsonTest(reg1)
ncvTest(reg1) # 등분산성 가정

#단순선형회귀(women_data)
data(women)
head(women)
tail(women)
plot(weight~height, data=women) # y축~x축
fit <- lm(weight~height, data=women)
abline(fit, col="blue") # 선형회귀선
summary(fit)
cor.test(women$weight, women$height) # 상관분석
0.9954948^2 # 상관계수의 제곱이 R-squared
par(mfrow=c(2,2))
plot(fit) # 정규성, 등분산성, 선형성, 등분산성, 특정 잔차의 영향력
par(mfrow=c(1,1))
fit2 <- lm(weight~height+ I(height^2), data=women) # 다항회귀분석(y=x^2)
summary(fit2)
plot(weight~height, data=women)
lines(women$height, fitted(fit2), col=2) # 곡선회귀선
plot(fit2)
fit3 <- lm(weight~height+ I(height^2)+I(height^3), data=women) # 3차 방정식
plot(fit3)

#다중회귀분석(state.x77_data)
head(state.x77, 3)
states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])
head(states, 3)
fit <- lm(Murder~., data=states) # 회귀모형
summary(fit)
install.packages("car", dependencies = T)
library(car)
vif(fit) # 다중공선성 가정(분산팽창지수 > 2)
sqrt(vif(fit)) # 루트
plot(fit)

##### 이상관측치 #####
influencePlot(fit, id.method="identify") # outlier, 큰지레점(hat-통계량), 영향관측치(cook's D)

#회귀분석 가정(수치)
mt <- mtcars
head(mt)
mt_lm <- step(lm(mpg~., data=mt), direction = "both")
library(car)
car::ncvTest(mt_lm) # 등분산성
shapiro.test(mt_lm$residuals) # 정규분포
influencePlot(mt_lm, id.method="identify")
