### 제0장 패키지의 인스톨 ####

install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}

required.packages = c("mosaic", "mosaicCalc", "distrEx", "distrTeach", 
                      "TeachingDemos", "stats4")
install(required.packages)

### 제1장 ####

library(mosaic) 
library(mosaicCalc)

##  1.1 미분과 적분 ####

f = makeFun(a * x^2 + b ~ x)
f(x = 2, a = 1, b = 10)

plotFun(f(x, a=1, b=2) ~ x, x.lim=range(-10,10))
plotFun(f(x, a=2, b=3) ~ x, add=TRUE, npts=300, alpha=.2)
plotFun(dnorm(x)~x,  xlim=range(-4,4)) 

# 미분
D(a * x^2 + b ~ x)     
D(exp(-x^2/2)/sqrt(2*pi) ~ x)
D(dnorm(x) ~ x)

# 적분
antiD(a * x^2 + b ~ x)  

F = antiD(exp(-x^2/2)/sqrt(2*pi) ~ x)
plotFun(F(x)~x,  xlim=range(-4,4))   # cdf
F(Inf) - F(-Inf)

F_N = antiD(dnorm(x,mu,sigma) ~ x) 
F_N(10, mu=10, sigma=2)

# 참고 : https://mran.microsoft.com/snapshot/2015-01-06/web/packages/mosaic/vignettes/V6Calculus.pdf

###  1.2 동전던지기 ####

rflip(6)         # 동전 6개 던지기
do(4) * rflip(6)  # 동전 6개 던지기를 4번 반복

# 동전 6개 던지기를 1,000번 반복하고 표, 히스토그램 그리기
r_rflip <- do(1000) * rflip(6)

tally(~heads, data=r_rflip)
histogram(~ heads, data=r_rflip, width=1)

### 제2장 ####

library(mosaic) 
library(mosaicCalc)
library(TeachingDemos)

##  2.1 주사위던지기 ####

plot(dice(12,1))

n_num =100000
r_dice = dice(n_num,1)
taa = tally(~Red, data=r_dice)
 taa/n_num 
barplot(taa/n_num, ylim=c(0,0.5))
 abline(b=0, a=1/6, col=2)

## 2.2 연속형 확률변수의 기댓값 ####
 
 f  = makeFun(2*exp(-2*x) ~ x)  
 F  = antiD(x*f(x) ~ x)
 Ex = F(Inf) - F(0) 
  
##  2.3 동전 던지기 ####
 
 n_num =10000
 r_rflip = do(n_num) * rflip(2) 
 
 # 앞면의 수의 분포 근사가
 taa = tally(~heads, data=r_rflip)
  taa/n_num
 histogram(~ heads, data=r_rflip, width=1) 

 # 기댓값과 분산의 근삿값
 mean(r_rflip$heads) 
 var(r_rflip$heads)
 
### 제3장 ####

library(mosaic)  
library(distrEx)

## 3.1 이항분포 ####
 
 size1 = 5
 prob1 = 0.5
 Binom = Binom(size1, prob1)

 plot(Binom, mfColRow = FALSE, to.draw.arg="d", ylab="")
 
 E(Binom)
 var(Binom)
     
 n_num = 10000
 r_binom = rbinom(n_num, size1, prob1)
 histogram(r_binom, width=1)
 
 mean(r_binom)
 var(r_binom)
 
 ## 3.2 포아송분포 ####
 
 Poisson_2 = Pois(2)
 Binom_21  = Binom(10, 0.2)
 Binom_22  = Binom(100, 0.02)
 
 par(mfrow=c(3,1))
 plot(Poisson_2, mfColRow = FALSE, to.draw.arg="d", ylab="")
 plot(Binom_21, mfColRow = FALSE, to.draw.arg="d", ylab="", xlim=c(0, 12))
 plot(Binom_22, mfColRow = FALSE, to.draw.arg="d", ylab="", xlim=c(0, 12))
 
 b1 = d(Poisson_2)(0:12)
 b2 = d(Binom_21)(0:12)
 b3 = d(Binom_22)(0:12)
 
 mean(abs(b2-b1))
 mean(abs(b3-b1))
 
 ## 3.3 이항분포의 정규분포 근사 ####
 
 plotDist("binom", size=100, prob=.30, col=2, lwd=2) +
  plotDist("norm", mean=30, sd=sqrt(100 * .3 * .7), add=TRUE)
 
 ## 3.4 정규분포의 비교 ####
 plotDist("norm", mean=1, sd=1/2, lwd=2, xlim=c(-6,6) ) +
  plotDist("norm", col=2, lwd=2, add=TRUE) +
  plotDist("norm", mean=-1, sd=2, lwd=2, col=1, add=TRUE)
 
#### 제 4 장  표본분포

 # 예 4.8
 n=100000
 X1 = rnorm(n,1,1)
 X2 = rnorm(n,2,1) 
 SX = X1+X2
 plot(density(SX, bw=0.8), xlim=c(-4,9),ylim=c(0,0.35), main="", xlab="")
  lines(density(X1, bw=0.8),lty=2, col=4)
  lines(density(X2, bw=0.8),lty=2, col=2)
  legend("topright", c(expression(X[1]+X[2]), 
    expression(X[1]), expression(X[2])), lty=c(1,2,2), col=c(1,4,2))
 
# 합의 확률분포
 library(distrEx)
 Bi_sum = Binom(5,0.3) + Binom(2, 0.3) + Binom(3, 0.3)
 Bi_sum
 Po_sum = Pois(5) + Pois(2) + Pois(3)
 Po_sum
 Norm_sum = Norm(5,2) + Norm(2,4) + Norm(3,3)
 Norm_sum 
 Norm_mean = (Norm(2,2) + Norm(2,2) + Norm(2,2))/3
 
 # t분포
 library(distrEx)
 Ta = Norm()/sqrt(Chisq(5)/5)
 par(mfrow=c(2,1))
 plot(Td(5), mfColRow = FALSE, to.draw.arg="d", ylab="", xlim=c(-6, 6))
 plot(Ta, mfColRow = FALSE, to.draw.arg="d", ylab="", xlim=c(-6, 6))
 p(Ta)(2);p(Td(5))(2)
 
 # 대수의 법칙
 library(distrTeach)
 illustrateLLN(Distr = Pois(2), main="Posssion(2)")
 
 # 중심극한정리
 library(TeachingDemos)
  clt.examp(1)
  clt.examp(30)
 
  
 
 
 
 
 
 
 
 
 
 
     
 
 





