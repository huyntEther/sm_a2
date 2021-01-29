### cau 2
# Y: muc do ben deo cua nhua
Y <- c(37.8, 22.5, 17.1, 10.8, 7.2, 42.3, 30.2, 19.4, 14.8, 9.5, 32.4, 21.6)
# X1: do day cua vat lieu
X1 <- c(4, 4, 3, 2, 1, 6, 4, 4, 1, 1, 3, 4)
# X2: mat do cua vat lieu
X2 <- c(4.0, 3.6, 3.1, 3.2, 3.0, 3.8, 3.8, 2.9, 3.8, 2.8, 3.4, 2.8)

##### 2.1 Cac mo hinh hoi quy tuyen tinh
model1 <- lm(Y~X1)
summary(model1)

model2 <- lm(Y~X2)
summary(model2)

model3 <- lm(Y~X1+X2)
summary(model3)

##### 2.2 He so xac dinh R-square
summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared

#He so xac dinh hieu chinh Adj-R-square
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
summary(model3)$adj.r.squared

##### 2.3 Bang ANOVA cho mo hinh 2 bien
anova(model3)

##### 2.4 Kiem dinh gia thuyet H0: Beta1 = Beta2 = 0
SSE <- anova(model3)[3,2]
SSR <- anova(model3)[1,2] + anova(model3)[2,2]

#p = 3 so luong he so hoi quy
#n = 12 tong so quan sat trong mau
F_obs <- (SSR/2)/(SSE/9) #Gia tri thong ke
F_obs

isFail_H0 <- F_obs > qf(1-0.05, 2, 9)
isFail_H0
#True. => Bac bo H0

##### 2.5 Khoang tin cay voi muc y nghia 5% cho Beta1
#tren mo hinh chi co bien 'do day vat lieu'
confint(model1, level = 0.95)[2,]

#Khoang tin cay voi muc y nghia 5% cho cac he so hoi quy
#tren mo hinh 2 bien doc lap
confint(model3, level = 0.95)


###bai 3
y <- c(12,14,10,16,14,19,21,19,21,16,19,21,25,21)
x1 <- c(2,1,3,6,7,8,8,5,5,8,4,9,12,7)
x2 <- c(45,43,43,47,42,41,32,33,41,38,32,31,35,29)
x3 <- c(121,132,154,145,129,156,132,147,128,163,161,172,174,180)
##### 3.1 + 3.2
M12 <- lm(y ~ x1 + x2)
coef(M12)
M23 <- lm(y ~ x2 + x3)
coef(M23)
M13 <- lm(y ~ x1 + x2)
coef(M13)
##### 3.3
confint(M12)
##### 3.4
summary(M12)
summary(M23)
summary(M13)
##### 3.6
model <- lm(y ~ x1 + x2 + x3)
summary(model)
matrix(y,ncol=1)
n <- length(y)
matrix(c(c(rep(1,n)),x1,x2,x3),ncol=4)
##### 3.7
coef(model)
##### 3.8
out <- summary(model)
#b0
se_B0 <- out$coefficients[1,2]
se_B0
V_B0 <- (se_B0)**2
V_B0
#b1
se_B1 <- out$coefficients[2,2]
se_B1
V_B1 <- (se_B1)**2
V_B1
#b2
se_B2 <- out$coefficients[3,2]
se_B2
V_B2 <- (se_B2)**2
V_B2
#b3
se_B3 <- out$coefficients[4,2]
se_B3
V_B3 <- (se_B3)**2
V_B3
#v(e)
model_sigma <- out$sigma
model_sigma **2
##### 3.9
qnorm(1-0.05/2)*model_sigma / sqrt(n)
##### 3.10
M1 <- lm(y ~ x1)
anova(M1)
M132 <- lm(y ~ x1 + x3 +x2)
anova(M132)

SSE_H0 <- anova(M1)$`Sum Sq`[2]
df_H0 <- anova(M1)$Df[2]
SSE_H1 <- anova(M132)$`Sum Sq`[4]
df_H1 <- anova(M132)$Df[4]
r <- (df_H0 - df_H1)
F_obs <- ((SSE_H0 - SSE_H1)/r)/(SSE_H1/df_H1)
F_obs
F_val <- qf(1-0.05,r,df_H1)
F_val
F_obs >= F_val

summary(M1)
summary(M132)
