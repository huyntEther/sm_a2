# Y: muc do ben deo cua nhua
Y <- c(37.8, 22.5, 17.1, 10.8, 7.2, 42.3, 30.2, 19.4, 14.8, 9.5, 32.4, 21.6)
# X1: do day cua vat lieu
X1 <- c(4, 4, 3, 2, 1, 6, 4, 4, 1, 1, 3, 4)
# X2: mat do cua vat lieu
X2 <- c(4.0, 6.3, 3.1, 3.2, 3.0, 3.8, 3.8, 2.9, 3.8, 2.8, 3., 2.8)
model1 <- lm(Y~X1)
confint(model1, level = 0.95)[2,]
