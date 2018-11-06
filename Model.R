data <- read.csv("Joined2.csv")
head(data)
is.na(data$Migrants.rounded)
data[which(is.na(data$Migrants.rounded)), "Migrants.rounded"] <- 0
data$colony <- 0 
data[which(data$Country=="France"), "colony"] <- 1
data
attach(data)
summary(mod <- lm(Migrants.rounded ~ Score + GDP + colony))
n <- nrow(data)
k <- 4
library(stats)
sum(complete.cases(data[,2:5])/1)
plot(hatvalues(mod))
abline(h=c((3*(k+1))/n, lty = 2))
identify(1:111, hatvalues(mod), row.names(data))
data
plot(cooks.distance(mod))
abline(h=(4/111), lty=2)
identify(1:111, cooks.distance(mod), row.names(data))
plot(fitted.values(mod), rstudent(mod))
abline(h=0, lty=2)
library(car)
spreadLevelPlot(mod)
attach(data)
#test for heteroskedasticity
om <- na.omit(data)
om
om$Score
(R2 <- summary(lm(mod$resid ~ om$Score + om$GDP + om$colony +
                    (om$Score^2) + (om$GDP^2) + (om$colony^2) +
                    om$Score*om$GDP + om$Score*om$colony + om$GDP*om$colony +
                    om$Score*om$GDP*om$colony))$r.sq)
(TS <- R2 * nobs(mod))
pchisq(TS, nobs(mod)-194, lower.tail=F)

#Stargazing 
e <- mod$residuals
plot(om$Score, e)
plot(om$GDP, e)
plot(om$colony, e)

y.hat <- mod$fitted
y.hat2 <- y.hat^2
(R2 <- summary(lm(mod$resid ~ y.hat + y.hat2))$r.sq)
(TS <- R2 * nobs(mod))
pchisq(TS, nobs(mod)-3, lower.tail=F)

#Least weighted Squares
library(ggplot2)

summary(OLS <- lm(Migrants.rounded ~ Score + GDP + colony))
SD <- sum((sd(Score) + (sd(GDP)) + (sd(colony))))
summary(WLS <- lm(om$Migrants.rounded ~ om$Score + om$GDP + om$colony, weights~1/SD^2))
G <- ggplot(data, aes(Score + GDP + colony, Migrants.rounded)) +
  geom_point(aes(col=SD), size=4) +
  theme_classic() +
  geom_abline(intercept=OLS$coef[1], slope=OLS$coef[2], col="orange", lwd=2)


#White's Robust Standard Erros
library(lmtest)
library(car)
coeftest(mod, hccm(mod, type="hc0"))



#Adjusted R-squared
summary(mod.a <- lm(Migrants.rounded ~ GDP))
summary(mod.b <- lm(Migrants.rounded ~ colony))
summary(mod.c <- lm(Migrants.rounded ~ GDP + colony))
summary(mod.d <- lm(Migrants.rounded ~ Score))
summary(mod.e <- lm(Migrants.rounded ~ Score + colony))
summary(mod.f <- lm(Migrants.rounded ~ Score + GDP))
mod.g <- lm(Migrants.rounded ~ Score + GDP + colony)
1-(1-summary(mod.a)$r.sq)*(nobs(mod.a)-1)/(nobs(mod.a)-k)
1-(1-summary(mod.b)$r.sq)*(nobs(mod.b)-1)/(nobs(mod.b)-k)
1-(1-summary(mod.c)$r.sq)*(nobs(mod.c)-1)/(nobs(mod.c)-k)
1-(1-summary(mod.d)$r.sq)*(nobs(mod.d)-1)/(nobs(mod.d)-k)
1-(1-summary(mod.e)$r.sq)*(nobs(mod.e)-1)/(nobs(mod.e)-k)
1-(1-summary(mod.f)$r.sq)*(nobs(mod.f)-1)/(nobs(mod.f)-k)
#AIC
2.718281828459045^(2*k/nobs(mod.a))*sum(mod.a$resid^2)/nobs(mod.a)
2.718281828459045^(2*k/nobs(mod.b))*sum(mod.b$resid^2)/nobs(mod.b)
2.718281828459045^(2*k/nobs(mod.c))*sum(mod.c$resid^2)/nobs(mod.c)
2.718281828459045^(2*k/nobs(mod.d))*sum(mod.d$resid^2)/nobs(mod.d)
2.718281828459045^(2*k/nobs(mod.e))*sum(mod.e$resid^2)/nobs(mod.e)
2.718281828459045^(2*k/nobs(mod.f))*sum(mod.f$resid^2)/nobs(mod.f)
2.718281828459045^(2*k/nobs(mod.g))*sum(mod.g$resid^2)/nobs(mod.g)
#BIC
nobs(mod.a)^(k/nobs(mod.a))*sum(mod.a$resid^2)/nobs(mod.a)nobs(mod.c)^(k/nobs(mod.c))*sum(mod.c$resid^2)/nobs(mod.c)
nobs(mod.b)^(k/nobs(mod.b))*sum(mod.b$resid^2)/nobs(mod.b)
nobs(mod.c)^(k/nobs(mod.c))*sum(mod.c$resid^2)/nobs(mod.c)
nobs(mod.d)^(k/nobs(mod.d))*sum(mod.d$resid^2)/nobs(mod.d)
nobs(mod.e)^(k/nobs(mod.e))*sum(mod.e$resid^2)/nobs(mod.e)
nobs(mod.f)^(k/nobs(mod.f))*sum(mod.f$resid^2)/nobs(mod.f)
nobs(mod.g)^(k/nobs(mod.g))*sum(mod.g$resid^2)/nobs(mod.g)
