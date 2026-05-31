# Wiederholung

library(tidyverse)

dat <- read_rds("daten/lynx.rds")
m2 <- lm(lynx ~ y, data = dat)
summary(m2)

nd <- data.frame(y = 35:80)
nd$lynx <- predict(m2, newdata = nd)

plot(nd$y, nd$lynx, xlab = "Breitengrad", 
     ylab = "Vorkommenswahrscheinlichkeit", 
     type = "l")
abline(a = 0, b = 0, col = "red")
abline(a = 1, b = 0, col = "red")


# Mit einem GLM
m3 <- glm(lynx ~ y, data = dat, 
          family = binomial())
summary(m3)

nd <- data.frame(y = 35:80)
nd$lynx <- predict(m3, newdata = nd)

plot(nd$y, nd$lynx, xlab = "Breitengrad", 
     ylab = "Vorkommenswahrscheinlichkeit", 
     type = "l")
abline(a = 0, b = 0, col = "red")
abline(a = 1, b = 0, col = "red")

# Nochmals predict
nd$lynx <- predict(m3, newdata = nd, type = "response")

plot(nd$y, nd$lynx, xlab = "Breitengrad", 
     ylab = "Vorkommenswahrscheinlichkeit", 
     type = "l")
abline(a = 0, b = 0, col = "red")
abline(a = 1, b = 0, col = "red")
points(dat$y, dat$lynx)

plot(nd$y, nd$lynx, xlab = "Breitengrad", 
     ylab = "Vorkommenswahrscheinlichkeit", 
     type = "l", 
     ylim = c(-0.1, 1.1))
abline(a = 0, b = 0, col = "red")
abline(a = 1, b = 0, col = "red")
points(dat$y, jitter(dat$lynx, amount = 0.1), 
       pch = 20, 
       col = adjustcolor("black", alpha = 0.1))

