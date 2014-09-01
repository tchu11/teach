# McConway KJ, Jones MC, Taylor PC.
# Statistical Modelling using GENSTAT.
# London, UK: Arnold, 1999.
# ISBN: 0340759852

# Code fragment for diagnostic plots
par(mfrow = c(2, 3))
plot(model, which = 1:6)
dev.off()

plot(hatvalues(model), pch = 4) # index plot of leverages
plot(cooks.distance(model), pch = 4)
plot(rstandard(model), pch = 4)
plot(rstudent(model), pch = 4) # leave-one-out deletion
plot(residuals(model), pch = 4) # GLM default is deviance
abline(h = 0)

# Methods for influence
# see Chambers JM, Hastie TJ. Statistical Models in S. London, UK: Chapman & Hall, 1993. pages 129-31.
lms <- summary(model)
lmi <- lm.influence(model)
e <- residuals(model)
s <- lms$sigma
xxi <- diag(lms$cov.unscaled)
si <- lmi$sigma
h <- lmi$hat
bi <- t(coef(model) - t(coef(lmi)))
standardized.residuals <- e/(s * (1-h)^.5)
studentized.residuals <- e/(si * (1-h)^.5)
DFBETAS <- bi/(si %o% xxi^.5)
DFFIT <- h * e/(1-h)
DFFITS <- h^.5 * e/(si*(1-h))

# Chapter 11: Diagnostic checking
rubber <- read.table('rubber.dat', header = TRUE)
model <- lm(loss ~ hardness, rubber)
par(mfrow = c(1, 2))
rubber1 <- rubber
rubber1$loss[1] <- rubber$loss[1] + 1000
plot(loss ~ hardness, rubber1, pch = 4, ylim = c(0, 1400))
abline(model, lty = 'dashed')
abline(lm(loss ~ hardness, rubber1))
rubber1 <- rubber
rubber1$loss[5] <- rubber$loss[5] + 1000
plot(loss ~ hardness, rubber1, pch = 4, ylim = c(0, 1400))
abline(model, lty = 'dashed')
abline(lm(loss ~ hardness, rubber1))
rm(rubber1)
dev.off()
plot(hatvalues(model) ~ rubber$hardness, pch = 4, ylim = c(0, .2))
plot(hatvalues(model) ~ rubber$loss, pch = 4, ylim = c(0, .2))
plot(hatvalues(model) ~ predict(model), pch = 4, ylim = c(0, .2))

peru <- read.table('peru.dat', header = TRUE)
plot(years ~ weight, peru[-1, ], pch = 4)
identify(peru$years ~ peru$weight)
summary(model1 <- lm(sbp ~ years + weight, peru[-1, ]))
summary(model2 <- lm(sbp ~ years + weight, peru[-c(1, 39), ]))
round(hatvalues(model1), 3) # sum(leverages) = number of model coefficients
round(hatvalues(model2), 3)
model2 <- lm(sbp ~ years + weight, peru)
par(mfrow = c(1, 2))
plot(cooks.distance(model2), pch = 4)
plot(cooks.distance(model1), pch = 4)
identify(cooks.distance(model1))

crime <- read.table('crime.dat', header = TRUE)
model <- lm(crime ~ malyth + school + pol60 + unemid + poor, crime)
model <- lm(crime ~ malyth + school + pol60 + unemid + poor, crime[-29, ])
model <- lm(crime ~ school + pol60 + poor, crime[-29, ])
summary(model)

crashes <- read.table('crashes.dat', header = TRUE)
model <- glm(crashes ~ log(cover + .5), crashes, family = 'poisson')
data.frame(residuals(model), residuals(model, 'deviance'), residuals(model, 'pearson'), rstandard(model), rstudent(model))
plot(residuals(model, 'pearson'), pch = 4, ylim = c(-1.75, 1.25), 
     ylab = 'Residuals', xlab = 'Unit numbers')
points(residuals(model))
points(rstudent(model), pch = 3)
legend('bottomleft', 
       c('Pearson residuals', 'deviance residuals', 'studentized residuals'), 
       bty = 'n', pch = c(4, 1, 3))

wetbirds <- read.table('wetbirds.dat', header = TRUE)
model <- glm(woodduck ~ grav2, wetbirds, family = 'binomial')

leuksurv <- read.table('leuksurv.dat', header = TRUE)
summary(model <- glm(time ~ ag + log(wbc), leuksurv, family = 'Gamma'))
summary(model <- glm(time ~ ag + log(wbc), leuksurv[-2, ], family = 'Gamma'))

gvhd <- read.table('gvhd.dat', header = TRUE)
model <- glm(gvhd ~ log(indx) + donage, gvhd, family = 'binomial')
plot(gvhd$indx, pch = c(4, 20)[gvhd$gvhd + 1])
model <- glm(gvhd ~ indx + donage, gvhd, family = 'binomial')
model <- update(model, .~. - donage)
model <- update(model, .~ log(indx))
plot(hatvalues(model) ~ gvhd$indx, pch = 4)
data.frame(indx = gvhd$indx, predict(model, type = 'response', se.fit = TRUE))[c(14, 35, 37), ]

ship1907 <- read.table('ship1907.dat', header = TRUE)
model <- glm(crew ~ log(tonnage), ship1907, family = 'poisson')
model <- glm(crew ~ log(tonnage), ship1907[-16, ], family = 'poisson')
