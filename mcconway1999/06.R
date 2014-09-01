# McConway KJ, Jones MC, Taylor PC.
# Statistical Modelling using GENSTAT.
# London, UK: Arnold, 1999.
# ISBN: 0340759852

# Code fragment for diagnostic plots
par(mfrow = c(2, 3))
plot(model, which = 1:6)
dev.off()

# Chapter 6: Multiple linear regression
rubber <- read.table('rubber.dat', header = TRUE)
par(mfrow = c(1, 2))
plot(loss ~ hardness, data = rubber, pch = 4)
plot(loss ~ strength, data = rubber, pch = 4)
anova(model <- lm(loss ~ hardness, data = rubber))
anova(model <- lm(loss ~ strength, data = rubber))
anova(model <- lm(loss ~ hardness + strength, data = rubber))
summary(model)
round(predict(model, newdata = data.frame(hardness = 60, strength = 205), interval = 'confidence'), 1)
round(predict(model, newdata = data.frame(hardness = 60, strength = 205), interval = 'prediction'), 1)

cemheat <- read.table('cemheat.dat', header = TRUE)
par(mfrow = c(1, 2))
plot(heat ~ TA, data = cemheat, pch = 4)
plot(heat ~ TS, data = cemheat, pch = 4)
anova(model <- lm(heat ~ TA, data = cemheat))
anova(model <- lm(heat ~ TS, data = cemheat))
anova(model <- lm(heat ~ TA + TS, data = cemheat))
summary(model)
predict(model, newdata = data.frame(TA = 15, TS = 55))

anaerob <- read.table('anaerob.dat', header = TRUE)
anova(model <- lm(ventil ~ oxygen, data = anaerob))
anova(model <- update(model, .~. + I(oxygen^2)))
summary(model)

peru <- read.table('peru.dat', header = TRUE)
pairs(peru, pch = 4)
pairs(peru[-1, ], pch = 4)
summary(model <- lm(sbp ~ ., data = peru[-1, ]))
round(cor(peru[-1, -9]), 3)
step(model, direction = 'both', scope = .~.) # bad practice
summary(update(model, .~ years + weight))
step(lm(sbp ~ 1, data = peru[-1, ]), direction = 'both', scope = sbp ~ age + years + weight + height + chin + forearm + calf + pulse)

crime <- read.table('crime.dat', header = TRUE)
pairs(crime)
summary(model <- lm(crime ~ ., data = crime))
round(cor(crime[, -1]), 3)
step(model, direction = 'both', scope = .~.) # bad practice
step(lm(crime ~ 1, data = crime), direction = 'both', scope = crime ~ malyth + state + school + pol60 + pol59 + empyth + mf + popn + race + uneyth + unemid + income + poor)
summary(model <- lm(crime ~ malyth + school + pol60 + unemid + poor, data = crime))
predict(model, newdata = data.frame(malyth = 140, school = 110, pol60 = 98, unemid = 20, poor = 180), se.fit = TRUE, interval = 'prediction')

water <- read.table('water.dat', header = TRUE)
summary(model <- lm(mortalty ~ calcium * factor(north), data = water))
summary(model <- lm(mortalty ~ calcium + factor(north), data = water))

uffi <- read.table('uffi.dat', header = TRUE)
plot(conc ~ airtight, data = uffi, pch = c(4, 20)[uffi + 1])
legend('topleft', bty = 'n', pch = c(20, 4), legend = c('uffi = 1', 'uffi = 0'))
summary(model <- lm(conc ~ airtight * factor(uffi), data = uffi))
summary(model <- lm(conc ~ airtight + factor(uffi), data = uffi))
summary(model <- lm(conc ~ airtight, data = uffi))

pea <- read.table('pea.dat', header = TRUE)
pea$sugar <- factor(pea$sugar, levels = c('control', 'glucose', 'fructose', 'g&f', 'sucrose'))
summary(model <- lm(length ~ sugar, data = pea))
pea$length1 <- 1000 / pea$length
summary(update(model, length1 ~ .))
pea1 <- read.table('pea1.dat', header = TRUE)
summary(model <- lm(length ~ ., data = pea1))

singers <- read.fwf('singers.dat', widths = c(7, 8), header = FALSE, skip = 1, strip.white = TRUE)
colnames(singers) <- c('height', 'voice')
singers$voice <- relevel(singers$voice, ref = 'Tenor 1')
anova(model <- lm(height ~ voice, data = singers))
summary(model)
