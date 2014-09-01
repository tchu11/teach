# McConway KJ, Jones MC, Taylor PC.
# Statistical Modelling using GENSTAT.
# London, UK: Arnold, 1999.
# ISBN: 0340759852

# Code fragment for diagnostic plots
par(mfrow = c(2, 3))
plot(model, which = 1:6)
dev.off()

# Chapter 4: Linear regression with one explanatory variable
rubber <- read.table('rubber.dat', header = TRUE)
plot(loss ~ hardness, data = rubber, pch = 4)
model <- lm(loss ~ hardness, data = rubber)
print(model)
abline(model)
summary(model)
anova(model)
round(confint(model), 1)
plot(residuals(model) ~ strength, data = rubber, pch = 4)
round(cor(rubber), 3)

bristol <- read.table('bristol.dat', header = TRUE)
plot(after ~ before, data = bristol, pch = 4, ylim = c(0, 20), xlim = c(0, 20))
print(model <- lm(after ~ before - 1, data = bristol))
abline(model)

marine <- read.table('marine.dat', header = TRUE)
plot(extinct ~ mybp, data = marine, pch = 4)

peanuts <- read.table('peanuts.dat', header = TRUE)
plot(percent ~ toxin, data = peanuts, pch = 4)
print(model <- lm(percent ~ toxin, data = peanuts))
abline(model)
signif(predict(model, newdata = data.frame(toxin = 13.2), interval = 'confidence', level = .9), 4)
signif(predict(model, newdata = data.frame(toxin = 13.2), interval = 'prediction', level = .9), 4)

taps <- read.table('taps.dat', header = TRUE)
plot(taps ~ dose, data = taps)
print(model <- lm(taps ~ dose, data = taps))
abline(model)
anova(model)
summary(model)

temperat <- read.table('temperat.dat', header = TRUE)
plot(gascons ~ tempdiff, data = temperat, pch = 4, ylim = c(0, 120), xlim = c(0, 20))
summary(model <- lm(gascons ~ tempdiff, data = temperat))
signif(confint(model)[1, ], 4)

iron <- read.table('iron.dat', header = TRUE)
plot(chemical ~ magnetic, data = iron, pch = 4)
print(model <- lm(chemical ~ magnetic, data = iron))
scatter.smooth(residuals(model), pch = 4)
abline(h = 0, col = 'grey')

cream <- read.table('cream.dat', header = TRUE)
plot(pints ~ temp, data = cream, pch = 4)
print(model <- lm(pints ~ temp, data = cream))
scatter.smooth(residuals(model), pch = 4)
abline(h = 0, col = 'grey')

strong <- read.table('strong.dat', header = TRUE)
plot(p ~ t, data = strong, pch = 4, ylim = c(0, 1))
plot(log(p) ~ t, data = strong, pch = 4)
plot(p ~ log(t), data = strong, pch = 4, ylim = c(0, 1))
print(model <- lm(p ~ log(t), data = strong))

cemstren <- read.table('cemstren.dat', header = TRUE)
par(mfrow = c(2, 2))
plot(strength ~ curetime, data = cemstren, pch = 4)
plot(log(strength) ~ curetime, data = cemstren, pch = 4)
plot(sqrt(strength) ~ curetime, data = cemstren, pch = 4)
plot(I(1 / strength) ~ curetime, data = cemstren, pch = 4)

plot(log(strength) ~ curetime, data = cemstren, pch = 4)
plot(log(strength) ~ log(curetime), data = cemstren, pch = 4)
plot(log(strength) ~ sqrt(curetime), data = cemstren, pch = 4)
plot(log(strength) ~ I(1 / curetime), data = cemstren, pch = 4)
print(model <- lm(log(strength) ~ I(1 / curetime), data = cemstren))

wind <- read.table('wind.dat', header = TRUE)
par(mfrow = c(2, 2))
plot(output ~ speed, data = wind, pch = 4)
plot(I(output^2) ~ speed, data = wind, pch = 4)
plot(output ~ I(speed^-1), data = wind, pch = 4)
plot(output ~ I(speed^-.5), data = wind, pch = 4)
print(model <- lm(output ~ I(speed^-.5), data = wind))

hardness <- read.table('hardness.dat', header = TRUE)
par(mfrow = c(1, 2))
plot(hardness ~ density, data = hardness, pch = 4)
plot(log(hardness) ~ log(density), data = hardness, pch = 4)
print(model <- lm(log(hardness) ~ log(density), data = hardness))

water <- read.table('water.dat', header = TRUE)
par(mfrow = c(1, 2))
plot(mortalty ~ calcium, data = water, pch = 4)
plot(mortalty ~ calcium, data = water, pch = c(20, 4)[north + 1])
legend('top', bty = 'n', cex = .8, pch = c(4, 20), legend = c('northern towns', 'southern towns'))
summary(model <- lm(mortalty ~ calcium, data = water, subset = north == 1))
abline(model)
summary(model <- lm(mortalty ~ calcium, data = water, subset = north == 0))
abline(model, lty = 'dashed')

digoxin <- read.table('digoxin.dat', header = TRUE)
plot(log(digoxin) ~ log(creatin), data = digoxin, pch = 4)
round(cor(digoxin), 3)
