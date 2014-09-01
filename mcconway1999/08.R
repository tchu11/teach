# McConway KJ, Jones MC, Taylor PC.
# Statistical Modelling using GENSTAT.
# London, UK: Arnold, 1999.
# ISBN: 0340759852
library(nlme)

# Code fragment for diagnostic plots
par(mfrow = c(2, 3))
plot(model, which = 1:6)
dev.off()

# Chapter 8: Experiments with blocking
grapeft <- read.table('grapeft.dat', header = TRUE)
plot(shaded ~ fruit, grapeft)
points(exposed ~ fruit, grapeft, pch = 4)
plot(exposed ~ shaded, grapeft)
abline(a = 0, b = 1, lty = 'dotted')
grapeft <- transform(grapeft, diff = exposed - shaded)
hist(grapeft$diff)
t.test(grapeft$exposed, grapeft$shaded, paired = TRUE, var.equal = TRUE)

wtgain <- read.table('wtgain.dat', header = TRUE)
plot(wtgain ~ as.numeric(diet), wtgain, pch = 20, col = litter)
summary(model <- aov(wtgain ~ factor(litter) + diet, wtgain))
summary(model <- aov(wtgain ~ diet + factor(litter), wtgain))
summary(update(model, .~ diet * factor(litter)))

chicks <- read.table('chicks.dat', header = TRUE)
summary(model <- aov(weight ~ factor(block) + drug, chicks))
x <- matrix(-.5, 3, 3)
diag(x) <- 1
print(x <- cbind(1/3, x[, 1:2]))
x <- solve(t(x))[, -1]
summary(model <- lm(weight ~ factor(block) + drug, chicks, contrasts = list(drug = x)))
confint(model)[9, ]

grapeft1 <- read.table('grapeft1.dat', header = TRUE)
summary(model <- aov(solid ~ factor(fruit) + sun, grapeft1))
summary.lm(model)

turnip <- read.table('turnip.dat', header = TRUE)
summary(model <- aov(weight ~ factor(block) + density * sowing * variety, turnip))
model.tables(model, type = 'means')
tapply(turnip$weight, turnip$variety, mean)
round(tapply(turnip$weight, list(turnip$density, turnip$sowing), mean), 2)

martinda <- read.table('martinda.dat', header = TRUE)
print(with(martinda, ftable(run, material, position)), zero.print = '.') # Latin square design
plot(wear ~ as.numeric(material), martinda, pch = 4)
summary(model <- aov(wear ~ factor(run) + factor(position) + material, martinda))

dishwash <- read.table('dishwash.dat', header = TRUE)
print(with(dishwash, ftable(session, detergen)), zero.print = '.') # balanced incomplete block design
plot(plates ~ detergen, dishwash, pch = 4)
par(mfrow = c(1, 2))
plot(plates ~ detergen, dishwash, pch = 20, col = session)
plot(plates ~ session, dishwash, pch = 20, col = detergen)
summary(model <- aov(plates ~ factor(detergen) + Error(factor(session)), data = dishwash))

vandal <- read.table('vandal.dat', header = TRUE)
print(with(vandal, ftable(row, variety, column)), zero.print = '.') # unbalanced incomplete block design
plot(weight ~ as.numeric(variety), vandal, pch = 4)
summary(model <- aov(weight ~ factor(row) + factor(column) + variety, vandal))
summary.lm(model)

oats <- read.table('oats.dat', header = TRUE)
oats$wholplot <- factor(oats$wholplot)
with(oats, ftable(block, variety, nitrogen)) # split plot design
par(mfrow = c(1, 2))
plot(yield ~ as.numeric(nitrogen), oats, pch = 20, col = as.numeric(variety))
plot(yield ~ as.numeric(variety), oats, pch = 20, col = as.numeric(nitrogen))
summary(model <- aov(yield ~ variety * nitrogen + Error(block + block:wholplot + block:wholplot:subplot), data = oats))
anova(model <- lme(yield ~ variety * nitrogen, random = ~ 1 | block / wholplot, data = oats))
with(oats, interaction.plot(nitrogen, variety, yield))
summary(model <- aov(yield ~ variety * nitrogen + Error(block + block:variety), data = oats))
anova(model <- lme(yield ~ variety * nitrogen, random = ~ 1 | block / variety, data = oats))
# see Venables WN, Ripley BD. Modern Applied Statistics with S, 4th edition. New York, USA: Springer, 2002. pages 281-6.

chloride <- read.table('chloride.dat', header = TRUE)
print(with(chloride, ftable(produnit, clamount, clqualty, treat, block)), zero.print = '.') # incomplete block design with interaction confounded with the blocks
par(mfrow = c(1, 2))
plot(yield ~ treat, chloride, pch = c(4, 1, 3, 20)[block])
model <- lm(yield ~ factor(block), chloride)
chloride$adjyield <- residuals(model)
plot(adjyield ~ treat, chloride, pch = c(4, 1, 3, 20)[block])
anova(model <- lme(yield ~ factor(clamount) * factor(clqualty) * factor(produnit), random = ~ 1 | block / produnit, data = chloride))
summary(model <- aov(yield ~ factor(clamount) * factor(clqualty) * factor(produnit) + Error(factor(block)), chloride))
model.tables(model, type = 'means')
with(chloride, tapply(yield, clamount, mean))
with(chloride, tapply(yield, produnit, mean))
model.tables(model, type = 'effects', se = TRUE)
se.contrast(model, list(clamount == '1', clamount == '2'), data = chloride)
se.contrast(model, list(produnit == '1', produnit == '2'), data = chloride)
