# McConway KJ, Jones MC, Taylor PC.
# Statistical Modelling using GENSTAT.
# London, UK: Arnold, 1999.
# ISBN: 0340759852

# Code fragment for diagnostic plots
par(mfrow = c(2, 3))
plot(model, which = 1:6)
dev.off()

# Chapter 7: The analysis of factorial experiments
weight <- read.table('weight.dat', header = TRUE)
weight$amount <- relevel(weight$amount, ref = 'low')
summary(model <- aov(weight ~ treatmnt, data = weight))
x <- matrix(c(-.5, .5, -.5, .5, .5, .5, -.5, -.5, -1, 1, 1, -1), ncol = 3)
print(x <- cbind(1/4, x))
x <- solve(t(x))[, -1]
summary.lm(model <- aov(weight ~ treatmnt, data = weight, contrasts = list(treatmnt = x)))
summary(model <- aov(weight ~ source * amount, data = weight))
with(weight, interaction.plot(amount, source, weight))

weight3 <- read.table('weight3.dat', header = TRUE)
weight3$amount <- relevel(weight3$amount, ref = 'low')
summary(model <- aov(weight ~ source * amount, data = weight3))
anova(lm(weight ~ source * amount, data = weight3))
tapply(weight3$weight, weight3$amount, mean)
with(weight3, interaction.plot(amount, source, weight))

influ <- read.table('influ.dat', header = TRUE)
influ$pipette <- relevel(influ$pipette, ref = 'single')
with(influ, tapply(titrate, list(operator, pipette), var))
summary(model <- aov(titrate ~ operator * pipette, data = influ))
model.tables(model, type = 'means')
with(influ, tapply(titrate, list(operator, pipette), mean))
with(influ, interaction.plot(pipette, operator, titrate))

headache <- read.table('headache.dat', header = TRUE)
summary(model <- aov(score ~ headache * treat, data = headache))
summary(model <- aov(log(score) ~ headache * treat, data = headache))
with(headache, interaction.plot(treat, headache, log(score)))
model.tables(model, type = 'means')
tapply(log(headache$score), headache$treat, mean)
model <- aov(log(score) ~ headache * treat, data = headache[-c(5, 18), ])
is.na(headache[c(5, 18), ]$score) <- TRUE
summary(aov(log(score) ~ headache * treat, data = headache))
summary(model)
options()$na.action

plant <- read.table('plant.dat', header = TRUE)
summary(model <- aov(yield ~ temperat * concentr * catalyst, data = plant))
with(plant, interaction.plot(temperat, catalyst, yield))
model.tables(model, type = 'mean')
with(plant, tapply(yield, list(temperat, catalyst), mean))

bath <- read.table('bath.dat', header = TRUE)
summary(model <- aov(log(fc30) ~ time * vigour * sex, data = bath))
model.tables(model, type = 'means')
round(exp(with(bath, tapply(log(fc30), list(time, vigour), mean))), 1)
with(bath, tapply(log(fc30), sex, mean))
summary(update(model, sqrt(fc30) ~ .))
summary(model <- update(model, log(tc30) ~ .))
model.tables(model, type = 'means')
with(bath, tapply(log(tc30), sex, mean))
summary(update(model, sqrt(tc30) ~ .))

foster <- read.table('foster.dat', header = TRUE)
table(foster$litter, foster$mother)
summary(model <- aov(littwt ~ litter * mother, data = foster))
summary(model <- aov(littwt ~ litter * mother, data = foster[-35, ]))
summary(model <- aov(littwt ~ mother * litter, data = foster[-35, ]))

wool <- read.table('wool.dat', header = TRUE)
with(wool, ftable(length, ampli, load))
summary(model <- aov(log(cycles) ~ length * ampli * load, data = wool))
summary(update(model, .~ (length + ampli + load)^2))

soil <- read.table('soil.dat', header = TRUE)
summary(aov(yield ~ treat * soil, data = soil))
with(soil, interaction.plot(soil, treat, yield))
