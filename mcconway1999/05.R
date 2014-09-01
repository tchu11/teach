# McConway KJ, Jones MC, Taylor PC.
# Statistical Modelling using GENSTAT.
# London, UK: Arnold, 1999.
# ISBN: 0340759852

# Code fragment for diagnostic plots
par(mfrow = c(2, 3))
plot(model, which = 1:6)
dev.off()

# Chapter 5: One-way analysis of variance
etruscan <- read.table('etruscan.dat', header = TRUE)
table(etruscan$origin)
round(tapply(etruscan$breadth, etruscan$origin, mean), 1)
round(tapply(etruscan$breadth, etruscan$origin, var), 2)
plot(breadth ~ origin, data = etruscan)
t.test(breadth ~ origin, data = etruscan, var.equal = TRUE)
etruscan$orvar <- as.numeric(etruscan$origin) - 1
print(model <- lm(breadth ~ orvar, data = etruscan))
anova(model)
anova(update(model, . ~ origin))

fats <- read.table('fats.dat', header = TRUE)
tapply(fats$absorb, fats$fat, summary)
plot(absorb ~ fat, data = fats)
anova(model <- lm(absorb ~ fat, data = fats))

salinity <- read.table('salinity.dat', header = TRUE)
round(tapply(salinity$salinity, salinity$wtrmass, var), 4)
plot(salinity ~ wtrmass, data = salinity)

plywood <- read.table('plywood.dat', header = TRUE)
tapply(plywood$strength, plywood$glue, summary)
tapply(plywood$strength, plywood$glue, var)
plot(strength ~ glue, data = plywood)
anova(model <- lm(strength ~ glue, data = plywood))

singers <- read.fwf('singers.dat', widths = c(7, 8), header = FALSE, skip = 1, strip.white = TRUE)
colnames(singers) <- c('height', 'voice')
anova(lm(height ~ voice, data = singers))
# see http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm
singers$tenor <- singers$voice
levels(singers$tenor) <- c(0, 0, 1, 1)
print(x <- matrix(c(rep(.25, 4), -1, rep(1/3, 3), rep(-.5, 2), rep(.5, 2), rep(-1/3, 3), 1), ncol = 4))
x <- solve(t(x))[, -1]
summary(lm(height ~ voice, data = singers, contrasts = list(voice = x)))
summary(lm(height ~ tenor, data = singers))

pea <- read.table('pea.dat', header = TRUE)
pea$sugar <- factor(pea$sugar, levels = c('control', 'glucose', 'fructose', 'g&f', 'sucrose'))
round(tapply(pea$length, pea$sugar, var), 3)
plot(length ~ sugar, data = pea)
model <- lm(length ~ sugar, data = pea)
anova(model)
summary(model)
# see http://www.ats.ucla.edu/stat/r/faq/testing_contrasts.htm
pea$case <- as.numeric(pea$sugar != 'control')
x <- matrix(-.25, nrow = 5, ncol = 5)
diag(x) <- 1
y <- matrix(c(rep(.5, 2), rep(-1/3, 3), rep(1/3, 3), rep(-.5, 2)), ncol = 2)
print(x <- cbind(1/5, x[, 1], y, -x[, 5]))
x <- solve(t(x))[, -1] # remove intercept term
summary(lm(length ~ case, data = pea))
summary(model <- lm(length ~ sugar, data = pea, contrasts = list(sugar = x)))
round(confint(model)[2, ], 2)
y <- matrix(-1/3, nrow = 5, ncol = 5)
diag(y) <- 1
y[1, 5] <- y[5, 4] <- y[4, 3] <- y[3, 2] <- 0
y[, 1] <- 1/5
y <- solve(t(y))[, -1]
summary(model <- lm(length ~ sugar, data = pea, contrasts = list(sugar = y)))

babies <- read.fwf('babies.dat', widths = c(9, 16), header = FALSE, skip = 1, strip.white = TRUE)
colnames(babies) <- c('walktime', 'exercise')
babies$exercise <- factor(babies$exercise, levels = c('8-week controls', 'none', 'passive', 'active'))
babies$group <- rep(1:4, c(6, 6, 6, 5))
with(babies, table(exercise, group))
plot(walktime ~ group, data = babies, pch = 4)
anova(model <- lm(walktime ~ exercise, data = babies))
anova(model <- lm(walktime ~ exercise, data = babies[-c(5, 12), ]))
anova(model <- lm(walktime ~ exercise, data = babies[-c(5, 12, 15), ]))

brain <- read.table('brain.dat', header = TRUE)
brain$hemi <- factor(brain$hemi, levels = c('left', 'right', 'both'))
par(mfrow = c(1, 2))
plot(recall ~ hemi, data = brain)
plot(recall ~ as.numeric(hemi), data = brain, pch = 4)
anova(model <- lm(recall ~ hemi, data = brain))
brain$both <- as.numeric(brain$hemi == 'both')
x <- matrix(-.5, nrow = 3, ncol = 3)
diag(x) <- 1
x[, 1] <- 1/3
x <- solve(t(x))[, -1]
summary(lm(recall ~ hemi, data = brain, contrasts = list(hemi = x)))
summary(update(model, . ~ both))
