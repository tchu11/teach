# McConway KJ, Jones MC, Taylor PC.
# Statistical Modelling using GENSTAT.
# London, UK: Arnold, 1999.
# ISBN: 0340759852

# Chapter 2: Review of statistical concepts
hald <- read.table('hald.dat', header = TRUE)
forearm <- read.table('forearm.dat', header = TRUE)
magsus <- read.table('magsus.dat', header = TRUE)
hald <- hald$hald
forearm <- forearm$forearm
magsus <- magsus$magsus

par(mfrow = c(2, 2))
hist(hald, breaks = seq(1, 3.5, by = .25), xlab = 'Breaking strength (kg)')
hist(forearm, xlab = 'Forearm length (inches)')
hist(magsus, breaks = 0:14, xlab = 'Magnetic susceptibility (SI units)')

par(mfrow = c(1, 2))
hist(hald, breaks = seq(1, 3.5, by = .25), freq = FALSE, 
     xlab = 'Breaking strength (kg)', ylim = c(0, 1))
x <- sort(hald)
curve(dnorm(x, mean = 2.299, sd = sqrt(.1689)), add = TRUE)
hist(forearm, freq = FALSE, xlab = 'Forearm length (inches)')

par(mfrow = c(2, 2))
qqnorm(hald, main = 'hald', pch = 20)
qqnorm(forearm, main = 'forearm', pch = 20)
qqnorm(magsus, main = 'magsus', pch = 20)

par(mfrow = c(2, 2))
hist(log(magsus), breaks = seq(0, 3, by = .25))
qqnorm(log(magsus), pch = 20)
hist(sqrt(magsus), breaks = seq(1, 4, by = .25))
qqnorm(sqrt(magsus), pch = 20)
dev.off()

t.test(hald)
t.test(forearm, conf.level = .9)
t.test(hald, mu = 2)
t.test(hald, mu = 2, alternative = 'greater')
t.test(forearm, mu = 18.5, conf.level = .9)

weantoil <- read.table('weantoil.dat', header = TRUE)
weantoil$diff <- with(weantoil, toil - wean)
qqnorm(weantoil$diff, pch = 20)
t.test(weantoil$diff)
t.test(weantoil$diff, alternative = 'greater')

carpet <- read.table('carpet.dat', header = TRUE)
round(apply(carpet, 2, mean), 2)
round(apply(carpet, 2, var), 2)
t.test(carpet$carp, carpet$uncarp, var.equal = TRUE)

hcii <- read.table('hcii.dat', header = TRUE)
hcii <- transform(hcii, 
                  Group1 = a1 - b1, 
                  Group2 = a2 - b2)
x <- stack(hcii[, 5:6])
plot(values ~ ind, data = x, horizontal = TRUE, 
     xlab = '', ylab = 'After-before differences')
par(mfrow = c(1, 2))
qqnorm(hcii$Group1)
qqnorm(hcii$Group2)
round(apply(hcii, 2, var), 4)
t.test(hcii$Group1, hcii$Group2, var.equal = TRUE)

cu <- qchisq(.975, 49)
cl <- qchisq(.025, 49)
round(var(hald) * 49 / c(cu, cl), 4)
var.test(hcii$Group1, hcii$Group2)

honeybee <- read.table('honeybee.dat', header = TRUE)
honeybee <- honeybee$honeybee
round(prop.table(table(honeybee, useNA = 'ifany')), 3)
prop.test(sum(honeybee), length(honeybee))

hospdth <- read.table('hospdth.dat', header = TRUE)
x <- rep(hospdth$x, hospdth$fx)
mean(x)
par(mfrow = c(1, 2))
barplot(hospdth$fx, ylab = 'Frequency', 
        names.arg = hospdth$x, xlab = 'Number of deaths')
barplot(table(rpois(1000, mean(x))))
var(x)
exp(confint.default(glm(x ~ 1, family = 'poisson'), level = .9))
round(mean(x) + c(-1, 1) * qnorm(.95) * sqrt(mean(x) / length(x)), 3)

smith <- read.table('smith.dat', header = TRUE)
x <- smith$smith
t.test(x)
round(mean(x) + c(-1, 1) * qnorm(.95) * sqrt(var(x) / length(x)), 3)
