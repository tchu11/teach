# McConway KJ, Jones MC, Taylor PC.
# Statistical Modelling using GENSTAT.
# London, UK: Arnold, 1999.
# ISBN: 0340759852

# Code fragment for model selection
print(model)
anova(model, update(model, .~ 1), test = 'Chisq')
summary(model)
exp(coef(model))
drop1(model, test = 'Chisq')
model <- step(model, direction = 'both') # from full model, bad practice

# Code fragment for diagnostic plots
par(mfrow = c(2, 3))
plot(model, which = 1:6)
dev.off()

# Chapter 9: Binary regression
burns <- read.table('burns.dat', header = TRUE)
par(mfcol = c(2, 1))
plot(survival ~ larea, burns, pch = 4)
plot(larea ~ factor(survival), burns, horizontal = TRUE)
burns$group <- cut(burns$larea, c(1.2, 1.5, seq(1.7, 2.4, by = .1)), right = FALSE)
levels(burns$group) <- c(1.35, 1.6, seq(1.75, 2.35, by = .1))
burns$group <- as.numeric(levels(burns$group))[burns$group]
x <- aggregate(burns$survival ~ burns$group, FUN = sum)
y <- table(burns$group)
x <- cbind(x, y)[, -3]
colnames(x) <- c('larea', 'surviving', 'number')
x$proportion <- x$surviving / x$number
print(model <- lm(proportion ~ larea, x))
plot(proportion ~ larea, x, pch = 20, ylim = c(0, 1.3))
abline(model)
model <- glm(survival ~ larea, burns, family = 'binomial')
plot(survival ~ larea, burns, pch = 4)
x <- seq(1.2, 2.4, by = .02)
y <- predict(model, newdata = data.frame(larea = x), type = 'response')
points(y ~ x, type = 'l')
predict(model, newdata = data.frame(larea = 2), type = 'response')
exp(.1 * coef(model)[2])

fasten <- read.table('fasten.dat', header = TRUE)
fasten$propn <- fasten$failed / fasten$samsize
plot(propn ~ load, fasten, pch = 4)

p <- function(x, a = 0, b = 1) 1 / (1 + exp(-(a + b * x)))
x <- seq(-10, 10, by = .1)
par(mfrow = c(1, 2))
y <- seq(0, 1, length = length(x))
plot(y ~ x, type = 'n')
for(i in seq(-4, 4, by = 2)) {
  y <- p(x, a = i)
  points(y ~ x, type = 'l')
}
plot(y ~ x, type = 'n')
for(i in c(-3, -.5, .3, 1, 2)) {
  y <- p(x, b = i)
  points(y ~ x, type = 'l')
}

gvhd <- read.table('gvhd.dat', header = TRUE)
gvhd <- transform(gvhd, lindx = log(indx), donmfp = factor(donmfp), type = factor(type))
gvhd$indx <- NULL
model <- glm(gvhd ~ ., gvhd, family = 'binomial')
model <- glm(gvhd ~ 1, gvhd, family = 'binomial')
add1(model, .~. + recage + recsex + donage + donmfp + type + lindx, test = 'Chisq')
round(cor(gvhd[, -(4:6)]), 3)
model <- step(model, direction = 'both', scope = gvhd ~ recage + recsex + donage + donmfp + type + lindx)
add1(model, .~. + recage + donmfp, test = 'Chisq')
model <- update(model, .~. + recage)

wetbirds <- read.table('wetbirds.dat', header = TRUE)
wetbirds$larea <- log(wetbirds$area)
wetbirds$area <- NULL
round(cor(wetbirds[, -(1:2)]), 3)
pairs(wetbirds[, c(15, 6, 10, 14, 1, 2)], pch = 4)
model <- glm(woodduck ~ 1, wetbirds, family = 'binomial') # null model
add1(model, .~. + larea + tht4 + cfor4 + grav4, test = 'Chisq')
model <- glm(woodduck ~ ., wetbirds[, -1], family = 'binomial') # all main effects
model <- step(model, direction = 'both', # from null model, bad practice
              scope = paste('woodduck ~', paste(colnames(wetbirds)[-(1:2)], collapse = '+')))
model <- glm(woodduck ~ grav2, wetbirds, family = 'binomial')
plot(woodduck ~ grav2, wetbirds, pch = 4)
x <- seq(0, 20, by = .1)
y <- predict(model, newdata = data.frame(grav2 = x), type = 'response')
points(y ~ x, type = 'l')
model <- update(model, data = wetbirds[-9, ])

model <- glm(gbheron ~ ., wetbirds[, -2], family = 'binomial') # all main effects
model <- glm(gbheron ~ 1, wetbirds, family = 'binomial') # null model
model <- step(model, direction = 'both', # from null model, bad practice
              scope = paste('gbheron ~', paste(colnames(wetbirds)[-(1:2)], collapse = '+')))

death <- read.table('death.dat', header = TRUE)
apply(death[, c(2, 8, 4, 7, 6, 5, 9, 1)], 2, function(i) table(death = death$Death, i))
round(cor(death[, c(2, 8, 1, 4, 7, 6, 5, 9)]), 3)
death <- cbind(death[, c(3, 1)], apply(death[, c(2, 4:9)], 2, function(i) i - 1))
model <- glm(Death ~ 1, death, family = 'binomial')
add1(model, .~. + BD + WV + AC + FV + VS + V2 + MS + YV, test = 'Chisq')
model <- glm(Death ~ ., death, family = 'binomial')
model <- glm(Death ~ WV + VS, death, family = 'binomial')
add1(model, .~. + BD, test = 'Chisq')
model <- update(model, .~. + BD)
add1(model, .~.^2, test = 'Chisq')
print(x <- expand.grid(VS = 0:1, WV = 0:1))
p.hat <- predict(model, newdata = data.frame(x), type = 'response')
round(p.hat / (1 - p.hat), 3) # odds
round(p.hat, 2)
