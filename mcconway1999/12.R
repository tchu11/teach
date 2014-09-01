# McConway KJ, Jones MC, Taylor PC.
# Statistical Modelling using GENSTAT.
# London, UK: Arnold, 1999.
# ISBN: 0340759852

anova(model, update(model, .~.^5), test = 'Chisq')
anova(model, test = 'Chisq')
summary(model)
exp(coef(model))

# Chapter 12: Loglinear models for contingency tables
toxic <- read.table('toxic.dat', header = TRUE)
x <- toxic[rep(1:4, toxic$count), 2:3]
print(table(x))
chisq.test(table(x), correct = FALSE)
fisher.test(table(x))
model <- glm(count ~ impsulph + toxicity, toxic, family = 'poisson')
model <- update(model, .~.^2)
predict(model, type = 'response')
chisq.test(table(x), correct = FALSE)$expected

beetles <- read.table('beetles.dat', header = TRUE)
x <- beetles[rep(1:8, beetles$count), 2:3]
chisq.test(table(x), correct = FALSE)
model <- glm(count ~ colour + season, beetles, family = 'poisson')

banvote <- read.table('banvote.dat', header = TRUE)
x <- banvote[rep(1:8, banvote$count), 2:4]
model1 <- glm(count ~ class + vote + gender, banvote, family = 'poisson')
model2 <- update(model1, .~.^2)
model3 <- update(model1, .~.^3)
anova(model1, model3, test = 'Chisq')
anova(model2, model3, test = 'Chisq')
drop1(model2, test = 'Chisq')
model <- update(model2, .~. - class:gender)
y <- cbind(fits = round(predict(model, type = 'response'), 1), banvote)
with(x, table(vote, class))
tapply(y$fits, list(y$vote, y$class), sum)
with(x, table(vote, gender))
tapply(y$fits, list(y$vote, y$gender), sum)
with(x, table(class, gender))
tapply(y$fits, list(y$class, y$gender), sum)

helping <- read.table('helping.dat', header = TRUE)
x <- helping[rep(1:4, helping$count), 2:3]
chisq.test(table(x), correct = FALSE)
fisher.test(table(x))
model <- glm(count ~ gender + help, helping, family = 'poisson')

pineseed <- read.table('pineseed.dat', header = TRUE)
model <- glm(count ~ depth + seedtype + mortalty, pineseed, family = 'poisson')
model1 <- update(model, .~.^3)
model2 <- update(model, .~ depth * seedtype + mortalty)
model3 <- update(model, .~.^2)
model4 <- update(model2, .~. + mortalty:depth)
model5 <- update(model2, .~. + mortalty:seedtype)
anova(model2, model1, test = 'Chisq')
anova(model3, model1, test = 'Chisq')
anova(model4, model1, test = 'Chisq')
anova(model5, model1, test = 'Chisq')
x <- with(pineseed, interaction(depth, seedtype))
pineseed$id <- as.integer(x)
pineseed <- reshape(pineseed, direction = 'wide', idvar = 'id', timevar = 'mortalty')
pineseed <- pineseed[, c(2, 5:7)]
colnames(pineseed) <- c('dead', 'alive', 'depth', 'seedtype')
model <- glm(as.matrix(pineseed[, 1:2]) ~ depth + seedtype, pineseed, family = 'binomial')
summary(model3)
summary(model)

danish <- read.table('danish.dat', header = TRUE)
danish$work <- relevel(danish$work, ref = 'skilled')
danish$tenure <- relevel(danish$tenure, ref = 'rent')
model <- model0 <- glm(count ~ age + work + tenure + acctype + resp, danish, family = 'poisson')
model <- update(model0, .~.^2)
model <- update(model0, .~.^3)
model <- step(model, direction = 'both', scope = list(upper = as.formula('count ~ (age + work + tenure + acctype + resp)^3'), lower = as.formula('count ~ (age + work + tenure + acctype + resp)^2')))
model <- step(model, direction = 'both', scope = list(upper = as.formula('count ~ (age + work + tenure + acctype + resp)^3'), lower = as.formula('count ~ age + work + tenure + acctype + resp')))
model <- glm(count ~ resp + (age + work + tenure + acctype)^4, danish, family = 'poisson')
model <- glm(count ~ resp * (age + work + tenure + acctype)^4, danish, family = 'poisson')
model <- step(model, direction = 'both', scope = list(upper = as.formula('count ~ resp * (age + work + tenure + acctype)^4'), lower = as.formula('count ~ resp + (age + work + tenure + acctype)^4')))
model <- update(model, .~. - resp:age:work:tenure:acctype)
model <- update(model, .~. - resp:age:work:acctype)

danishlr <- read.table('danishlr.dat', header = TRUE)
danishlr$work1 <- relevel(danishlr$work1, ref = 'skilled')
danishlr$tenure1 <- relevel(danishlr$tenure1, ref = 'rent')
model <- glm(as.matrix(danishlr[, 1:2]) ~ (age1 + work1 + tenure1 + acctype1)^3, danishlr, family = 'binomial')
model <- glm(as.matrix(danishlr[, 1:2]) ~ (age1 + work1 + tenure1 + acctype1)^2, danishlr, family = 'binomial')
model <- step(model)
