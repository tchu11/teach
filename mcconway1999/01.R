# McConway KJ, Jones MC, Taylor PC.
# Statistical Modelling using GENSTAT.
# London, UK: Arnold, 1999.
# ISBN: 0340759852

# Chapter 1: Introduction
gvhd <- read.table('gvhd.dat', header = TRUE)
plot(donage ~ recage, data = gvhd, pch = 20)
plot(gvhd ~ recage, data = gvhd, pch = 20)
plot(recage ~ factor(gvhd), data = gvhd, horizontal = TRUE)
plot(donage ~ factor(gvhd), data = gvhd, horizontal = TRUE)
plot(indx ~ factor(gvhd), data = gvhd, horizontal = TRUE)

print(x <- table(GvHD = gvhd$gvhd, RecipientSex = gvhd$recsex))
print(x <- table(GvHD = gvhd$gvhd, type = gvhd$type))
print(x <- table(GvHD = gvhd$gvhd, donmfp = gvhd$donmfp))
round(prop.table(x, 2), 2)
