library(car)

movies <- read.csv("movie_metadata.csv", header=T, na.strings = "")
str(movies)

is.fact <- sapply(movies, is.factor)
movies_nofactors <- movies[, !is.fact]
movies_noNA <- movies[complete.cases(movies),]
movies_nofactorsnoNA <- movies_nofactors[complete.cases(movies_nofactors),]

lm1 <- lm(gross~., data=movies_nofactors)
summary(lm1)

na_cols <-data.frame(sapply(movies, function(x) sum(length(which(is.na(x))))))
na_rows <- data.frame(apply(movies, 1, function(x) length(which(is.na(x)))))

lm_budget <- lm(gross~budget, data=movies_nofactors)
