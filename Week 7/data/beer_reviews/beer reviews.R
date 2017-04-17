library(reshape2)
library(plyr)

df <- read.csv("./data/beer_reviews.csv")

head(df)

n <- 100
beer_counts <- table(df$beer_name)
beers <- names(beer_counts[beer_counts > n])
df <- df[df$beer_name %in% beers,]

df.wide <- dcast(df, beer_name ~ review_profilename,
                 value.var='review_overall', mean, fill=0)
head(df.wide)
dists <- dist(df.wide[,-1], method="euclidean")
dists <- as.data.frame(as.matrix(dists))
colnames(dists) <- df.wide$beer_name
dists$beer_name <- df.wide$beer_name
head(dists)


getSimilarBeers <- function(beers_i_like) {
  beers_i_like <- as.character(beers_i_like)
  cols <- c("beer_name", beers_i_like)
  best.beers <- dists[,cols]
  if (ncol(best.beers) > 2) {
    best.beers <- data.frame(beer_name=best.beers$beer_name, V1=rowSums(best.beers[,-1]))
  }
  results <- best.beers[order(best.beers[,-1]),]
  names(results) <- c("beer_name", "similarity")
  results[! results$beer_name %in% beers_i_like,]
}
getSimilarBeers(c("Coors Light"))

model.transform <- function(df) {
  df
}

model.predict <- function(df) {
  getSimilarBeers(df$beers)
}

testcase <- data.frame(beers=c("Coors Light"))
model.predict(testcase)


library(yhatr)
yhat.config <- c(username="{ USERNAME }", apikey="{ APIKEY }", env="http://cloud.yhathq.com/")
yhat.deploy("BeerRecR")