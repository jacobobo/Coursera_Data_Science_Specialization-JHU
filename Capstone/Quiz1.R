## Q1

file.info("Data/Original/en_US/en_US.blogs.txt")$size / 1024^2

## Q2

conTwitter <- file("Data/Original/en_US/en_US.twitter.txt", "r")
tweets <- readLines(conTwitter)
length(tweets)

## Q3

conNews <- file("Data/Original/en_US/en_US.news.txt", "r")
conBlogs <- file("Data/Original/en_US/en_US.blogs.txt", "r")
news <- readLines(conNews)
blogs <- readLines(conBlogs)

max(nchar(blogs))
max(nchar(news))

## Q4

love <- sum(grepl("love",tweets))
hate <- sum(grepl("hate",tweets))
love/hate

## Q5

answer <- grep("biostats",tweets)
tweets[answer]

## Q6

sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing",tweets))