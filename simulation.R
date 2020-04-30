odata <- read.csv("Will Lopez PF Dataset for Min Zhang.csv")
ndata <- odata[odata$Sample.Type != "PF",]
nrow(ndata)

res <- c() # a vector of 0/1, 0:not detected, 1:detected
for (i in 1:10000) {
  sampleid <- sample(1:nrow(ndata), size = 30, replace = T) # change size correspondingly
  sampledat <- ndata[sampleid, ]
  if (sum(sampledat$BS.Result == "POS") >= 1) {
    res[i] <- 1
  } else {
    res[i] <- 0
  }
}
power <- mean(res)
power



power %>% print # this is a change