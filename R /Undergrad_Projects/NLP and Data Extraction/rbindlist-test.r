library(data.table)

x <- data.table(A = c(3,2,1), B = 4:6)
y <- data.table(A = 1:4, C = 5:8)
z <- data.table(A = 2:3, D = 5:6)

tbls <- list(x, y, z)
#lapply(tbls, function(i) setkey(i, A))

merged1 <- Reduce(function(...) merge(..., all = T), tbls)



tbls <- list(c, c1, c2,c3)
lapply(tbls, function(i) setkey(i, A))

merged1 <- Reduce(function(...) merge(..., all = T), tbls)

###
c <- cbind(ID = rownames(c), c)
rownames(c) <- 1:nrow(c)



c <- (data.frame(logit_long1$coefficients))
#c$ID <- 1:nrow(c)
c <- cbind(ID = rownames(c), c)
rownames(c) <- 1:nrow(c)
c <-dcast(melt(c, id.vars = "ID"), variable ~ ID)


c1 <- data.frame(logit_long2$coefficients)
#c1$ID <- 1:nrow(c1)
c1 <- cbind(ID = rownames(c1), c1)
rownames(c1) <- 1:nrow(c1)
c1 <-dcast(melt(c1, id.vars = "ID"), variable ~ ID)


c2 <- data.frame(logit_long3$coefficients)
#c2$ID <- 1:nrow(c2)
c2 <- cbind(ID = rownames(c2), c2)
rownames(c2) <- 1:nrow(c2)
c2 <-dcast(melt(c2, id.vars = "ID"), variable ~ ID)


c3 <- data.frame(logit_long4$coefficients)
#c3$ID <- 1:nrow(c3)
c3 <- cbind(ID = rownames(c3), c3)
rownames(c3) <- 1:nrow(c3)
c3 <-dcast(melt(c3, id.vars = "ID"), variable ~ ID)

c4 <- data.frame(logit_long5$coefficients)
#c3$ID <- 1:nrow(c3)
c4 <- cbind(ID = rownames(c4), c4)
rownames(c4) <- 1:nrow(c4)
c4 <-dcast(melt(c4, id.vars = "ID"), variable ~ ID)

c5 <- data.frame(logit_long6$coefficients)
#c3$ID <- 1:nrow(c3)
c5 <- cbind(ID = rownames(c5), c5)
rownames(c5) <- 1:nrow(c5)
c5 <-dcast(melt(c5, id.vars = "ID"), variable ~ ID)

c6 <- data.frame(logit_long7$coefficients)
#c3$ID <- 1:nrow(c3)
c6 <- cbind(ID = rownames(c6), c6)
rownames(c6) <- 1:nrow(c6)
c6 <-dcast(melt(c6, id.vars = "ID"), variable ~ ID)

EXTRACT <- rbindlist(list(c,c1,c2,c3,c4,c5,c6 ), fill=T)



tbls <- list(c, c1, c2,c3)
lapply(tbls, function(i) setkey(i, ID))

merged1 <- Reduce(function(...) merge(..., all = T), tbls)
