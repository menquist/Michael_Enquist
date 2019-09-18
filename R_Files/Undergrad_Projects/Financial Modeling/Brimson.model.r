install.packages("pa")
library(pa)
data(year)
names(year)

data(jan)
str(jan)
str(year)
br.single <- brinson(x = jan, date.var = "date",
                         cat.var = "sector",
                       bench.weight = "benchmark",
                       portfolio.weight = "portfolio",
                       ret.var = "return")
summary(br.single)
plot(br.single, var = "sector", type = "return")

#multi brimson model
data(quarter)
str(quarter)
br.multi <- brinson(quarter, date.var = "date",

                    cat.var = "sector",
                    bench.weight = "benchmark",
                    portfolio.weight = "portfolio",
                    ret.var = "return")

exposure(br.multi, var = "size")

returns(br.multi, type = "linking")
plot(br.multi, type = "return")



#regress

rb.single <- regress(quarter, date.var = "date",
                                     ret.var = "return",
                                      reg.var = c("sector", "growth",
                                                    "size"),
                                      benchmark.weight = "benchmark",
                                      portfolio.weight = "portfolio")
exposure(rb.single, var = "growth")
