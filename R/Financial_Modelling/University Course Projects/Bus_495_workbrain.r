
setwd("~/Downloads")

workb <- read.csv("bus495.EV.wb.comp.csv")
salesb <-read.csv("bus496.wb.salesgrowth.csv")

bp.1 <- ggplot(workb, aes(x="", y= Enterprise.Value, fill = Company)) + geom_bar(width = 1,
                                                                                 stat = "identity")
bp.1

workb$Enterprise.Value <- reorder(workb$Enterprise.Value, X = workb$Enterprise.Value, FUN = function(x) -length(x))

at <- nrow(workb) - as.numeric(cumsum(sort(table(workb)))-0.5*sort(table(workb)))

label=paste0(round(sort(table(workb))/sum(table(workb)),2) * 100,"%")


pie.Nasdaq.1 <- bp.1 + coord_polar("y", start = 0) + theme_bw()+  ggtitle("Figure B: ERM Industry Enterprise Value (2003)") + theme(
  plot.title = element_text(color="black", size=25, face="bold.italic"))+theme(plot.title = element_text(hjust = 0.5))+  coord_polar(theta="y") 
pie.Nasdaq.1

geom_bar(width = 1) +
  coord_polar(theta="y") +

salesb <- t(salesb)

mdf <- melt(salesb, id.vars="X")

colnames(mdf) <- c("Year","Metrics", "Rate.of.Change")

ggplot(data=mdf, aes(x=Year, y=Rate.of.Change, group=Metrics, colour=Metrics)) +
  geom_line() + theme_bw() + geom_point()+ggtitle("Figure A: Workbrain Historical and Projected Growth Rates 2001-2005") + 
  theme(plot.title = element_text(color="black", size=20, face="bold.italic"))+
  theme(plot.title = element_text(hjust = 0.5))
