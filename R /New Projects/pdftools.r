library(pdftools)
setwd("~/Downloads/chatbotdata/")
filenames <- list.files(pattern="*.pdf",all.files = T)
txt = lapply(filenames, pdf_text)
n.obs = sapply(txt, length)
seq.max = seq_len(max(n.obs))
mat <- t(sapply(txt,"[",i= seq.max))
mat <- data.frame(mat)
#Text = gsub(', [^A-za-z]+', ' ',do.call(paste,c(mat,sep=' ')))
#mat <- data.frame(Text, stringsAsFactors = FALSE)
#mat$Text <- gsub('\\NA', "", mat$Text)
x <- colnames(mat)
mat$Title <- as.factor(filenames)
mat$section <- "Stats"
library(reshape)
long <- melt(mat, id.vars = c("Title","section"))
long$variable <- sub("^X","",long$variable)
colnames(long)[3] <- "Page Number"
colnames(long)[4] <- "Text"
long <- long[!(is.na(long$Text) | long$Text==""), ]
library(rJava)
#library(XLConnect)

#creating an Excel workbook. Both .xls and .xlsx file formats can be used.
#wb <- loadWorkbook("Stat Learn summary.xlsx", create = TRUE)

#creating sheets within an Excel workbook
#createSheet(wb, name = "documents")

#writing into sheets within an Excel workbook : 
#writing ChickWeight data frame into chickSheet
#writeWorksheet(wb, long, sheet = "documents")

#saving a workbook to an Excel file :
#saves a workbook to the corresponding Excel file and writes the file to disk.
#saveWorkbook(wb)
library(xlsx)

#wb <- createWorkbook()
#sheet1 <- createSheet(wb, sheetName = 'documents')
setwd("~/Slack-AI-ChatBot/Chatbot")
write.xlsx2(long, "doc2vec-dat.xlsx", sheetName = 'documents', append = FALSE)

#str(long)
