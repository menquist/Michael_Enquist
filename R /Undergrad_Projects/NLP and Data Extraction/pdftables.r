library(pdftools)

download.file("http://www.rrexploration.com/pdf/Financials_2016Q4.pdf", "Financials_2016Q4.pdf", mode = "wb")

txt <- pdf_text("Financials_2016Q4.pdf")
cat(txt[1])
toc <- pdf_toc("Financials_2016Q4.pdf")
jsonlite::toJSON(toc, auto_unbox = TRUE, pretty = TRUE)
# Author, version, etc
info <- pdf_info("Financials_2016Q4.pdf")

# Table with fonts
fonts <- pdf_fonts("Financials_2016Q4.pdf")

# renders pdf to bitmap array
bitmap <- pdf_render_page("Financials_2016Q4.pdf", page = 1)

# save bitmap image
png::writePNG(bitmap, "page.png")
jpeg::writeJPEG(bitmap, "page.jpeg")
webp::write_webp(bitmap, "page.webp")
  
BS <- cat(txt[3])


library(tabulizer)
library(devtools)
library(dplyr)

# Location of WARN notice pdf file
location <- 'http://www.rrexploration.com/pdf/Financials_2016Q4.pdf'

# Extract the table
out <- extract_tables(location)

final <- do.call(rbind, out[-length(out)])


lst <- extract_tables(location, encoding="UTF-8") 
# peep into the doc for further specs (page, location etc.)!

# after examing the list you want to do some tidying
# 1st delete blank columns
lst[[1]] <- lst[[1]][, -3]
lst[[2]] <- lst[[2]][, -4]

# 2nd bind the list elements, if you want and create a df...
table <- do.call(rbind, lst)
table <- as.data.frame(table[c(2:37, 40:nrow(table)), ],
                       stringsAsFactors=FALSE) # ...w/o obsolete rows

# 3rd take over colnames, cache rownames to vector
colnames(table) <- table[1, ]
rn <- table[2:71, 1]
table <- table[-1,-1] # and bounce them out of the table

# 4th I'm sure you want coerce to numeric 
table <- as.data.frame(apply(table[1:70,1:10], 2, 
                             function(x) as.numeric(as.character(x))))
rownames(table) <- rn # bring back rownames 

table # voilà

