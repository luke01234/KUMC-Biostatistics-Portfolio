library(readr)
require(sqldf)


table = read.csv.sql("NewS2DwithAVG.csv", "select * from file where TOTAL_QSPEC_FILTER_SCORE>0")
write.csv(table,"Filtered_S2D.csv", row.names = FALSE)