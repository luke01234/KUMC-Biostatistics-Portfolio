library(readr)
require(sqldf)

#seeking determines the string we search for in the columns
#search for "_AVG" to look for averages, "LogFoldChange" to look for LogFoldChange, "Zstatistic" to look for Zstat

seeking ="_AVG"

table = read.csv.sql("Filtered_S2D.csv", "select * from file where TOTAL_QSPEC_FILTER_SCORE>0")
newDataFrame = data.frame(table["NCBI_Gene"],table["Locus"],table["Length"])
for (x in colnames(table))
{
  if (grepl(seeking,x,fixed=TRUE))
  {
    newDataFrame = cbind(newDataFrame,table[x])
    #print(x)
  }
}

write.csv(newDataFrame,paste("Filtered_S2D",seeking,".csv",sep=""), row.names=FALSE)