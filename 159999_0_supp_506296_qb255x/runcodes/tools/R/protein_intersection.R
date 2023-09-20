library(readr)
require(sqldf)
library("dplyr")

fileName = "topS_filtered_by_10"

proteinTable = read_csv("TCGA_genes.csv")

topSTable = read_csv(paste(fileName,".csv",sep=""))

intersection = inner_join(topSTable,proteinTable, by = "ROW_NAMES")

difference = sqldf("SELECT * FROM topSTable EXCEPT SELECT * FROM intersection")

write.csv(intersection,paste(fileName,"_intersection_protiens.csv",sep=""), row.names = FALSE)
write.csv(difference,paste(fileName,"_lost_proteins.csv",sep=""), row.names = FALSE)