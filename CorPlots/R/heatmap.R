library("heatmaply")
library("readr")

myData = read.csv("Replicate Averages.csv")

#create a new dataframe to loop through protein names in the preys, and remove the duplicates
newFrame=data.frame(myData[1,])
newFrame=newFrame[-1,]
i=1
nameList=list()
  while (i<=nrow(myData))
{
  if (myData[i,1] %in% nameList)
  {
    print("skip")
  }
  else
  {
    nameList[length(nameList)+1]=myData[i,1]
    newFrame[nrow(newFrame)+1,]=myData[i,]
  }
    i=i+1
  }
write.csv(newFrame,"no_repeats.csv", row.names = FALSE)

#write the new, no duplicate version of the file, run the heatmap on that

#myData = read.csv("filtered topS/topS_filtered_by_50.csv", header=TRUE,sep =",", row.names = 1)
myData = read.csv("no_repeats.csv", header=TRUE,sep =",", row.names = 1)


#myData = read.csv("filtered topS/topS_filtered_by_50.csv")

#myData = cor(data)

heatmaply(myData,
  xlab = "Baits",
  ylab = "Preys", 
  main = "Data transformation Raw")

heatmaply(
  percentize(myData),
  xlab = "Baits",
  ylab = "Preys", 
  main = "Data transformation using 'percentize'"
)

heatmaply_cor(
  cor(myData),
  xlab = "Halo_ARID4A_REPLICATE_dS_AVG",
  ylab = "ROW_NAMES",
  k_col = 3,
  k_row = 3
)

