This directory contains the files pertaining to the runcodes tool I was given to work with during my time at KUMC

This tool was used for to continue data processing, but first I had to further format the input data.

1)  I was given a list of TCGA genes which can be found in the "TCGA_genes"" directory. I was told to intersect the gene names from the filtered topological scores from my previous work with the gene names in this file.  

2)  I wrote a script to do so and the outputs are within the "topS_intersected" directory. I also included the genes that were lost during the process in separate files. 
(the script can be found in "tools/R")

3)  I then ran the "runcodes" tool found in the "tools/runcodes" directory, using the "topS_filtered_by_50_intersection_proteins.csv" file as my input. The output was then saved under the "runcodes output" direcory.

4)  I then sent these outputs back to my supervisor.