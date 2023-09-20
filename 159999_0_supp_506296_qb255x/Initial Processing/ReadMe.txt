"159999_0_supp_506296_qb255x.xlsx" is an excel workbook that contains supplementary tables for a KUMC study

The sheet of interest within this workbook was sheet "S2D" and it is the one from which I extracted and formatted data.

I was tasked with:

1)  Cleaning the file of any text that did not pertain to the table within the sheet

2)  Changing the cutoffs for protein filter scores (Zstat and LogFoldChange)

3)  Creating a new column that contained the average dS value for each protein across their replicates
(Steps 1, 2 and 3 were done by hand in excel and saved to "NewS2DwithAVG.xlsx" and my work continued from said file.)

4)  Filter out all rows (proteins) that did not show significance (no filter score of 2 for any other protein.)
(this was then saved under "Filtered_S2D.csv" where work continued.)

5)  Create a table containing only the proteins' dS Average value columns
(saved under "Filtered_S2D_dS_AVERAGES.csv".)

6)  Create a table containing only the proteins' Zscores
(saved under "Filtered_S2D_Zscores.csv".)

7)  Create a table containing only the proteins' LogFoldChange
(saved under "Filtered_S2D_LogFoldChange.csv".)

8)  Finally compile the above 3 files into one excel workbook 
(saved under "Final_Filtered_S2D".)

(Steps 4 through 8 were completed using R scripts written by myself in the process of gaining an understanding of the language. They can be found in the "R" directory.)

This concludes the initial processing section. Work continues in the topS directory.