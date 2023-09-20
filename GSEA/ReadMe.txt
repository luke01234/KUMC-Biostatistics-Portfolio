This folder contains All inputs and outputs of the GSEA function as well as the required files to run the function.

GSEA is a tool to infer biological pathway activity from gene expressions.
The functions used in the GSEA script are copied from here: https://bioinformaticsbreakdown.com/how-to-gsea/ with slight edits made by myself to make the code work to our needs.

I was tasked with reading through the documentation and understanding this tool for our own personal use.

In order to achieve this many steps had to be taken:

1)  I needed to read through the code on the page above to understand the functions provided and add them to our own script. This script can be found in the "R" directory.

2)  Download the necessary files (Go_Files) to identify gene pathways, as well as a sample input file so that I could test whether the code was functioning properly or not.
(these can be found in "Go_Files" and "input Files" respectively. S4_Table was the file used as a testing dataset)

3)  I then had to clear some errors from the provided code.

4)  Once done I could run the code and save the plots as PDFS, each of which can be found in the "output Files" directory.