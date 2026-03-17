![AppsForBiostats Logo](https://github.com/paulrheeder-rgb/shinyappsforbiostats/blob/main/images/image%20(2).png)



USING SHINY APPS FOR BIOSTATISTICS
=================================

Data analysis always has several steps: import data, fix format and clean names and white spaces, 
fix data types (such as factors with levels), check missing values, check and manage extreme values, do 
exploratory graphs and descriptive statistics, create tables, more box and scatter plots and finally 
regresion analysis.

Here we have an App for each of these steps.
1. DataDictionary: merge data with a dictionary file 
2. PostImport: open file and fix data format and names, choose data types
3. DplyrEdit: do data manipulation with Dplyr
4. Table1AllStats: look at/explore the data 
5. Table1or2: make nice tables, single or more groups
6. GraphAllvars: graph the numerical and categorical variables
7. GraphAllVarsComined: graph numerical and categorical variables, basic box and scatterplots
8. Boxplots
9. Scatterplots
10. Regression: linear and logistic regression, test assumptions, show detail such as 
goodness of fit and calibration, show plots.

These Apps were developed with the help of Copilot and ChatGPT.

How to get started:

STEP 1: Install R
https://cran.r-project.org
------------------

STEP 2: Install RStudio
------------------
https://posit.co/download/rstudio-desktop/

STEP 3: Find zipped folder
------------------
Follow these steps exactly:

Open this link in your web browser
👉

https://github.com/paulrheeder-rgb

click on shinyappsforbiostats

On the GitHub page, click the green “Code” button (top right)

Click “Download ZIP”

Wait for the download to finish
(A file called something like shinyappsforbiostats-main.zip will appear)

Go to your Downloads folder

Right-click the ZIP file
→ choose “Extract All…”
→ click Extract

 Extract to Documents on your PC (or any other folder)
 Not to Documents/shinyapps............... just Documents/

or in a drive eg google drive so you can access anywhere

You should now have:

Documents/
└── shinyappsforbiostats-main/

STEP 4: Rename the folder (recommended)
------------------

Right-click the folder:

shinyappsforbiostats-main


Rename it to:

shinyappsforbiostats

STEP 5: Open RStudio
------------------
 choose  file>>>>Open Project>>>>> go to the shinyappsforbiostats   folder
 click on the shinyappsfor biostats project file
 
 This project will now open in RStudio and in the Right pane under files you will see all the Apps
 
STEP 6: Open an app
------------------
Open the folder and click on app.R
The is app will appear as a script file
At the top of the file it will say which libraries you need to install

Step 6: Then choose run App
------------------
