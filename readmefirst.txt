![AppsForBiostats Logo](https://github.com/paulrheeder-rgb/shinyappsforbiostats/blob/main/images/ShinyAppsLogo3.jpg)

USING SHINY APPS FOR BIOSTATISTICS
=================================

STEP 1: Install R
https://cran.r-project.org

STEP 2: Install RStudio

https://posit.co/download/rstudio-desktop/

STEP 3: Find zipped folder
Follow these steps exactly:

Open this link in your web browser
👉

https://github.com/paulrheeder-rgb/shinyappsfiorbiostats

On the GitHub page, click the green “Code” button (top right)

Click “Download ZIP”

Wait for the download to finish
(A file called something like shinyappsforbiostats-main.zip will appear)

Go to your Downloads folder

Right-click the ZIP file
→ choose “Extract All…”
→ click Extract

Move the extracted folder to:

Documents

You should now have:

Documents/
└── shinyappsforbiostats-main/

STEP 4: Rename the folder (recommended)

Right-click the folder:

shinyappsforbiostats-main


Rename it to:

Biostats_Shiny_Apps

STEP 4: Open RStudio

STEP 5: Install required packages (run once)
-----------------------------------------------
install.packages(c(
  "shiny",
  "ggplot2",
  "dplyr",
  "emmeans",
  "marginaleffects"
))

STEP 6: Run an app
------------------
Example (Boxplots):

shiny::runApp("Biostats_Shiny_Apps/08_Boxplots")

If that does not work, use the full path, e.g.:

shiny::runApp(
  "C:/Users/YOURNAME/Documents/Biostats_Shiny_Apps/08_Boxplots"
)
