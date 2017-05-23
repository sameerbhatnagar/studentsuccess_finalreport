## Data Mining for Student Success and Perseverance

This repository hosts the code needed to generate the final report for the PAREA grant.

The electronic version of this report is always up-to date and on the web, viewable at sameerbhatnagar.github.io/studentsuccess_finalreport

### To contribute to the report:
 - clone the repository
 - `mkdir bin/data`
 - make your own working branch off of the master
 - Open the script `data-setup.R` in the folder `bin`
 - set the variable `path_to_data` at the top of the script to point to the absolute path where the .Rdata built by @JonathanGuillemette resides
 - run the script to produce some of useful R objects (will automatically be placed in the folder `bin/data`)
 - press `build-book` in RStudio IDE to get a local preview of the entire book in
 - Modify **only the `.Rmd` files**. Commit, and make a pull request back to me (Sameer), and I will merge to master branch, which will automatically update the live web version

### PDF version 
 - should always available at https://github.com/sameerbhatnagar/studentsuccess_finalreport/blob/master/docs/studentsuccess_final_report.pdf

