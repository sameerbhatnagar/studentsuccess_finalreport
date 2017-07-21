## Data Mining for Student Success and Perseverance

This repository hosts the code needed to generate the final report for the PAREA grant.

The electronic version of this report is always up-to date and on the web, viewable at sameerbhatnagar.github.io/studentsuccess_finalreport

### To contribute to the report:
 - clone the repository
 - `mkdir bin/data`
 - make your own working branch off of the master
 - Open the R Project file `studentsuccess_finalreport.Rproj` with un updated version of 
     - R 
     - RStudio
 - in the R console, run the command 
 ```
 > Sys.setenv(R_path_to_data_directory = '<Enter Absolute Path to your local copy of Data>')
```

 - Open the script `data-setup.R` in the folder `bin`
 - run the script to produce some of useful R objects (will automatically be placed in the folder `bin/data`)
 - press `build-book` in RStudio IDE to get a local preview of the entire book in
 - Modify **only the `.Rmd` files**. Commit, and make a pull request back to me (Sameer), and I will merge to master branch, which will automatically update the live web version

### PDF version 
 - should always available at https://github.com/sameerbhatnagar/studentsuccess_finalreport/blob/master/docs/studentsuccess_final_report.pdf


 ### Collaborating Colleges
  - for any CEGEP that would like to extract their CLARA tables, into the same anonymized format as what was used for this project, please refer to the SQL procedures in the folder `bin/SQL`


## This project was made possible by:
 - PAREA (Programme d'Aide de Recherche et Enseignment et Apprentissage)
 - Derek Gaucher and Anna Likht, from the Dawson College department of Information Systems and Technology, for writing the data extraction scripts that could be used by collaborating colleges
 - Diane Gauvin, Academic Dean, Dawson College, for setting in motion the process of creating a mirror database of CLARA, for the purposes of research into Student Success and Perseverance
 - Partners at Collaborating Colleges
     - Larry Calahan, and Ross Tzankov (especially for additional feedback in generalizing SQL procedures)
     - Kim Rousseau and Greg Bagshaw from John Abbott College

## To Do List:
 - Investigate baseline models that colleges can use for identifying at-risk students (Jon G)
 - Finish Littl Review (Sam B) 


