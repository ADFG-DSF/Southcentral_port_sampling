# Southcentral Halibut and Groundfish

Data analysis for the ADF&G port sampling project in the ports of Homer, Kodiak, Seward, Valdez, and Whittier.

The working directory for this repositorey is located at S:\\BIO\\Southcentral halibut and groundfish.

In general there has been minimal biometric support for this project prior to what is documented here. That said, the original project biologist was a good SAS programmer and has written a voluminous amount of SAS code to produce data sets and output associated with this project. The analysis in this repository moves everything associated whit the rockfish report into R, although most of the datasets are still coming from SAS. A complete transition to R would be a good intermediate term goal.

Folder Structure

-   Data: Two excel files with raw interview data from this project, the SAS code used to generate the raw data, the R code used to refine the raw data, and the .rds file that contains the analysis ready data.

-   doc_styles: The files in this folder can be used to control the output styles used when a Rmarkdown document is rendered to word. I can't recall why tI produced 3 templates but left them as they were so as not to break anything. It the analysis is revisited the new biometrican should clean this up.

-   Lingcod Report_03-17: I used lingcod to work out the spatial distribution methods before utilizing the same methods for rockfish. I'm not sure if I made slight modifications after transitioning to rockfish but the anlysis and folder/file structure is similar. When the lingcod report is reinitialized I would compare the rockfish and lingcod files to look for any differences before rerunning the analysis with additional years..

-   Op plan 22-25, Op plan_16-18, Op plan_19-21. .docx and .pdf documents associated with operational planing.

-   Rockfish report_96-19:All of the code and report prep materials associated with the rockfish report.

    -   composition: RFspcomp.R is the main analysis file. The .rds files in this folder are outputs from RFspcomp.R and take a fair bit of time to run due to extensive bootstrapping. The rest of the files in this folder are either input data, the sas codes that produced the input data or misc other sas output which was not used.

    -   interview post: All of the output from either multinomial rockfish.R or multinomial LOOCV.R

    -   lengthage: Code to test which length-age were most supported by the data.

    -   Appendix.rmd, Figures.rmd, Table.rmd: The Rmarkdown documents which produced most fo the tables, figures and appendices associated with this report. The .docx files of the same name are the output from these files.

    -   multinomial rockfish.R: This code makes some inferences about how harvest and effort were distributed across the geographic areas. I used multinomial logistic regression with over-dispersion to fit the data. Overdispersion is substantial and precludes wusing the model output for any sort of prediction. Instead the models were compared using LOOCV (in the code multinomial LOOCV.R) with the model most supported by the data allowing us to draw some conclusion about the patterns describing the spatial distribution of effort and harvest.

    -   RFwglength.R: A model describing weight vrs. length for rockfish species. No model selection as I was recreating a sas output Martin had already run.

    -   Effort_composition.rmd and Rockfish composition.rmd: these Rmarkdown files and theri output were used to update biologists about the spatial composition analysis I was developing. I'm not certain they contain the most up-to-date analysis.

-   Sampling: pdf's describing sampling plans. I read some of these during operational planning but never made modifications to the study design.

-   functions.R contain functions used by one or more codes within this repository.

-   The .txt documents in in this folder are the actual jags model codes used for the spatial composition analyses.

-   models.R is the code to produce the jags models for the analysis. The LOOCV models are produces in the analysis code.
