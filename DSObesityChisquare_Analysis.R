
##### Run Chi-square tests 
##### Please note that the test are run muliple times inserting data from summary tables

#### 3 By 3 Table Analysis

mainSatus_table<-matrix(c(66,94,186,222,142,221,170,95,187),ncol=3,byrow=TRUE)

colnames(mainSatus_table)<-c("DV_cat1","DV_cat2","DV_cat3")    ## e.g Weight Status - Obese, Overweight, Normal or Under weight

rownames(mainSatus_table)<-c("IV_cat1","IV_cat2","IV-cat3")    ## e.g IDD sub-group DS, ASD, IDD

mainSatus_table<-as.table(mainSatus_table)
mainSatus_table
chi_cat<-chisq.test(mainSatus_table)  
chi_cat
round(chi_cat$residuals, 3) #### getting standerdized residulas to see cell contribution

  
#### 3 By 2 Table Analysis

mainChange_table<-matrix(c(130,216,246,339,174,278),ncol=2,byrow=TRUE)

colnames(mainChange_table)<-c("DV_cat1","DV_cat2")     ## e.g Weight Status Change - Gone_up, Stay Same or gone down

rownames(mainChange_table)<-c("IV_cat1","IV_cat2","IV-cat3")   ## e.g IDD sub-group DS, ASD, IDD

mainChange_table<-as.table(mainChange_table)
mainChange_table
chi_chg<-chisq.test(mainChgCat)
chi_chg
round(chi_chg$residuals, 3)   



##### 2 By 3 Table Analysis

mainComob_table<-matrix(c(100,43,35,87,52,135),ncol=3,byrow=TRUE)

colnames(mainComob_table)<-c("DV_cat1","DV_cat2","DV_cat3")  ## e.g Weight Status - Obese, Overweight, Normal or Under weight

rownames(mainComob_table)<-c("IV_cat1","IV_cat2")       ## e.g CHD_Yes, CHD_No

mainComob_table<-as.table(mainComob_table)
mainComob_table
chi_cat<-chisq.test(mainComob_table)
chi_cat
round(chi_cat$residuals, 3) 



###### 2 By 2 Table Analysis

Change_table<-matrix(c(42,6,238,60),ncol=2,byrow=TRUE)

colnames(Change_table)<-c("DV_cat1","DV_cat2")       ## e.g Weight Status Change - Gone_up, Stay Same or gone down

rownames(Change_table)<-c("IV_cat1","IV_cat2")       ## e.g CHD_Yes, CHD_No

Change_table<-as.table(Change_table)
Change_table
chi_chg<-chisq.test(Change_table)
chi_chg
round(chi_chg$residuals, 3)







