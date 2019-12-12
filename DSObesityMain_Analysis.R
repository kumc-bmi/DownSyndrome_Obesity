
########################################################################################################
### Date: 12/05/2019                                                                                 ###
### Title: DS/Obesity Paper                                                                          ###
### Analyst: Dammika Lakmal Walpitage                                                                ###
### Research Team: Russ Waitman, Mina Monsheni, Ann Davis,                                           ###
###                Lauren Potmey, Merdith Dreyer, Evan Dean                                          ###
### Team Affilitaions: University of Kansas Medical Center, KS, Childerns Mercy Hospital, KS         ###
########################################################################################################

#################################################################################################################################################################
#### Now we will use the created functions to repeat the analysis for 3 main IDD categories (DS/ASD/IDD)


#### First, we will analyze the data for DS Patients
################################################################################ DS mian analysis ###############################################################
##### Setting up the data for DS analysis
ds_flow<-kid_test("ds")
ds_both<-kidadultdevide(ds_flow)
query <- paste0("select * from ds_bmidemo")
data_demo<-dbGetQuery(c_connect, query)

####################################### DS- Kid analysis
kid_cut<-kidanalyze(ds_both)
#### Print the summary for DS- Kids
kidmainPrint(kid_cut)
#### Above summary- Sex and Race wise
kiddemoPrint(kid_cut,data_demo)

####################################### DS - Adults
adult_cut<-adultanalyze(ds_both)
#### Print the summary for DS- Adults
adultmainPrint(adult_cut)
#### Above summary- Sex and Race wise
adultdemoPrint(adult_cut,data_demo)


###### Team has selected 
     # Congenital Heart Defect (abbreviated in code as “CHD”)
     # Obstructive Sleep Apnea (“SA”)
     # Pulmonary Hypertension (“PLH”)
     # Type II diabetes (“T2D”)
     # Dementia (“DE”)
     # Hyperthyroidism (“HYT”)
     # Systolic Hypertension (“SYT”)
######                              for analysis.  

################################################################# DS - Comorbidity ####################################
            ##### CHD  
kidcomm_data<-combCutPrint("ds","CHD",kid_cut)
kidmainPrint(kidcomm_data)   ### Summary for DS (Kids) patients with comorbidity (CHD)

adultcom_data<-combCutPrint("ds","CHD",adult_cut)
adultmainPrint(adultcom_data) ### Summary for DS (Adults) patients with comorbidity (CHD)

comobPrevCut("ds","CHD")   ### Comorbidity prevalence main cohort

#### Next, we are repeating the same analysis for other six comorbidities
            ##### SA
kidcomm_data<-combCutPrint("ds","SA",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("ds","SA",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("ds","SA")

            ##### PH
kidcomm_data<-combCutPrint("ds","PH",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("ds","PH",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("ds","PH")

            ##### T2D
kidcomm_data<-combCutPrint("ds","T2D",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("ds","T2D",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("ds","T2D")

            #### DE
kidcomm_data<-combCutPrint("ds","DE",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("ds","DE",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("ds","DE")

            #### HYT
kidcomm_data<-combCutPrint("ds","HYT",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("ds","HYT",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("ds","HYT")


           #### SYH
kidcomm_data<-combCutPrint("ds","SYH",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("ds","SYH",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("ds","SYH")

############################################# How many patients with multiple comorbidities

mutiComobid_Cnt("ds","kid_cut")    ## Kids
mutiComobid_Cnt("ds","adult_cut")  ## Adults

############################################# Gender and Race wise parametric comparisons (Main BMI Group - DS) with Chi-sqaure
######### Results are included in the excel file( sheeet: )
kidChi_ds("kid_cut")   ### Kids
adultChi_ds("adult_cut")   #### Adults

############################################ Diferences for patient with comobidity and no-comobidity
kidnocomb_data<-combnoCutPrint("DS","CHD",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("DS","CHD",adult_cut)
adultmainPrint(adultnocomb_data)
##### These summaries will be update to the excel file to create two-way tables
##### Cho-square analysis will be conducted using "Chisquare_Analysis.R" syntax by filling in appropriate numbers

##### Repeate above for the other comorbidities

kidnocomb_data<-combnoCutPrint("DS","SA",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("DS","SA",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("DS","PH",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("DS","PH",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("DS","T2D",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("DS","T2D",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("DS","DE",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("DS","DE",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("DS","HYT",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("DS","HYT",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("DS","SYH",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("DS","SYH",adult_cut)
adultmainPrint(adultnocomb_data)


######################################################################################### ASD main Analysis #####################################################

##### Refer to the comments in DS sections for explanations

autnods_flow<-kid_test("autnods")
autnods_both<-kidadultdevide(autnods_flow)
query <- paste0("select * from autnods_bmidemo")
data_demo<-dbGetQuery(c_connect, query)
###### Kid
kid_cut<-kidanalyze(autnods_both)
kidmainPrint(kid_cut)
kiddemoPrint(kid_cut,data_demo)

#### Adults
adult_cut<-adultanalyze(autnods_both)
adultmainPrint(adult_cut)
adultdemoPrint(adult_cut,data_demo)

###### Comorbidity 
    ##### CHD
kidcomm_data<-combCutPrint("autnods","CHD",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("autnods","CHD",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("autnods","CHD")   ### Comorbidity pevelance main cohort

    ##### SA
kidcomm_data<-combCutPrint("autnods","SA",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("autnods","SA",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("autnods","SA")

##### PH
kidcomm_data<-combCutPrint("autnods","PH",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("autnods","PH",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("autnods","PH")

    ##### T2D
kidcomm_data<-combCutPrint("autnods","T2D",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("autnods","T2D",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("autnods","T2D")

    #### DE
kidcomm_data<-combCutPrint("autnods","DE",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("autnods","DE",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("autnods","DE")

    #### HYT
kidcomm_data<-combCutPrint("autnods","HYT",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("autnods","HYT",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("autnods","HYT")


    #### SYH
kidcomm_data<-combCutPrint("autnods","SYH",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("autnods","SYH",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("autnods","SYH")

############################################# How many patients with multiple comorbidities

mutiComobid_Cnt("autnods","kid_cut")    ## Kids
mutiComobid_Cnt("autnods","adult_cut")  ## Adults

############################################# Gender and Race wise parametric comparisons (Main BMI Group - DS) with Chi-sqaure
######### Results are included in the excel file( sheeet: )
kidChi_ds("kid_cut")   ### Kids
adultChi_ds("adult_cut")   #### Adults

############################################ Diferences for patient with comobidity and no-comobidity
kidnocomb_data<-combnoCutPrint("autnods","CHD",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("autnods","CHD",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("autnods","SA",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("autnods","SA",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("autnods","PH",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("autnods","PH",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("autnods","T2D",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("autnods","T2D",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("autnods","DE",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("autnods","DE",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("autnods","HYT",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("autnods","HYT",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("autnods","SYH",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("autnods","SYH",adult_cut)
adultmainPrint(adultnocomb_data)



######################################################## IDD Main Analysis   ##################################################################
iddnodsaut_flow<-kid_test("iddnodsaut")
iddnodsaut_both<-kidadultdevide(iddnodsaut_flow)
query <- paste0("select * from iddnodsaut_bmidemo")
data_demo<-dbGetQuery(c_connect, query)
###### Kid
kid_cut<-kidanalyze(iddnodsaut_both)
kidmainPrint(kid_cut)
kiddemoPrint(kid_cut,data_demo)

#### Adults
adult_cut<-adultanalyze(iddnodsaut_both)
adultmainPrint(adult_cut)
adultdemoPrint(adult_cut,data_demo)

###### Comobidity 
      ##### CHD
kidcomm_data<-combCutPrint("iddnodsaut","CHD",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("iddnodsaut","CHD",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("iddnodsaut","CHD")


      ##### SA
kidcomm_data<-combCutPrint("iddnodsaut","SA",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("iddnodsaut","SA",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("iddnodsaut","SA")

      ##### PH
kidcomm_data<-combCutPrint("iddnodsaut","PH",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("iddnodsaut","PH",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("iddnodsaut","PH")

      ##### T2D
kidcomm_data<-combCutPrint("iddnodsaut","T2D",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("iddnodsaut","T2D",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("iddnodsaut","T2D")

      #### DE
kidcomm_data<-combCutPrint("iddnodsaut","DE",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("iddnodsaut","DE",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("iddnodsaut","DE")

      #### HYT
kidcomm_data<-combCutPrint("iddnodsaut","HYT",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("iddnodsaut","HYT",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("iddnodsaut","HYT")

      #### SYH
kidcomm_data<-combCutPrint("iddnodsaut","SYH",kid_cut)
kidmainPrint(kidcomm_data)

adultcom_data<-combCutPrint("iddnodsaut","SYH",adult_cut)
adultmainPrint(adultcom_data)

comobPrevCut("iddnodsaut","SYH")

############################################# How many patients with multiple comorbidities

mutiComobid_Cnt("iddnodsaut","kid_cut")    ## Kids
mutiComobid_Cnt("iddnodsaut","adult_cut")  ## Adults

############################################# Gender and Race wise parametric comparisons (Main BMI Group - DS) with Chi-sqaure
######### Results are included in the excel file( sheeet: )
kidChi_ds("kid_cut")   ### Kids
adultChi_ds("adult_cut")   #### Adults

############################################ Diferences for patient with comobidity and no-comobidity
kidnocomb_data<-combnoCutPrint("iddnodsaut","CHD",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("iddnodsaut","CHD",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("iddnodsaut","SA",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("iddnodsaut","SA",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("iddnodsaut","PH",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("iddnodsaut","PH",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("iddnodsaut","T2D",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("iddnodsaut","T2D",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("iddnodsaut","DE",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("iddnodsaut","DE",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("iddnodsaut","HYT",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("iddnodsaut","HYT",adult_cut)
adultmainPrint(adultnocomb_data)

kidnocomb_data<-combnoCutPrint("iddnodsauts","SYH",kid_cut)
kidmainPrint(kidnocomb_data)

adultnocomb_data<-combnoCutPrint("iddnodsaut","SYH",adult_cut)
adultmainPrint(adultnocomb_data)


