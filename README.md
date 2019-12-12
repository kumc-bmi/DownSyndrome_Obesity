# DownSyndrome_Obesity
This repositoray holds scripts for the analysis on weight management among IDD patients

Down Syndrome and Obesity Project

Research Team: Lakmal Walpitage (Analyst/Programmer), Russ Waitman, Mina Mosheni, 
           		  Ann Davis, Lauren Potmey, Merdith Dreyer, Evan Dean
                
### Introduction

In people with intellectual and developmental disabilities (IDD), the rates of obesity are even higher.  Several studies have reported that the prevalence of obesity among adults with IDD ranges from 26.5% to 58.5% in the United States (6-8). The prevalence of overweight and obesity (BMI ≥ the 85th%ile) in adolescents with IDD ranges from 19% to 60.5%(9-11), with the prevalence in children ranging from 10-45%(12). Compared with the general population (13),obesity is a significantly greater problem in individuals with IDD

### Goals/Objectives 
To further advance our understanding of obesity prevalence in children and adults with Intellectual and Development Disabilities (IDD) this study utilized electronic medical records from individuals with IDD being treated at The University of Kansas Health Systems to answer the following questions: 
1. What is the body weight status and weight management status among children and adults with Downs Syndrome (DS), Autism Spectrum Disorder (ASD), and other IDDs by sex and race?
2. What is the prevalence of common health conditions (comorbidities as defined below) in children and adults with DS, ASD, and other IDDs by BMI status? 
3. How different the association between weight status and weight management status among patients with given main diagnosis (DS/ASD/other IDD) and with a comorbidity vs patient with given main diagnosis but without the comorbidity.

Patient sub-group and comorbidity Definitions:

Sub-groups
-DS – Patients with Downs Syndrome 
-ASD – Patients with ASD but not with DS
-Other IDD – Patients with IDD but without DS or ASD

Comorbidities
-CHD – Congenital Heart Defect
-OSA (or SA) – Obstructive Sleep apnea
-PLH (or PH) – Pulmonary Hyper tension
-T2D – Type II Diabetes
-DEM (or DE) – Demetria
-HYT – Hyperthyroidism
-SYH – Systolic Hypertension
*All ICD 9 and ICD 10 code used for this study is given in the supplementary material section of the working paper*

### Contents
- `dsBuild.sql` – This SQL syntax file is used to create the 4 tables schema); DS_COHORT, DS_BMI, DS_BMIDEMO, DS_BMICOMOBID, which were later accessed by the R scripts to do the analysis for DS sub-cohort. These tables comprised of BMI data, Comorbidities, and Demographics needed for the analysis
- `autBuild.sql` - Create similar tables; AUTNODS_COHORT, AUTNODS _BMI, AUTNODS _BMIDEMO, AUTNODS _BMICOMOBID to conduct analysis for ASD sub-cohort
- `iddBuild.sql` - Create similar tables; IDDNODSAUT_COHORT, IDDNODSAUT _BMI, IDDNODSAUT _BMIDEMO, IDDNODSAUT _BMICOMOBID to conduct analysis for mother IDD sub-cohort
- `DSObesitySummary_Functions.R`– Functions created to automate the same analysis for DS/ASD/IDD sub-groups. First, run these function definitions before running the analytic file (DSObesityMain_Analysis.R)
- `DSObesityMain_Analysis.R` – Contained the analytic steps separated by each patient sub-group            
- `DSObesityChiSquare_Analysis.R` – This is a script (template) for conducting many chi-square tests to investigate the difference between Weight Status/Weight management Status against Sex, Race, Comorbidities. 
