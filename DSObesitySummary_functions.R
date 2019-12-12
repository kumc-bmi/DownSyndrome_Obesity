

###### Set-up

rm(list=ls())  

setwd("/home/XXXXX/")     #### Add username

##### Loading the required R libraries

##### 

library(plyr)
library(magrittr)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)
library(foreign)
library(ggplot2)
library(data.table)
library(memisc)
library(gdata)
library("ROracle")
library(sqldf)
library(RSQLite)
library(glue)
library(rcompanion)


###### RRead Data : First we will make the connection to the database and my schema to access the row data files

db_config <- read.csv('Account_access_new.csv', stringsAsFactors=FALSE)
c_connect <-
  dbConnect(Oracle(), db_config$account, db_config$password, db_config$access)

#### Read the data from yellowfin for Analysis

query3 <- paste0("select * from ","ds","_bmidemo")     

data_demo<-dbGetQuery(c_connect, query3)

### Since our analysis is for two main age groups ("Kids and Adolescents" and "Adults"), most of the time we will create
### Two separate functions as Kids use BMI Percentiles and Adults use BMI

#### First, write analytic function for Kids

kid_test<-function(dataf) {
  ### This function gives us a summray of 
  ###               DS patients we have
  ###               Patients with 2 BMI measure over 3 months
  ###               Patients with 2 BMI's as a Kid or 2 BMI's as a Adult
  ### Also, restricts the sample for analysis
  
  query1 <- paste0("select * from ",dataf,"_cohort")     
  
  data_cohort<-dbGetQuery(c_connect, query1)
  
  query2 <- paste0("select * from ",dataf,"_bmi")     
                  
  data_bmi<-dbGetQuery(c_connect, query2)
  
  query3 <- paste0("select * from ",dataf,"_bmidemo")     
  
  data_bmidemo<-dbGetQuery(c_connect, query3)
  
cat("Number of Patients for analysis \n")
print(sqldf("
      select count(distinct patient_num) as Patients
      from data_bmidemo
      ")
      )
cat("\n")
#################
bmi_2<-sqldf(" with tab_int as (
              select *
              from data_bmi
              where (weight is not null and height is not null and bmiper is not null)
                  and height_dt=weight_dt                       
                    and weight_dt=bmiper_dt 
              union all
              select *
              from data_bmi
              where (weight is not null and height is not null and bmiper is null)
                  and height_dt=weight_dt                       
                    and weight_dt=bmi_dt
              )
              select *
              from tab_int 
              where patient_num in (select patient_num 
                                     from data_bmidemo) 

            " )

bmi_2new<-bmi_2 %>%
    dplyr::group_by(PATIENT_NUM) %>%
    dplyr::mutate(MINWEIGHT_dt=min(WEIGHT_DT)) %>%
    dplyr::group_by(PATIENT_NUM,MINWEIGHT_dt) %>%
    dplyr::filter(row_number() == 1)


minweightdt <- sqldf("
            
                select bmt.* 
                  ,cht.Dx_date
                ,tbl.minweight_dt
                from  bmi_2 bmt
                join  bmi_2new tbl
                on bmt.patient_num=tbl.patient_num
                join data_cohort cht
                on tbl.patient_num=cht.patient_num
                where tbl.minweight_dt >= cht.Dx_date   
                ")

weight3months_1 <- minweightdt %>%
                  dplyr::filter(difftime(WEIGHT_DT,MINWEIGHT_dt,units="day")==0)

weight3months_2 <- minweightdt %>%
                    dplyr::filter(difftime(WEIGHT_DT,MINWEIGHT_dt,units="day")>91.44)

weight3months<-rbind(weight3months_1,weight3months_2)


weight3mntlong <- sqldf ("
                          with tab_int as (
                            select patient_num
                            ,count(distinct encounter_num) as enc_cnt
                            from weight3months
                            group by patient_num
                          )
                          select wgt.*
                            ,tbl.enc_cnt
                          from weight3months wgt
                          join tab_int tbl 
                          on wgt.patient_num=tbl.patient_num
                          where tbl.enc_cnt>1
                          ")

 
cat("Number of patients with BMIs measures over 3 months:\n" )
print(sqldf("
       select count(distinct patient_num) as Patients
       from weight3mntlong
      ")
)
cat("\n" )
   

bmiafetrDx_t1<- sqldf("

                        select tbm.patient_num
                        ,tdm.birth_date
                        ,tdm.sex
                        ,tdm.race
                        ,tbm.Dx_date
                        ,tbm.encounter_num
                        ,tbm.encounter_dt
                        ,tbm.weight_dt
                        ,tbm.weight
                        ,tbm.height_dt
                        ,tbm.height
                        ,tbm.bmiper_dt
                        ,tbm.bmiper
                        ,tbm.bmi_dt
                        ,tbm.bmi
                        ----from weight3mntlong tbm
                        from weight3mntlong  tbm
                        ----join data_demo tdm
                        join data_bmidemo tdm
                        on tbm.patient_num=tdm.patient_num   
                         "
                        )
  


bmiafetrDx_t2<-bmiafetrDx_t1 %>%
  mutate(age_meas_yr=as.numeric(difftime(WEIGHT_DT,BIRTH_DATE,units="day")/365.25)) %>%
  mutate(age_meas_mo=as.numeric(difftime(WEIGHT_DT,BIRTH_DATE,units="day")/30.44))
  
bmiafetrDx<-sqldf("
                  select tbl.*
                  ,case when tbl.age_meas_mo<= 240 then 'Kid'
                             else 'Adult' end as Age_cat
                  from bmiafetrDx_t2 tbl
                  
                  ")

bmiafetrDx1<- sqldf("    
                  select *
                    from bmiafetrDx
                  where age_Cat='Kid'
                  and BMIper is not null
                  union all
                  select *
                    from bmiafetrDx
                  where age_Cat='Adult'
                  and BMI is not null
                  ")
                    
kidadultenc <- sqldf ("
                      with tab_int as (
                        select patient_num
                        ,age_cat
                        ,count(distinct encounter_num) as N_encounters
                        from bmiafetrDx1
                        group by patient_num, age_cat
                      )
                      select patient_num
                      from tab_int
                      where (age_cat='Kid' and N_encounters=1)
                      and patient_num in ( select patient_num
                                           from tab_int
                                           where age_cat='Adult' and N_encounters=1)
                        ")


bmiafetrDx2 <- sqldf ("
                        select * 
                        from bmiafetrDx1
                        where patient_num not in ( select patient_num
                                                   from kidadultenc)
                      ")

cat("Number of patients with BMIs at least two BMI (as a Kid or Adult) measures over 3 months:\n" )
print(sqldf("
       select count(distinct patient_num) as Patients
       from bmiafetrDx2
      ")
      )
cat("\n" )


return(bmiafetrDx2)

}

######### Writing the second function to divide Kid and Adult encounters accordingly


kidadultdevide<-function(data) {
  
  ### This will show how many kids and how many adults we have for the analysis
  ### Here, it shows how patients are have data points to consider as both kid and adult
  ### Research team has decided to keep assigning the patient into one category base on the maximum number of
  ### encounters either as a kid or adult
  
  commonpat <-sqldf("
                    with tab_int as (
                      select distinct patient_num
                      from data
                      where age_cat='Kid'
                      and patient_num in ( select patient_num
                                           from data
                                           where age_cat='Adult')
                    )
                    select btb.patient_num
                    ,btb.age_cat
                    ,count(distinct btb.encounter_num) as N_encounters
                    from data btb
                    join tab_int itb
                    on btb.patient_num=itb.patient_num
                    group by btb.patient_num,btb.age_cat
                    order by btb.patient_num,btb.age_cat desc
                  ")

  commonpat_sel <- commonpat %>%
    dplyr::arrange(PATIENT_NUM, N_encounters) %>% 
    group_by(PATIENT_NUM) %>% 
    mutate(rank=row_number()) %>%
    dplyr::filter(rank==2)
  
  cat("Number of patients as both adult and kid:\n" )
  print(sqldf("
               select count(distinct patient_num) as Patients
               from commonpat_sel
              ")
        )
  cat("\n" ) 
  
  commonpat_enc <- sqldf (" 
                  select distinct patient_num
                  ,age_cat as age_cat2
                  from data 
                  where patient_num not in ( select patient_num from commonpat)
                  union all
                  select patient_num
                  ,age_cat as age_cat2
                  from commonpat_sel
        ")
                          
  
  

  
bmiafetrDx_2 <- sqldf("
                          select tb1.*
                            ,tb2.age_cat2
                          from data  tb1
                          join commonpat_enc tb2
                          on tb1.patient_num=tb2.patient_num
                          where age_cat=age_cat2;
                          ")
#########
cat("Number of patients for analysis:\n" )
print(sqldf("
               select count(distinct patient_num) as Patients
               from bmiafetrDx_2
              ")
)
cat("\n" )
#########
cat("Number of Kids for analysis:\n" )
print(sqldf("
        select count (distinct patient_num)
        from bmiafetrDx_2
        where age_cat='Kid'
        ")
)
cat("\n" )
########
cat("Number of Adults for analysis:\n" )
print(sqldf("
        select count (distinct patient_num)
        from bmiafetrDx_2
        where age_cat='Adult'
        ")
)
return(bmiafetrDx_2)
}

###############################################################################################################################
################################ Now create function to cut the data for Kids    #######################################
###############################################################################################################################

kidanalyze<-function(data) {
  
  #### Here we will cut the data only for kid sub-population and structure the data for analysis
  
kidfr<-data %>%
           dplyr::filter(Age_cat=="Kid")%>%
           group_by(PATIENT_NUM) %>%
           mutate(first_weight_dt=min(WEIGHT_DT)) %>%
           dplyr::filter(row_number()==1) %>%
           dplyr::select(PATIENT_NUM,first_weight_dt) %>%
           inner_join(data,by="PATIENT_NUM") %>%
           dplyr::filter(Age_cat=="Kid" & WEIGHT_DT==first_weight_dt) %>%
           group_by(PATIENT_NUM)  %>%
           mutate(first_weight_dt=min(WEIGHT_DT)
                  ,first_meas_age=mean(age_meas_mo)
                  ,first_weight=mean(WEIGHT)
                  ,first_height=mean(HEIGHT)
                  ,first_bmiper=mean(BMIPER)) %>%
           dplyr::filter(row_number()==1) 
  
kidls<-data %>%
  dplyr::filter(Age_cat=="Kid")%>%
  group_by(PATIENT_NUM) %>%
  mutate(last_weight_dt=max(WEIGHT_DT)) %>%
  dplyr::filter(row_number()==1) %>%
  dplyr::select(PATIENT_NUM,last_weight_dt) %>%
  inner_join(data,by="PATIENT_NUM") %>%
  dplyr::filter(Age_cat=="Kid" & WEIGHT_DT==last_weight_dt) %>%
  group_by(PATIENT_NUM)  %>%
  mutate(last_weight_dt=max(WEIGHT_DT)
         ,last_meas_age=mean(age_meas_mo)
         ,last_weight=mean(WEIGHT)
         ,last_height=mean(HEIGHT)
         ,last_bmiper=mean(BMIPER)) %>%
  dplyr::filter(row_number()==1) 

bmikid<-sqldf("
              select tbf.patient_num
              ,tbf.first_weight_dt
              ,tbl.last_weight_dt
              ,tbf.first_meas_age
              ,tbl.last_meas_age
              ,tbf.first_weight
              ,tbl.last_weight
              ,tbf.first_height
              ,tbl.last_height
              ,tbf.first_bmiper
              ,tbl.last_bmiper
              from kidfr tbf
              Join kidls tbl
              on tbf.patient_num=tbl.patient_num
              ")

bmikid_t<- bmikid %>%
              mutate(bmi_time_gap=as.numeric(difftime(last_weight_dt,first_weight_dt,units="day")/30.44))
  
bmikidana <-sqldf("
        select kid.*
        ,(first_bmiper+last_bmiper)/2 as avgbmiper_firstlast
        ,case
        when ((first_bmiper+last_bmiper)/2)<5 then 'Underweight'
        when ((first_bmiper+last_bmiper)/2)>=5 and ((first_bmiper+last_bmiper)/2)<85 then 'Normal Weight'
        when ((first_bmiper+last_bmiper)/2)>=85 and ((first_bmiper+last_bmiper)/2)<95 then 'Overweight'
        when ((first_bmiper+last_bmiper)/2)>=95 then 'Obese'
        else 'Error' end as BMI_cat
        ,case 
        when ((last_bmiper-first_bmiper)/first_bmiper)*100 > 5  then 'Gone-up'
        when ((last_bmiper-first_bmiper)/first_bmiper)*100 < -5  then 'Gone-down'
        else 'No change' end as BMIPER_change
        ,case 
        when ((last_weight-first_weight)/first_weight)*100 > 5 then 'Gained'
        when ((last_weight-first_weight)/first_weight)*100 < -5 then 'Lost'
        else 'No change' end as weight_change
        ,case 
        when ((last_height-first_height)/first_height)*100 > 5 then 'Gained'
        when ((last_height-first_height)/first_height)*100 < -5 then 'Lost'
        else 'No change' end as height_change   
        from bmikid_t kid         
        ")
return(bmikidana)
}

########## Print (Summary) function for kids

kidmainPrint<- function(data) {
  
  #### This will print the BMI categories, BMI % Change categories, Average BMI% for the group
  #### and BMI measure time gaps

cat("BMI distribution of kids \n" )
print(sqldf("
            with tab_cnt as (
            select BMI_cat
            ,count(distinct patient_num) as Patients
            from data 
            group by BMI_cat
            order by BMI_cat
            )
            select tct.*
            ,round((Patients*1.0/(select count(distinct patient_num) from data))*100,1) as Percentage
            from tab_cnt tct
            ")
      )
cat("\n" )
##########
cat("BMI averages  of kids \n" )
print(sqldf("
        select min(avgbmiper_firstlast)
        ,avg(avgbmiper_firstlast)
        ,median(avgbmiper_firstlast)
        ,max(avgbmiper_firstlast)
        from data
        ")
      )
cat("\n" )
###########
cat("BMI Change distribution of kids \n" )
print(sqldf("
            
            with tab_cnt as (
            select BMIPER_change
            ,count(distinct patient_num) as Patients
            from data
            group by BMIPER_change
            order by BMIPER_change
            )
            select tct.*
            ,round((Patients*1.0/(select count(distinct patient_num) from data))*100,1) as Percentage
            from tab_cnt tct
            
            ")
)
cat("\n" )
##########
cat("BMI Times of kids \n" )
print(sqldf("
        select min(bmi_time_gap)
        ,avg(bmi_time_gap)
        ,median(bmi_time_gap)
        ,max(bmi_time_gap)
        from data
  ")
)
cat("\n")

}

###########################################################################################################################################
#### Demographic-Wise Analysis
### Kids

kiddemoPrint<-function(data,demodata) {
  #### This is the above summary function enhanced to do the Sex and Race wise analysis
  #### Same output measures will be reported
  
demokidana<-sqldf("
              select dem.race
              ,dem.sex
              ,ana.*
                ,case 
              when race='white' then 'white'
              when race='black' then 'black'
              else 'other' end as race_rec
              from demodata dem
              join data ana
              on dem.patient_num=ana.patient_num
              ")

################
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ \n")
cat("BMI Patients Race Distribution \n" )
print(sqldf("
           with tab_cnt as (
           select race_rec
          ,count (distinct patient_num) as Patients
          from demokidana
          group by race_rec
          order by race_rec
            )
            select tct.*
            ,round((Patients*1.0/(select count(distinct patient_num) from data))*100,1) as Percentage
            from tab_cnt tct


          ")
       )
cat("\n")
###
cat("BMI for Race categories \n" )
print(sqldf("
        select race_rec
      ,min(avgbmiper_firstlast)
      ,avg(avgbmiper_firstlast)
      ,median(avgbmiper_firstlast)
      ,max(avgbmiper_firstlast)
      from demokidana
      group by race_rec
      ")
      )
cat("\n")
#### Statistics for white   *************************************************
demokidana_wh<-sqldf("
                    select *
                    from demokidana
                    where race_rec='white'
                    ")
cat("************************************************** \n")
cat("BMI statistics for Race - white \n" )
kidmainPrint(demokidana_wh)
cat("\n")
####   Statistics for black **************************************************
demokidana_bl<-sqldf("
                    select *
                    from demokidana
                    where race_rec='black'
                    ")
cat("************************************************** \n")
cat("BMI statistics for Race - black \n" )
kidmainPrint(demokidana_bl)
####   Statistics for Other Race **************************************************
demokidana_ot<-sqldf("
                    select *
                    from demokidana
                    where race_rec='other'
                    ")
cat("************************************************** \n")
cat("BMI statistics for Race - other \n" )
kidmainPrint(demokidana_ot)
###############
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ \n")
cat("BMI Patients Gender Distribution \n" )
print(sqldf("
             with tab_cnt as (
             select sex
            ,count (distinct patient_num) as Patients
            from demokidana
            group by sex
            order by sex
              )
              select tct.*
              ,round((Patients*1.0/(select count(distinct patient_num) from data))*100,1) as Percentage
              from tab_cnt tct
          ")
    )

###
cat("BMI for Gender categories \n" )
print(sqldf("
        select sex
      ,min(avgbmiper_firstlast)
      ,avg(avgbmiper_firstlast)
      ,median(avgbmiper_firstlast)
      ,max(avgbmiper_firstlast)
      from demokidana
      group by sex
      ")
)
cat("\n")
#### Statistics for male   *************************************************
demokidana_m<-sqldf("
                     select *
                     from demokidana
                     where sex='m'
                     ")
cat("************************************************** \n")
cat("BMI statistics for Gender - male \n" )
kidmainPrint(demokidana_m)
cat("\n")
####   Statistics for female **************************************************
demokidana_f<-sqldf("
                     select *
                     from demokidana
                     where sex='f'
                     ")
cat("************************************************** \n")
cat("BMI statistics for Gender - female \n" )
kidmainPrint(demokidana_f)
cat("\n")
}


###############################################################################################################################
################################ Now create another function to cut the data for Adults  ######################################
###############################################################################################################################

#### Following functions "adultanalyze","adultmainPrint", and "adultdemoPrint' will work exactly same as the  equivalent
#### Kids functions using BMI and weight data

adultanalyze<-function(data) {
  
  adultfr<-data %>%
    dplyr::filter(Age_cat=="Adult")%>%
    group_by(PATIENT_NUM) %>%
    mutate(first_weight_dt=min(WEIGHT_DT)) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::select(PATIENT_NUM,first_weight_dt) %>%
    inner_join(data,by="PATIENT_NUM") %>%
    dplyr::filter(Age_cat=="Adult" & WEIGHT_DT==first_weight_dt) %>%
    group_by(PATIENT_NUM)  %>%
    mutate(first_weight_dt=min(WEIGHT_DT)
           ,first_meas_age=mean(age_meas_mo)
           ,first_weight=mean(WEIGHT)
           ,first_height=mean(HEIGHT)
           ,first_bmi=mean(BMI)) %>%
    dplyr::filter(row_number()==1) 
  
  adultls<-data %>%
    dplyr::filter(Age_cat=="Adult")%>%
    group_by(PATIENT_NUM) %>%
    mutate(last_weight_dt=max(WEIGHT_DT)) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::select(PATIENT_NUM,last_weight_dt) %>%
    inner_join(data,by="PATIENT_NUM") %>%
    dplyr::filter(Age_cat=="Adult" & WEIGHT_DT==last_weight_dt) %>%
    group_by(PATIENT_NUM)  %>%
    mutate(last_weight_dt=max(WEIGHT_DT)
           ,last_meas_age=mean(age_meas_mo)
           ,last_weight=mean(WEIGHT)
           ,last_height=mean(HEIGHT)
           ,last_bmi=mean(BMI)) %>%
    dplyr::filter(row_number()==1) 
  
  bmiadult<-sqldf("
                select tbf.patient_num
                ,tbf.first_weight_dt
                ,tbl.last_weight_dt
                ,tbf.first_meas_age
                ,tbl.last_meas_age
                ,tbf.first_weight
                ,tbl.last_weight
                ,tbf.first_height
                ,tbl.last_height
                ,tbf.first_bmi
                ,tbl.last_bmi
                from adultfr tbf
                Join adultls tbl
                on tbf.patient_num=tbl.patient_num
                ")
  
  bmiadult_t<- bmiadult %>%
    mutate(bmi_time_gap=as.numeric(difftime(last_weight_dt,first_weight_dt,units="day")/30.44))
  
  bmiadultana <-sqldf("
                    select adl.*
                    ,(first_bmi+last_bmi)/2 as avgbmi_firstlast
                    ,case
                         when ((first_bmi+last_bmi)/2)<18.5 then 'Underweight'
                         when ((first_bmi+last_bmi)/2)>=18.5 and((first_bmi+last_bmi)/2)<25 then 'Normal Weight'
                         when ((first_bmi+last_bmi)/2)>=25 and ((first_bmi+last_bmi)/2)<30 then 'Overweight'
                         when ((first_bmi+last_bmi)/2)>=30 then 'Obese'
                         else 'Error' end as BMI_cat
                    ,case 
                         when ((last_bmi-first_bmi)/first_bmi)*100 > 5  then 'Gone-up'
                         when ((last_bmi-first_bmi)/first_bmi)*100 < -5  then 'Gone-down'
                         else 'No change' end as BMI_change
                    ,case 
                         when ((last_weight-first_weight)/first_weight)*100 > 5 then 'Gained'
                         when ((last_weight-first_weight)/first_weight)*100 < -5 then 'Lost'
                         else 'No change' end as weight_change
                    ,case 
                         when ((last_height-first_height)/first_height)*100 > 5 then 'Gained'
                         when ((last_height-first_height)/first_height)*100 < -5 then 'Lost'
                         else 'No change' end as height_change   
                    from bmiadult_t adl         
                    ")
  return(bmiadultana)
}

########## print function for kids

adultmainPrint<- function(data) {
  
  cat("BMI distribution of adults \n" )
  print(sqldf("
              with tab_cnt as (
              select BMI_cat
              ,count(distinct patient_num) as Patients
              from data 
              group by BMI_cat
              order by BMI_cat
              )
              select tct.*
              ,round((Patients*1.0/(select count(distinct patient_num) from data))*100,1) as Percentage
              from tab_cnt tct
              ")
  )
  cat("\n" )
  ##########
  cat("BMI averages  of adults \n" )
  print(sqldf("
              select min(avgbmi_firstlast)
              ,avg(avgbmi_firstlast)
              ,median(avgbmi_firstlast)
              ,max(avgbmi_firstlast)
              from data
              where avgbmi_firstlast<=60
              ")
  )
  cat("\n" )
  ###########
  cat("BMI Change distribution of adults \n" )
  print(sqldf("
              
              with tab_cnt as (
              select weight_change
              ,count(distinct patient_num) as Patients
              from data
              group by weight_change
              order by weight_change
              )
              select tct.*
              ,round((Patients*1.0/(select count(distinct patient_num) from data))*100,1) as Percentage
              from tab_cnt tct
              
              ")
  )
  cat("\n" )
  ##########
  cat("BMI Times of adults \n" )
  print(sqldf("
              select min(bmi_time_gap)
              ,avg(bmi_time_gap)
              ,median(bmi_time_gap)
              ,max(bmi_time_gap)
              from data
              ")
  )
  cat("\n")
  
}

#################################################################################################################################
#### Demographics Wise Analysis
#### Adults

adultdemoPrint<-function(data,demodata) {
  
  demoadultana<-sqldf("
                    select dem.race
                    ,dem.sex
                    ,ana.*
                    ,case 
                    when race='white' then 'white'
                    when race='black' then 'black'
                    else 'other' end as race_rec
                    from demodata dem
                    join data ana
                    on dem.patient_num=ana.patient_num
                    ")
  
  ################
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ \n")
  cat("BMI Patients Race Distribution \n" )
  print(sqldf("
              with tab_cnt as (
              select race_rec
              ,count (distinct patient_num) as Patients
              from demoadultana
              group by race_rec
              order by race_rec
              )
              select tct.*
              ,round((Patients*1.0/(select count(distinct patient_num) from data))*100,1) as Percentage
              from tab_cnt tct
              
              
              ")
  )
  cat("\n")
  ###
  cat("BMI for Race categories \n" )
  print(sqldf("
              select race_rec
              ,min(avgbmi_firstlast)
              ,avg(avgbmi_firstlast)
              ,median(avgbmi_firstlast)
              ,max(avgbmi_firstlast)
              from demoadultana
              where avgbmi_firstlast<=60
              group by race_rec
              ")
  )
  cat("\n")
  #### Statistics for white   *************************************************
  demoadultana_wh<-sqldf("
                       select *
                       from demoadultana
                       where race_rec='white'
                       ")
  cat("************************************************** \n")
  cat("BMI statistics for Race - white \n" )
  adultmainPrint(demoadultana_wh)
  cat("\n")
  ####   Statistics for black **************************************************
  demoadultana_bl<-sqldf("
                       select *
                       from demoadultana
                       where race_rec='black'
                       ")
  cat("************************************************** \n")
  cat("BMI statistics for Race - black \n" )
  adultmainPrint(demoadultana_bl)
  ####   Statistics for Other Race **************************************************
  demoadultana_ot<-sqldf("
                       select *
                       from demoadultana
                       where race_rec='other'
                       ")
  cat("************************************************** \n")
  cat("BMI statistics for Race - other \n" )
  adultmainPrint(demoadultana_ot)
  ###############
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ \n")
  cat("BMI Patients Gender Distribution \n" )
  print(sqldf("
              with tab_cnt as (
              select sex
              ,count (distinct patient_num) as Patients
              from demoadultana
              group by sex
              order by sex
              )
              select tct.*
              ,round((Patients*1.0/(select count(distinct patient_num) from data))*100,1) as Percentage
              from tab_cnt tct
              ")
  )
  ####
  ###
  cat("BMI for Gender categories \n" )
  print(sqldf("
              select sex
              ,min(avgbmi_firstlast)
              ,avg(avgbmi_firstlast)
              ,median(avgbmi_firstlast)
              ,max(avgbmi_firstlast)
              from demoadultana
              where avgbmi_firstlast <=60
              group by sex
              ")
)
cat("\n")
#### Statistics for male   *************************************************
demoadultana_m<-sqldf("
                    select *
                    from demoadultana
                    where sex='m'
                    ")
cat("************************************************** \n")
cat("BMI statistics for Gender - male \n" )
adultmainPrint(demoadultana_m)
cat("\n")
####   Statistics for female **************************************************
demoadultana_f<-sqldf("
                    select *
                    from demoadultana
                    where sex='f'
                    ")
cat("************************************************** \n")
cat("BMI statistics for Gender - female \n" )
adultmainPrint(demoadultana_f)
cat("\n")
}


##########################################################################################################################################################
########################################################### Functions for comorbidity analysis  ###########################################################
##########################################################################################################################################################


combCutPrint<-function(data,comb,data_m) {
  
  #### This function will identify patients with a given comorbidity
     #### Then it will return the number and cut the related data for BMI analysis
  
  query4 <- paste0("select * from ",data,"_bmicomobid")     
  ### Link the data in comorbidity data tables created in oracle schema - Refer to SQL syntaxes
  data_bmicomobid<-dbGetQuery(c_connect, query4)
  
  ##### Combine Kid cut with BMI
  
  cat("\n")
  cat(paste0("Number of patients for comobidity analysis   \n"))        
  print(sqldf("select count(distinct patient_num) as Patients
              from data_m                      
              
              ")
  )
  
  cat("\n")
  
  
  data_combosel<-sqldf(paste0("select distinct patient_num
                              ,'yes' as comb_dt
                              from data_bmicomobid
                              where ",comb,"Dx_dt is not null")     
                       
  )
  
  data_combBMI<-sqldf("
                      select tbl.*
                      ,comb_dt
                      from data_m  tbl                  
                      left join  data_combosel tcm
                      on tbl.patient_num=tcm.patient_num
                      ")
  
  data_combBMIAna<-sqldf("
                         select *
                         from data_combBMI
                         where comb_dt is not null
                         
                         ")
  
  
  cat("\n")
  cat(paste0("Number of Patients with ",comb,"   \n"))       
  print(sqldf("select count(distinct patient_num) as Patients
              from data_combBMIAna
              
              ")
  )
  
  cat("\n")
  
  return(data_combBMIAna)
}


######################################################################################################################################################
#################################### Function for Overall sample comorbidity prevalance ##############################################################
######################################################################################################################################################

#### This is irrespective of the patient having 2 BMI measure over 3 months
##### We have the overall DS population (as Kid or Adult) for this analysis

comobPrevCut<-function(data,comb) {
 
  ### This function will provider selected comorbidity prevalence separately for Kids and Adults

  query2 <- paste0("select * from ",data,"_bmi")     
  
  data_bmi<-dbGetQuery(c_connect, query2)
  
  query3 <- paste0("select * from ",data,"_bmidemo")     
  
  data_bmidemo<-dbGetQuery(c_connect, query3)
  
  query4 <- paste0("select * from ",data,"_bmicomobid")     
  
  data_bmicomobid<-dbGetQuery(c_connect, query4)
  
  ###################### 
  
  bmidata_t1<- sqldf("
                     select distinct tbm.patient_num
                     ,tdm.birth_date
                     
                     
                     ,tdm.Dx_date
                     ,tbm.encounter_num
                     ,tbm.encounter_dt
                     
                     
                     from data_bmi  tbm
                     
                     join data_bmidemo tdm
                     on tbm.patient_num=tdm.patient_num     
                     "
  )
  
  
  
  bmidata_t2<-bmidata_t1 %>%
    mutate(age_enc_yr=as.numeric(difftime(ENCOUNTER_DT,BIRTH_DATE,units="day")/365.25)) %>%
    mutate(age_enc_mo=as.numeric(difftime(ENCOUNTER_DT,BIRTH_DATE,units="day")/30.44))
  
  bmidata_t3<-sqldf("
                    select tbl.*
                    ,case when tbl.age_enc_mo<= 240 then 'Kid'
                    else 'Adult' end as Age_cat
                    from bmidata_t2 tbl
                    
                    ")
  
  
  cat("Number of Patients for analysis \n")
  print(sqldf("
              select count(distinct patient_num) as Patients
              from bmidata_t3
              ")
  )
  cat("\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ \n")
  divide_data<-kidadultdevide(bmidata_t3)
  
  
  divide_data_t<- sqldf("
                        
                        select patient_num 
                        ,age_cat
                        from divide_data
                        ")
  
  
  
  data_combosel<-sqldf(paste0("select distinct patient_num
                              ,'yes' as comb_dt
                              from data_bmicomobid
                              where ",comb,"Dx_dt is not null")     
                       
  )
  
  data_combBMI<-sqldf("
                      select tbl.*
                      ,comb_dt
                      from  divide_data_t  tbl                  
                      left join  data_combosel tcm
                      on tbl.patient_num=tcm.patient_num
                      ")
  
  cat("Comorbidity prevelance in the main study cohort \n")  
  print(sqldf("
              with tab_all as (
              select age_cat
              ,count (distinct patient_num) as all_cnt
              from  data_combBMI
              group by age_cat
              
              )
              ,tab_comb as (
              select age_cat
              ,count (distinct patient_num) as comb_cnt
              from  data_combBMI
              where comb_dt is not null
              group by age_cat
              )
              select tb1.age_cat
              ,tb1.all_cnt
              ,tb2.comb_cnt
              ,round(((tb2.comb_cnt*1.0/tb1.all_cnt)*100),2) as prevalance
              from tab_all tb1
              join tab_comb tb2
              on tb1.age_cat=tb2.age_cat
              ")
      )
  
}


######################################################################################################################
######################################################################################################################

### We are interested in comparing the BMIs and Weight for patients with comorbidities vs Patients without comorbidities
### Thus, we need to identify these patients. Following function is used for this task.
### These data are needed to run the ch-square tests.

combnoCutPrint<-function(data,comb,data_m) {
  
  query4 <- paste0("select * from ",data,"_bmicomobid")     
  
  data_bmicomobid<-dbGetQuery(c_connect, query4)
  
  ##### Combine Kid cut with bmi
  
  cat("\n")
  cat(paste0("Number of patients for comobidity analysis   \n"))        
  print(sqldf("select count(distinct patient_num) as Patients
              from data_m                      
              
              ")
  )
  
  cat("\n")
  
  
  data_nocombosel<-sqldf(paste0("select distinct patient_num
                              ,'no' as comb_dt
                              from data_bmicomobid
                              where ",comb,"Dx_dt is null")     
                       
  )
  
  data_nocombBMI<-sqldf("
                      select tbl.*
                      ,comb_dt
                      from data_m  tbl                  
                      left join  data_nocombosel tcm
                      on tbl.patient_num=tcm.patient_num
                      ")
  
  data_nocombBMIAna<-sqldf("
                         select *
                         from data_nocombBMI
                         where comb_dt is not null
                         
                         ")
  
  
  
  
  cat("\n")
  cat(paste0("Number of Patients without ",comb,"   \n"))       
  print(sqldf("select count(distinct patient_num) as Patients
              from data_nocombBMIAna
              
              ")
  )
  
  cat("\n")
  
  return(data_nocombBMIAna)
}



##################################################################################################################
#################### Function to cut data for Chi-square Analysis ################################################

kidChi_ds<-function(data) {
  
  demo<-sqldf(paste0(" select distinct patient_num
                     ,sex
                     ,case 
                     when race='white' then 'white'
                     when race='black' then 'black'
                     else 'other' end as race_rec
                     from data_demo
                     where patient_num in (select distinct patient_num from ",data,") 
                     ")
  )
  
  
  chidata<-sqldf(paste0("
                        select ktb.*
                        ,dtb.sex
                        ,dtb.race_rec
                        ,case 
                        when ktb.BMI_cat='Obese' then 'Obese'
                        when ktb.BMI_cat='Overweight' then 'Overweight'
                        else 'Normal or Under Weight' end as weight_status
                        from ",data," ktb
                        join demo dtb
                        on ktb.patient_num=dtb.patient_num
                        ")
  )
  
  
  
  cat("Weight Status vs Sex \n")
  sexst_tab <- table(chidata$SEX, chidata$weight_status) 
  print(sexst_tab) 
  print(chisq.test(sexst_tab))
  cat("\n\n")
  
  cat("Weight Change vs Sex \n")   
  sexch_tab <- table(chidata$SEX, chidata$BMIPER_change) 
  print(sexch_tab)
  print(chisq.test(sexch_tab))
  cat("\n\n")
  
  cat("Weight Status vs Race \n")
  racest_tab <- table(chidata$race_rec, chidata$weight_status)
  print(racest_tab)
  print(chisq.test(racest_tab))
  cat("\n\n")
  
  cat("Weight Change vs Race \n")
  racech_tab <- table(chidata$race_rec, chidata$BMIPER_change)
  print(racech_tab)
  print(chisq.test(racech_tab))
  cat("\n\n")
  
}


adultChi_ds<-function(data) {
  
  demo<-sqldf(paste0(" select distinct patient_num
                     ,sex
                     ,case 
                     when race='white' then 'white'
                     when race='black' then 'black'
                     else 'other' end as race_rec
                     from data_demo
                     where patient_num in (select distinct patient_num from ",data,") 
                     ")
  )
  
  
  chidata<-sqldf(paste0("
                        select ktb.*
                        ,dtb.sex
                        ,dtb.race_rec
                        ,case 
                        when ktb.BMI_cat='Obese' then 'Obese'
                        when ktb.BMI_cat='Overweight' then 'Overweight'
                        else 'Normal or Under Weight' end as weight_status
                        from ",data," ktb
                        join demo dtb
                        on ktb.patient_num=dtb.patient_num
                        ")
  )
  
  cat("Weight Status vs Sex \n")
  sexst_tab <- table(chidata$SEX, chidata$weight_status) 
  print(sexst_tab) 
  print(chisq.test(sexst_tab))
  cat("\n\n")
  
  cat("Weight Change vs Sex \n")   
  sexch_tab <- table(chidata$SEX, chidata$weight_change) 
  print(sexch_tab)
  print(chisq.test(sexch_tab))
  ###print(cramersV(sexch_tab))
  cat("\n\n")
  
  cat("Weight Status vs Race \n")
  racest_tab <- table(chidata$race_rec, chidata$weight_status)
  print(racest_tab)
  print(chisq.test(racest_tab))
  cat("\n\n")
  
  cat("Weight Change vs Race \n")
  racech_tab <- table(chidata$race_rec, chidata$weight_change)
  print(racech_tab)
  print(chisq.test(racech_tab))
  cat("\n\n")
  
}


############################################################################################################################################################
################  Create a function for count multiple comorbidities ########################################################################################


mutiComobid_Cnt <- function(data,data_m){

query4 <- paste0("select * from ",data,"_bmicomobid")     

data_bmicomobid<-dbGetQuery(c_connect, query4)


comobid_data<-sqldf("   select distinct patient_num
                            ,case when
                                 max(age_chd_mo) is not null then 1
                                    else 0 end as chd_ind
                            ,case when
                                 max(age_sa_mo) is not null then 1
                                    else 0 end as sa_ind
                            ,case when
                                 max(age_ph_mo) is not null then 1
                                    else 0 end as ph_ind
                            ,case when
                                 max(age_t2d_mo) is not null then 1
                                   else 0 end as t2d_ind
                            ,case when
                                 max(age_de_mo) is not null then 1
                                    else 0 end as de_ind
                            ,case when
                                 max(age_hyt_mo) is not null then 1
                                    else 0 end as hyt_ind
                            ,case when
                                 max(age_syh_mo) is not null then 1
                                    else 0 end as syh_ind
                        from data_bmicomobid
                        group by patient_num
                    "
                    )

comobd_cut<-sqldf(paste0("select distinct pat.*
                          ,(chd_ind+sa_ind+ph_ind+t2d_ind+de_ind+hyt_ind+syh_ind) as comb_cnt
                          from  comobid_data pat
                         where pat.patient_num in (select patient_num
                         from ",data_m," )
                         ")
                  )

cat("Number of patients with mutiple comorbidities \n")

print(sqldf(" select comb_cnt
         ,count(distinct patient_num)
         from comobd_cut
         group by comb_cnt
      
       ")
     )

}








