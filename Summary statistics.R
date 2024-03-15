library(foreign)
library(dplyr)
install.packages("xtable")
library("xtable")
install.packages("readstata13")
library(readstata13)

#read data
dt <- read.dta13("/Users/tung-yen/Desktop/112-2
                 /計量經濟學/C00250_2en/j3w7s_May2017.dta")

#2 Samples
table(dt$gsbirth) 
age <- 113 - dt$gsbirth
table(age)

dt<-mutate(dt, gsedu = 0)
table(dt$gsedu)
for(i in c(1:1875)){
  if (dt$gs3032a1[i] == 5){
    dt$gsedu[i] <- dt$gs3032a3[i]
  }else if (dt$gs3032b1[i] == 5){
    dt$gsedu[i] <- dt$gs3032b3[i]
  }else if (dt$gs3032c1[i] == 5){
    dt$gsedu[i] <- dt$gs3032c3[i]
  }else if (dt$gs3032d1[i] == 5){
    dt$gsedu[i] <- dt$gs3032d3[i]
  }else if (dt$gs3032e1[i] == 5){
    dt$gsedu[i] <- dt$gs3032e3[i]
  }else if (dt$gs3032f1[i] == 5){
    dt$gsedu[i] <- dt$gs3032f3[i]
  }else {dt$gsedu[i] <- NA}
  print(dt$gsedu[i])
}
table(dt$gsedu)
table(dt$gsedu,dt$gsfaedu)

edu <- table(dt$gsedu)
edu_output <- xtable(edu) 
print.xtable(edu_output,type ="latex"
             , file="econ_metric_hw1_edu.tex") 

birth <- table(dt$gsbirth)
birth_output <- xtable(birth) 
print.xtable(birth_output,type ="latex"
             , file="econ_metric_hw1_birth.tex") 

sex <- table(dt$gssex)
sex_output <- xtable(sex) 
print.xtable(sex_output,type ="latex"
             , file="econ_metric_hw1_sex.tex") 

#3 Analysis
income <- table(dt$gsincome)
income_output <- xtable(income) 
print.xtable(income_output,type ="latex"
             , file="econ_metric_hw1_income.tex") 

schooling <- table(dt$gs113010)
schooling_output <- xtable(schooling)
print.xtable(schooling_output,type ="latex"
             , file="econ_metric_hw1_schooling.tex") 


sex_cfriend <- table(dt$gssex,dt$gs347000)
sex_cfriend_output <- xtable(sex_cfriend)
print.xtable(sex_cfriend_output,type ="latex"
             , file="econ_metric_hw1_sex_cfriend.tex")


income_cfriend <- table(dt$gsincome,dt$gs347000)
income_cfriend_output <- xtable(income_cfriend)
print.xtable(income_cfriend_output,type ="latex"
             , file="econ_metric_hw1_income_cfriend.tex")

table(dt$gsincome,dt$gs347000)

