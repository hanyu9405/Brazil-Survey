setwd("C:/Users/Hanyu Huang/Dropbox/self/Master of International Public Policy/
      IP631 Economic Policy and Human Security/Assignment 4/All data")
library(readstata13)
children<-read.dta13("children_recode_1996.dta")
#create dataframe of relevent variables (weight/height, education year, toilet facility, 
  #breastfeed time,settlement type, drinking water source)
  relevent<-data.frame(children$hw5,children$hw8,children$hw11,children$v133,children$v116,children$m4,children$v134,
                      children$v113)
#remove all N/A entries
  relevent<-na.omit(relevent)
#remvoe all entries of 9998 (outside of calculation)
  relevent<-subset(relevent,children.hw5!=9998)
  relevent<-subset(relevent,children.hw8!=9998)
  relevent<-subset(relevent,children.hw11!=9998)
#divide all entreies in relevent$children.hw11 by 100 to get the z-score
  relevent$children.hw5<-relevent$children.hw5/100
  relevent$children.hw8<-relevent$children.hw8/100
  relevent$children.hw11<-relevent$children.hw11/100
#remove "other" from v113 (types of drinking water)
  relevent<-subset(relevent,children.v113!="other")
#remove breastfeed entries "94" and "95" since they indicate children are no longer breastfed
  relevent<-subset(relevent,children.m4<90)
#remove overnutrition from dataset 
  #according to the WHO, overnutrition is when weight-for-height is >+ 2SD from median
  relevent<-subset(relevent,children.hw5<=2)
  relevent<-subset(relevent,children.hw8<=2)
  relevent<-subset(relevent,children.hw11<=2)


#height for age standard deviation
  h_a<-relevent$children.hw5
#weight for age standard deviation
  w_a<-relevent$children.hw8
#weight to height no overweight
  w_h<-relevent$children.hw11
#no overweight breastfeeding
  breastfeed<-relevent$children.m4  
#education no overweight
  education<-relevent$children.v133
#toilet type
  toilet_char<-as.character(relevent$children.v116)
  toilet<-as.numeric(relevent$children.v116)
#settlement type
  settlement_char<-as.character(relevent$children.v134)
  settlement<-as.numeric(relevent$children.v134)
#drinking water
  drink_char<-as.character(relevent$children.v113)
  drink_water<-as.numeric(relevent$children.v113)
#regression of no-overweight children
  reg_h_a<-lm(h_a~breastfeed+drink_water+education+settlement+toilet)
  reg_w_a<-lm(w_a~breastfeed+drink_water+education+settlement+toilet)
  reg_w_h<-lm(w_h~breastfeed+drink_water+education+settlement+toilet)
  
#export results
  library(stargazer)
  stargazer(reg_h_a,reg_w_a,reg_w_h,type="text",out="table.txt")