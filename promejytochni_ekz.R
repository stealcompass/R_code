library ("zoo")
library("dplyr")
library ("ggplot2")
library("lmtest")
library("sandwich")



help(read.csv)
library ("devtools")
devtools::install_github("bdemeshev/rlms", force=TRUE)
library("rlms")
f<-read.rlms("/home/dataminer/Загрузки/r22i_os25e.sav")
glimpse(f)


f2<-select(f,rj13.2,rh6,rh5,r_diplom,status,rj1.1.1)
head(f2)
f2<-mutate(f2,age=2013-rh6)
glimpse(f2)
table(f2$status)

f3<-filter(f2,status=="областной центр" | status== "город")
summary(f3$status)
help(filter)
glimpse(f3)
table(f3$rj1.1.1)
f4<-filter(f3,rj1.1.1=="ПОЛНОСТЬЮ УДОВЛЕТВОРЕНЫ"|rj1.1.1=="СКОРЕЕ УДОВЛЕТВОРЕНЫ")
summary(f4$r_diplom)
f5<-filter(f4,r_diplom!="ЗАТРУДНЯЮСЬ ОТВЕТИТЬ" & r_diplom!="ОТКАЗ ОТ ОТВЕТА" &r_diplom!="НЕТ ОТВЕТА")
summary(f5$r_diplom)

f5<-mutate(f5,bin_sex=ifelse(rh5=="МУЖСКОЙ",1,0))
f5<-mutate(f5,bin_sex=as.factor(bin_sex))
f5<-mutate(f5,bin_np=ifelse(status=="город",1,0))
f5<-mutate(f5,bin_np=as.factor(bin_np))
f5<-mutate(f5,bin_wrk=ifelse(rj1.1.1=="ПОЛНОСТЬЮ УДОВЛЕТВОРЕНЫ",1,0))
f5<-mutate(f5,bin_wrk=as.factor(bin_wrk))

f5<-mutate(f5,new_edu=ifelse(r_diplom=="окончил 0 - 6 классов"| 
                               r_diplom=="незаконченное среднее образование (7 - 8 кл)"|
                               r_diplom=="незаконченное среднее образование (7 - 8 кл) + что-то еще",1,
                             ifelse(r_diplom=="законченное среднее образование",2,
                             ifelse(r_diplom=="законченное среднее специальное образование",3,
                             ifelse(r_diplom=="законченное высшее образование и выше",4,0)))))
                                

f5<-mutate(f5,new_edu=as.factor(new_edu))

max(f5$age)
sum(is.na(f5$rj13.2))

table(f5$new_edu)
fin<-na.omit(f5)


table(fin$zp)

help(rename)

fin<-rename(fin, zp=rj13.2,b_year=rh6,sex=rh5,edu=r_diplom,np=status,wrk=rj1.1.1)
qplot(zp/1000,data=fin,bins=50, fill="red")
help(hist)

summary(fin$zp)
qplot(age,data=fin)+facet_grid(~sex)

fin_<-mutate(fin,zp_=zp/1000)
qplot(zp_,data=fin_)+facet_grid(~sex)
qplot(age,data=fin,bins=10)+facet_grid(~sex)


fin_<-filter(fin,new_edu!=1)
glimpse(fin_)


m_vova<-lm(data=fin_,zp~age+bin_sex+new_edu+bin_np+bin_wrk)
summary(m_vova)
ftest(m_vova)
vcovHC(m_vova)


coeftest(m_vova,.vcov=vcovHAC(m_vova))




help(vcovHC)
glimpse(fin)

sqrt(404158.849)



