library(dplyr)
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(plotrix)
g=read_xlsx("MFG10yeardata.xlsx")
View(g)

##summary
summary(g)

##head
head(g)

##changing the columns which needs to be factor
g$termreason_desc=as.factor(g$termreason_desc)
g$termtype_desc=as.factor(g$termtype_desc)
g$BUSINESS_UNIT=as.factor(g$BUSINESS_UNIT)

## extracting unique last record value for each emp_id
g1=g %>% group_by(EmployeeID) %>% slice(n())
View(g1)
class(g1$recorddate_key)

## converting the column to date
g1$recorddate_key <- as.Date(g1$recorddate_key ,format='%m/%d/%Y')
g1$birthdate_key <- as.Date(g1$birthdate_key ,format='%m/%d/%Y')
g1$orighiredate_key <- as.Date(g1$orighiredate_key ,format='%m/%d/%Y')
g1$terminationdate_key<- as.Date(g1$terminationdate_key ,format='%m/%d/%Y')

## feature engineering to introduce new column for age
g1$age=ceiling((g1$recorddate_key-g1$birthdate_key)/365)
## feature engineering to introduce new column for length of service
g1$length_of_service=ceiling(((g1$recorddate_key-g1$orighiredate_key)/365)-1)

###feature engineering to introduce new column to show active or terminated from the organisation
g1$status=ifelse(substring(g1$terminationdate_key,1,4)==1900,"Active","Terminated")

##re summarising after data cleaning
summary(g1)

###plotting business unit and city name
ggplot(g1,aes(city_name,BUSINESS_UNIT))+geom_point(color="red",size=3)+coord_flip()
##conclusion- head office is located at vancouver 


### extracting data for vancouver head office
v1=subset(g1,g1$city_name=="Vancouver" & g1$BUSINESS_UNIT=="HEADOFFICE")

## extracting data for all the stores
v2=subset(g1,g1$BUSINESS_UNIT=="STORES")

##plotting job tittles versus various departments
ggplot(v1,aes(department_name,job_title))+geom_point(size=3,color="orange")
##inferences- from the plot we can classify the various jobs pertaining to a particular departments at the head office
## we also visualise the various hierarchy present for each job title within every department

##plotting job tittles versus various departments
ggplot(v2,aes(department_name,job_title))+geom_point(size=3,color="orange")
##inferences- from the plot we can classify the various jobs pertaining to a particular departments at the stores


##number of stores in each city
ggplot(v2,aes(city_name,store_name))+geom_point(size=3,color="blue")+  coord_flip()

#visulaising trend for number of peope in each city wrt the job title in stores
ggplot(v2,aes(city_name,job_title))+geom_bin2d()+coord_flip()

#visulaising trend for number of peope in each city wrt the job title in head
pie(table(v1$job_title),col = rainbow(7))


##length of service versus age

ggplot(g1,aes(age,length_of_service)) + geom_line(color="red")
##conclusion we can see the trend of lenght of service wrt age
## after age 50 , we can observe that there many people who have served for large span of yeaars
##also as the age increases the lenght f services increases


##attrition rate distribution with respect to gender
ggplot(g1,aes(status))+geom_bar(aes(fill=gender_full),title = "Status of employee",position=position_dodge()) +
  facet_wrap(~BUSINESS_UNIT,scales = "free")
#conclusion-in head office -many people have left as compared to stores
#conclusion - number of people in store is more than the number of people in head office as expected
#colcusion-the ratio of male to female is almost the same in all the scenario


##attrition rate distribution wrt age
ggplot(g1,aes(status,age))+geom_point(title = "Status of employee",position=position_dodge()) +
  facet_wrap(~BUSINESS_UNIT,scales = "free") 
##conclusion- in head office people with age greater than 58 have left the organisation, and no such pattern is obserrved in stores


###pie chart for overall termination reason
lbls=c("Layoff","Active","Resignation","Retirement")
pie3D(table(g1$termreason_desc),explode = 0.2,labels = lbls,radius = 0.7,theta=1)
##conclusion- the majority ypeople leaving the organisation is due to retirement 














