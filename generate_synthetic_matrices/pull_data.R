load('input/pop/popratio.rdata')
load('input/pop/popratio_female.rdata')
load('input/pop/popratio_male.rdata')

colnames(popratio)[4:25] <- sapply(colnames(popratio[,4:25]),function(x){paste0(x,"_popratio")})
colnames(popratio_female)[4:25] <- sapply(colnames(popratio_female[,4:25]),function(x){paste0(x,"_popratio_female")})
colnames(popratio_male)[4:25] <- sapply(colnames(popratio_male[,4:25]),function(x){paste0(x,"_popratio_male")})

dataset <- merge(popratio, popratio_female, by=c("iso3c","year","countryname"))
dataset <- merge(dataset, popratio_male, by=c("iso3c","year","countryname"))

load('input/pop/urbanicity.rdata')
load('input/pop/popUrban.rdata')
load('input/pop/popRural.rdata')

colnames(urbanicity$female)[2:23] <- sapply(colnames(urbanicity$female[,2:23]),function(x){paste0(x,"_urbanratio_female")}) 
colnames(urbanicity$male)[2:23] <- sapply(colnames(urbanicity$male[,2:23]),function(x){paste0(x,"_urbanratio_male")}) 

dataset <- merge(dataset,urbanicity$female,by="iso3c")
dataset <- merge(dataset,urbanicity$male,by="iso3c")

colnames(popUrban$ratio_female)[4:25] <- sapply(colnames(popUrban$ratio_female[,4:25]),function(x){paste0(x,"_ratio_of_urban_females")}) 
colnames(popUrban$ratio_male)[4:25] <- sapply(colnames(popUrban$ratio_male[,4:25]),function(x){paste0(x,"_ratio_of_urban_males")}) 

dataset <- merge(dataset,popUrban$ratio_female,by=c("iso3c","countryname","year"))
dataset <- merge(dataset,popUrban$ratio_male,by=c("iso3c","countryname","year"))

colnames(popRural$ratio_female)[4:25] <- sapply(colnames(popRural$ratio_female[,4:25]),function(x){paste0(x,"_ratio_of_rural_females")}) 
colnames(popRural$ratio_male)[4:25] <- sapply(colnames(popRural$ratio_male[,4:25]),function(x){paste0(x,"_ratio_of_rural_males")}) 

dataset <- merge(dataset,popRural$ratio_female,by=c("iso3c","countryname","year"))
dataset <- merge(dataset,popRural$ratio_male,by=c("iso3c","countryname","year"))

total_vars <- which(colnames(dataset) %in% c("total_ratio_of_rural_males","total_ratio_of_rural_females",
                                              "total_ratio_of_urban_males","total_ratio_of_urban_females",
                                              "total_urbanratio_male","total_urbanratio_female",
                                              "total_popratio_male","total_popratio_female","total_popratio"))
colnames(dataset)[total_vars] <- c("pop_total","pop_female_total","pop_male_total",
                                   "total_female_urban_ratio","total_male_urban_ratio",
                                   "total_urban_female","total_urban_male",
                                   "total_rural_female","total_rural_male")
################################################################################

load('input/work/workpopage.rdata')

workpopage$total <- workpopage$total[,-c(2,3,5,6,7,8,20,21)]
colnames(workpopage$total)[3:13] <- sapply(colnames(workpopage$total[,3:13]),function(x){paste0(x,"_workpop_ratio")}) 
workpopage$male <- workpopage$male[,-c(2,3,5,6,7,8,20,21)]
colnames(workpopage$male)[3:13] <- sapply(colnames(workpopage$male[,3:13]),function(x){paste0(x,"_male_workpop_ratio")}) 
workpopage$female <- workpopage$female[,-c(2,3,5,6,7,8,20,21)]
colnames(workpopage$female)[3:13] <- sapply(colnames(workpopage$female[,3:13]),function(x){paste0(x,"_female_workpop_ratio")}) 

dataset <- merge(dataset,workpopage$total,by=c("iso3c","year"))
dataset <- merge(dataset,workpopage$male,by=c("iso3c","year"))
dataset <- merge(dataset,workpopage$female,by=c("iso3c","year"))


load('input/school/enrolment.rdata')
colnames(enrolment)[2:6] <- sapply(colnames(enrolment[,2:6]),function(x){paste0(x,"_mean_enrolment")}) 
dataset <- merge(dataset,enrolment,by.x="iso3c",by.y="iso")

load('input/school/end_age_school.rdata')
colnames(end_age_school)[2:5] <- sapply(colnames(end_age_school[,2:5]),function(x){paste0(x,"_end_age")}) 
dataset <- merge(dataset,end_age_school,by.x="iso3c",by.y="iso")

load('input/school/start_age_school.rdata')
colnames(start_age_school)[2:5] <- sapply(colnames(start_age_school[,2:5]),function(x){paste0(x,"_start_age")}) 
dataset <- merge(dataset,start_age_school,by.x="iso3c",by.y="iso")

load('input/school/pupiltoteacherratio.rdata')
colnames(PTR)[2:5] <- sapply(colnames(PTR[,2:5]),function(x){paste0(x,"_pupil_to_teacher_ratio")}) 
dataset <- merge(dataset,PTR,by.x="iso3c",by.y="iso")

load('input/school/secteachers.rdata')
colnames(secteachers)[2] <- "number_sec_teachers"
dataset <- merge(dataset,secteachers,by.x="iso3c",by.y="iso")

load('input/school/students.rdata')
students <- students[,1:6]
colnames(students)[2:6] <- sapply(colnames(students)[2:6],function(x){paste0(x,"_num_students")})
dataset <- merge(dataset,students,by.x="iso3c",by.y="iso")

load('input/school/teachers.rdata')
teachers <- teachers[,-c(2:5,16,17)]
colnames(teachers)[2:11] <- sapply(colnames(teachers)[2:11],function(x){paste0(x,"_num_teachers")})
dataset <- merge(dataset,teachers,by.x="iso3c",by.y="iso")

save(dataset,file="collated_inputs.RData")

load('input/wbindicators.rdata') #cannot work, pull from country_indicators
