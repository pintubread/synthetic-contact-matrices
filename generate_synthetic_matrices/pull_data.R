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

countries <- as.vector(unique(dataset$iso3c))
save(countries, file="countries.RData")

################################################################################

load("wb_dataset.RData")
dataset_final <- merge(dataset,wb_dataset,by="iso3c",all.x = T)
dataset_final <- dataset_final[which(rowSums(is.na(dataset_final))==0),]

save(dataset_final,file="dataset_final.RData")

################################################################################
load('input/pop/popfemale.rdata')
load('input/pop/popmale.rdata')


colnames(popfemale)[4:25] <- sapply(colnames(popfemale[,4:25]),function(x){paste0(x,"_popfemale")})
colnames(popmale)[4:25] <- sapply(colnames(popmale[,4:25]),function(x){paste0(x,"_popmale")})

dataset_final <- merge(dataset_final, popfemale, by=c("iso3c","year","countryname"))
dataset_final <- merge(dataset_final, popmale, by=c("iso3c","year","countryname"))


colnames(popUrban$total_female)[4:25] <- sapply(colnames(popUrban$total_female[,4:25]),function(x){paste0(x,"_number_urban_females")}) 
colnames(popUrban$total_male)[4:25] <- sapply(colnames(popUrban$total_male[,4:25]),function(x){paste0(x,"_number_urban_males")}) 

dataset_final <- merge(dataset_final,popUrban$total_female,by=c("iso3c","countryname","year"))
dataset_final <- merge(dataset_final,popUrban$total_male,by=c("iso3c","countryname","year"))

colnames(popRural$total_female)[4:25] <- sapply(colnames(popRural$total_female[,4:25]),function(x){paste0(x,"_number_rural_females")}) 
colnames(popRural$total_male)[4:25] <- sapply(colnames(popRural$total_male[,4:25]),function(x){paste0(x,"_number_rural_males")}) 

dataset_final <- merge(dataset_final,popRural$total_female,by=c("iso3c","countryname","year"))
dataset_final <- merge(dataset_final,popRural$total_male,by=c("iso3c","countryname","year"))

save(dataset_final,file="dataset_final.RData")

###############################################################################
# load('dataset_final.RData')
# load('input/school/schoolage.rdata')
# colnames(school)[4:17] <- sapply(colnames(popmale[,4:17]),function(x){paste0(x,"_school")})
# dataset_final <- merge(dataset_final,school,by.x="iso3c",by.y="iso")
# dataset_final <- dataset_final[,-c(442,443)]

load('input/work/workpopage_urban.rdata')
load('input/work/workpopage_rural.rdata')

workpopage_rural$total <- workpopage_rural$total[,-c(2,3,5,6,7,8,20,21)]
colnames(workpopage_rural$total)[3:13] <- sapply(colnames(workpopage_rural$total[,3:13]),function(x){paste0(x,"_workpop_ratio_rural")}) 
workpopage_rural$male <- workpopage_rural$male[,-c(2,3,5,6,7,8,20,21)]
colnames(workpopage_rural$male)[3:13] <- sapply(colnames(workpopage_rural$male[,3:13]),function(x){paste0(x,"_male_workpop_ratio_rural")}) 
workpopage_rural$female <- workpopage_rural$female[,-c(2,3,5,6,7,8,20,21)]
colnames(workpopage_rural$female)[3:13] <- sapply(colnames(workpopage_rural$female[,3:13]),function(x){paste0(x,"_female_workpop_ratio_rural")}) 

dataset_final <- merge(dataset_final,workpopage_rural$total,by=c("iso3c","year"))
dataset_final <- merge(dataset_final,workpopage_rural$male,by=c("iso3c","year"))
dataset_final <- merge(dataset_final,workpopage_rural$female,by=c("iso3c","year"))



workpopage_urban$total <- workpopage_urban$total[,-c(2,3,5,6,7,8,20,21)]
colnames(workpopage_urban$total)[3:13] <- sapply(colnames(workpopage_urban$total[,3:13]),function(x){paste0(x,"_workpop_ratio_urban")}) 
workpopage_urban$male <- workpopage_urban$male[,-c(2,3,5,6,7,8,20,21)]
colnames(workpopage_urban$male)[3:13] <- sapply(colnames(workpopage_urban$male[,3:13]),function(x){paste0(x,"_male_workpop_ratio_urban")}) 
workpopage_urban$female <- workpopage_urban$female[,-c(2,3,5,6,7,8,20,21)]
colnames(workpopage_urban$female)[3:13] <- sapply(colnames(workpopage_urban$female[,3:13]),function(x){paste0(x,"_female_workpop_ratio_urban")}) 

dataset_final <- merge(dataset_final,workpopage_urban$total,by=c("iso3c","year"))
dataset_final <- merge(dataset_final,workpopage_urban$male,by=c("iso3c","year"))
dataset_final <- merge(dataset_final,workpopage_urban$female,by=c("iso3c","year"))


save(dataset_final,file="dataset_final.RData")
################################################################################
load("dataset_final.RData")

#dataset_final <- dataset_final[,-(428:441)]

colnames(school)[2:17] <- sapply(colnames(school[,2:17]),function(x){paste0(x,"_school")})
dataset_final <- merge(dataset_final,school,by.x="iso3c",by.y="iso")
dataset_final <- dataset_final[,-c(508,509)]

save(dataset_final,file="dataset_final.RData")

################################################################################
dataset_final <- dataset_final[,!colnames(dataset_final) %in% c('total_rural_female',
                                                                'total_urban_female',
                                                                'total_urban_male',
                                                                'total_rural_male',
                                                                'pop_female_total',
                                                                'pop_male_total')]
load('input/school/students_rural.rdata')
load('input/school/students_urban.rdata')

students_rural <- students_rural[,1:6]
students_urban <- students_urban[,1:6]

load('input/school/teachers_rural.rdata')
load('input/school/teachers_urban.rdata')

teachers_rural <- teachers_rural[,-c(2:5,16:17)]
teachers_urban <- teachers_urban[,-c(2:5,16:17)]

colnames(students_rural)[2:6] <- sapply(colnames(students_rural)[2:6],
                                        function(x){paste0(x,"_num_students_rural")})

colnames(students_urban)[2:6] <- sapply(colnames(students_urban)[2:6],
                                        function(x){paste0(x,"_num_students_urban")})

dataset_final <- merge(dataset_final, students_rural, by.x="iso3c",by.y="iso",
                       all.x = T)
dataset_final <- merge(dataset_final, students_urban, by.x="iso3c",by.y="iso",
                       all.x = T)

colnames(teachers_rural)[2:11] <- sapply(colnames(teachers_rural)[2:11],
                                        function(x){paste0(x,"_num_teachers_rural")})
colnames(teachers_urban)[2:11] <- sapply(colnames(teachers_urban)[2:11],
                                         function(x){paste0(x,"_num_teachers_urban")})
dataset_final <- merge(dataset_final, teachers_rural, by.x="iso3c",by.y="iso",
                       all.x = T)
dataset_final <- merge(dataset_final, teachers_urban, by.x="iso3c",by.y="iso",
                       all.x = T)

load('input/school/schoolage_urban.rdata')
load('input/school/schoolage_rural.rdata')

school_rural <- school_rural[,-c(16:17)]
school_urban <- school_urban[,-c(16:17)]

colnames(school_rural)[2:15] <- sapply(colnames(school_rural)[2:15],
                                         function(x){paste0(x,"_school_rural")})
colnames(school_urban)[2:15] <- sapply(colnames(school_urban)[2:15],
                                         function(x){paste0(x,"_school_urban")})

dataset_final <- merge(dataset_final, school_rural, by.x="iso3c",by.y="iso",
                       all.x = T)
dataset_final <- merge(dataset_final, school_urban, by.x="iso3c",by.y="iso",
                       all.x = T)

save(dataset_final,file = "dataset_final.RData")
