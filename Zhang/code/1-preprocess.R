## read in data
library(readxl)
library(dplyr)
dat=read_excel("./data/CommonCourseApp.xlsx")
dat=dat[complete.cases(dat),] #26766
colnames(dat)=c("id","program","period","course_id","section_id","nc")

## preprocess data
# merge course_id and section_id as new column: course
dat$course_id=gsub("PH\\.","",dat$course_id)
dat= dat %>% mutate(course=paste(course_id,section_id,sep="."))

# extract term info
dat= dat %>% mutate(term=gsub(".* .* ","",period)) 
enroll = dat %>% select(id,program,term,course,nc)

# unique course id
course_all=unique(dat$course)

#write.csv(enroll,"./data/enrollment.csv",row.names = F)
