library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

comb=readRDS("./Zhang/data/comb_time.rds")
# Calculate time interval for specific day, start and end time

time_interval <- function(day,start,end){
    
    if(is.na(day)||is.na(start)||is.na(end)) interval=NA
    if(!(is.na(day)||is.na(start)||is.na(end))) {
        int_st=ymd_hm(paste(day,start))
        int_end=ymd_hm(paste(day,end))
        interval=interval(int_st,int_end)
    }
    
    return(interval)
}

# Search interval for input course and term
search_interval <- function(id,t){
    
    row_id=which(as.character(comb$course_id)==as.character(id) & as.character(comb$term)==as.character(t))
    day=comb[row_id,paste0(rep("day",7),1:7)]
    day_len=sum(!is.na(day))
    
    if(day_len>0 && row_id==1){
        start=comb[row_id,"start"]
        end=comb[row_id,"end"]
        
        int_list=vector("list",day_len)
        for(i in 1: day_len){
            int_list[[i]] = time_interval(day[i],start,end)
        }
    }
    
    if(day_len>0 && row_id>1){
        
        int_list=vector("list",day_len)
        row_each=apply(day,1,function(x) sum(!is.na(x)))
        
        
        k=0
        for(j in 1:length(row_id)){
            
            start=comb[row_id[j],"start"]
            end=comb[row_id[j],"end"]
            for(i in 1: row_each[j]){
                k=k+1
                int_list[[k]] = time_interval(day[j,i],start,end)
            }
        }
    }
    
    
    if(day_len==0){
        day="2016-01-08" #impossible virtual day to conflict
        start="1:00am"
        end="1:30am"
        int_list=list(time_interval(day,start,end))
    }
    
    return(int_list)
}

# int1=search_interval("410.610.01","3rd")
# int2=search_interval("120.600.01","1st")

# return a vector indicating whether a recommended course is conflict with the selected list or not

time_conflict <- function(recommend,recommend_term,course_list,course_list_term){
    
    recommend_int = search_interval(recommend,recommend_term)
    conflict=vector(length = length(course_list))
    
    for(i in 1: length(course_list)){
        
        int=search_interval(course_list[i],course_list_term[i])
        count=c()
        for(j in 1:length(recommend_int)){
            
            for(k in 1:length(int)){
                
                count=c(count,int_overlaps(recommend_int[[j]],int[[k]]))
            }
        }
        
        conflict[i]=ifelse(sum(count)>0,TRUE,FALSE)
    }
    
    return(conflict)
}

time_conflict

