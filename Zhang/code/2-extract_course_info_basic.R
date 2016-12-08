### Extract info online (web-scrapping)
library(rvest)
library(stringr)
library(tidyr)

## extract course links
url="http://www.jhsph.edu/courses/list/?yearId=2015"
html_course=read_html(url)
nds=html_nodes(html_course,xpath='//*[@id="courseResultsContainer"]')
link=paste0("http://www.jhsph.edu",html_attr(html_nodes(nds, "a"), "href")[-1])

## get information (course name/id, term, time)
get_info <- function(link){
    
    htmlfile = read_html(link)
    ## Extract nodes from html file: daytime, term, instructor, coursename
    nds_daytime = html_nodes(htmlfile,xpath = '//*[@id="modCourseSearch"]/div[2]/div[1]/dl/dd[6]/ul/li
                             ')
    nds_term=html_nodes(htmlfile,xpath='//*[@id="modCourseSearch"]/div[2]/div[1]/dl/dd[2]')
    nds_course=html_nodes(htmlfile,xpath = '//*[@id="modCourseSearch"]/h1')
    
    ## Extract contents
    time = html_text(nds_daytime) %>% str_replace_all(pattern="\n",replacement="") %>%
        str_replace_all(pattern="\t",replacement="") %>%
        str_replace_all(pattern=" ",replacement="")
    if (length(time) > 0) time <- unique(time) else time <- NA

    
    term=gsub(" term","",html_text(nds_term))
    
    course_id= gsub(" .*","",html_text(nds_course))
    course_name= html_text(nds_course) %>% str_replace_all(pattern="[0-9]",replacement="") %>%
        str_replace_all(pattern="\\.",replacement="") %>% str_trim(side="left")
    
    return(c(link,course_id,course_name,term,time))
    
}

num=setdiff(1:length(link),c(1371,1391,1407,1425,1430))
info=matrix(NA,nrow=length(link),ncol=6)  #omit 5 links of same class: 1371,1391,1407,1425,1430 (not found page)
# special case: 1450 (specific times/dates)

for(i in num){
    info[i,]=c(get_info(link[i]),rep(NA,6-length(get_info(link[i]))))
    print(i)
}


info1=as.data.frame(info[which(!is.na(info[,1])),],stringsAsFactors = F) # total: 1614 courses
colnames(info1)=c("link","course_id","course_name","term","time1","time2")


saveRDS(info1,"./data/info.rds")
write.csv(info1,"./data/info.csv",row.names = F)

# process courses which are different time on different day
diff_id=which(!is.na(info1[,6]))
info_diff=info1[diff_id,c("link","course_id","course_name","term","time2")]
colnames(info_diff)[5]="time1"
info2=rbind(info1[,-6],info_diff) #total:1617 courses (duplicated)

saveRDS(info2,"./data/info_duplicate.rds")
write.csv(info2,"./data/info_duplicate.csv",row.names = F)
