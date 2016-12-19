head(mydat)

list_program <- mydat$program %>% unique
list_term <- mydat$term %>% unique

for (i in 1:length(list_program)) {
    for (j in 1:length(list_term)) {
        user.program <- list_program[i]
        user.term <- list_term[j]
        purchase <- mydat %>% filter(program==user.program & term== user.term) 
        items <- CourseRank(user.program,user.term)$recommand.courses
        mybinary <- CovertBinary(items,purchase)
        if (is.na(mybinary) == FALSE ) {
            rules <- AssociationRule(mybinary)
            saveRDS(rules, file.path("Yang", paste0(i, "_", j, ".rds")))
        }
        j <- j + 1
    }
    i <- i+1
}