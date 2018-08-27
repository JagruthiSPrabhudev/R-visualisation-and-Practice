library(stringr)
library(dplyr)
library(readxl)

files = list.files(pattern = "task")
tempt = list()
dat = data.frame()

for(i in 1:length(files)){
  tempt[[i]] = read.csv(files[i])
  dat = rbind(dat, tempt[[i]])
}

section.id = paste(str_sub(as.character(dat$PrimarySubject), -6, -2),
                   "20183", sep = "-") # need to change the term for each term 
dat = cbind(section.id, dat)

# skip this when read SVM data only

dat.subject = dat[dat$TaskType == "SubjectViewManagement",] 

# end skip code

dashboard = read_xlsx("Dashboard Viewer.xlsx")

# join tables

str(dashboard)
dashboard$id = str_sub(as.character(dashboard$id), -11,-1)

full.dat = left_join(dat, dashboard, by = c("section.id" = "id"))

missing.subject = full.dat[is.na(full.dat$Name),]
View(missing.subject)

View(full.dat)

names(full.dat)

diff.dates = full.dat[which(as.Date(full.dat$StartDate) != as.Date(full.dat$`Start Date`)),]
View(diff.dates) # StartDate from 'task' data have the most curent dates (manual changes)

full.dat2 = full.dat %>% 
  select(-c(`Start Date`, `End Date`, Name)) %>% 
  filter(TaskOwnerStatus != "Deleted")

write.csv(full.dat2, "Tableau.data.csv", row.names = F)








