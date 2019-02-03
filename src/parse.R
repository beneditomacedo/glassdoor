library(dplyr)
glassdoor <- parse_glassdoor_dir("~/Apps/glassdoor/data/raw")

glassdoor$Company[str_detect(glassdoor$Company,"Estudos")] <- "CESAR"
glassdoor$Company[str_detect(glassdoor$Company,"CERTI")] <- "CERTI"
glassdoor$Company[str_detect(glassdoor$Company,"Eldorado")] <- "Eldorado"
glassdoor$Company[str_detect(glassdoor$Company,"FIT")] <- "FIT"
glassdoor$Company[str_detect(glassdoor$Company,"FPF")] <- "FPF"
glassdoor$Company[str_detect(glassdoor$Company,"Inatel")] <- "Inatel"
glassdoor$Company[str_detect(glassdoor$Company,"SIDI")] <- "SIDI"
glassdoor$Company[str_detect(glassdoor$Company,"Venturus")] <- "Venturus"
glassdoor$Company[str_detect(glassdoor$Company,"SENAI")] <- "Cimatec"
glassdoor$Company<-as.factor(glassdoor$Company)
glassdoor <- glassdoor %>% mutate (id = row_number()) %>% select(id,everything())

save(glassdoor, file="~/Apps/glassdoor/data/db/glassdoor.Rda")

library(cld3)

glassdoor_en <- glassdoor %>% 
  filter (detect_language(Pros) != "pt" & detect_language(Pros) != "fr" ) 

save(glassdoor_en, file="~/Apps/glassdoor/data/db/glassdoor_en.Rda")