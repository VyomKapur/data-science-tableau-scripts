setwd("~/Jupyter Notebooks/EDA/Tables Used")
data <- read.csv("01_District_wise_crimes_committed_IPC_2001_2012.csv")

library(dplyr)

crimes <- data %>%
  group_by(data$STATE.UT, data$YEAR) %>%
  summarise(across(TOTAL.IPC.CRIMES, sum))

head(crimes)

crimes <- as.data.frame(crimes)
colnames(crimes) <- c("NAME", "YEAR", "TOTAL_CRIMES")

## Merging pop data (pls kill me)

library(data.table)
library(Rmpfr)

for(i in 1:12){
  if(i > 9){
    pop <- fread(paste("./Population Data/data_20",i,".csv", sep=""), select=c("NAME", "YEAR", "TOT_P"))
  } else{
    pop <- fread(paste("./Population Data/data_200",i,".csv", sep=""), select=c("NAME", "YEAR", "TOT_P"))
  }
  if(i == 1){
    colnames(pop) <- c("NAME", "YEAR", "TOTAL_POP")
    mega_pop <- pop[1:37,]
  } else if(i == 11) {
    colnames(pop) <- c("NAME", "YEAR", "TOTAL_POP")
    mega_pop <- rbind(mega_pop, pop[1:37,])
  } else{
    pop1 <- pop[c(1:69, 74),]
    colnames(pop1) <- colnames(pop)
    pop1 <- as.data.frame(pop1)
    pop1 <- pop1 %>%
      group_by(pop1$NAME, pop1$YEAR) %>%
      summarise(across(c(TOT_P), sum))
    colnames(pop1) <- c("NAME", "YEAR", "TOTAL_POP")
    print(unique(pop1$NAME))
    mega_pop <- rbind(mega_pop, pop1) 
  }
}


head(mega_pop)

mega_pop = mega_pop[mega_pop$NAME != "India",]
mega_pop = mega_pop[mega_pop$NAME != "TELANGANA",]
mega_pop = mega_pop[mega_pop$NAME != "UTTARANCHAL",]

unique(mega_pop$NAME)

unique(crimes$NAME)

mega_pop[mega_pop$NAME == "NCT OF DELHI",]$NAME = "DELHI"

crimes[crimes$NAME == "DELHI UT",]$NAME = "DELHI"
crimes[crimes$NAME == "D & N HAVELI",]$NAME = "DADRA & NAGAR HAVELI"
crimes[crimes$NAME == "A & N ISLANDS",]$NAME =  "ANDAMAN & NICOBAR ISLANDS"

head(mega_pop)

sort(unique(crimes$NAME)) == sort(unique(mega_pop$NAME))


## State size data 


states <- read.table("states.txt", header=FALSE, sep="\t")

states <- data.frame(states)
states <- states[2:3]
head(states)
colnames(states) <- c("NAME", "AREA")
head(states)

states$NAME <- toupper(states$NAME)

head(states)

unique(states$NAME)

states[states$NAME == "DADRA & NAGAR HAVELI AND DAMAN & DIU",]$NAME = "DADRA & NAGAR HAVELI"
states[states$NAME == "A. & N. ISLANDS",]$NAME =  "ANDAMAN & NICOBAR ISLANDS"

states = states[states$NAME != "LADAKH",]
states = states[states$NAME != "TELANGANA",]


states[states$NAME  == "DADRA & NAGAR HAVELI",]$AREA = 491
states <- rbind(states, data.frame(NAME="DAMAN & DIU", AREA=112))
sort(unique(crimes$NAME)) == sort(unique(states$NAME))

write.csv(crimes, "IPC_crimes_district_wise.csv", row.names = TRUE)
write.csv(mega_pop, "Population_data.csv", row.names=TRUE)
write.csv(states, "State_Area.csv", row.names=TRUE)
