####Load Packages####
library(ggplot2)
library(dplyr)
library(ggthemes)

####Import Data into R###
pc <- read.csv("C:/R/ghana_2016_parliamentary_candidates/ghana_parliamentary_candidates_2016.csv")

####Total number of constituencies###
pc_constituency <- group_by(pc, constituency)
pc_constituency <- summarise(pc_constituency,n = n())
nrow(pc_constituency)

OR

length(unique(pc$constituency))

####Total Parliamentary candidates####
nrow(pc)

####Total number of regions####
length(unique(pc$region))

####Total number of female Parliamentary Candidates####
summarise(filter(pc, sex == "F"), n())

####Mean age of each party####
pc_constituency <- group_by(pc, party)
by_age <- summarise(pc_constituency,a = mean(age))
by_age

####Mean age of each constitueny####
pc_constituency <- group_by(pc, constituency)
by_age <- summarise(pc_constituency, mean(age))
filter(by_age, constituency == "CAPE COAST NORTH")
filter(by_age, constituency == "CAPE COAST SOUTH")

####P.C.s of each Party below 30yrs####
filter(pc, party == "NPP", age < 30)
filter(pc, party == "NDC", age < 30)
filter(pc, party == "CPP", age < 30)
filter(pc, party == "PPP", age < 30)
filter(pc, party == "PPP", age < 30)
filter(pc, party == "NDP", age < 30)

#### % of Female Parliamentary candidates in each party####
((summarise(filter(pc, party == "NDP", sex == "F"), n()))/(summarise(filter(pc, party == "NDP"), n())))*100
((summarise(filter(pc, party == "NPP", sex == "F"), n()))/(summarise(filter(pc, party == "NPP"), n())))*100
((summarise(filter(pc, party == "NDC", sex == "F"), n()))/(summarise(filter(pc, party == "NDC"), n())))*100
((summarise(filter(pc, party == "CPP", sex == "F"), n()))/(summarise(filter(pc, party == "CPP"), n())))*100
((summarise(filter(pc, party == "PPP", sex == "F"), n()))/(summarise(filter(pc, party == "PPP"), n())))*100
((summarise(filter(pc, party == "GCPP", sex == "F"), n()))/(summarise(filter(pc, party == "GCPP"), n())))*100
((summarise(filter(pc, party == "PNC", sex == "F"), n()))/(summarise(filter(pc, party == "PNC"), n())))*100

####P.C.s with UCC eduational bakground####
filter(pc, educational_institution == "UNIV. OF CAPE COAST"| 
         educational_institution =="UNIVERSITY OF CAPE COAST"|
         educational_institution == "UNIV. CAPE COAST"|
         educational_institution == "UNI. OF CAPE COAST"| 
         educational_institution == "UCC CAPE COAST"| 
         educational_institution == "UCC"| educational_institution == "U.C.C"| 
         educational_institution == "U C C")

#### % of P.C.s of party in region####
((summarise(filter(pc, party == "NDP", region == "Volta"), n()))/(summarise(filter(pc, party == "NDP"), n())))*100
((summarise(filter(pc, party == "CPP", region == "Northern" | region =="uppereast" | region =="upperwest"), n()))/(summarise(filter(pc, party == "CPP"), n())))*100

####Each Political Parties P.C.in each region as % of Each Total P.C.####
f <- filter(pc, party == "PPP")
pc_region <- group_by(f, region)
by_region <- summarise(pc_constituency,PPP = (n()/nrow(f))*100)
f <- filter(pc, party == "NDP")
pc_constituency <- group_by(f, region)
by_region1 <- summarise(pc_constituency,NDP = (n()/nrow(f))*100)
a<- merge(by_region, by_region1, all.x = T)
f <- filter(pc, party == "GCPP")
pc_constituency <- group_by(f, region)
by_region1 <- summarise(pc_constituency, GCPP = (n()/nrow(f))*100)
a <- merge(a, by_region1, all.x = T)
f <- filter(pc, party == "APC")
pc_constituency <- group_by(f, region)
by_region1 <- summarise(pc_constituency, APC = (n()/nrow(f))*100)
a <- merge(a, by_region1, all.x = T)
f <- filter(pc, party == "CPP")
pc_constituency <- group_by(f, region)
by_region1 <- summarise(pc_constituency, CPP = (n()/nrow(f))*100)
a <- merge(a, by_region1, all.x = T)
f <- filter(pc, party == "PNC")
pc_constituency <- group_by(f, region)
by_region1 <- summarise(pc_constituency, PNC = (n()/nrow(f))*100)
a <- merge(a, by_region1, all.x = T)

####Create standard P.C. %####
standard <- c((47/275)*100, (29/275)*100,
              (23/275)*100,
              (33/275)*100,
              (34/275)*100,
              (31/275)*100,
              (15/275)*100,
              (11/275)*100,
              (26/275)*100,
              (26/275)*100)

a$standard <- standard

a[is.na(a)] <- 0

####PC difference % from standard %####
a <- mutate(a, cppdiff = CPP - standard,
            gcppdiff = GCPP - standard, 
            apcdiff = APC - standard, 
            ndpdiff = NDP - standard, 
            pppdiff = PPP - standard, 
            pncdiff = PNC - standard)

####Plotting time!
ggplot(a, aes(x = region, y = gcppdiff, fill = pppdiff)) + 
  geom_bar(stat = "identity") + 
  ggtitle("PPP") + coord_flip() + theme_base() + geom_hline(yintercept = 0) + 
  scale_fill_gradient(low = "red", high = "green")+ xlab("Region") + ylab("% Difference from even distribution of Party Parliamentarians")
#last_plot() +coord_flip()

#Gives gradient plot without scale!
ggplot(a, aes(x = region, y = pncdiff, fill = pncdiff)) + 
  geom_bar(stat = "identity") + 
  ggtitle("PPP") + coord_flip() + theme_base() + geom_hline(yintercept = 0) + 
  scale_fill_gradient("Legend", low = "#F4D03F", high = "#16A085")+ xlab("Region") + 
  ylab("% Difference from even distribution of Party Parliamentarians") + 
  ylim(-22.5,22.5) + theme(panel.background=element_rect("mintcream"), 
                           axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 20), 
                           title = element_text(colour = "navyblue")) + 
  geom_text(aes(label = c(paste0(round(a$pncdiff),"%"))), size = 6, hjust= ifelse(a$pncdiff>= 0, 0, 1))

ggplot(a, aes(x = region, y = pncdiff, fill = pncdiff)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Peoples National Convention (PNC)") + coord_flip() + theme_classic() + geom_hline(yintercept = 0) + 
  scale_fill_gradient("Legend", low = "#F4D03F", high = "#16A085")+ xlab("Region") + 
  ylab("% Difference from expected % distribution of Party Parliamentary Candidates") + ylim(-22.5,22.5) + 
  theme(plot.background=element_rect("white"), panel.background=element_rect("white"), 
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 20), 
        title = element_text(colour = "navyblue", size = 20), legend.position = "none") + 
  geom_text(aes(label = c(paste0(round(a$pncdiff),"%"))), size = 6, hjust= ifelse(a$pncdiff>= 0, 0, 1))

ggplot(a, aes(x = region, y = pncdiff, fill = pncdiff)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Peoples National Convention (PNC)") + coord_flip() + theme_classic() + geom_hline(yintercept = 0) + 
  scale_fill_gradient("Legend", low = "#F4D03F", high = "#16A085")+ xlab("Region") + 
  ylab("% Difference from expected % distribution of Party Parliamentary Candidates") + ylim(-22.5,22.5) + 
  theme(plot.background=element_rect("white"), panel.background=element_rect("white"), 
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 20), 
        title = element_text(colour = "navyblue", size = 20), legend.position = "none") + 
  geom_text(aes(label = c(paste0(round(a$pncdiff),"%"))), size = 6, hjust= ifelse(a$pncdiff>= 0, 0, 1))


filter(pc, party =="NDC"| 
         party =="NPP"|party == "UFP"|party == "UPP"|
         party == "DPP"|party =="APC"|party == "GCPP"|
         party == "NDP"|party == "PNC"|party == "CPP") %>% ggplot(aes(age,party)) +
  geom_point() + coord_flip() + theme(plot.background=element_rect("white"), panel.background=element_rect("white"), 
                                      axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 20), 
                                      title = element_text(colour = "navyblue", size = 20), legend.position = "none") + title("Ages for each Porlitical Party")

a <- read.csv("C:/R/a.csv")
b <- ggplot(a, aes(region, diff, frame = party)) + geom_bar(stat = "identity") + coord_flip()
b <- ggplot(a) + geom_bar(aes(region, diff, frame = party),stat = "identity") + 
  ggtitle("Peoples National Convention (PNC)") + coord_flip() + theme_classic() + geom_hline(yintercept = 0) + 
  scale_fill_gradient("Legend", low = "#F4D03F", high = "#16A085")

gg_animate(b, outfile = "outfile.gif", convert = "gm convert", 
           ani.width = 700, title_frame = TRUE)