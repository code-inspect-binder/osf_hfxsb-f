#packages 
library(tidyverse)
library(haven)
library(scales)

# reading in anes data
anes <- read_dta("data/anes_timeseries_2016.dta")

# need to select group ft's from anes
# need to select group vote behavior 
anes <- anes %>% select(Ideology = V161126, 
                      Income = V161361x, race = V161310x, 
                      gender = V161342, mode = V160501, educ = V161270, age = V161267x, ftgr_xfund = V162095, 
                      ftgr_feminists = V162096, ftgr_liberals = V162097, ftgr_unions = V162098, ftgr_poor = V162099, 
                      ftgr_bigbus = V162100, ftcasi_illegal = V162313, ftgr_cons = V162101, ftgr_gay = V162103, 
                      ftgr_muslims = V162106, ftcasi_asian = V162310, ftcasi_hisp = V162311, ftcasi_black = V162312, 
                      ftcasi_white = V162314, ftgr_xian = V162107, 
                      ftgr_rich = V162105, ftgr_jews = V162108, 
                      ftgr_scientists = V162112, ftgr_police = V162110, 
                      ftgr_trans = V162111, 
                      post_vote = V162034a, lgb = V161511, 
                      relig_attend = V161247a, 
                      relig_noattend = V161247b, 
                      feminist= V161345, 
                      fundamentalist = V161266a, 
                      union = V161303, 
                      hispanic = V161309, income = V161361x, 
                      pid7 = V161158x)
# recoding ft targets 
anes$xfund <- 100-anes$ftgr_xfund
anes$feminists <- 100-anes$ftgr_feminists
anes$liberals <- 100-anes$ftgr_liberals
anes$unions <- 100-anes$ftgr_unions
anes$poor <- 100-anes$ftgr_poor
anes$cons <- 100-anes$ftgr_cons
anes$gay <- 100-anes$ftgr_gay
anes$muslims <- 100-anes$ftgr_muslims
anes$xian <- 100-anes$ftgr_xian
anes$asian <- 100-anes$ftcasi_asian
anes$hisp <- 100-anes$ftcasi_hisp
anes$black <- 100-anes$ftcasi_black
anes$white <- 100-anes$ftcasi_white
anes$jews <- 100-anes$ftgr_jews
anes$police <- 100-anes$ftgr_police
anes$rich <- 100-anes$ftgr_rich

# making long 
anes$pid <- 1:nrow(anes)

aneslong <- tidyr::gather(anes, target, prejudice, xfund:rich)
unique(aneslong$target)

aneslong$target[aneslong$target=="xfund"] <- "Christian Fundamentalists" 
aneslong$target[aneslong$target=="feminists"] <- "Feminists"
aneslong$target[aneslong$target=="liberals"] <- "Liberals"
aneslong$target[aneslong$target=="unions"] <- "Labor Unions"
aneslong$target[aneslong$target=="poor"] <- "Poor People"
aneslong$target[aneslong$target=="cons"] <- "Conservatives"
aneslong$target[aneslong$target=="gay"] <- "Gay Men and Lesbians"
aneslong$target[aneslong$target=="muslims"] <- "Muslims"
aneslong$target[aneslong$target=="xian"] <- "Christians" 
aneslong$target[aneslong$target=="asian"] <- "Asian Americans"
aneslong$target[aneslong$target=="hisp"] <- "Hispanic People"
aneslong$target[aneslong$target=="black"] <- "Black People"
aneslong$target[aneslong$target=="white"] <- "White People" 
aneslong$target[aneslong$target == "jews"] <- "Jewish People"
aneslong$target[aneslong$target == "police"] <- "Police"
aneslong$target[aneslong$target == "rich"] <- "Rich People"

# reading in ideo ratings and matching names 
group_ideo <- read.csv("data/group_ratings_2016.csv")
group_ideo <- subset(group_ideo, Group %in% unique(aneslong$target)) 

groups <- group_ideo$Group

# pulling party id 
aneslong <- subset(aneslong, aneslong$pid7 > 0)
betas <- rep(NA, 16)
range(aneslong$pid7)

# each person rates each group once max so we can work with long 
# dataset subsetting by group and fitting simple lm by group 
for(i in 1:length(groups)){
  dat <- subset(aneslong, aneslong$target == groups[i])
  mod <- lm(prejudice ~ pid7, data = dat)
  betas[i] <- summary(mod)$coefficients[2]
}

# now pulling voting behavior for those we have 

anes$Rich <- ifelse(anes$Income > 27, 1, 0)
anes$Poor <- ifelse(anes$Income > 0 & anes$Income < 9, 1, 0) 
poor <- subset(anes, anes$Poor == 1) 
rich <- subset(anes, anes$Rich == 1) 
poor_vote <- sum(poor$post_vote == 2)  / (sum(poor$post_vote ==1) + sum(poor$post_vote == 2)) 
rich_vote <- sum(rich$post_vote == 2)  / (sum(rich$post_vote ==1) + sum(rich$post_vote == 2))
rich_vote

#### unions 
union <- subset(anes, anes$union == 1)
union_vote <- sum(union$post_vote == 2)/ (sum(union$post_vote == 1) + sum(union$post_vote == 2))
union_vote

#### liberals

# subsetting those respondents who give symbolic ideo 
anes <- subset(anes,(anes$Ideology > 0 & anes$Ideology <= 7)) 
anes$Conservative <- ifelse(anes$Ideology > 4, 1, 0)
anes$Liberal <- ifelse(anes$Ideology < 4, 1, 0)

liberals <- subset(anes, anes$Liberal == 1)
liberal_vote <- sum(liberals$post_vote == 2)/ (sum(liberals$post_vote == 1) + sum(liberals$post_vote == 2))


#### conservatives 
conservatives <- subset(anes, anes$Conservative == 1)
conservative_vote <- sum(conservatives$post_vote == 2) / (sum(conservatives$post_vote == 1) + sum(conservatives$post_vote == 2))

#### christian fundamentalists
fund <- subset(anes,anes$fundamentalist == 1 & (anes$relig_attend == 1 | anes$relig_attend == 2)) 
fund_vote <- sum(fund$post_vote == 2) / (sum(fund$post_vote == 1) + sum(fund$post_vote == 2))

#### feminists 
aneslong$feminist
feminists <- subset(anes, (anes$feminist == 1 | anes$feminist == 2))
fem_vote <- sum(feminists$post_vote == 2) / (sum(feminists$post_vote == 1) + sum(feminists$post_vote == 2))


#### sexual minorities
lgb <- subset(anes, (anes$lgb == 2 | anes$lgb == 3))
lgb_vote <- sum(lgb$post_vote == 2) / (sum(lgb$post_vote == 1) + sum(lgb$post_vote == 2))
lgb_vote

#### muslims value taken from pew 
muslim_vote <- .093

#### christians 
chris <- subset(anes, (anes$relig_attend == 1 | anes$relig_attend == 2 | 
                             anes$relig_noattend == 1 | anes$relig_noattend == 2))
chris_vote <- sum(chris$post_vote == 2) / (sum(chris$post_vote == 1) + sum(chris$post_vote == 2))

#### whites
aneslong$race
white <- subset(anes, anes$race == 1)
white_vote <- sum(white$post_vote == 2) / (sum(white$post_vote == 1) + sum(white$post_vote == 2))
white_vote

#### Jewish people
jewish <- subset(anes, (anes$relig_attend == 3 | anes$relig_noattend == 3))
jewish_vote <- sum(jewish$post_vote == 2) / (sum(jewish$post_vote == 1) + sum(jewish$post_vote == 2))
jewish_vote

#### police from mag survey 
police_vote <- 84/92


#### Asian Americans
AA <- subset(anes, anes$race == 3)
AA_vote <- sum(AA$post_vote == 2) / (sum(AA$post_vote == 1) + sum(AA$post_vote == 2))

#### Black Americans
Blacks <- subset(anes, anes$race == 2)
Black_vote <- sum(Blacks$post_vote == 2) / (sum(Blacks$post_vote == 1) + sum(Blacks$post_vote == 2))
Black_vote

#### Hispanic Americans 
Hispanics <- subset(anes, anes$race == 5)
Hispanic_vote<- sum(Hispanics$post_vote == 2) / (sum(Hispanics$post_vote == 1) + sum(Hispanics$post_vote == 2))


votes <- c(fem_vote, liberal_vote, union_vote, poor_vote, conservative_vote, 
           lgb_vote, muslim_vote, chris_vote, rich_vote, AA_vote, Hispanic_vote, 
           Black_vote, white_vote, fund_vote, jewish_vote, police_vote)

all_group_dat <- cbind(groups, votes, betas, group_ideo$Ideology)
all_group_dat <- as.data.frame(all_group_dat)
names(all_group_dat) <- c("Group", "Percent Vote Rep", "Beta Hat", "Ideology")

#### writing into csv
write.csv(all_group_dat, "data/group_dat_2016.csv", row.names = FALSE)


