## Long gaps paper
## Emma Templeton

# Code to reproduce all analyses, in order that they are reported

install.packages(c('lme4','lmerTest','lsr'))

library(lme4)
library(lmerTest)
library(lsr)

setwd("/Users/Emma/Dropbox/Manuscripts/long_gaps_repo")

#################### STUDY 1 ###########################

# ______________________________________________________
# Frequency of long gaps by conversation type

data_friends <- read.csv("./Analyses/turn_taking_friends.csv")
data_strangers <- read.csv("./Analyses/turn_taking_strangers.csv")

t.test(data_friends$mean_gap_convo, data_strangers$mean_gap_convo, var.equal = FALSE)
cohensD(data_friends$mean_gap_convo, data_strangers$mean_gap_convo, method = 'unequal')

t.test(data_friends$median_gap_convo, data_strangers$median_gap_convo, var.equal = FALSE)
cohensD(data_friends$median_gap_convo, data_strangers$median_gap_convo, method = 'unequal')

# chi-square test is in the notebook "Study 1 - chi-square test (do friends use more "long gaps"?)"
# in the project's Github repo

# ______________________________________________________
# Reported connection by conversation type

connection_friends <- read.csv("./Analyses/connection_friends.csv")
connection_strangers <- read.csv("./Analyses/connection_strangers.csv")

mean(connection_friends$avg_connection)
sd(connection_friends$avg_connection)

mean(connection_strangers$avg_connection)
sd(connection_strangers$avg_connection)

t.test(connection_friends$avg_connection, connection_strangers$avg_connection, var.equal = FALSE)
cohensD(connection_friends$avg_connection, connection_strangers$avg_connection, method = 'unequal')

# ______________________________________________________
# Long gaps and self-reported connection

# stats for this section is in the notebook "Study 1 - long gaps and self-reported connection"
# in the project's Github repo

#################### STUDY 2 ###########################

# code to compute inter-rater reliability for each variable 
# is in the notebook "Study 2 - analyze long gap rating task"
# in the project's Github repo

# ______________________________________________________
# LONG GAP RATING TASK

data_rating <- read.csv("./Analyses/long_gap_ratings_long_format.csv")
data_info <- read.csv("./Data/long_gap_stimuli_info.csv")
merged <- merge(data_rating, data_info, by=c("video_num"))

merged$video_num <- as.factor(merged$video_num)
merged$rater <- as.factor(merged$rater)
merged$laughter_present <- as.factor(merged$laughter_present)
merged$laughter_who <- as.factor(merged$laughter_who)
merged$gestures_present <- as.factor(merged$gestures_present)
merged$rater_know_person <- as.factor(merged$rater_know_person)

# Difference by condition (friend / stranger)
# (for continous variables)

model_awkward <- lmer(scale(awkward) ~ condition +  
                        (1 | rater), data=merged)
summary(model_awkward)
tapply(merged$awkward, merged$condition, mean)
tapply(merged$awkward, merged$condition, sd)

model_connected <- lmer(scale(connected) ~ condition +  
                          (1 | rater), data=merged)
summary(model_connected)
tapply(merged$connected, merged$condition, mean)
tapply(merged$connected, merged$condition, sd)

model_topics <- lmer(scale(topics) ~ condition +  
                       (1 | rater), data=merged)
summary(model_topics)
tapply(merged$topics, merged$condition, mean)
tapply(merged$topics, merged$condition, sd)

model_genuine_laugh <- lmer(scale(laughter_genuine) ~ condition +  
                              (1 | rater), data=merged)
summary(model_genuine_laugh)
tapply(merged$laughter_genuine, merged$condition, mean, na.rm=TRUE)
tapply(merged$laughter_genuine, merged$condition, sd, na.rm=TRUE)

# Explore Condition x Interval interaction
# (for continuous variables)

model_awkward <- lmer(scale(awkward) ~ condition * scale(interval) +  
                        (1 | rater), data=merged)
summary(model_awkward)

model_connected <- lmer(scale(connected) ~ condition * scale(interval) +  
                          (1 | rater), data=merged)
summary(model_connected)

model_topics <- lmer(scale(topics) ~ condition * scale(interval) +  
                       (1 | rater), data=merged)
summary(model_topics)

model_genuine_laugh <- lmer(scale(laughter_genuine) ~ condition * scale(interval) +  
                              (1 | rater), data=merged)
summary(model_genuine_laugh)

# Differences in counts by condition (friend / stranger)
# (for categorical variables)

data_wide <- read.csv("./Analyses/long_gap_ratings_wide_format.csv")
data_info <- read.csv("./Data/long_gap_stimuli_info.csv")
merged <- merge(data_wide, data_info, by=c("video_num"))

merged$video_num <- as.factor(merged$video_num)
merged$laughter_consensus <- as.factor(merged$laughter_consensus)
merged$laughter_who_consensus <- as.factor(merged$laughter_who_consensus)
merged$laughter_who_consensus_binary <- as.factor(merged$laughter_who_consensus_binary)
merged$gestures_consensus <- as.factor(merged$gestures_consensus)

table(merged$condition, merged$laughter_consensus)
chisq.test(table(merged$condition, merged$laughter_consensus))

table(merged$condition, merged$gestures_consensus)
chisq.test(table(merged$condition, merged$gestures_consensus))

table(merged$condition, merged$laughter_who_consensus_binary)
chisq.test(table(merged$condition, merged$laughter_who_consensus_binary))
chisq.test(table(merged$condition, merged$laughter_who_consensus_binary), simulate.p.value = TRUE)

