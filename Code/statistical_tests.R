## Paper Title: Long gaps between turns are awkward for strangers but not for friends
## Author of this script: Emma Templeton

# Code to reproduce all analyses, in order that they are reported

install.packages(c('lme4','lmerTest','lsr'))
install.packages("lmerTest",dependencies=TRUE)

library(lme4)
library(lmerTest)
library(lsr)

setwd("/Users/Emma/Dropbox/Manuscripts/long_gaps_repo_revision")

#################### STUDY 1 ###########################

# ______________________________________________________
# Count of long gaps by conversation type

data <- read.csv("./Analyses/long_gap_counts.csv")
data$subID <- as.factor(data$subID)
data$partnerID <- as.factor(data$partnerID)
data$condition <- as.factor(data$condition)

# poisson regression approach 
model <- glmer(n_long_2000 ~ condition + log(n_total) + (1 | subID), family="poisson"(link = "log"), data=data)
summary(model) 

# check for overdispersion
rd <- glm(n_long_2000 ~ condition + log(n_total), data = data, family = poisson(link = "log"))
dispersiontest(rd)

# check zero-inflation
m <- glmer(n_long_2000 ~ condition + log(n_total) + (1 | subID), family = poisson(link = "log"), data = data)
check_zeroinflation(m)

# zero-inflated negative binomial approach
# (because poisson suffers from overdispersion and zero-inflation)

install.packages(c('glmmTMB'))
library(glmmTMB)

eq <- n_long_2000 ~ condition + offset(log(n_total)) + (1 | subID)
#eq <- n_long_2000 ~ condition + log(n_total) + (1 | subID)
zinbinom2b <- glmmTMB(eq,
                      data=data,
                      ziformula=~1,
                      family=nbinom2)
summary(zinbinom2b)

# SUPPLEMENTAL ANALYSIS

# How do different thresholds impact results?
# thresholds tested (ms): 500, 750, 1000, 1250, 1500, 1750, 2250, 2500, 2750, 3000 
# All cutoffs provide the same result: friends have more long gaps than strangers

# poisson regression approach 
model_supp <- glmer(n_long_3000 ~ condition + log(n_total) + (1 | subID), family="poisson"(link = "log"), data=data)
summary(model_supp)

# zero-inflated negative binomial approach
eq_supp <- n_long_500 ~ condition + offset(log(n_total)) + (1 | subID)
#eq_supp <- n_long_500 ~ condition + log(n_total) + (1 | subID)
zinbinom2b_supp <- glmmTMB(eq_supp,
                      data=data,
                      ziformula=~1,
                      family=nbinom2)
summary(zinbinom2b_supp)

# ______________________________________________________
# Long gaps and overall enjoyment (strangers)

data_counts <- read.csv("./Analyses/long_gap_counts.csv") 
data_enjoyment <- read.csv("./Data/post_convo_survey/strangers/data_common_questions.csv")
merged <- merge(data_counts, data_enjoyment, by=c("subID", "partnerID"))

merged$subID <- as.factor(merged$subID)
merged$partnerID <- as.factor(merged$partnerID)
merged$convo <- as.factor(merged$convo)

model <- lmer(convo_enjoy ~ n_long_2000 + n_total + (1 | subID) + (1 | convo), data=merged)
summary(model) 

# ______________________________________________________
# Reported connection by relationship type

connection_friends <- read.csv("./Analyses/connection_friends.csv")

connection_strangers_all <- read.csv("./Analyses/connection_strangers.csv")
strangers_inclusion_criteria <- read.csv("./Analyses/long_gap_counts.csv")
connection_strangers <- merge(connection_strangers_all, strangers_inclusion_criteria, by=c("subID", "partnerID"))

mean(connection_friends$avg_connection)
sd(connection_friends$avg_connection)

mean(connection_strangers$avg_connection)
sd(connection_strangers$avg_connection)

t.test(connection_friends$avg_connection, connection_strangers$avg_connection, var.equal = FALSE)
cohensD(connection_friends$avg_connection, connection_strangers$avg_connection, method = 'unequal')

# ______________________________________________________
# Changes in connection when entering and exiting long gaps

# First, examine strangers and friends separately
stacked <- read.csv("./Analyses/long_gap_connection_all_long_format_strangers.csv")
stacked <- read.csv("./Analyses/long_gap_connection_all_long_format_friends.csv")

stacked$subID <- as.factor(stacked$subID)
stacked$partnerID <- as.factor(stacked$partnerID)
stacked$dyad <- as.factor(stacked$dyad)

three_before <- stacked[ which(stacked$timepoint=='3_before'), ]
two_before <- stacked[ which(stacked$timepoint=='2_before'), ]
one_before <- stacked[ which(stacked$timepoint=='1_before'), ]
one_after <- stacked[ which(stacked$timepoint=='1_after'), ]
two_after <- stacked[ which(stacked$timepoint=='2_after'), ]
three_after <- stacked[ which(stacked$timepoint=='3_after'), ]

model <- lmer(connection_change ~ 1 +  
                (1 | subID), data=three_before)
summary(model)

model <- lmer(connection_change ~ 1 +  
                (1 | subID), data=two_before)
summary(model)

model <- lmer(connection_change ~ 1 +  
                (1 | subID), data=one_before)
summary(model) # entering a long gap

model <- lmer(connection_change ~ 1 +  
                (1 | subID), data=one_after)
summary(model) # exiting a long gap

model <- lmer(connection_change ~ 1 +  
                (1 | subID), data=two_after)
summary(model)

model <- lmer(connection_change ~ 1 +  
                (1 | subID), data=three_after)
summary(model)

# Next, examine group differences
stacked <- read.csv("./Analyses/long_gap_connection_all_long_format.csv")

stacked$subID <- as.factor(stacked$subID)
stacked$partnerID <- as.factor(stacked$partnerID)
stacked$dyad <- as.factor(stacked$dyad)
stacked$condition <- as.factor(stacked$condition)

three_before <- stacked[ which(stacked$timepoint=='3_before'), ]
two_before <- stacked[ which(stacked$timepoint=='2_before'), ]
one_before <- stacked[ which(stacked$timepoint=='1_before'), ]
one_after <- stacked[ which(stacked$timepoint=='1_after'), ]
two_after <- stacked[ which(stacked$timepoint=='2_after'), ]
three_after <- stacked[ which(stacked$timepoint=='3_after'), ]

model <- lmer(connection_change ~ condition +  
                (1 | subID), data=three_before)
summary(model)

model <- lmer(connection_change ~ condition +  
                (1 | subID), data=two_before)
summary(model)

model <- lmer(connection_change ~ condition +  
                (1 | subID), data=one_before)
summary(model) # entering a long gap

model <- lmer(connection_change ~ condition +  
                (1 | subID), data=one_after)
summary(model) # exiting a long gap

model <- lmer(connection_change ~ condition +  
                (1 | subID), data=two_after)
summary(model)

model <- lmer(connection_change ~ condition +  
                (1 | subID), data=three_after)
summary(model)

# ______________________________________________________
# Changes in connection when entering and exiting long gaps: Continuous Gap Length
# Answers the question: Does connection change more as long gaps get *longer*?

# First, examine strangers and friends separately
stacked <- read.csv("./Analyses/long_gap_connection_all_long_format_strangers.csv")
stacked <- read.csv("./Analyses/long_gap_connection_all_long_format_friends.csv")

stacked$subID <- as.factor(stacked$subID)
stacked$partnerID <- as.factor(stacked$partnerID)
stacked$dyad <- as.factor(stacked$dyad)

three_before <- stacked[ which(stacked$timepoint=='3_before'), ]
two_before <- stacked[ which(stacked$timepoint=='2_before'), ]
one_before <- stacked[ which(stacked$timepoint=='1_before'), ]
one_after <- stacked[ which(stacked$timepoint=='1_after'), ]
two_after <- stacked[ which(stacked$timepoint=='2_after'), ]
three_after <- stacked[ which(stacked$timepoint=='3_after'), ]

model <- lmer(connection_change ~ log(gap_length) +  
                (1 | subID), data=three_before)
summary(model)

model <- lmer(connection_change ~ log(gap_length) +  
                (1 | subID), data=two_before)
summary(model)

model <- lmer(connection_change ~ log(gap_length) +  
                (1 | subID), data=one_before)
summary(model) # entering a long gap

model <- lmer(connection_change ~ log(gap_length) +  
                (1 | subID), data=one_after)
summary(model) # exiting a long gap

model <- lmer(connection_change ~ log(gap_length) +  
                (1 | subID), data=two_after)
summary(model)

model <- lmer(connection_change ~ log(gap_length) +  
                (1 | subID), data=three_after)
summary(model)

# Next, examine group differences
stacked <- read.csv("./Analyses/long_gap_connection_all_long_format.csv")

stacked$subID <- as.factor(stacked$subID)
stacked$partnerID <- as.factor(stacked$partnerID)
stacked$dyad <- as.factor(stacked$dyad)
stacked$condition <- as.factor(stacked$condition)

three_before <- stacked[ which(stacked$timepoint=='3_before'), ]
two_before <- stacked[ which(stacked$timepoint=='2_before'), ]
one_before <- stacked[ which(stacked$timepoint=='1_before'), ]
one_after <- stacked[ which(stacked$timepoint=='1_after'), ]
two_after <- stacked[ which(stacked$timepoint=='2_after'), ]
three_after <- stacked[ which(stacked$timepoint=='3_after'), ]

model <- lmer(connection_change ~ log(gap_length) * condition +  
                (1 | subID), data=three_before)
summary(model)

model <- lmer(connection_change ~ log(gap_length) * condition +  
                (1 | subID), data=two_before)
summary(model)

model <- lmer(connection_change ~ log(gap_length) * condition +  
                (1 | subID), data=one_before)
summary(model) # entering a long gap

model <- lmer(connection_change ~ log(gap_length) * condition +  
                (1 | subID), data=one_after)
summary(model) # exiting a long gap

model <- lmer(connection_change ~ log(gap_length) * condition +  
                (1 | subID), data=two_after)
summary(model)

model <- lmer(connection_change ~ log(gap_length) * condition +  
                (1 | subID), data=three_after)
summary(model)

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
# (for continuous variables)

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

####################################################
#SUPPLEMENTAL ANALYSES

# ______________________________________________________
# Laughter as a mediator?

# STRANGERS ONLY
stacked <- read.csv("./Analyses/long_gap_connection_all_long_format_laughter_strangers.csv")
stacked$subID <- as.factor(stacked$subID)
stacked$partnerID <- as.factor(stacked$partnerID)

three_before <- stacked[ which(stacked$timepoint=='3_before'), ]
two_before <- stacked[ which(stacked$timepoint=='2_before'), ]
one_before <- stacked[ which(stacked$timepoint=='1_before'), ]
one_after <- stacked[ which(stacked$timepoint=='1_after'), ]
two_after <- stacked[ which(stacked$timepoint=='2_after'), ]
three_after <- stacked[ which(stacked$timepoint=='3_after'), ]

model <- lmer(connection_change ~ laughter +  
                (1 | subID), data=three_before)
summary(model)

model <- lmer(connection_change ~ laughter +  
                (1 | subID), data=two_before)
summary(model)

model <- lmer(connection_change ~ laughter +  
                (1 | subID), data=one_before)
summary(model) # entering a long gap

model <- lmer(connection_change ~ laughter +  
                (1 | subID), data=one_after)
summary(model) # exiting a long gap

model <- lmer(connection_change ~ laughter +  
                (1 | subID), data=two_after)
summary(model)

model <- lmer(connection_change ~ laughter +  
                (1 | subID), data=three_after)
summary(model)

# FRIENDS ONLY
stacked <- read.csv("./Analyses/long_gap_connection_all_long_format_laughter_friends.csv")
stacked$subID <- as.factor(stacked$subID)
stacked$partnerID <- as.factor(stacked$partnerID)

three_before <- stacked[ which(stacked$timepoint=='3_before'), ]
two_before <- stacked[ which(stacked$timepoint=='2_before'), ]
one_before <- stacked[ which(stacked$timepoint=='1_before'), ]
one_after <- stacked[ which(stacked$timepoint=='1_after'), ]
two_after <- stacked[ which(stacked$timepoint=='2_after'), ]
three_after <- stacked[ which(stacked$timepoint=='3_after'), ]

model <- lmer(connection_change ~ laughter +  
                (1 | subID), data=three_before)
summary(model)

model <- lmer(connection_change ~ laughter +  
                (1 | subID), data=two_before)
summary(model)

model <- lmer(connection_change ~ laughter +  
                (1 | subID), data=one_before)
summary(model) # entering a long gap

model <- lmer(connection_change ~ laughter +  
                (1 | subID), data=one_after)
summary(model) # exiting a long gap

model <- lmer(connection_change ~ laughter +  
                (1 | subID), data=two_after)
summary(model)

model <- lmer(connection_change ~ laughter +  
                (1 | subID), data=three_after)
summary(model)

# BOTH
stacked <- read.csv("./Analyses/long_gap_connection_all_long_format_laughter.csv")
stacked$subID <- as.factor(stacked$subID)
stacked$partnerID <- as.factor(stacked$partnerID)
stacked$laughter <- as.factor(stacked$laughter)

stacked$laughter<-ifelse(stacked$laughter=="yes",1,0)

three_before <- stacked[ which(stacked$timepoint=='3_before'), ]
two_before <- stacked[ which(stacked$timepoint=='2_before'), ]
one_before <- stacked[ which(stacked$timepoint=='1_before'), ]
one_after <- stacked[ which(stacked$timepoint=='1_after'), ]
two_after <- stacked[ which(stacked$timepoint=='2_after'), ]
three_after <- stacked[ which(stacked$timepoint=='3_after'), ]

model <- lmer(connection_change ~ condition_x * laughter +  
                (1 | subID) + (1 | dyad), data=three_before)
summary(model)

model <- lmer(connection_change ~ condition_x * laughter +  
                (1 | subID) + (1 | dyad), data=two_before)
summary(model)

model <- lmer(connection_change ~ condition_x * laughter +  
                (1 | subID), data=one_before)
summary(model) # entering a long gap

model <- lmer(connection_change ~ condition_x * laughter +  
                (1 | subID), data=one_after)
summary(model) # entering a long gap

model <- lmer(connection_change ~ condition_x * laughter +  
                (1 | subID) + (1 | dyad), data=two_after)
summary(model)

model <- lmer(connection_change ~ condition_x * laughter +  
                (1 | subID) + (1 | dyad), data=three_after)
summary(model)

# Test for mediation effects!
# Step 1: condition --> change in connection 
fit.totaleffect <- lmer(connection_change ~ condition_x +  
                          (1 | subID), data=one_before)
summary(fit.totaleffect)

# Step 2: condition --> laughter
fit.mediator <- glmer(laughter ~ condition_x + (1 | subID),
                      data = one_before,
                      family = binomial)
summary(fit.mediator)

# Step 3: condition + laughter --> change in connection
fit.dv <- lmer(connection_change ~ condition_x + laughter + 
                 (1 | subID), data=one_before)
summary(fit.dv)

# Step 4
install.packages("mediation") 
library(mediation)

results = mediate(fit.mediator, fit.dv, treat = "condition_x", mediator = "laughter", 
                  control.value = "strangers", treat.value = "friends")
summary(results)

# ______________________________________________________
# Semantic Content (question marks and word counts)

data <- read.csv("./Analyses/semantic_content_all.csv")

data$convo <- as.factor(data$convo)
data$position <- as.factor(data$position)
data$question <- as.factor(data$question)
data$condition <- as.factor(data$condition)
data$gap <- as.factor(data$gap)

strangers <- data[ which(data$condition=='strangers'), ]
friends <- data[ which(data$condition=='friends'), ]

# questions
model_1 <- glm(question ~ gap * position * condition,
               data = data,
               family = binomial)
summary(model_1)

model_1 <- glmer(question ~ gap * position + (1 | convo),
                 data = strangers,
                 family = binomial)
summary(model_1) 

model_1 <- glmer(question ~ gap * position + (1 | convo),
                 data = friends,
                 family = binomial)
summary(model_1) 

# word count
model_1 <- lmer(word_count ~ position * gap * condition + (1 | convo),
                data = data)
summary(model_1)

model_1 <- lmer(word_count ~ position * gap + (1 | convo),
                data = strangers)
summary(model_1)

model_1 <- lmer(word_count ~ position * gap + (1 | convo),
                data = friends)
summary(model_1)
