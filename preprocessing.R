library(rpart)

followings <- read.csv("C:/Users/MEHRINISO/Desktop/AraProje/DataSet/followings.csv")
comments <- read.csv("C:/Users/MEHRINISO/Desktop/AraProje/DataSet/understanding-language-4_comments.csv")
step_activity <- read.csv("C:/Users/MEHRINISO/Desktop/AraProje/DataSet/understanding-language-4_step-activity.csv")
enrolments <- read.csv("C:/Users/MEHRINISO/Desktop/AraProje/DataSet/understanding-language-4_enrolments.csv")
preprocessed<-enrolments[1]
colnames(preprocessed)[1]<-"learner_id"


#country type
not_official<-c("UA","JO","NL","SA","IT","RO","BR","KW","UZ","PL","JP","AM","MX","RU",
                                       "EG","ES","CO","PS","PE","AR","FR","BA","SK","PT","DE","BE","TR","QA",
                                       "IQ","VN","GE","AZ","EC","SY","SE","LA","AE","MM","GR","CN","TH","MK",
                                       "KZ","HR","DZ","KR","CZ","TJ","ID","SV","FI","AT","BY","AF","UY","MD",
                                       "LB","CL","PA","HU","OM","MN","MA","BG","RS","SI","TW","CH","DJ","IR",
                                       "NP","AN","SR","LY","VE","MZ","TL","MV","CY","IS","BT","TN","PY","BJ",
                                       "BO","ML","LT","LV","XK","CR","EE","ME","CD","TD","SN","LU","YE","DO",
                                       "HT","KG","AL","BF","MR","BH","DK","CI","KH","NO","SO","GN","HN","NI")
official_not_primary<-c("MY", "KE", "PK", "PH", "CM", "BW", "LK", "BD", "TZ", "SG", "IN", "NA", "ZA", "RW", "FJ", "BN", "MW", "SD", "IL", "ET", "ZW", "SS", "UG", "ZM", "PG", "SL", "MT", "LS", "GM")
Offical_and_primary<-c("US", "GB", "AU", "CW", "CA",  "GH", "NZ", "NG" , "IE", "JE", "TT", "TC", "HK", "GG", "BB", "PR", "JM", "MU")

enrolments[which(enrolments$country %in% not_official),13] <- "not_official"
enrolments[which(enrolments$country %in% official_not_primary),13] <- "official_not_primary"
enrolments[which(enrolments$country %in% Offical_and_primary),13] <- "official_primary"
enrolments[which(is.na(enrolments[,13])),13] <- "unknown"

colnames(enrolments)[13] <- "country_type"
preprocessed[2]<-enrolments[13]


#employment status
not_working<-c("not_working","retired","unemployed")
working<-c("full_time_student","looking_for_work","self_employed","working_full_time","working_part_time")

enrolments[which(enrolments$employment_status %in% not_working),14]<-"not_working"
enrolments[which(enrolments$employment_status %in% working),14]<-"working"
enrolments[which(is.na(enrolments[,14])),14] <- "unknown"

colnames(enrolments)[14]<-"employment_category"
preprocessed[3]<-enrolments[14]



#age range
child<-c("<18")
adult<-c("18-25","26-35","36-45")
old<-c("46-55","56-65",">65")

enrolments[which(enrolments$age_range %in% child),15]<-"child"
enrolments[which(enrolments$age_range %in% adult),15]<-"adult"
enrolments[which(enrolments$age_range %in% old),15]<-"old"
enrolments[which(is.na(enrolments[,15])),15] <- "unknown"

colnames(enrolments)[15]<-"age_category"
preprocessed[4]<-enrolments[15]



#working field
education<-c("teaching_and_education")
other_fields<-c("accountancy_banking_and_finance","armed_forces_and_emergency_services","business_consulting_and_management","charities_and_voluntary_work","creative_arts_and_culture","energy_and_utilities","engineering_and_manufacturing","environment_and_agriculture","health_and_social_care","hospitality_tourism_and_sport","it_and_information_services","law","marketing_advertising_and_pr",
                "media_and_publishing","property_and_construction","public_sector","recruitment_and_pr","retail_and_sales",
                "science_and_pharmaceuticals","transport_and_logistics")
enrolments[which(enrolments$employment_area %in% education),16]<-"education"
enrolments[which(enrolments$employment_area %in% other_fields),16]<-"other_fields"
enrolments[which(is.na(enrolments[,16])),16] <- "unknown"

colnames(enrolments)[16]<-"working_field"
preprocessed[5]<-enrolments[16]

#education level
less_educated<-c("apprenticeship","less_than_secondary")
middle_educated<-c("secondary","tertiary")
high_educated<-c("university_degree","university_doctorate","university_masters","professional")

enrolments[which(enrolments$highest_education_level %in% less_educated),17]<-"less_educated"
enrolments[which(enrolments$highest_education_level %in% middle_educated),17]<-"middle_educated"
enrolments[which(enrolments$highest_education_level %in% high_educated),17]<-"high_educated"
enrolments[which(is.na(enrolments[,17])),17] <- "unknown"


colnames(enrolments)[17]<-"education_category"
preprocessed[6]<-enrolments[17]

#gender
preprocessed[7]<-enrolments[7]
colnames(preprocessed)[7]<-"gender"



#performance
enrolments[which((enrolments$unenrolled_at=="") &
                (enrolments$fully_participated_at=="") &
                (enrolments$purchased_statement_at=="")),18]<-"continue"
enrolments[which((enrolments$unenrolled_at=="") &
                  (enrolments$fully_participated_at!="") &
                  (enrolments$purchased_statement_at=="")),18]<-"completed"
enrolments[which(enrolments$unenrolled_at!=""),18]<-"left"
enrolments[which(enrolments$purchased_statement_at!=""),18]<-"certified"


colnames(enrolments)[18]<-"performance"
preprocessed[8]<-enrolments[18]



#number of likes
liketable<- aggregate(comments$likes, by=list(author_id=comments$author_id),FUN=sum)
preprocessed<-merge(preprocessed, liketable, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[9]<-"likeCount"

#number of comments
commenttable <- aggregate(text ~ author_id , data=comments, FUN=length) 
preprocessed<-merge(preprocessed, commenttable, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[10]<-"commentCount"



#started step activities

startedtable <- aggregate(step_activity$first_visited_at, by=list(author_id=step_activity$learner_id),FUN=length)
preprocessed<-merge(preprocessed, startedtable, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[11]<-"started_stepCount"

#completed step activities

completedtable<-subset(step_activity,last_completed_at!="")
completed <- aggregate(completedtable$last_completed_at, by=list(author_id=completedtable$learner_id),FUN=length)
preprocessed<-merge(preprocessed, completed, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[12]<-"completed_stepCount"  



#started step for each week

week_1<-subset(step_activity,week_number==1)
step_count_1<-aggregate(week_1$first_visited_at, by=list(author_id=week_1$learner_id),FUN=length)
preprocessed<-merge(preprocessed, step_count_1, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[13]<-"week1stepCount"


week_2<-subset(step_activity,week_number==2)
step_count_2<-aggregate(week_2$first_visited_at, by=list(author_id=week_2$learner_id),FUN=length)
preprocessed<-merge(preprocessed, step_count_2, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[14]<-"week2stepCount"


week_3<-subset(step_activity,week_number==3)
step_count_3<-aggregate(week_3$first_visited_at, by=list(author_id=week_3$learner_id),FUN=length)
preprocessed<-merge(preprocessed, step_count_3, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[15]<-"week3stepCount"


week_4<-subset(step_activity,week_number==4)
step_count_4<-aggregate(week_4$first_visited_at, by=list(author_id=week_4$learner_id),FUN=length)
preprocessed<-merge(preprocessed, step_count_4, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[16]<-"week4stepCount"






#completed step for each week

w_1=subset(week_1,last_completed_at!="")
completed_1<-aggregate(last_completed_at ~ learner_id , data=w_1, FUN=length)
preprocessed<-merge(preprocessed, completed_1, by.x="learner_id",
                    by.y="learner_id",
                    all.x=TRUE)
colnames(preprocessed)[17]<-"week1completeCount"

w_2=subset(week_2,last_completed_at!="")
completed_2<-aggregate(w_2$last_completed_at, by=list(author_id=w_2$learner_id),FUN=length)
preprocessed<-merge(preprocessed, completed_2, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[18]<-"week2completeCount"

w_3=subset(week_3,last_completed_at!="")
completed_3<-aggregate(w_3$last_completed_at, by=list(author_id=w_3$learner_id),FUN=length)
preprocessed<-merge(preprocessed, completed_3, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[19]<-"week3completeCount"

w_4=subset(week_4,last_completed_at!="")
completed_4<-aggregate(w_4$last_completed_at, by=list(author_id=w_4$learner_id),FUN=length)
preprocessed<-merge(preprocessed, completed_4, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[20]<-"week4completeCount"



#comments for each week
comment_week1<-subset(comments,week_number==1)
comment_week2<-subset(comments,week_number==2)
comment_week3<-subset(comments,week_number==3)
comment_week4<-subset(comments,week_number==4)
comment_1<- aggregate(text ~ author_id , data=comment_week1, FUN=length)
preprocessed<-merge(preprocessed, comment_1, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[21]<-"comment_week1"

like_1 <- aggregate(comment_week1$likes, by=list(author_id=comment_week1$author_id),FUN=sum)
preprocessed<-merge(preprocessed, like_1, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[22]<-"like_week1"



comment_2 <- aggregate(text ~ author_id , data=comment_week2, FUN=length)
preprocessed<-merge(preprocessed, comment_2, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[23]<-"comment_week2"

like_2 <- aggregate(comment_week2$likes, by=list(author_id=comment_week2$author_id),FUN=sum)
preprocessed<-merge(preprocessed, like_2, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[24]<-"like_week2"

comment_3 <- aggregate(text ~ author_id , data=comment_week3, FUN=length)
preprocessed<-merge(preprocessed, comment_3, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[25]<-"comment_week3"

like_3 <- aggregate(comment_week3$likes, by=list(author_id=comment_week3$author_id),FUN=sum)
preprocessed<-merge(preprocessed, like_3, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[26]<-"like_week3"

comment_4 <- aggregate(text ~ author_id , data=comment_week4, FUN=length)
preprocessed<-merge(preprocessed, comment_4, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[27]<-"comment_week4"

like_4 <- aggregate(comment_week4$likes, by=list(author_id=comment_week4$author_id),FUN=sum)
preprocessed<-merge(preprocessed, like_4, by.x="learner_id",
                    by.y="author_id",
                    all.x=TRUE)
colnames(preprocessed)[28]<-"like_week4"



library(dplyr)
#replace NA's
for(i in 9:28)
  preprocessed[which(is.na(preprocessed[,i])),i] <- 0




for(i in 1:nrow(preprocessed))
  preprocessed$following_user[i] <- length(which(preprocessed$learner_id[i]== followings$follower_user_id))

for(i in 1:nrow(preprocessed))
  preprocessed$followed_user[i] <- length(which(preprocessed$learner_id[i]== followings$followed_user_id))

for(i in 1:nrow(preprocessed))
  preprocessed$followed_follower_user[i] <- length(which(preprocessed$learner_id[i]== followings$followed_user_id & preprocessed$learner_id[i]== followings$follower_user_id))



preprocessed[sapply(preprocessed, is.character)] <- lapply(preprocessed[sapply(preprocessed, is.character)], as.factor)

write.csv(preprocessed, file = "preprocessed_data.csv")





