#table:condition_assignment
#id:participant_id
#columns:ethnicity,Gender,eligindx,college

#table:essays
#id:id 
#columns: Finished=1

#table:spring2016cumgpa
#id:encrypt
#columns:major,level,gpa

#restrictions: Finished=1, eligindx>0
#restrict to those who finished the intervention and have known eligibility index and GPA > 0

library(sqldf)
library(ggplot2)
library(lsmeans)
essays<-read.csv(file = "data/essays.csv")
spring2016<-read.csv(file="data/spring16raw.csv")
condition<-read.csv(file="data/condition_assignment.csv")
spring2016merged<-sqldf("SELECT c.*,e.*,s.*, SUM(ENRL_UNITS_TAKEN) as total_units 
                        FROM condition c
                        LEFT JOIN essays e ON (e.id= c.participant_id AND e.Finished=1
                        AND e.essay IS NOT '') 
                        LEFT JOIN spring2016 s ON (s.encrypt=c.participant_id 
                        AND (s.gpa > 0 AND s.CTERM_ACAD_LEVEL_BOT_CD IS NOT NULL) )
                        WHERE eligindx > 0 AND ethnicity != 'Unknown' 
                        AND e.id IS NOT NULL
                        AND s.gpa IS NOT NULL
                        GROUP BY s.encrypt")




spring2016merged$fem[spring2016merged$Gender == "M"] <- 0
spring2016merged$fem[spring2016merged$Gender == "F"] <- 1

spring2016merged$male[spring2016merged$Gender == "M"] <- 1
spring2016merged$male[spring2016merged$Gender == "F"] <- 0

#Pacific Islander classified as URM
spring2016merged$ethn2[spring2016merged$ethnicity == "White"] <-  0
spring2016merged$ethn2[spring2016merged$ethnicity == "Asian/Pacific Islander"] <-  1
spring2016merged$ethn2[spring2016merged$ethnicity == "Black" | 
                         spring2016merged$ethnicity == "Hispanic" |
                         spring2016merged$ethnicity == "Pacific Islander" |
                         spring2016merged$ethnicity == "American Indian/Alaskan Native"] <-  2


#Indicator coding of race/ethnicity white is comparison
spring2016merged$white_urm[spring2016merged$ethnicity == "White"] <-  0
spring2016merged$white_urm[spring2016merged$ethnicity == "Asian/Pacific Islander"] <-  0
spring2016merged$white_urm[spring2016merged$ethnicity == "Black" | 
                             spring2016merged$ethnicity == "Hispanic" |
                             spring2016merged$ethnicity == "Pacific Islander" |
                             spring2016merged$ethnicity == "American Indian/Alaskan Native"] <-  1

spring2016merged$white_asian[spring2016merged$ethnicity == "White"] <-  0
spring2016merged$white_asian[spring2016merged$ethnicity == "Asian/Pacific Islander"] <-  1
spring2016merged$white_asian[spring2016merged$ethnicity == "Black" | 
                               spring2016merged$ethnicity == "Hispanic" |
                               spring2016merged$ethnicity == "Pacific Islander" |
                               spring2016merged$ethnicity == "American Indian/Alaskan Native"] <-  0

#urm is comparison
spring2016merged$urm_white[spring2016merged$ethnicity == "White"] <-  1
spring2016merged$urm_white[spring2016merged$ethnicity == "Asian/Pacific Islander"] <-  0
spring2016merged$urm_white[spring2016merged$ethnicity == "Black" | 
                             spring2016merged$ethnicity == "Hispanic" |
                             spring2016merged$ethnicity == "Pacific Islander" |
                             spring2016merged$ethnicity == "American Indian/Alaskan Native"] <-  0

spring2016merged$urm_asian[spring2016merged$ethnicity == "White"] <-  0
spring2016merged$urm_asian[spring2016merged$ethnicity == "Asian/Pacific Islander"] <-  1
spring2016merged$urm_asian[spring2016merged$ethnicity == "Black" | 
                             spring2016merged$ethnicity == "Hispanic" |
                             spring2016merged$ethnicity == "Pacific Islander" |
                             spring2016merged$ethnicity == "American Indian/Alaskan Native"] <-  0


spring2016merged$ethn2<-as.factor(spring2016merged$ethn2)
spring2016merged$cond<-as.factor(spring2016merged$cond)
spring2016merged$fem<-as.factor(spring2016merged$fem)



#Contrast control_treat (mean of control vs mean of both treatment)
spring2016merged$control_treat[spring2016merged$condition == 1] <- -2
spring2016merged$control_treat[spring2016merged$condition == 2] <- 1
spring2016merged$control_treat[spring2016merged$condition == 3] <- 1

# mean of transfer vs. mean of struggle
spring2016merged$struggle_transfer[spring2016merged$condition == 1] <- 0
spring2016merged$struggle_transfer[spring2016merged$condition == 2] <- -1
spring2016merged$struggle_transfer[spring2016merged$condition == 3] <- 1


#standardize eligibility index
spring2016merged$zelig=scale(spring2016merged$eligindx, center=TRUE, scale=TRUE)
spring2016merged$zeliglow=spring2016merged$zelig-1
spring2016merged$zelighigh=spring2016merged$zelig+1

spring2016merged$zgpa=scale(spring2016merged$gpa,center=TRUE,scale=TRUE)
spring2016merged$ztotal_units=scale(spring2016merged$total_units,center=TRUE,scale=TRUE)

p2<-ggplot() +    geom_jitter(data=spring2016merged, aes(x=1, y=gpa, color=cond))+facet_wrap(~ ethn2+fem) 
  geom_jitter(data=spring2016merged, aes(x=1, y=gpa, color=cond))+facet_wrap(~ ethn2+fem) +geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
ggsave(filename="gpa_scatter.jpg",plot=p2)
cols<-c("gpa","confident")

for (i in 1:length(cols)) {
  model<- spring2016merged[[cols[[i]]]] ~ struggle_transfer * white_urm * fem * zelig + struggle_transfer * white_asian * fem * zelig + control_treat * white_asian * fem * zelig + control_treat * white_urm * fem * zelig 
  reg<-lm(model,data=spring2016merged)
  fname<-trimws(cols[[i]],which="both")
  write.csv(summary(reg)[4],file = paste0(fname,".csv"))
  
  #graph predicted means by gender,ethnicity,eligbility index
  model2<-spring2016merged[[cols[[i]]]] ~fem*ethn2*cond*eligindx
  graph.lm1 <- lm(model2, data = spring2016merged)
  graph.lsm <- lsmeans(graph.lm1, list(~ fem*ethn2*cond*eligindx,~fem*ethn2), 
                       at = list(eligindx=mean(spring2016merged$eligindx,na.rm=TRUE)-sd(spring2016merged$eligindx,na.rm=TRUE)))
  df<-as.data.frame(summary(graph.lsm)) # put predicted means into a data frame
  names(df)[1]<-"fem"
  names(df)[2]<-"ethn2"
  names(df)[3]<-"cond"
  names(df)[4]<-"eligindx"
  names(df)[5]<-"lsmean"
  names(df)[6]<-"SE"
  names(df)[7]<-"df"
  names(df)[8]<-"lower.CL"
  names(df)[9]<-"upper.CL"
  
  
  spring2016merged$ethn2<-as.factor(spring2016merged$ethn2)
  spring2016merged$cond<-as.factor(spring2016merged$cond)
  spring2016merged$fem<-as.factor(spring2016merged$fem)
  spring2016merged$male<-as.factor(spring2016merged$male)
  
  title <- paste0("Predicted Means: ",cols[[i]])
  yaxis <- paste0("Predicted Mean: ",cols[[i]])
  
  df$ethn2<-factor(df$ethn2, levels=c(0,1,2), labels=c("Whites","Asians", "URMs"))
  df$cond<-factor(df$cond, labels = c("Control","Transfer","Struggle"))
  df$fem<-factor(df$fem, labels = c("Men","Women"))
  p<-ggplot(df, aes(x=fem, y=lsmean, fill=cond)) + 
    geom_bar(stat='identity', position = position_dodge(0.9)) + 
    geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), position = position_dodge(0.9), width = 0.25) +
    labs(x="Gender", y=yaxis) +
    ggtitle(title)+
    expand_limits(y=4) +     facet_wrap(~ ethn2)
  ggsave(filename=paste(fname,".jpg"), plot=p)
  
}

