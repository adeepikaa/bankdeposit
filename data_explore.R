#############################################
#  Data Exploration
#############################################


# Exploration of the variable "Age"

median(bank_data$age)

min(bank_data$age)

max(bank_data$age)

dist<-bank_data%>%
  group_by(age)%>%
  summarize(n=n())%>%
  ggplot(aes(x=age, y=n))+
  geom_line()+
  ggtitle(" Distribution of Age")

dist_dep<-bank_data%>%
  group_by(age)%>%
  summarize(n=n(), deposit_pct=sum(deposit=="yes")/n, .groups="drop")%>%
  ggplot(aes(x=age, y=deposit_pct))+
  geom_bar(stat="identity")+
  ggtitle(" Distribution of Deposits across Age")

grid.arrange(dist, dist_dep, ncol=1)

###########################################

# Explore Education and Marital Status 

e<-bank_data%>%
  ggplot(aes(x=education,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Distribution of Deposits across Education")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

m<-bank_data%>%
  ggplot(aes(x=marital,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Distribution of Deposits across Marital status")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

grid.arrange(e,m, nrow=1)

###########################################

# Explore Job

bank_data%>%
  ggplot(aes(x=job,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Distribution of Deposits with Job")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
bank_data%>%
  ggplot()+
  geom_boxplot(aes(job, age))+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

###########################################

# Explore house loan, personal loan and default

h<-bank_data%>%
  ggplot(aes(x=housing,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" House loans")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

l<-bank_data%>%
  ggplot(aes(x=loan,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Personal loans")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
d<-bank_data%>%
  ggplot(aes(x=default,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Credit Default")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
grid.arrange(h,l,d, nrow=1)

###########################################
# Explore contact type

bank_data%>%
  ggplot(aes(x=contact,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Distribution of Deposits with different types of contact")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
###########################################

# Explore this campaign contacts

mon<-bank_data%>%
  ggplot(aes(x=month,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle("Last contact Month for current campaign")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
day<-bank_data%>%
  ggplot(aes(x=day,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle("Last contact day for current campaign")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
grid.arrange(mon, day, nrow=1)

bank_data%>%
  ggplot(aes(x=campaign,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle("Number of contacts for current campaign")

###########################################
# Explore previous campaign contacts

bank_data%>%
  ggplot(aes(x=poutcome,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Distribution of Deposits with previous outcomes")


bank_data%>%
  ggplot(aes(x=pdays,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Days since last contact")+
  xlim(c(0,400))

bank_data%>%
  ggplot(aes(x=previous,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Contacts before this campaign")




###########################################
# Explore bank balance
###########################################

median(bank_data$balance)

min(bank_data$balance)

max(bank_data$balance)

bal1<-bank_data%>%
  ggplot(aes(x=deposit, y=balance, col=deposit))+
  geom_boxplot()+
  ggtitle("Distribution of Bank balance")
 
bal2<-bank_data%>%
  ggplot(aes(x=deposit, y=balance, col=deposit))+
  geom_boxplot()+
  ggtitle("Distribution of Bank balance")+
  ylim(0, 2500)

grid.arrange(bal1, bal2, nrow=1)

###################################
# Balance with other predictors.

bank_data%>%
  ggplot(aes(x=age, y=balance))+
  geom_point()+
  ggtitle(" Bank Balance for different ages")

bank_data%>%
  ggplot(aes(x=job,  y=balance))+
  geom_bar(stat="identity")+
  ggtitle(" Balance for different jobs")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

b1<-bank_data%>%
  ggplot(aes(x=education,  y=balance))+
  geom_bar(stat="identity")+
  ggtitle(" Balance for different Education")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

b2<-bank_data%>%
  ggplot(aes(x=marital,  y=balance))+
  geom_bar(stat="identity")+
  ggtitle(" Balance for Marital Status")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
grid.arrange(b1, b2, nrow=1)

h_b<-bank_data%>%
  ggplot(aes(x=housing,  y=balance))+
  geom_bar(stat="identity")+
  ggtitle(" Balance with Housing loan")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

l_b<-bank_data%>%
  ggplot(aes(x=loan,  y=balance))+
  geom_bar(stat="identity")+
  ggtitle(" Balance with Personal loan")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
d_b<-bank_data%>%
  ggplot(aes(x=default,  y=balance))+
  geom_bar(stat="identity")+
  ggtitle(" Balance with Credit Default")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

grid.arrange(h_b, l_b, d_b, nrow=1)



# Correlation of numeric predictors

cortable<-cor(bank_data[,c("age", "balance", "duration", "campaign", "pdays", "previous")])
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cortable, col=col, symm=TRUE)

# Corelation with duration as it is hevaily related to deposit column
cor(bank_data[,c("previous", "campaign","pdays", "age", "balance")], bank_data$duration)
