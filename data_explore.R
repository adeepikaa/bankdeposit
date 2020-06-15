#############################################
#  Data Exploration
#############################################



# Age spread
bank_data%>%
  filter(deposit=="yes")%>%
  ggplot(aes(x=age))+
  geom_histogram(bins=20, col="black")+
  ggtitle(" Distribution of Deposits across Age")


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

bank_data%>%
  ggplot(aes(x=job,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Distribution of Deposits with Job")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

bank_data%>%
  ggplot(aes(x=contact,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Distribution of Deposits with different types of contact")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

bank_data%>%
  ggplot(aes(x=poutcome,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Distribution of Deposits with previous outcomes")


bank_data%>%
  ggplot(aes(x=deposit, y=balance, col=deposit))+
  geom_boxplot()+
  ggtitle("Distribution of Bank balance")


mon<-bank_data%>%
  ggplot(aes(x=month,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle("Last contact Month")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
day<-bank_data%>%
  ggplot(aes(x=day,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle("Last contact day")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
grid.arrange(mon, day, nrow=1)



c<-bank_data%>%
  ggplot(aes(x=campaign,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle("Campaign contact")

pd<-bank_data%>%
  ggplot(aes(x=pdays,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Days since last contact")+
  xlim(c(0,1000))

prev<-bank_data%>%
  ggplot(aes(x=previous,  group=deposit, fill=deposit))+
  geom_histogram(stat="count", bins=20, col="black", position="dodge")+
  ggtitle(" Contacts before this campaign")

grid.arrange(c, pd, prev, nrow=1)


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

bank_data%>%
  ggplot(aes(x=education,  y=balance))+
  geom_bar(stat="identity")+
  ggtitle(" Balance for different Education")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

bank_data%>%
  ggplot(aes(x=marital,  y=balance))+
  geom_bar(stat="identity")+
  ggtitle(" Balance for Marital Status")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

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

bank_data%>%
  ggplot()+
  geom_boxplot(aes(job, age))+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))


bank_data%>%
  filter(pdays>=0)%>%
  ggplot(aes(x=previous,  y=pdays))+
  geom_point()+
  ggtitle(" Previous contacts Vs Days since")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))

cortable<-cor(bank_data[,c("age", "balance", "duration", "campaign", "pdays", "previous")])
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cortable, col=col, symm=TRUE)

cor(bank_data[,c("previous", "campaign","pdays", "age", "balance")], bank_data$duration)
