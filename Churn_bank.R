
# Load packages 
list.of.packages <- c("tidyverse","plotly","readxl","DT","lubridate","leaflet","devtools","GGally","ggthemr","wesanderson","GGally",
                      "magrittr","ggmap","tidygeocoder","devtools","ggpubr","zoo","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 
devtools::install_github('cttobin/ggthemr')
devtools::install_github('talgalili/heatmaply')
devtools::install_github("ricardo-bion/ggradar")
devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
pacman::p_load(e1071, ggplot2, caret, rmarkdown, corrplot,readr,GGally,lubridate,devtools,ggthemr,dplyr,
               corrplot,wesanderson,heatmaply,leaflet,superheat,ggradar,scales,fmsb,radarchart,vcd,pROC
               ,InformationValue,ROCR,randomForest,rpart,rpart.plot,vip)

# Them
ggthemr("dust")

# Load data
churn_df<- read_csv("Churn_Modelling.csv")
str(churn_df)
summary(churn_df)
for (i in 1:14){print(sum(is.na(churn_df[,i])))}

# Preprocessing 

churn_df <- churn_df[,-c(1,2,3)]
churn_df$IsActiveMember <- as.factor(churn_df$IsActiveMember)
churn_df$Exited <- as.factor(churn_df$Exited)
churn_df$HasCrCard <- as.factor(churn_df$HasCrCard )
churn_df$NumOfProducts<- as.factor(churn_df$NumOfProducts)

#Tenure by categorical

churn_df$Tenure_ca<-0
churn_df$Tenure_ca[churn_df$Tenure <=2]<-"Tenure_0-2"
churn_df$Tenure_ca[(churn_df$Tenure >2)&(churn_df$Tenure <= 5)]<-"Tenure_2-5"
churn_df$Tenure_ca[(churn_df$Tenure >5)&(churn_df$Tenure <= 7)]<-"Tenure_5-7"
churn_df$Tenure_ca[(churn_df$Tenure >7)&(churn_df$Tenure <= 10)]<-"Tenure_7-10"



## Explore dataset 
#Dependent variable(Exited)


df <- as.data.frame(churn_df) %>%group_by(Exited) %>% tally() %>%
  arrange(desc(n)) %>%
  mutate('prop' =round(n/sum(n)*100, digits = 1))

ggplot(df,aes(x=Exited,y=n,fill=Exited))+ 
  geom_bar(stat = "identity")+
  geom_text(aes(label =paste0(prop,"%")), vjust = -0.3)+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        legend.position = "none",
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))+
  labs(y='Count',title='Frequency of Chrun')


# Exited by country 
churn_df %>% group_by(Exited,Geography) %>% tally()

df2 <- as.data.frame(churn_df) %>%group_by(Exited,Geography) %>% tally() %>%
  arrange(desc(n)) %>%
  mutate('prop' = unlist(by(data=n, INDICES = Exited, FUN = function(x) round(x/sum(x)*100, digits = 1))))


ggplot(df2,aes(x=Exited,y=n))+ 
  geom_bar(aes(fill=Geography),stat='identity',position="dodge")+
  geom_text(aes(x=Exited,y=n,label =paste0(round(prop),"%"),group=Geography),position=position_dodge(width=1),vjust=-0.1)+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))+
  labs(y='Count',title='Frequency of Chrun by Country')+
  scale_fill_brewer(palette= "Set2")



# Country

df7<-as.data.frame(churn_df) %>%group_by(Geography) %>% tally()%>%
  arrange(desc(n)) %>%
  mutate('prop' =round(n/sum(n)*100, digits = 1))

df7$ymax <- cumsum(df7$prop)

df7$ymin <- c(0, head(df7$ymax, n=-1))

df7$labelPosition <- (df7$ymax + df7$ymin) / 2


ggplot(df7, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Geography)) +
  geom_rect() +
  geom_label(aes( x=3.5, y=labelPosition, label =paste0(Geography ,"\n",round(prop),"%")), size=6) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"))+
  scale_fill_brewer(palette= "Set2")+
  labs(title='Frequency of each country')



# Active members 

Active_cols<-c("seagreen","khaki1")

df6 <- as.data.frame(churn_df) %>%group_by(IsActiveMember) %>% tally() %>%
  arrange(desc(n)) %>%
  mutate('prop' =round(n/sum(n)*100, digits = 1))

ggplot(df6,aes(x=IsActiveMember,y=n,fill=IsActiveMember))+ 
  geom_bar(stat = "identity",color=Active_cols,fill=Active_cols)+
  geom_text(aes(label =paste0(prop,"%")), vjust = -0.3)+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        legend.position = "none",
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold")
        
  )+
  labs(y='Count',title='Frequency of Active member')

# Active members by country

churn_df %>% group_by(IsActiveMember,Geography) %>% tally()
df4 <- as.data.frame(churn_df) %>%group_by(IsActiveMember,Geography) %>% tally() %>%
  arrange(desc(n)) %>%
  mutate('prop' = unlist(by(data=n, INDICES = IsActiveMember, FUN = function(x) round(x/sum(x)*100, digits = 1))))


ggplot(df4,aes(x=IsActiveMember,y=n,label=paste0(prop,"%"),order=Geography))+ 
  geom_bar(aes(fill=Geography),stat='identity',position="dodge")+
  geom_text(aes(x=IsActiveMember,y=n,label =paste0(round(prop),"%"),group=Geography),position=position_dodge(width=1),vjust=-0.1)+
  labs(y='Count',title="Frequency of Active member by Country")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))+
  scale_fill_brewer(palette= "Set2")

# Active member by extied
df_act <- as.data.frame(churn_df) %>%group_by(IsActiveMember,Exited) %>% tally() %>%
  arrange(desc(n)) %>%
  mutate('prop' = unlist(by(data=n, INDICES = IsActiveMember, FUN = function(x) round(x/sum(x)*100, digits = 1))))

ggplot(df_act,aes(x=IsActiveMember,y=n,fill=Exited))+ 
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(label =paste0(round(df_act$prop),"%"),position=position_dodge(width=0.9),vjust=-0.1)+
  labs(y='Count',title='Frequency of Active member by a churn')+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))

#Gender 

gender_colors=c("#084594","#BB4444")
df_gen <- as.data.frame(churn_df) %>%group_by(Gender) %>% tally() %>%
  arrange(desc(n)) %>%
  mutate('prop' =round(n/sum(n)*100, digits = 1))

ggplot(df_gen,aes(x=Gender,y=n,fill=Gender))+ 
  geom_bar(stat = "identity",color=gender_colors,fill=gender_colors)+
  geom_text(aes(label =paste0(prop,"%")), vjust = -0.3)+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        legend.position = "none",
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))+
  labs(y='Count',title='Frequency of Age')

#Gender by Exited

df3<-as.data.frame(churn_df) %>%group_by(Exited, Gender) %>% tally()%>%
  arrange(desc(n)) %>%
  mutate('prop' = unlist(by(data=n, INDICES = Exited, FUN = function(x) round(x/sum(x)*100, digits = 1))))
df3

ggplot(df3, aes(x = 2, y = prop, fill = Gender)) +
  geom_bar(stat = "identity") +
  coord_polar(theta ="y",start=1)+  
  geom_text(aes(x=1, label =paste0(round(prop),"%")),position = position_stack(vjust = 0.6))+
  labs(title= "Churn rate by Gender", color="Gender", x="", y="")+
  scale_fill_manual(values=gender_colors)+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 10,hjust = 0.5))+
  facet_grid(~Exited) +
  xlim(0.2,2.5)


#Credit card or not 

cr_colors=c("lightsalmon1","turquoise4")

df_cr <- as.data.frame(churn_df) %>%group_by(HasCrCard) %>% tally() %>%
  arrange(desc(n)) %>%
  mutate('prop' =round(n/sum(n)*100, digits = 1))

ggplot(df_cr,aes(x=HasCrCard,y=n,fill=HasCrCard))+ 
  geom_bar(stat = "identity",color=cr_colors,fill=cr_colors)+
  geom_text(aes(label =paste0(prop,"%")), vjust = -0.3)+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        legend.position = "none",
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))+
  labs(y='Count',title='Frequency of Customers with credit card')

#Having credit card by Exited

df_cr2<-as.data.frame(churn_df) %>%group_by(Exited,HasCrCard) %>% tally()%>%
  arrange(desc(n)) %>%
  mutate('prop' = unlist(by(data=n, INDICES = Exited, FUN = function(x) round(x/sum(x)*100, digits = 1))))


ggplot(df_cr2, aes(x = 2, y = prop, fill = HasCrCard)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y",start=1)+  
  geom_text(aes(x=1, label =paste0(round(prop),"%")),position = position_stack(vjust = 0.6))+
  labs(title= "Churn rate by having a credit card", color="HasCrCard", x="", y="")+
  scale_fill_manual(values=cr_colors)+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 10,hjust = 0.5))+
  facet_grid(~Exited)+
  xlim(0.2,2.5)


# N of products
unique(churn_df$NumOfProducts)

df_pr <- as.data.frame(churn_df) %>%group_by(NumOfProducts) %>% tally() %>%
  arrange(desc(n)) %>%
  mutate('prop' =round(n/sum(n)*100, digits = 1))

ggplot(df_pr,aes(x=NumOfProducts,y=n,fill=NumOfProducts))+ 
  geom_bar(stat = "identity",alpha=0.8)+
  geom_text(aes(label =paste0(prop,"%")), vjust = -0.3)+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        legend.position = "none",
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))+
  labs(y='Count',title="Frequency of Product type")+
  scale_fill_brewer(palette= "Set3")



df5_nofpr <- as.data.frame(churn_df) %>%group_by(Exited,NumOfProducts) %>% tally() %>%
  arrange(desc(n)) %>%
  mutate('prop' = unlist(by(data=n, INDICES = Exited, FUN = function(x) round(x/sum(x)*100, digits = 1))))


ggplot(df5_nofpr, aes(x = 2, y = prop, fill = NumOfProducts)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y",start=1)+  
  geom_text(aes(x=1, label =paste0(round(prop),"%")),position = position_stack(vjust = 0.6))+
  labs(title= "Churn rate by Product type", color="NumOfProducts", x="", y="")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 20,hjust = 0.5))+
  facet_grid(~Exited)+
  xlim(0.2,2.5)+
  scale_fill_brewer(palette= "Set3")



#Credit score

credit_mean <- churn_df %>% group_by(Exited) %>% summarise(Mean = mean(CreditScore))
summary(churn_df$CreditScore)
a<- ggplot(churn_df)+geom_histogram(aes(x=CreditScore,color=Exited,fill=Exited),bins=30,alpha=0.5)+
  ggtitle("Distribution of Credit score by Exited")+
  geom_vline(data=credit_mean,aes(xintercept=Mean,color=Exited), linetype = "dashed")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 20,hjust = 0.5))
ggplotly(a)

b<-ggplot(churn_df,aes(x=Exited,y=CreditScore,fill=Exited))+
  geom_boxplot(color="lightsalmon4")+ 
  geom_violin(fill = "salmon2",alpha=0.2,color="darksalmon") +
  ggtitle("Distribtion of Credit score by churn")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
ggplotly(b)


c<-ggplot(churn_df,aes(x=Exited,y=CreditScore,fill=Exited))+
  geom_boxplot()+
  ggtitle("Distribtion of Credit score by churn each country")+
  facet_wrap(~Geography, scale="free")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
ggplotly(c)

# Balance 
Balance_mean <- churn_df %>% group_by(Exited) %>% summarise(Mean = mean(Balance))
churn_df %>% group_by(Exited,Geography) %>% summarise(min=min(Balance),Mean = mean(Balance),max=max(Balance))
summary(churn_df$Balance)

d<-ggplot(churn_df)+geom_histogram(aes(x=Balance,color=Exited,fill=Exited),bins=30,alpha=0.5)+
  ggtitle("Distribution of Balance by Exited")+
  geom_vline(data=Balance_mean,aes(xintercept=Mean,color=Exited), linetype = "dashed")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 20,hjust = 0.5))
ggplotly(d)

ggplot(churn_df,aes(x=Exited,y=Balance,fill=Exited))+
  geom_boxplot(color="lightsalmon4")+ 
  geom_violin(fill = "salmon2",alpha=0.2,color="darksalmon") +
  ggtitle("Distribtion of Balance by churn")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))


e<-ggplot(churn_df,aes(x=Exited,y=Balance,fill=Exited))+
  geom_boxplot()+
  ggtitle("Distribtion of Balance by churn each country")+
  facet_wrap(~Geography, scale="free")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
ggplotly(e)


# Estimated salary

EstimatedSalary_mean <- churn_df %>% group_by(Exited) %>% summarise(Mean = mean(EstimatedSalary))
summary(churn_df$EstimatedSalary)

f<-ggplot(churn_df)+geom_histogram(aes(x=EstimatedSalary,color=Exited,fill=Exited),bins=30,alpha=0.5)+
  ggtitle("Distribution of Estimated Salary by Exited")+
  geom_vline(data=EstimatedSalary_mean,aes(xintercept=Mean,color=Exited), linetype = "dashed")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 20,hjust = 0.5))

ggplotly(f)

g<-ggplot(churn_df,aes(x=Exited,y=EstimatedSalary,fill=Exited))+
  geom_boxplot(color="lightsalmon4")+ 
  geom_violin(fill = "salmon2",alpha=0.2,color="darksalmon") +
  ggtitle("Distribtion of EstimatedSalary by churn")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
ggplotly(g)

h<-ggplot(churn_df,aes(x=Exited,y=EstimatedSalary,fill=Exited))+
  geom_boxplot()+
  ggtitle("Distribtion of Estimated Salary by churn each country")+
  facet_wrap(~Geography, scale="free")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
ggplotly(h)



#Churn by age

age_mean <- churn_df %>% group_by(Exited) %>% summarise(Mean = mean(Age))
summary(churn_df$Age)

i<-ggplot(churn_df)+geom_histogram(aes(x=Age,color=Exited,fill=Exited),bins=30,alpha=0.5)+
  ggtitle("Distribution of Estimated Salary by Exited")+
  geom_vline(data=age_mean,aes(xintercept=Mean,color=Exited), linetype = "dashed")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 20,hjust = 0.5))

ggplotly(i)

j<-ggplot(churn_df,aes(x=Exited,y=Age,fill=Exited))+
  geom_boxplot(color="lightsalmon4")+ 
  geom_violin(fill = "salmon2",alpha=0.2,color="darksalmon") +
  ggtitle("Distribtion of Age by churn")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
ggplotly(j)

k<-ggplot(churn_df,aes(x=Exited,y=Age,fill=Exited))+
  geom_boxplot()+
  ggtitle("Distribtion of Age by churn each country")+
  facet_wrap(~Geography, scale="free")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
ggplotly(k)



# scatter plot 


m<-ggplot(churn_df, aes(x=Age,y=Balance))+
  geom_point(aes(color=Exited))+
  ggtitle("Age & Balance by Exited")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
ggplotly(m)

n <-ggplot(churn_df, aes(x=Balance,y=CreditScore))+
  geom_point(aes(color=Exited))+
  ggtitle("CreditScore & Balance by Exited")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
ggplotly(n)

o<-ggplot(churn_df, aes(x=CreditScore,y=Age))+
  geom_point(aes(color=Exited))+
  ggtitle("Credit Score & Age by Exited")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
ggplotly(o)



#Tenure 

i <-ggplot(churn_df)+
  geom_histogram(aes(x=Tenure,color=Exited,fill=Exited),bins=30,alpha=0.5)+
  ggtitle("Tenure distribution by Exited")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
ggplotly(i)

#Tenure_Ca 
df_tenure <- as.data.frame(churn_df) %>%group_by(Tenure_ca,Exited) %>% tally() %>%
  arrange(desc(n)) %>%
  mutate('prop' = unlist(by(data=n, INDICES =Tenure_ca, FUN = function(x) round(x/sum(x)*100, digits = 1))))

q<-ggplot(df_tenure,aes(x=Tenure_ca,y=n,fill=Exited))+
  geom_bar(stat = "identity")+
  geom_text(aes(label =paste0(round(prop),"%")), position = position_stack(vjust =1.019))+ 
  labs(y='Count',title="Frequency of Tenure by Exited")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
ggplotly(q)




# Correlation 
str(churn_df)
cor1 <-churn_df[,-c(2,3,7,8,9,11,12)]
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(cor1), method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
         # Combine with significance level
         sig.level = 0.01,  
         # hide correlation coefficient on the principal diagonal
         diag = FALSE )


colnames(churn_df[,-c(2,3,7,8,9,12)])
churn_df[,-c(2,3,7,8,9,12)]%>% ggpairs(mapping=aes(colour=Exited), title="Correlogram with numeric variables by churn")

#Radar chart comparison

data2<-churn_df %>% group_by(Exited) %>% 
  summarise(Credit_Score = mean(CreditScore),Age=mean(Age),Tenure=mean(Tenure),
            Balance=mean(Balance), Salary =mean(EstimatedSalary)) 

data2 %>% 
  mutate_each(funs(rescale),-Exited) %>%
  ggradar(grid.label.size = 4,
          axis.label.size = 4, 
          group.point.size = 5,
          group.line.width = 1.5,
          legend.text.size= 10) +
  labs(title = "Relatively comparison of churn")+
  theme(panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"))


# mosaic plot 

tbl <- xtabs(~Exited + IsActiveMember + Gender, churn_df)
ftable(tbl)

mosaic(tbl, 
       shade = TRUE,
       legend = TRUE,
       labeling_args = list(set_varnames = c(Gender = "Gender",
                                             IsActiveMember = "Member",
                                             Exited = "Exited")),
       set_labels = list(Exited = c("Stay", "Leave"),
                         IsActiveMember = c("Non_Member", "Member"),
                         Gender = c("Female", "Male")),
       main = "Gender, Membership, and Exited")

tbl2 <- xtabs(~Exited + Geography + Tenure_ca, churn_df)
ftable(tbl2)

mosaic(tbl2, 
       shade = TRUE,
       legend = TRUE,
       labeling_args = list(set_varnames = c(Geography = "Country",
                                             Tenure_ca = "Tenure",
                                             Exited = "Exited")),
       set_labels = list(Exited = c("Stay", "Leave"),
                         Tenure_ca = c("0-2", "2-5","5-7","7-10"),
                         Geography = c("France", "Germany","Spain")),
       main = "Tenure, Geography, and Exited")
#https://rkabacoff.github.io/datavis/Models.html#Corrplot
#blue means there are more observations in that cell than would be expected under 
#the null model (independence). Red means there are fewer observations than would 
#have been expected.

set.seed(42)
options(scipen=999)
churn_df1<-churn_df[,-c(1,6)]
train.index <- createDataPartition(churn_df1$Exited, p=0.8, list=FALSE) 
churn_train <- churn_df1[train.index, ]
churn_test <- churn_df1[-train.index,  ]

logitmod <- glm(Exited ~ ., family = "binomial", data=churn_train)
summary(logitmod)

pred_log <- predict(logitmod, newdata = churn_test, type = "response")

y_pred_num <- ifelse(pred_log > 0.5, 1, 0)
y_pred_log <- factor(y_pred_num, levels=c(0, 1))
y_act <- churn_test$Exited
p_log = prediction(pred_log,churn_test$Exited)

#Area Under the Curve.
auc_log = as.numeric(performance (p_log, "auc")@y.values)
auc_log
result_log = paste("Area Under The Curve = ", round(auc_log,4))
cat(result_log)

perf_log = performance(p_log,'tpr','fpr')
pf_log = data.frame(FPR=perf_log@x.values[[1]],TPR=perf_log@y.values[[1]])

g1 = ggplot() +
  geom_line(data=pf_log,aes(x=FPR,y=TPR),color="red", size = 0.8) +
  geom_line(aes(x=c(0,1),y=c(0,1)), color="black", linetype = "dashed")  +
  annotate("text", label = result_log, x = 0.7, y = 0.05, size = 5, colour = "black")+
  xlab("False Positive Rate (1-Specificity)")+ylab("True Positive Rate (Sensitivity)")+ggtitle("ROC Curve")
print(g1)




#  Decision tree 

decision_fit <- rpart(Exited ~ .,method="class", data=churn_train)

printcp(decision_fit) # display the results
plotcp(decision_fit) # visualize cross-validation results
summary(decision_fit) # detailed summary of splits

# plot tree
rpart.plot(decision_fit, main="Classification Tree")
#https://www.guru99.com/r-decision-trees.html

pred_tr<-predict(decision_fit, newdata=churn_test, type = 'class')

tree_accuracy<-mean(pred_tr == y_act)

#Area Under the Curve.
pred_tr1<-predict(decision_fit, newdata=churn_test, type = 'prob')
p_tr = prediction(pred_tr1[,2],churn_test$Exited)

auc_tr = as.numeric(performance (p_tr, "auc")@y.values)
auc_tr
result_tr = paste("Area Under The Curve = ",round(auc_tr,4))
cat(result_tr)

perf_tr = performance(p_tr,'tpr','fpr')
pf_tr = data.frame(FPR=perf_tr@x.values[[1]],TPR=perf_tr@y.values[[1]])

g2 = ggplot() +
  geom_line(data=pf_tr,aes(x=FPR,y=TPR),color="blue", size = 0.8) +
  geom_line(aes(x=c(0,1),y=c(0,1)),  color="black", linetype = "dashed")  +
  annotate("text", label = result_tr, x = 0.7, y = 0.05, size = 5, colour = "black")+
  xlab("False Positive Rate (1-Specificity)")+ylab("True Positive Rate (Sensitivity)")+ggtitle("ROC Curve")
print(g2)




# Support vector machine



svmfit =train(Exited~., data=churn_train , kernel ="svmLinear",
              gamma = 2^(-1:1), cost=2^(2:4),
              trControl=trainControl(method = "repeatedcv",number = 10, repeats = 3),
              preProcess=c("center","scale"),
              probability=TRUE)


# Generate predictions
pred_svm<- predict(svmfit,churn_test)
pred_svm



# Roc curve 

pred_svm2<-predict(svmfit,churn_test,type='prob')
log_roc<-roc(churn_test$Exited,pred_svm2[,2])
p_svm = prediction(pred_svm2[,2],churn_test$Exited)

auc_svm = as.numeric(performance (p_svm, "auc")@y.values)
auc_svm
result_svm = paste("Area Under The Curve = ", round(auc_svm,4))
cat(result_svm)

perf_svm = performance(p_svm,'tpr','fpr')
pf_svm = data.frame(FPR=perf_svm@x.values[[1]],TPR=perf_svm@y.values[[1]])

g3= ggplot() +
  geom_line(data=pf_svm,aes(x=FPR,y=TPR),color="green", size = 0.8) +
  geom_line(aes(x=c(0,1),y=c(0,1)), color="black", linetype = "dashed")  +
  annotate("text", label = result_svm, x = 0.7, y = 0.05, size = 5, colour = "red")+
  xlab("False Positive Rate (1-Specificity)")+ylab("True Positive Rate (Sensitivity)")+ggtitle("ROC Curve")
print(g3)




# Random forest 

# Create model with default paramters


control <- trainControl(method="repeatedcv", number=10, repeats=3)

x<-churn_train[,1:9]
mtry <- sqrt(ncol(x)) #Number of variables randomly sampled as candidates at each split.
tunegrid <- expand.grid(.mtry=mtry)



rf_fit <- train(Exited ~ ., data = churn_train,
                method = "rf",
                preProcess = c("center", "scale"),
                tuneGrid = tunegrid,
                trControl = control,         
                family= "binomial",
                metric= "Accuracy" 
)

pred_rf<-predict(rf_fit,churn_test,type='raw')



# Roc curve 
pred_rf2<-predict(rf_fit,churn_test,type='prob')
aud_rf<-roc(churn_test$Exited,pred_rf2[,2])
pred_rf2

p_rf = prediction(pred_rf2[,2],churn_test$Exited)
auc_rf = as.numeric(performance (p_rf, "auc")@y.values)
result_rf =  paste("Area Under The Curve = ", round(auc_rf,4))
cat(result_rf)

perf_rf= performance(p_rf,'tpr','fpr')
pf_rf = data.frame(FPR=perf_rf@x.values[[1]],TPR=perf_rf@y.values[[1]])

g4 = ggplot() +
  geom_line(data=pf_rf,aes(x=FPR,y=TPR),color="forestgreen", size = 0.8) +
  geom_line(aes(x=c(0,1),y=c(0,1)), color="black", linetype = "dashed")  +
  annotate("text", label = result_rf, x = 0.7, y = 0.05, size = 5, colour = "black")+
  xlab("False Positive Rate (1-Specificity)")+ylab("True Positive Rate (Sensitivity)")+ggtitle("ROC Curve")
print(g4)

result_rf = paste("Area Under The Curve = ", round(auc_rf,2))
cat(result_rf)

perf_rf= performance(p_rf,'tpr','fpr')
pf_rf = data.frame(FPR=perf_rf@x.values[[1]],TPR=perf_rf@y.values[[1]])

# ALL Roc Curve 
g = ggplot() +
  geom_line(data=pf_log,aes(x=FPR,y=TPR),color="red", size = 0.8) +
  geom_line(data=pf_rf,aes(x=FPR,y=TPR),color="forestgreen", size = 0.8) +
  geom_line(data=pf_svm,aes(x=FPR,y=TPR),color="green", size = 0.8) +
  geom_line(data=pf_tr,aes(x=FPR,y=TPR),color="blue", size = 0.8) +
  geom_line(aes(x=c(0,1),y=c(0,1)), color="black")  +
  annotate("text", label = result_rf, x = 0.7, y = 0.05, size = 5, colour = "forestgreen")+
  annotate("text", label = result_svm, x = 0.7, y = 0.10, size = 5, colour = "green")+
  annotate("text", label = result_log, x = 0.7, y = 0.15, size = 5, colour = "red")+
  annotate("text", label = result_tr, x = 0.7, y = 0.20, size = 5, colour = "blue")+
  xlab("False Positive Rate (1-Specificity)")+ylab("True Positive Rate (Sensitivity)")+
  ggtitle("ROC Curve of logistic regression(red) & Decision tree(blue) 
          & SVM(Green) & Random forest(Dark green)")+
  theme(legend.position="none",
        panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
print(g)


# ALL Confusion matics
confusion_matrix_log<-as.data.frame(table(y_pred_log, y_act))
ggplot(data = confusion_matrix_log,
       mapping = aes(x = y_pred_log,
                     y = y_act)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, fontface  = "bold") +
  scale_fill_gradient(low = "coral1",
                      high = "aquamarine1",
                      trans = "log") +
  labs(title = "Comfusion matrix of Logistic regression")+
  theme(legend.position="none",
        panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))

confusion_matrix_tree<-as.data.frame(table(pred_tr, y_act))
ggplot(data = confusion_matrix_tree,
       mapping = aes(x = pred_tr,
                     y = y_act)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, fontface  = "bold") +
  scale_fill_gradient(low = "coral1",
                      high = "aquamarine1",
                      trans = "log") +
  labs(title = "Comfusion matrix of Decision tree")+
  theme(legend.position="none",
        panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))

confusion_matrix_svm<-as.data.frame(table(pred_svm, y_act))
ggplot(data = confusion_matrix_svm,
       mapping = aes(x = pred_svm,
                     y = y_act)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, fontface  = "bold") +
  scale_fill_gradient(low = "coral1",
                      high = "aquamarine1",
                      trans = "log") +
  labs(title = "Comfusion matrix of SVM")+
  theme(legend.position="none",
        panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))

confusion_matrix_rf<-as.data.frame(table(pred_rf, y_act))
ggplot(data = confusion_matrix_rf,
       mapping = aes(x = pred_rf,
                     y = y_act)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, fontface  = "bold") +
  scale_fill_gradient(low = "coral1",
                      high = "aquamarine1",
                      trans = "log") +
  labs(title = "Comfusion matrix of Random Forest")+
  theme(legend.position="none",
        panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))

# feature importance 


vip::vip(decision_fit)+
  labs(title = "Feature importance with Decision tree")+
  theme(legend.position="none",
        panel.border = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text( size=20,hjust = 0.5, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))
