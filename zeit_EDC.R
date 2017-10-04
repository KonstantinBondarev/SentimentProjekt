# load libraries
library(diezeit)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ISOweek)
library(zoo)
library(dygraphs)
library(xts)

# provide API access token
zeit_client()

#aec3f7174e4dc545704c78ec25f7402fda54daaf3f5e8bd72899

# Pull data from Zeit API
list1=zeit_search("content", c("schuldenkrise","finanzkrise"), limit = 1000) # 1000
list2=zeit_search("content", c("schuldenkrise","finanzkrise"), limit = 1000, offset = 1000) # 1001-2000
list3=zeit_search("content", c("schuldenkrise","finanzkrise"), limit = 1000, offset = 2000) # 2001-3000
list4=zeit_search("content", c("schuldenkrise","finanzkrise"), limit = 1000, offset = 3000) # 3001-4000
list5=zeit_search("content", c("schuldenkrise","finanzkrise"), limit = 1000, offset = 4000) # 4001-5000
list6=zeit_search("content", c("schuldenkrise","finanzkrise"), limit = 1000, offset = 5000) # 5001-6000
list7=zeit_search("content", c("schuldenkrise","finanzkrise"), limit = 1000, offset = 6000) # 6001-7000

# save raw data to local disk
setwd("C:/Users/benjamin/Downloads")
save(list1, file = "zeitAPI_results_set1.Rdata")
save(list2, file = "zeitAPI_results_set2.Rdata")
save(list3, file = "zeitAPI_results_set3.Rdata")
save(list4, file = "zeitAPI_results_set4.Rdata")
save(list5, file = "zeitAPI_results_set5.Rdata")
save(list6, file = "zeitAPI_results_set6.Rdata")
save(list7, file = "zeitAPI_results_set7.Rdata")


# prepare data for plotting
df1=data.frame(matrix(NA, nrow=length(list1$matches), ncol=1))
colnames(df1)="Date"
df2=data.frame(matrix(NA, nrow=length(list2$matches), ncol=1))
colnames(df2)="Date"
df3=data.frame(matrix(NA, nrow=length(list3$matches), ncol=1))
colnames(df3)="Date"
df4=data.frame(matrix(NA, nrow=length(list4$matches), ncol=1))
colnames(df4)="Date"
df5=data.frame(matrix(NA, nrow=length(list5$matches), ncol=1))
colnames(df5)="Date"
df6=data.frame(matrix(NA, nrow=length(list6$matches), ncol=1))
colnames(df6)="Date"
df7=data.frame(matrix(NA, nrow=length(list7$matches), ncol=1))
colnames(df7)="Date"

for (i in 1:length(list1$matches)){
print(as.Date(list1$matches[[i]]$release_date))
  df1$Date[i]=as.Date(list1$matches[[i]]$release_date)
}

for (i in 1:length(list2$matches)){
  print(as.Date(list2$matches[[i]]$release_date))
  df2$Date[i]=as.Date(list2$matches[[i]]$release_date)
}

for (i in 1:length(list3$matches)){
  print(as.Date(list3$matches[[i]]$release_date))
  df3$Date[i]=as.Date(list3$matches[[i]]$release_date)
}

for (i in 1:length(list4$matches)){
  print(as.Date(list4$matches[[i]]$release_date))
  df4$Date[i]=as.Date(list4$matches[[i]]$release_date)
}

for (i in 1:length(list5$matches)){
  print(as.Date(list5$matches[[i]]$release_date))
  df5$Date[i]=as.Date(list5$matches[[i]]$release_date)
}

for (i in 1:length(list6$matches)){
  print(as.Date(list6$matches[[i]]$release_date))
  df6$Date[i]=as.Date(list6$matches[[i]]$release_date)
}

for (i in 1:length(list7$matches)){
  print(as.Date(list7$matches[[i]]$release_date))
  df7$Date[i]=as.Date(list7$matches[[i]]$release_date)
}


df1=data.frame(Date=as.Date(df1$Date, origin = "1970-01-01"))
df2=data.frame(Date=as.Date(df2$Date, origin = "1970-01-01"))
df3=data.frame(Date=as.Date(df3$Date, origin = "1970-01-01"))
df4=data.frame(Date=as.Date(df4$Date, origin = "1970-01-01"))
df5=data.frame(Date=as.Date(df5$Date, origin = "1970-01-01"))
df6=data.frame(Date=as.Date(df6$Date, origin = "1970-01-01"))
df7=data.frame(Date=as.Date(df7$Date, origin = "1970-01-01"))

df=rbind(df1,df2,df3,df4,df5,df6,df7)


df=filter(df, Date > "2006-12-31")


df$date=as.Date(as.yearmon(df$Date))
df.counts=df %>% group_by(date) %>% summarise(count.articles=length(date))
df.counts$date=as.yearmon(df.counts$date)

df.date=data.frame(date=as.yearmon(seq(as.Date("2007-02-28"),as.Date("2017-07-31"), by="months" )))

df.counts=merge(x=df.date,y=df.counts, by="date", all.x = TRUE)
df.counts$count.articles[is.na(df.counts$count.articles)]=0
df.counts$date=as.Date(df.counts$date, frac = 1)
df.counts.xts=xts(df.counts, order.by = df.counts$date)

# Plot results
dygraph(df.counts.xts) %>%
  dyEvent("2008-10-01", "Failure Lehman Brothers", labelLoc = "top") %>%
  dyEvent("2010-06-01", "First Greek Bailout", labelLoc = "top") %>%
  dyEvent("2010-12-01", "Irish Bailout", labelLoc = "top") %>%
  dyEvent("2011-05-01", "Portuguese Bailout", labelLoc = "top") %>%
  dyEvent("2011-08-01", "First Greek Debt Haircut", labelLoc = "top") %>%
  dyEvent("2012-03-01", "Second Greek Bailout", labelLoc = "top") %>%
  dyEvent("2012-08-01", "Draghi: Whatever It Takes", labelLoc = "top") %>%
  dyEvent("2013-04-01", "Cypriot Bailout", labelLoc = "top") %>%
  dyEvent("2015-04-01", "ECB Quantitative Easing Starts", labelLoc = "top") %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.4)



# 
# ggplot(df.counts, aes(date)) +
#   geom_bar(stat="identity", aes(y=count.articles)) +
#   scale_fill_manual(labels=c("bla")) +
#   theme(legend.position = "bottom") +
#   theme(legend.title = element_blank()) +
#   ylab("Anzahl Artikel") +
#   xlab("") +
#   # theme(axis.text.x=element_blank(),
#   #       axis.ticks.x=element_blank()) +
#   ggtitle("Anzahl der Artikel mit dem Begriff 'Finanzkrise' (Zeit API)") +
#   theme(plot.title = element_text(hjust = 0.5))


