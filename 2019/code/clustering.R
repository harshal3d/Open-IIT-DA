data = read.csv('data.csv', header = T, sep = ",")
data = subset(data, select = -c(Customer,State,Response,Effective.To.Date,Gender,Location.Code,Marital.Status,Policy,Sales.Channel,Vehicle.Size) )

#Hierrarchial Clustering
dist = dist(data, method = "euclidean")
fit = hclust(dist, method = "ward.D")
plot(fit)

#groups
groups = cutree(fit, k = 3)

DataWithHistCluster = data.frame(data, groups)

ggplot(data, aes(x=Coverage, y=Monthly.Premium.Auto,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Mean Premium Value for Coverages ")

ggplot(data, aes(x=Coverage, y=Customer.Lifetime.Value,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Mean Customer Life Time Value for Coverages ")

ggplot(data, aes(x=Vehicle.Class, y=Customer.Lifetime.Value,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Customer Life Time Value Based on Coverage and Vehicle Class ")+facet_wrap(~Coverage)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data, aes(x=Vehicle.Class, y=Total.Claim.Amount,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Claim Amount Based on Coverage and Vehicle Class ")+facet_wrap(~Coverage)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data, aes(x=Renew.Offer.Type, y=Customer.Lifetime.Value,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Offer, Coverages and CLTV")+facet_wrap(~Coverage)
ggplot(data, aes(x=EmploymentStatus, y=Customer.Lifetime.Value,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Employement status, Coverages and CLTV")+facet_wrap(~Coverage)+theme(axis.text.x = element_text(angle = 90, hjust = 1))


group3=DataWithHistCluster[DataWithHistCluster$groups==3,]
ggplot(group3,aes(Coverage,Customer.Lifetime.Value,col="rainbow"))+stat_summary(fun.y="mean", geom="bar")+geom_jitter()+
  ggtitle("Customer Life Time Value Distribution Of Group3")


group2=DataWithHistCluster[DataWithHistCluster$groups==2,]
ggplot(group2,aes(Coverage,Customer.Lifetime.Value,col="rainbow"))+stat_summary(fun.y="mean", geom="bar")+geom_jitter()+
  ggtitle("Customer Life Time Value Distribution Of Group2")

group1=DataWithHistCluster[DataWithHistCluster$groups==1,]
ggplot(group1,aes(Coverage,Customer.Lifetime.Value,col="rainbow"))+stat_summary(fun.y="mean", geom="bar")+geom_jitter()+
  ggtitle("Customer Life Time Value Distribution Of Group1")