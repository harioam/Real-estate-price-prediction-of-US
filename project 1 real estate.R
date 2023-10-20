
setwd("/Users/harioam datas")
ld_train=read.csv("housing_train (1).csv",stringsAsFactors = F)
ld_test=read.csv("housing_test (1).csv",stringsAsFactors = F)

dim(ld_train)
dim(ld_test)
head(ld_train)
View(ld_test)

####  taking out NA in various entrie 
is.na(ld_train
  
)
sum(is.na(ld_train$ld_train))
apply(ld_train,2,function(ld_train)sum(is.na(ld_train)))
ld_train$Bedroom2[is.na(ld_train$Bedroom2)]=median(ld_train$Bedroom2,na.rm = T)
sum(is.na(ld_train$Bedroom2))
ld_train$Bathroom[is.na(ld_train$Bathroom)]=median(ld_train$Bathroom,na.rm = T)
ld_train$Car[is.na(ld_train$Car)]=median(ld_train$Car,na.rm = T)
ld_train$Bedroom2[is.na(ld_train$Bedroom2)]=median(ld_train$Bedroom2,na.rm = T)
head(ld_train)
ld_train$Landsize[is.na(ld_train$Landsize)]=round(median(ld_train$Landsize,na.rm = T))
sum(is.na(ld_train$Landsize))
ld_train$BuildingArea[is.na(ld_train$BuildingArea)]=round(mean(ld_train$BuildingArea,na.rm=T))
                                                          
sum(is.na(ld_train$BuildingArea))
ld_train$YearBuilt[is.na(ld_train$YearBuilt)]=round(mean(ld_train$YearBuilt,na.rm = T))                                                          
apply(ld_train,2,function(ld_train)sum(is.na(ld_train)))    
##########so the Na values in all train data columns are removed ....
#####now move on to test data to impute Nas
apply(ld_test,2,function(ld_test)sum(is.na(ld_test)))
##to reomove all na an filling it with mean
ld_test$Bedroom2[is.na(ld_test$Bedroom2)]=round(mean(ld_test$Bedroom2,na.rm = T))


ld_test$Bathroom[is.na(ld_test$Bathroom)]=round(median(ld_test$Bathroom,na.rm = T))
ld_test$Car[is.na(ld_test$Car)]=round(median(ld_test$Car,na.rm = T))
ld_test$Landsize[(is.na(ld_test$Landsize))]=round(median(ld_test$Landsize,na.rm=T))
ld_test$BuildingArea[is.na(ld_test$BuildingArea)]=round(mean(ld_test$BuildingArea,na.rm = T))
ld_test$YearBuilt[is.na(ld_test$YearBuilt)]=round(median(ld_test$YearBuilt,na.rm = T))
apply(ld_test,2,function(ld_test)sum(is.na(ld_test)))
###here all na vales in test is replaced with mean values and price is added by me NA values..so we will leave it undisturbed
######step 2 is data prep by combining test andtrain 
ld_test$Price=NA
ld_train$data='Train'
ld_test$data='Test'
View(ld_test)

View(ld_test)
View(ld_train)

###i had created 2 columns for price and Price need to deleate one of it
ld_test=ld_test[,c(1,2,3,4,16,5,6,7,8,9,10,11,12,13,14,15,17)]
View(ld_test)
all_data=rbind(ld_train,ld_test)
View(all_data)
#######now we have cleanedd data perrfectly....
head(all_data)
sum(is.na(all_data
      ))
########creating dummies variable
t=table(all_data$Suburb)
View(t)
t1=round(tapply(all_data$Price,all_data$Suburb,mean,na.rm=T),0)
View(t1)
t1=sort(t1)
library(dplyr)
glimpse(all_data)

#########after taking price mean according to the different suburbs ..we have sorted and try making dummies of variable of nearly same mean price
all_data=mutate(all_data,
    sub_1=as.numeric(Suburb c("Campbellfield","Jacana")),
    sub_2=as.numeric(Suburb%in%c("Kealba","Brooklyn","Albion","Sunshine West","Ripponlea","Fawkner")),
    sub_3=as.numeric(Suburb%in%c("Glenroy","Southbank","Sunshine North","Keilor Park","Heidelberg West","Reservoir","Braybrook","Kingsbury","Gowanbrae","Hadfield","Watsonia","Footscray","South Kingsville","Balaclava","Melbourne","Maidstone","Sunshine")),
    sub_4=as.numeric(Suburb%in%c("Airport West","Heidelberg Heights","Pascoe Vale","West Footscray","Altona North","Williamstown North","Brunswick West","Keilor East","Oak Park","Maribyrnong","Altona","Flemington","Coburg North","Yallambie","Avondale Heights","Bellfield")),
    sub_5=as.numeric(Suburb%in%c("Strathmore Heights","Glen Huntly","Kensington","Essendon North","St Kilda","Preston","North Melbourne","Coburg","Kingsville","Collingwood","Brunswick East","Gardenvale","Thornbury","Niddrie","West Melbourne","Viewbank")),
    sub_6=as.numeric(Suburb%in%c("Spotswood","Carnegie","Elwood","Heidelberg","Moorabbin","Oakleigh","Rosanna","Docklands","Yarraville","Cremorne","Seddon","Brunswick","Oakleigh South","Ascot Vale","Windsor","Caulfield","Essendon West","Newport")),
    sub_7=as.numeric(Suburb%in%c("Chadstone","South Yarra","Essendon","Bentleigh East","Murrumbeena","Hughesdale","Fairfield","Ashwood","Clifton Hill","Caulfield North","Abbotsford","Carlton","Prahran","Fitzroy","Ivanhoe","Hampton East","Caulfield East")),
    sub_8=as.numeric(Suburb%in%c("Richmond","Travancore","Templestowe Lower","Ormond","Caulfield South","Moonee Ponds","Hawthorn","Box Hill","Bulleen","Burnley","Burwood","Strathmore","Port Melbourne","Fitzroy North","Alphington")),
    sub_9=as.numeric(Suburb%in%c("Doncaster","South Melbourne","Northcote","Aberfeldie","Elsternwick","Bentleigh","Kooyong","Parkville")),
    sub_10=as.numeric(Suburb%in%c("Williamstown","East Melbourne","Seaholme")),
    sub_11=as.numeric(Suburb%in%c("Malvern East","Carlton North","Hawthorn East","Surrey Hills")),
    sub_12=as.numeric(Suburb%in%c("Princes Hill","Mont Albert","Armadale","Kew East","Glen Iris","Ashburton")),
    sub_13=as.numeric(Suburb%in%c("Brighton East","Eaglemont","Hampton")),
    sub_14=as.numeric(Suburb%in%c("Toorak","Ivanhoe East","Camberwell","Balwyn North","Kew")),
    sub_15=as.numeric(Suburb%in%c("Brighton","Middle Park")),
    sub_16=as.numeric(Suburb%in%c("Albert Park","Balwyn","Malvern"))
   ) 
  
  all_data$Suburb=NULL
glimpse(all_data)
View(all_data)
class(all_data)    

summary(all_data)
########as all the dummies for suburb has been crreated and its numeric
#####now we will deleate address varibale as it is unique and weaken the model
all_data$Address=NULL
####now we will make dummies for type as its char
t3=table(all_data$Type)
View(t3)
####since h is large no. we will try to ignore the h and make dummies for t and u n-1
all_data=mutate(
  all_data,typeT=as.numeric(Type=="t"),
          typeU=as.numeric(Type=="u"))
all_data$Type=NULL          
glimpse(all_data)
######now we will make dummies for Method
t4=table(all_data$Method)
View(t4)
all_data=mutate(all_data,
                methodPI=as.numeric(Method=="PI"),
                methodSA=as.numeric(Method=="SA"),
                methodSP=as.numeric(Method=="SP"),
                methodVB=as.numeric(Method=="VB"),
                
                )
glimpse(all_data)
all_data$Method=NULL
####we have make dummies for thevariabe method and delated it now we will mkvw into next char variable 
#####dummies for seller
t5=table(all_data$SellerG)
View(t5)
all_data=mutate(all_data,
                Snelson=as.numeric(SellerG=="Nelson"),
                SJellis=as.numeric(SellerG=="Jellis"),
                Sstuart=as.numeric(SellerG=="hockingstuart"),
                SBarry=as.numeric(SellerG=="Barry"),
                SMarshall=as.numeric(SellerG=="Marshall"),
                SWoodards=as.numeric(SellerG=="Woodards"),
                SBrad=as.numeric(SellerG=="Brad"),
                SBiggin=as.numeric(SellerG=="Biggin"),
                SRay=as.numeric(SellerG=="Ray"),
                SFletchers=as.numeric(SellerG=="Fletchers"),
                SRT=as.numeric(SellerG=="RT"),
                SSweeney=as.numeric(SellerG=="Sweeney"),
                SGreg=as.numeric(SellerG=="Greg"),
                SNoel=as.numeric(SellerG=="Noel"),
                SGary=as.numeric(SellerG=="Gary"),
                SJas=as.numeric(SellerG=="Jas"),
                SMiles=as.numeric(SellerG=="Miles"),
                SMcGrath=as.numeric(SellerG=="McGrath"),
                SHodges=as.numeric(SellerG=="Hodges"),
                SKay=as.numeric(SellerG=="Kay"),
                SStockdale=as.numeric(SellerG=="Stockdale"),
                Love=as.numeric(SellerG=="Love"),
                SDouglas=as.numeric(SellerG=="Douglas"),
                Williams=as.numeric(SellerG=="Williams"),
                SVillage=as.numeric(SellerG=="Village"),
                SRaine=as.numeric(SellerG=="Raine"),
                SRendina=as.numeric(SellerG=="Rendina"),
                SChisholm=as.numeric(SellerG=="Chisholm"),
                SCollins=as.numeric(SellerG=="Collins"),
                SLITTLE=as.numeric(SellerG=="LITTLE"),
                SNick=as.numeric(SellerG=="Nick"),
                SHarcourts=as.numeric(SellerG=="Harcourts"),
                SCayzer=as.numeric(SellerG=="Cayzer"),
                SMoonee=as.numeric(SellerG=="Moonee"),
                SYPA=as.numeric(SellerG=="YPA"),
                sDINGLE=as.numeric(SellerG=="Dingle"),
                SBurnham=as.numeric(SellerG=="Burnham"),
                SPeter=as.numeric(SellerG=="Peter"),
                SEview=as.numeric(SellerG=="Eview"),
                SFrank=as.numeric(SellerG=="Frank"),
                SThomson=as.numeric(SellerG=="Thomson"),
                SBells=as.numeric(SellerG=="Bells"),
                SC21=as.numeric(SellerG=="C21"),
                SConsidine=as.numeric(SellerG=="Considine"),
                SAlexkarbon=as.numeric(SellerG=="Alexkarbon"),
                SRW=as.numeric(SellerG=="RW"),
                SMcdonald=as.numeric(SellerG=="McDonald"),
                SEdward=as.numeric(SellerG=="Edward"),
)
glimpse(all_data)
all_data$SellerG=NULL
####now as all dummies for sellers are created wil create forcouncil area####
t6=table(all_data$CouncilArea)
View(t6)
all_data=mutate(all_data,
                CA_Banyule=as.numeric(CouncilArea=="Banyule"),
                 CA_Bayside=as.numeric(CouncilArea=="Bayside"),
                 CA_Boroondara=as.numeric(CouncilArea=="Boroondara"),
                 CA_Brimbank=as.numeric(CouncilArea=="Brimbank"),
                 CA_Darebin=as.numeric(CouncilArea=="Darebin"),
                 CA_Glen_Eira=as.numeric(CouncilArea=="Glen Eira"),
                 CA_Monash=as.numeric(CouncilArea=="Monash"),
                 CA_Melbourne=as.numeric(CouncilArea=="Melbourne"),
                 CA_Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"),
                 CA_Manningham=as.numeric(CouncilArea=="Manningham"),
                 CA_Kingston=as.numeric(CouncilArea=="Kingston"),
                 CA_Hume=as.numeric(CouncilArea=="Hume"),
                 CA_HobsonsB=as.numeric(CouncilArea=="Hobsons Bay"),
                 CA_MoonValley=as.numeric(CouncilArea=="Moonee Valley"),
                 CA_Moreland=as.numeric(CouncilArea=="Moreland"),
                 CA_PortP=as.numeric(CouncilArea=="Port Phillip"),
                 CA_Stonnington=as.numeric(CouncilArea=="Stonnington"),
                 CA_Whitehorse=as.numeric(CouncilArea=="Whitehorse"),
                 CA_Yarra=as.numeric(CouncilArea=="Yarra"),
               )
glimpse(all_data)
all_data$CouncilArea=NULL
glimpse(all_data)
######now the data is clean now we will separate test and train data=ld_train same data just diving after cleanup
train=filter(all_data,data=="Train")
View(train)
glimpse(train)
test=filter(all_data,data=="Test")
View(test)
test$data=NULL
train$data=NULL
View(train)
######now we will divide train dataset in 75%of train and 25% of test
set.seed(123)
s=sample(1:nrow(train),0.75*nrow(train))
train_75=train[s,]
test_25=train[-s,]
View(train_75)
View(test_25)
#########################now as the data is ready for moodel building and prediction
####using R lang

View(cor(train_75))
library(car)
fit=lm(Price~. ,data = train_75)
summary(fit)
######now we have created the model and we will use vif to remove unwanted columns with less importance for our model

a=vif(fit)
sort(a,decreasing = T)
####we will take out higher 3 vif to check how model is performing
fit1=lm(Price~.-sub_3-sub_7-sub_6,data=train_75)
summary(fit1)
a1=vif(fit1)
sort(a1,decreasing = T)
###again we will remove postcode and unimportant variable
fit2=lm(Price~.-Postcode,data=train_75)
summary(fit2)
a3=vif(fit2)
sort(a3,decreasing = T)
###removing again all the unwanted data
fit3=lm(Price~.-Postcode-sub_3-sub_7-sub_6-sub_8,data = train_75 )
summary(fit3)
a4=vif(fit3)
sort(a4,decreasing = T)
#####now removing all the variables which has p values >0.05
Fit4=lm(Price~.-Postcode-sub_3-sub_7-sub_6-sub_8-sub_1-sub_5-methodSA-Sstuart-SBarry-SWoodards-SBrad-SBiggin
        -SRay-SFletchers-SRT-SSweeney-SGreg-SNoel-SGary-SJas-SMiles-SMcGrath-SHodges-SKay-SStockdale-Love-
          SDouglas-Williams-SVillage-SRaine-SRendina-SChisholm-SCollins-SLITTLE-SNick-SHarcourts-SCayzer-SMoonee-
          SYPA-sDINGLE-SBurnham-SPeter-SEview-SFrank-SThomson-SBells-SC21-SConsidine-SAlexkarbon-SRW-SMcdonald-
          SEdward-CA_Glen_Eira -CA_Monash-CA_Manningham-CA_Kingston-CA_Hume-CA_Whitehorse-CA_Stonnington-
          CA_PortP,data = train_75)
summary(Fit4)
vif(Fit4)###vif less than 5 good
#####as all unwanted varibles have been removed will go for prediction on test data
########now we wiil check with pred value and actual values tthorugh tables
Trainres=cbind.data.frame(Actual=train_75$Price,predvalues=fitted(Fit4),errors=residuals(Fit4))####actula from out train data after processing and predicted on its train dtat..

View(Trainres)
View(predtest25)
###########now rmse of train model that we have created that is in train_75 we have created model

rmse_train75=sqrt(mean(Trainres$errors^2))
rmse_train75
####now we will draw actual vs predicted....predvalues and actual after binding colums of data trainresult 
library(ggplot2)
ggplot(data = Trainres,mapping = aes(Actual,predvalues))+geom_point()
######after viewing the graph we can see that actual and pred values are  vaery close to each other..which is good for model.
###now we will check for normality of errors...
ggplot(Trainres,aes(x=errors))+geom_histogram()
##now as we can seee the errors curve it follows normal distribution and mean to zero....which is good for model so our assumptions are clearing
###now we will check for last assumptions of Homoscadasticity and independence of errors##########
ggplot(Trainres,aes(x=predvalues,y=errors))+geom_point()
#####so now we cann see the graphs as its scattered with any relatoins so its independance of errors..and also homosscadasticity is seen by no funnel shaped

##which is good for models
###now checking perfomances on test data
ir_predict=predict(Fit4,newdata = test_25)
###now test 25 result we will see
test_25res=cbind.data.frame(Testactual=test_25$Price,testpred=ir_predict,errors=test_25$Price-ir_predict)###making data frame of pred values for test25 and its actual price so
##that we can compare easily
####
View(test_25res)### we can view predicted values
###now we will check rmse values of error
res=test_25$Price-ir_predict
##for sqrt
rmse_test25res=sqrt(mean(test_25res$errors^2))
rmse_test25res###now the rmse is 403712.9 ...now the value rmse should be same in both train75 and test25 data i,e 37000 in train nearly same
####now we will check for assumptions
###1 check for actual and pred curve
ggplot(data = test_25res,aes(x=Testactual,y=testpred))+geom_point()###now the graph is scattered no relation or collinearity
###now we will check error normality
ggplot(test_25res,aes(errors))+geom_histogram()###mean is also zero it follows normal distributuin
######now checking for homoscadasticity and
ggplot(test_25res,aes(x=testpred,y=errors))+geom_point()####now this doesnt follow homoscadasticity....model is woking fine'
######now we this is the model we need...we will predict on given data which our system hasnt seen...
ir_predict1=predict(Fit4,newdata = test)
ir_predict1=round(ir_predict1,1)
class(ir_predict1)
View(ir_predict1)
write.csv(ir_predict1,file = "ir_predict1.csv")
summary(Fit4)
var(train$Price)####question 1
var(ir_predict1)
sum(is.na(ld_train$YearBuilt))
###now storing
write.csv(ir_predict1,"hariOam_Chaturvedi.csv",row.names = T)
###lets have some formulae
p=round(tapply(ld_train$Price,ld_train$Type,mean,na.rm=T))
view(p)
#######we will check for rmse on wholse train data combinig test25 and trqin75
alltrain=cbind(test_25,train_75)
View(alltrain)
ir_predict2=predict(Fit4,newdata = alltrain)
alltrainres=cbind.data.frame(allact=alltrain$Price,allpred=ir_predict2,errores=alltrain$Price-ir_predict2)
alltrainres
rmsealltrain=sqrt(mean(alltrainres$errores^2))
rmsealltrain#####rmse on train is 403712.9
sum(is.na(alltrain))
