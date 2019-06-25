#Convert MP3 files into wave format
setwd("D:\\Audio Case study\\mp3")
library(tuneR)
library(dplyr)
file.names<-list.files(pattern=".mp3")
for(i in 1:length(file.names))
{
  file=readMP3(file.names[i])
  writeWave(file,paste(gsub(".mp3","",file.names[i]),".wav"))
}

#Spectrogram: Visual Representation of wave files
library(seewave)
FnF=readWave("fastandfur_BPItcpsx .wav")
spectro(FnF,f=FnF@samp.rate,main="Fast and Furious Theme Music",flim=c(2,15))
left=FnF@left

#Fast Fourier Transform
FFT=fft(left)
class(FFT)
head(FFT)

Freq=Re(FFT)
Amp=Im(FFT)

#Frequency Domain Plot
library(ggplot2)
FD=data.frame(Freq[1:25000],Amp[1:25000]) #For simplicity
summary(FD)
plot=ggplot(FD,aes(x=FD$Freq,y=FD$Amp))+geom_point()+geom_line(lineend="butt")+scale_x_continuous(breaks=c(0,6.33e+06,1.8e+09))+scale_y_continuous(breaks=c(0,6.280e+06,1.8e+09))+xlab("Frequency in Hz")+ylab("Amplitude")+ggtitle("Frequency Domain")
plot

#Importing wave files and extract channels
setwd("C:\\Users\\Kafeel\\Desktop\\R Files\\Data sets\\Audio")
file.names<-list.files(pattern=".wav")
left=list()
right=list()
for(i in 1:length(file.names))
{
  file=readWave(file.names[i],from=0,to=30,units=c("seconds"))
  left[[as.character(i)]]=file@left[1:5000] #For simplicity
  right[[as.character(i)]]=file@right[1:5000]
}
length=lapply(right,length)

#write.csv(as.data.frame(left),"left5.csv",row.names=FALSE)
#write.csv(as.data.frame(right),"right5.csv",row.names=FALSE)

labels=substr(file.names,1,4)
table(labels)

#Identical channels?
identical(left,right)

length=lapply(left,length)
data=data.frame(left)
dim(data)
#View(data)

#Fast Fourier Transform
data1=apply(data,2,fft)
class(data1)
#View(data1)
data2=apply(data1,2,Mod)
class(data2)
#View(data2)
df=as.data.frame(t(data2)) #Transpose
#View(df)
#df$genre=c(rep("blue",100),rep("clas",100),rep("coun",100),rep("disc",100),rep("hiph",100),rep("jazz",100))
df$genre=c(rep(1,100),rep(2,100),rep(3,100),rep(4,100),rep(5,100),rep(6,100))
#View(df)

#Split the data into train and test
index<- sort(sample(nrow(df),nrow(df)*0.8))
train<-df[index,]
test<-df[-index,]

which(names(train)=="genre")
X_train=as.matrix(train[,-5001])
y_train=train$genre

y_test=test$genre
X_test=as.matrix(test[,-5001])
class(X_test)

set.seed(350)
library(glmnet)
cv.mlr=cv.glmnet(X_train,as.factor(y_train),family="multinomial",alpha=0,nfolds=5)

lambda=cv.mlr$lambda.1se
lambda

#Predict class
prob=predict(cv.mlr,newx=X_test,s=lambda,type="class")
head(prob)

#Confusion Matrix and Missclassification
table(prob,y_test)

mean(as.character(prob) != as.character(y_test))

#Accuracy
sum(diag(table(prob,y_test)))/nrow(X_test)

#With our six genres,for instance, random guessing would result in only 16.7 percent. We will consider accurary more than 16.7%.