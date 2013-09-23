library(Hmisc)

png(file="~/Sites/decoded_presentation/templates/reveal/small_data_tweets.png",width = 1033,height = 484)
par(bg="white",mar=c(10,10,2,2))
y1 = c(5,6,10,11,18,20,120,150,80,10,15,20,25,30,50,100,200,75,10,5,10,3,7,5)
x1 = c(1:7,8,9,10,11,12,13,14,15,16,17,18,19,20,21:24);
plot(x1,y1 + runif(24,0,10) + 5,xlab="",ylab="",ylim=c(0,350),xlim=c(1,24),axes=F,
	pch = 16,col="darkblue",cex = 2)
axis(1,at=c(1,5,8.5,12.5,16,20,24),labels = c("midnight","6am","9am","noon","3pm","6pm","midnight"),line=2,cex.axis=2)
axis(2, at= c(0,100,200,300), labels = c(0,100,200,300),line=2,cex.axis=2)
title(xlab="Time", cex.lab = 2, line = 6)
title(ylab="#Tweets (million)",cex.lab = 2,line=6)
dev.off();

small_data = cbind(x1,y1);
colnames(small_data) = c("Time", "No.Tweets")
small_data = data.frame(small_data)

require(png)
require(grid)
z <- readPNG("~/Sites/decoded_presentation/templates/reveal/tweet_small_data.png")

## Function to draw your statement
copyright.draw <- function(label, image, x, y, size, ...) {
  lab <- textGrob(label = label,
    x = unit(x, "npc"), y = unit(y, "npc"),
    just = c("left", "centre"), gp = gpar(...))
  logo <- rasterGrob(image = image,
    x = unit(x, "npc") + unit(1, "grobwidth", lab), y = unit(y, "npc"),
    width = unit(size, "cm"), height = unit(size, "cm"),
    just = c("left", "centre"), gp = gpar(...))
  grid.draw(lab)
  grid.draw(logo)
}


#San Fran, NYC, London, Moscow, Delhi, Manilla
#10, 13, 18, 21, 23,1
#-8, -5, 3, 5, 7

#london
x1 = sort(c(rnorm(500,9,5), rnorm(500,18,5)))
y1 = c(runif(125,5,15) + (1:125),sort(rnorm(125,180,60)),sort(rnorm(125,180,20),decreasing=T),runif(250,30,140),
	sort(rnorm(125,180,20)),sort(rnorm(125,180,60),decreasing=T),runif(125,5,15)-(1:125))
x1b = x1 + runif(500,0,1); #x1b[which(x1b > 24)] = 24;
y1b = y1 +runif(500,1,50); #y1b[which(y1b > 300)] = 300;
ll = which(y1b > 300)
y1b[ll] = 300 + runif(ll,0,50)*((-1)^(1:ll)) 
zz = which(y1b < 0); y1b[zz] = runif(length(zz),20,60)
shift0 = x1b + 0;
shift0[which(shift0 > 24)] = shift0[(which(shift0>24))] - 24;
shift0[which(shift0 < 0)] = shift0[(which(shift0<0))] + 24;
kol = as.vector(col2rgb("blue"))/1000; 
x_london = shift0; y_london = y1b;

num.languages <- floor(runif(length(y1),2,7)); #num.languages = num.languages + 3;
num.hashtags <- floor(runif(length(y1),1,4))
slang <- round(rnorm(length(y1),30,30),2)
images <- rpois(length(y1),40)
connections <- rpois(length(y1),700)
Self <- round(rnorm(length(y1),170,30),2)
Facebook <- round(rnorm(length(y1),250,30),2)
Links <- round(rnorm(length(y1),750,100),2)


big_data = cbind(round(shift0,2),round(y1,2),num.languages,num.hashtags,Links,slang,images,Facebook,connections,Self);
colnames(big_data) = c("Time", "No.Tweets","Languages","Hashes","Links","Slang","Images","Facebook","Connections","Self")
big_data = data.frame(big_data)


#moscow
x2 = sort(c(rnorm(500,9,5), rnorm(500,18,5)));
y2 = c(runif(125,5,15) + (1:125),sort(rnorm(125,150,90)),sort(rnorm(125,150,20),decreasing=T),runif(250,30,120),
	sort(rnorm(125,150,20)),sort(rnorm(125,150,50),decreasing=T),runif(125,5,15)-(1:125))
x2b = x2 + runif(500,0,0.5); #x1b[which(x1b > 24)] = 24;
y2b = y2 +runif(500,1,80); 
ll = which(y2b > 250)
y2b[ll] = 250 + runif(ll,0,50)*((-1)^(1:ll)) 
zz = which(y2b < 0); y2b[zz] = runif(length(zz),20,60)
kol2 = as.vector(col2rgb("red"))/1000; 
shift3 = x2b + 3;
shift3[which(shift3 > 24)] = shift3[(which(shift3>24))] - 24;
shift3[which(shift3 < 0)] = shift3[(which(shift3<0))] + 24;
x_moscow = shift3; y_moscow = y2b;
#plot(x2b[shift3],y2b,ylab="#Tweets (M)", xlab = "Time (hrs)",xlim=c(1,24),ylim=c(0,350),pch=16,col = rgb(kol2[1],kol2[1],kol2[3],alpha = 0.2))

#delhi
x3 = sort(c(rnorm(500,9,5), rnorm(500,18,5)));
y3 = c(runif(125,15,35) + (1:125),sort(rnorm(125,100,90)),sort(rnorm(125,100,90),decreasing=T),runif(250,30,100),
	sort(rnorm(125,100,20)),sort(rnorm(125,100,50),decreasing=T),runif(125,15,35)-(1:125))
x3b = x3 + runif(500,0,1); #x1b[which(x1b > 24)] = 24;
y3b = y3 +runif(500,1,50); 
ll = which(y3b > 150)
y3b[ll] = 150 + runif(ll,0,30)*((-1)^(1:ll)) 
zz = which(y3b < 0); y3b[zz] = runif(length(zz),20,60)
kol3 = as.vector(col2rgb("green"))/1000; 
shift5 = x3b + 5;
shift5[which(shift5 > 24)] = shift5[(which(shift5>24))] - 24;
shift5[which(shift5 < 0)] = shift5[(which(shift5<0))] + 24;
x_delhi = shift5; y_delhi = y3b;
#plot(x2b[shift3

#manilla
x4 = sort(c(rnorm(500,9,5), rnorm(500,18,5)));
y4 = c(runif(125,5,15) + (1:125),sort(rnorm(125,150,50)),sort(rnorm(125,150,50),decreasing=T),runif(250,30,100),
	sort(rnorm(125,100,20)),sort(rnorm(125,100,20),decreasing=T),runif(125,5,15)-(1:125))
x4b = x4 + runif(500,0,0.5); #x1b[which(x1b > 24)] = 24;
y4b = y4 +runif(500,1,50); 
ll = which(y4b > 550)
y4b[ll] = 550 + runif(ll,0,30)*((-1)^(1:ll)) 
zz = which(y4b < 0); y4b[zz] = runif(length(zz),20,60)
kol3 = as.vector(col2rgb("green"))/1000; 
shift7 = x4b + 7;
shift7[which(shift7 > 24)] = shift7[(which(shift7>24))] - 24;
shift7[which(shift7 < 0)] = shift7[(which(shift7<0))] + 24;
x_manilla = shift7; y_manilla = y4b;
#plot(x2b[shift3

#SF
x5 = sort(c(rnorm(500,9,5), rnorm(500,18,5)));
y5 = c(runif(125,5,15) + (1:125),sort(rnorm(125,200,30)),sort(rnorm(125,200,30),decreasing=T),runif(250,30,100),
	sort(rnorm(125,300,50)),sort(rnorm(125,300,50),decreasing=T),runif(125,15,35)-(1:125))
x5b = x5 + runif(500,0,0.5); #x1b[which(x1b > 24)] = 24;
y5b = y5 +runif(500,1,50); 
ll = which(y5b > 450)
y5b[ll] = 450 + runif(ll,0,50)*((-1)^(1:ll)) 
zz = which(y5b < 0); y5b[zz] = runif(length(zz),20,60)
kol3 = as.vector(col2rgb("green"))/1000; 
shift8 = x5b - 8;
shift8[which(shift8 > 24)] = shift8[(which(shift8>24))] - 24;
shift8[which(shift8 < 0)] = shift8[(which(shift8<0))] + 24;
x_sf = shift8; y_sf = y5b;
#plot(x2b[shift3

#NYC
x6 = sort(c(rnorm(500,9,5), rnorm(500,18,5)));
y6 = c(runif(125,5,105) + (1:125),sort(rnorm(125,200,20)),sort(rnorm(125,200,20),decreasing=T),runif(250,30,200),
	sort(rnorm(125,400,100)),sort(rnorm(125,400,100),decreasing=T),runif(125,5,205)-(1:125))
x6b = x6 + runif(500,0,0.5); #x1b[which(x1b > 24)] = 24;
y6b = y6 +runif(500,1,50); 
ll = which(y6b > 450)
y6b[ll] = 450 + runif(ll,0,50)*((-1)^(1:ll)) 
zz = which(y6b < 0); y6b[zz] = runif(length(zz),20,60)
kol3 = as.vector(col2rgb("green"))/1000; 
shiftnyc = x6b - 5;
shiftnyc[which(shiftnyc > 24)] = shiftnyc[(which(shiftnyc>24))] - 24;
shiftnyc[which(shiftnyc < 0)] = shiftnyc[(which(shiftnyc<0))] + 24;
x_nyc = shiftnyc; y_nyc = y6b;
#plot(x2b[shift3

data = cbind(x_sf,y_sf,x_nyc,y_nyc,x_london,y_london,x_moscow,y_moscow, x_delhi,y_delhi,x_manilla,y_manilla);
write.csv(data,"~/Sites/decoded_presentation/templates/reveal/tweet_simulated_data.csv",quote=F,row.names=F)

plot(x_london,y_london,ylab="", xlab = "",xlim=c(0,24),ylim=c(0,600),pch=16,col = rgb(0.7,kol[1],kol[3],alpha = 0.2),axes=F)
points(x_moscow,y_moscow,pch=16,col = rgb(kol2[1],kol2[1],kol2[3],alpha = 0.2))
points(x_delhi,y_delhi,pch=16,col = rgb(0.5,0.1,0.7,alpha = 0.2))
points(x_nyc,y_nyc,pch=16,col = rgb(0.1,0.1,0.7,alpha = 0.2))
points(x_sf,y_sf,pch=16,col = rgb(0.7,0.7,0.1,alpha = 0.2))
points(x_manilla,y_manilla,pch=16,col = rgb(0.4,0.4,0.1,alpha = 0.2))
axis(1,at=c(1,5,8.5,12.5,16,20,24),labels = c("midnight","6am","9am","noon","3pm","6pm","midnight"),line=2)
axis(2, at= c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500),line = 2)
title(xlab="Time", line = 6)
title(ylab="#Tweets (million)",line=6)
legend("topleft",bty="n",col=c(rgb(0.7,kol[1],kol[3],alpha = 1),rgb(kol2[1],kol2[1],kol2[3],alpha = 1),rgb(0.5,0.1,0.7,alpha = 1)),legend=c("UK","Moscow","Delhi"),ncol=3,pch=c(16,16,16))


png(file="~/Sites/decoded_presentation/templates/reveal/big_data_tweets_colour.png",width = 1033,height = 484)
par(bg="white",mar=c(10,10,2,2))
plot(x_london,y_london,ylab="", xlab = "",xlim=c(0,24),ylim=c(0,600),pch=16,col = rgb(0.7,kol[1],kol[3],alpha = 0.2),axes=F,cex=2,cex.lab=2)
points(x_moscow,y_moscow,pch=16,col = rgb(kol2[1],kol2[1],kol2[3],alpha = 0.2),cex=2)
points(x_delhi,y_delhi,pch=16,col = rgb(0.5,0.1,0.7,alpha = 0.2),cex=2)
points(x_nyc,y_nyc,pch=16,col = rgb(0.1,0.1,0.7,alpha = 0.2),cex=2)
points(x_sf,y_sf,pch=16,col = rgb(0.7,0.7,0.1,alpha = 0.2),cex=2)
points(x_manilla,y_manilla,pch=16,col = rgb(0.4,0.4,0.1,alpha = 0.2),cex=2)
axis(1,at=c(1,5,8.5,12.5,16,20,24),labels = c("midnight","6am","9am","noon","3pm","6pm","midnight"),cex.axis=2,line=2)
axis(2, at= c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500),cex.axis=2, line = 2)
title(xlab="Time", cex.lab = 2, line = 6)
title(ylab="#Tweets (million)",cex.lab = 2,line=6)
legend("topleft",bty="n",col=c(rgb(0.7,0.7,0.1,alpha = 1),rgb(0.1,0.1,0.7,alpha = 1),rgb(0.7,kol[1],kol[3],alpha = 1),rgb(kol2[1],kol2[1],kol2[3],alpha = 1),rgb(0.5,0.1,0.7,alpha = 1),rgb(0.4,0.4,0.1,alpha = 1)),legend=c("SF","NYC","London","Moscow","Delhi","Manilla"),ncol=6,pch=c(16,16,16),cex=2)
dev.off();

png(file="~/Sites/decoded_presentation/templates/reveal/big_data_tweets_black.png",width = 1033,height = 484)
par(bg="white",mar=c(10,10,2,2))
plot(x_london,y_london,ylab="", xlab = "",xlim=c(0,24),ylim=c(0,600),pch=16,col = rgb(0,0,0,alpha = 0.2),axes=F,cex=2,cex.lab=2)
points(x_moscow,y_moscow,pch=16,col = rgb(0,0,0,alpha = 0.2),cex=2)
points(x_delhi,y_delhi,pch=16,col = rgb(0,0,0,alpha = 0.2),cex=2)
points(x_nyc,y_nyc,pch=16,col = rgb(0,0,0,alpha = 0.2),cex=2)
points(x_sf,y_sf,pch=16,col = rgb(0,0,0,alpha = 0.2),cex=2)
points(x_manilla,y_manilla,pch=16,col = rgb(0,0,0,alpha = 0.2),cex=2)
axis(1,at=c(1,5,8.5,12.5,16,20,24),labels = c("midnight","6am","9am","noon","3pm","6pm","midnight"),cex.axis=2,line=2)
axis(2, at= c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500),cex.axis=2, line = 2)
title(xlab="Time", cex.lab = 2, line = 6)
title(ylab="#Tweets (million)",cex.lab = 2,line=6)
#legend("topleft",bty="n",col=c(rgb(0.7,0.7,0.1,alpha = 1),rgb(0.1,0.1,0.7,alpha = 1),rgb(0.7,kol[1],kol[3],alpha = 1),rgb(kol2[1],kol2[1],kol2[3],alpha = 1),rgb(0.5,0.1,0.7,alpha = 1),rgb(0.4,0.4,0.1,alpha = 1)),legend=c("SF","NYC","London","Moscow","Delhi","Manilla"),ncol=6,pch=c(16,16,16),cex=2)
dev.off();


#####################################################################################################################

n=1000;
individual <- paste("Indiv", 1:n, sep=" ")
age = floor(rnorm(n,50,30));
if (any(age < 20)){
	ll = which(age < 20);
	age[ll] = 20 + runif(length(ll),0,30)
}
if (any(age > 70)){
	ll = which(age > 70);
	age[ll] = 70 - runif(length(ll),0,30)
}
age = floor(age);
gender = sample(c("Male","Female"),n,replace=T)
Mult.Sclerosis = c();
for (i in 1:n){
	Mult.Sclerosis[i] = "No";
	u = runif(1); 
	if (gender[i] == "Male"){ if (u < 0.01){ Mult.Sclerosis[i] = "Yes"; } }
	if (gender[i] == "Female"){ if (u<0.05){ Mult.Sclerosis[i] = "Yes" } }
}
#Mult.Sclerosis[10] = "Yes";
#Mult.Sclerosis[8] = "No";
#Mult.Sclerosis[7] = "No";

MS_data = cbind(individual,gender,age,Mult.Sclerosis);
MS_data = data.frame(MS_data)

gend = 1*(gender == "Male")
status = 1*(Mult.Sclerosis == "Yes")

prob.case.if.F = length(which(status[which(gend == 0)] == 1))/length(which(gend == 0))
prob.case.if.M = length(which(status[which(gend == 1)] == 1))/length(which(gend == 1))
 
barplot(c(prob.case.if.M,prob.case.if.F),ylim=c(0,0.1),names.arg = c("Male","Female"),col="darkgreen",ylab="Probability")

myfun <- function(vect){return(length(which(vect == 1))/length(vect))}
n_per_bin <- function(vect){return(length(vect))}


depth.class <- cut(age, seq(range(age)[1],range(age)[2]+1,by=5), include.lowest = TRUE)
prop.status <- tapply(status, depth.class, myfun)
bins_sizes <- tapply(status, depth.class, n_per_bin)


CI.u.status <- prop.status + 1.96*sqrt( prop.status*(1-prop.status)/bins_sizes)
CI.l.status <- prop.status - 1.96*sqrt( prop.status*(1-prop.status)/bins_sizes)
plot(seq(range(age)[1],range(age)[2]+1,by=5)[-1] - 2.5,prop.status,ylim=c(0,0.2),pch=16,col="darkblue")

png(file="~/Sites/decoded_presentation/templates/reveal/MS_age.png",width = 500,height = 484)
par(mfrow=c(2,1),mar=c(7,7,2,2))
barplot(c(prob.case.if.M,prob.case.if.F),ylim=c(0,0.2),names.arg = c("",""),col="darkgreen",ylab="",
	cex.lab=4,axes=F,cex.axis = 2)
axis(1, at = c(0.7,1.9),labels = c("Male","Female"),line=2,cex.axis = 1.5,tck = F)
axis(1, at = c(0.7,1.9),labels = c("",""),line=2,tck = F, col="white",lty=5)
axis(2,at=seq(0,0.2,length = 5),labels=seq(0,0.2,length = 5),line=2,cex.axis = 1.5);
title(ylab="Probability MS",line=5, cex.lab=2)


errbar((seq(range(age)[1],range(age)[2]+1,by=5)[-1] - 2.5),prop.status,CI.u.status, CI.l.status,ylim=c(min(CI.l.status),0.2),pch=16,cex=2,lwd=2,
col="darkblue",lty=2,axes = F,xlab="",ylab="")
abline(mean(status),0,lwd=2,lty=2,col=2)
axis(1,at=(seq(range(age)[1],range(age)[2]+1,by=5)[-1] - 2.5),labels = sort(unique(depth.class)),line=2,cex.axis = 1.5)
axis(2,at=seq(0,0.2,length = 5),labels=seq(0,0.2,length = 5),line=2,cex.axis = 1.5);
title(xlab="Age (years)", line = 5,cex.lab = 1.5)
title(ylab="Probability MS",line=5, cex.lab=2)
dev.off();

library(gridExtra)
library(graphics)

png(file="~/Sites/decoded_presentation/templates/reveal/MS_data.png",width = 1100,height = 484)
grid.table(MS_data[1:17,]);
dev.off();



png(file="~/Sites/decoded_presentation/templates/reveal/MS_age2.png",width = 1100,height = 484)

split.screen(c(1,2))        # split display into two screens
split.screen(c(2,1), screen = 2) #
screen(1)
grid.table(MS_data[1:20,]);

screen(3)
par(mfrow=c(2,1),mar=c(7,7,2,2))
barplot(c(prob.case.if.M,prob.case.if.F),ylim=c(0,0.2),names.arg = c("",""),col="darkgreen",ylab="",
	cex.lab=4,axes=F,cex.axis = 2)
axis(1, at = c(0.7,1.9),labels = c("Male","Female"),line=2,cex.axis = 1.5,tck = F)
axis(1, at = c(0.7,1.9),labels = c("",""),line=2,tck = F, col="white",lty=5)
axis(2,at=seq(0,0.2,length = 5),labels=seq(0,0.2,length = 5),line=2,cex.axis = 1.5);
title(ylab="Probability MS",line=5, cex.lab=2)


screen(4);
par(mfrow=c(2,1),mar=c(7,7,2,2))
errbar((seq(range(age)[1],range(age)[2]+1,by=5)[-1] - 2.5),prop.status,CI.u.status, CI.l.status,ylim=c(min(CI.l.status),0.2),pch=16,cex=2,lwd=2,
col="darkblue",lty=2,axes = F,xlab="",ylab="")
abline(mean(status),0,lwd=2,lty=2,col=2)
axis(1,at=(seq(range(age)[1],range(age)[2]+1,by=5)[-1] - 2.5),labels = sort(unique(depth.class)),line=2,cex.axis = 1.5)
axis(2,at=seq(0,0.2,length = 5),labels=seq(0,0.2,length = 5),line=2,cex.axis = 1.5);
title(xlab="Age (years)", line = 5,cex.lab = 1.5)
title(ylab="Probability MS",line=5, cex.lab=2)
close.screen(all = TRUE)  

dev.off();

par(bg = "white")           # default is likely to be transparent
split.screen(c(1,2))        # split display into two screens
split.screen(c(2,1), screen = 2) #
screen(1)
grid.table(head(MS_data))
screen(3)
plot(1:10)
screen(4)
plot(15:20)
close.screen(all = TRUE)  

#git add .
#git commit -m "update 3"
#git push -u origin master
