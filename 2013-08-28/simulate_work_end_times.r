


sf = rchisq(1000,3,4) #blue
nyc = rchisq(1000,5,5); #orange
london = rchisq(1000,3,4); #green
moscow = rchisq(1000,3,4); #red
delhi = rchisq(1000,2,3); #purple
manila = rchisq(1000,3,3); #brown

do_cities = rbind(sf,nyc,london,moscow,delhi,manila);

DD = list();
for (i in 1:6){
	DD[[i]] = density(do_cities[i,],from=0)
}

do_colours = c("blue","orange","darkgreen","darkred","purple","brown")

#jpeg(file="/Users/dionysiakiaraxifara/Sites/decoded_presentations/2013-09-24/img/cities_work_end_times.jpg")

postscript(file="/home/xifara/Decoded/cities_work_end_times.eps",width=40,height=10)
#pdf(file="/home/xifara/Decoded/cities_work_end_times.pdf",width=40,height=10)
plot(1:5,1:5,xlim=c(0,max(c(DD[[1]]$x,DD[[2]]$x,DD[[3]]$x,DD[[4]]$x,DD[[5]]$x,DD[[6]]$x))),
ylim=c(0,max(c(DD[[1]]$y,DD[[2]]$y,DD[[3]]$y,DD[[4]]$y,DD[[5]]$y,DD[[6]]$y)))+0.01, axes=F,
xlab="Time (hour of day)",ylab="% of popln",bty="n",cex.lab=2);

for (i in 1:6){
	lines(DD[[i]]$x,DD[[i]]$y,lwd=3,col=do_colours[i])
}
legend ("topleft",legend = c("     SF   ","   NYC   ","   London   ","   Moscow   ","   Delhi   ","   Manila   "),col=do_colours,pch=19,cex=1.1,bty="n",
	ncol=6);

axis(1,at=c(5,15,25,35)-3,labels = c("17","19","21","23"),cex.axis=2)
axis(2,at=c(0.006,0.04,0.08,0.12),labels=c(5,10,15,20),cex.axis = 2,las = 2)
dev.off();


pdf(file="/home/xifara/Decoded/cities_work_end_times.pdf",width=40,height=10)
par(mfrow=c(6,6,1,1))
plot(1:5,1:5,xlim=c(0,max(c(DD[[1]]$x,DD[[2]]$x,DD[[3]]$x,DD[[4]]$x,DD[[5]]$x,DD[[6]]$x))),
ylim=c(0,max(c(DD[[1]]$y,DD[[2]]$y,DD[[3]]$y,DD[[4]]$y,DD[[5]]$y,DD[[6]]$y)))+0.01, axes=F,
xlab="Time (hour of day)",ylab="% of popln",bty="n",cex.lab=3);

for (i in 1:6){
	lines(DD[[i]]$x,DD[[i]]$y,lwd=3,col=do_colours[i])
}
legend ("topleft",legend = c("     SF   ","   NYC   ","   London   ","   Moscow   ","   Delhi   ","   Manila   "),col=do_colours,pch=19,cex=2,bty="n",
	ncol=6);

axis(1,at=c(5,15,25,35)-3,labels = c("17","19","21","23"),cex.axis=3)
axis(2,at=c(0.006,0.04,0.08,0.12),labels=c(5,10,15,20),cex.axis = 3,las = 2)
dev.off();