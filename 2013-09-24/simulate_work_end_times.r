


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

plot(1:5,1:5,xlim=c(0,max(c(DD[[1]]$x,DD[[2]]$x,DD[[3]]$x,DD[[4]]$x,DD[[5]]$x,DD[[6]]$x))),
ylim=c(0,max(c(DD[[1]]$y,DD[[2]]$y,DD[[3]]$y,DD[[4]]$y,DD[[5]]$y,DD[[6]]$y)))+0.03, axes=F,
xlab="Time (hour of day)",ylab="% of popln",bty="n",cex.lab=1);

for (i in 1:6){
	lines(DD[[i]]$x,DD[[i]]$y,lwd=3,col=do_colours[i])
}
legend ("topleft",legend = c("SF","NYC","London","Moscow","Delhi","Manila"),col=do_colours,pch=19,cex=0.8,bty="n",
	ncol=6);

axis(1,at=c(5,15,25,35)-3,labels = c("17","19","21","23"),cex.axis=0.8)
axis(2,at=c(0.04,0.08,0.12),labels=c(10,20,30),cex.axis = 0.8,las = 2)
