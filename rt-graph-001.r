library(grid)
dev.new()

back=rectGrob(name="back",gp=gpar(col=NA,fill="black"))

full.screen=viewport(name="full.screen",.5,.5,1,1)
pushViewport(full.screen)
upViewport()

V= viewport(name="V",.5,.5,.75,.3)
pushViewport(V)
upViewport()

x=round(runif(1,.1,.9),1)
y=round(runif(1,.1,.9),1)
x2=round(runif(1,.1,.9),1)
y2=round(runif(1,.1,.9),1)

min.speed=.016
max.speed=.032

xt=runif(1,min.speed,max.speed)
yt=runif(1,min.speed,max.speed)
x2t=-runif(1,min.speed,max.speed)
y2t=runif(1,min.speed,max.speed)

R=.02
u=0
Ds=as.numeric()


for (dq in 1:100){

if(x>1-R){xt=-runif(1,min.speed,max.speed)}
if(x<R){xt=runif(1,min.speed,max.speed)}
if(y>1-R){yt=-runif(1,min.speed,max.speed)}
if(y<R){yt=runif(1,min.speed,max.speed)}
if(x2>1-R){x2t=-runif(1,min.speed,max.speed)}
if(x2<R){x2t=runif(1,min.speed,max.speed)}
if(y2>1-R){y2t=-runif(1,min.speed,max.speed)}
if(y2<R){y2t=runif(1,min.speed,max.speed)}

x=x+xt
y=y+yt
x2=x2+x2t
y2=y2+y2t
u=u+1

distance = sqrt( (y2-y)^2 + (x2-x)^2 )

Ds=c(Ds,distance)
			}



for (i in 1:100){

	dev.flush()
	seekViewport("full.screen")
	grid.draw(back)
	seekViewport("V")
	grid.yaxis(at=c(0,.5,1),label=c("0",".5","1"),gp=gpar(fontsize=8,col="white"))
	grid.lines(c(0,1),c(2*R,2*R),gp=gpar(col="red"))

for (f in 2:(i-1)){

grid.lines(x=c(1-((i-f-1)/100),1-((i-f)/100)) , y=c(Ds[f],Ds[f-1]),gp=gpar(col="green"))
	
     			 }
		    }
	


repeat{

if(x>1-R){xt=-runif(1,min.speed,max.speed)}
if(x<R){xt=runif(1,min.speed,max.speed)}
if(y>1-R){yt=-runif(1,min.speed,max.speed)}
if(y<R){yt=runif(1,min.speed,max.speed)}
if(x2>1-R){x2t=-runif(1,min.speed,max.speed)}
if(x2<R){x2t=runif(1,min.speed,max.speed)}
if(y2>1-R){y2t=-runif(1,min.speed,max.speed)}
if(y2<R){y2t=runif(1,min.speed,max.speed)}

x=x+xt
y=y+yt
x2=x2+x2t
y2=y2+y2t
u=u+1

distance = sqrt( (abs(y2-y))^2 + (abs(x2-x))^2 )

Ds=c(Ds,distance)
Ds=Ds[-1]


dev.flush()
seekViewport("full.screen")
grid.draw(back)
seekViewport("V")
grid.yaxis(at=c(0,.5,1),label=c("0",".5","1"),gp=gpar(fontsize=8,col="white"))
grid.lines(c(0,1),c(2*R,2*R),gp=gpar(col="red"))

for (f in 1:100){
	grid.lines(x=c(f/100,(f+1)/100) , y=c(Ds[f],Ds[f+1]),gp=gpar(col="green"))
		    }

	}
