############## RANDOM POLYGONS ########################

library(grid)

dev.new()

grid.rect(gp=gpar(fill="black"))

x1=runif(1,0,1)
x2=runif(1,0,1)
y1=runif(1,0,1)
y2=runif(1,0,1)
x3=runif(1,0,1)
x4=runif(1,0,1)
y3=runif(1,0,1)
y4=runif(1,0,1)

x1t=.01
x2t=.01
y1t=.01
y2t=.01
x3t=.01
x4t=.01
y3t=.01
y4t=.01

w=round(runif(1,1,100),digit=0)

counter=1
n=7
Y=1
T=0
color=1

C=textGrob(T,name="C",gp=gpar(col=color),just="right")
pushViewport(viewport(x=1,y=0,height=.05,width=.05,just=c("right","bottom")))
grid.draw(C)
upViewport()


repeat{

if(x1>1){x1t=-runif(1,.01,.02)}
if(x1<0){x1t=runif(1,.01,.02)}
if(y1>1){y1t=-runif(1,.01,.02)}
if(y1<0){y1t=runif(1,.01,.02)}
if(x2>1){x2t=-runif(1,.01,.02)}
if(x2<0){x2t=runif(1,.01,.02)}
if(y2>1){y2t=-runif(1,.01,.02)}
if(y2<0){y2t=runif(1,.01,.02)}
if(x3>1){x3t=-runif(1,.01,.02)}
if(x3<0){x3t=runif(1,.01,.02)}
if(y3>1){y3t=-runif(1,.01,.02)}
if(y3<0){y3t=runif(1,.01,.02)}
if(x4>1){x4t=-runif(1,.01,.02)}
if(x4<0){x4t=runif(1,.01,.02)}
if(y4>1){y4t=-runif(1,.01,.02)}
if(y4<0){y4t=runif(1,.01,.02)}

x1=x1+x1t
x2=x2+x2t
y1=y1+y1t
y2=y2+y2t
x3=x3+x3t
x4=x4+x4t
y3=y3+y3t
y4=y4+y4t

w=w+1
if (w==301){w=1}
color=rainbow(300)[w]
T=T+1


if(counter<=n)
	
	{p=polygonGrob(name=toString(counter),c(x1,x2,x3,x4),c(y1,y2,y3,y4),gp=gpar(col=color,lwd=counter/2));grid.draw(p);counter=counter+1}
else
	{grid.edit(toString(Y),x=unit(c(x1,x2,x3,x4),"npc"),y=unit(c(y1,y2,y3,y4),"npc"),gp=gpar(col=color));Y=Y+1;if(Y>n){Y=1};E=Y

	for (re in 1:(n-1)){
		E=E-1
		if(E==0){E=n}
		grid.edit(toString(E),gp=gpar(lwd=(n+1-re)/2))

				}

	}	

grid.edit("C",label=T,gp=gpar(col=color))	

dev.flush()

Sys.sleep(.01)

}
