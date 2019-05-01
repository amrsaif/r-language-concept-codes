library(grid)

o=0
ln=0
c=0
bg="#F7F0E6"

dev.new(width=10,height=12,bg=bg)

main=viewport(name="main",0,2/12,1,10/12,just=c("left","bottom"))
graph=viewport(name="graph",9/10,0,8.4/10,2/12,just=c("right","bottom"))
meantext=viewport(name="meantext",1,0,1/10,1/12,just=c("right","bottom"))
linestext=viewport(name="linestext",1,1/12,1/10,1/12,just=c("right","bottom"))

pushViewport(main)
grid.rect()
upViewport()

pushViewport(graph)
grid.rect()
grid.yaxis(at=seq(1/7,6/7,1/7),label=seq(0,1.25,.25),gp=gpar(fontsize=7,lwd=1))
upViewport()

pushViewport(meantext)
grid.rect()
pushViewport(viewport(.5,.75,1,.5))
grid.text("Mean length",gp=gpar(fontsize=7),just="top")
upViewport(2)

pushViewport(linestext)
grid.rect()
pushViewport(viewport(.5,.75,1,.5))
grid.text("No. of lines",gp=gpar(fontsize=7),just="top")
upViewport(2)


repeat{
x1=runif(1,0,1)
x2=runif(1,0,1)
y1=runif(1,0,1)
y2=runif(1,0,1)

L= sqrt((abs(x2-x1))^2+(abs(y2-y1))^2)
ln=ln+1
o=o+1
c=c+L
M=c/ln

seekViewport("main")
grid.lines(c(x1,x2),c(y1,y2),gp=gpar(col=(rainbow(100)[runif(1,1,100)])))

seekViewport("meantext")
pushViewport(viewport(.5,.25,1,.5))
grid.rect(width=.9,height=.9,gp=gpar(fill=bg,col=NA))
grid.text(formatC(M,format="f",4),gp=gpar(fontsize=8,col="blue"),just=c("center","bottom"))

seekViewport("linestext")
pushViewport(viewport(.5,.25,1,.5))
grid.rect(width=.9,height=.9,gp=gpar(fill=bg,col=NA))
grid.text(ln,gp=gpar(fontsize=8,col="red"),just=c("center","bottom"))

seekViewport("graph")
if(o>1000){o=1;grid.rect(gp=gpar(fill=bg))}
grid.circle(x=o/1002,y=(L/1.75)+1/7,r=.01,gp=gpar(col=NA,fill="red",alpha=.5))
grid.circle(x=o/1002,y=(M/1.75)+1/7,r=.01,gp=gpar(col="blue"))

Sys.sleep(.01)
dev.flush()
	}
