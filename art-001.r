library(grid)

pass=0
circle.number=0
circle.data=data.frame(X=numeric(),Y=numeric(),R=numeric())
dev.new()


repeat{
x=runif(1,0,1)
y=runif(1,0,1)
r=runif(1,.003,.1)

if (nrow(circle.data)>0){
	for (p in 1:nrow(circle.data)){
		X=circle.data[p,"X"]
		Y=circle.data[p,"Y"]
		R=circle.data[p,"R"]

		if ( sqrt( (X-x)^2 + (Y-y)^2 ) < R+r | sqrt( (.5-x)^2 + (.5-y)^2 ) > 0.5-r ){break}
		  else {pass=p}
				   		 }
	     		 }

if (pass==nrow(circle.data) &  sqrt( (.5-x)^2 + (.5-y)^2 ) < 0.5-r){
	grid.circle(x,y,r,gp=gpar(col=NA,fill=rainbow(20)[runif(1,1,20)],alpha=0.5))
	circle.data[nrow(circle.data)+1,]=c(x,y,r)
										       }			
	}
