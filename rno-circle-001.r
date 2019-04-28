rno.circle=function(data=1:10,labels=data,sep=1){


if (length(data) != length(labels))
{stop("Data and label vectors must be of the same length")}

if (sep>10 | sep <1 | is.numeric(sep)==FALSE)
{stop("Separation value must be a number between 1 and 10")}

time1=Sys.time()

library(grid)
dev.new()

pushViewport(viewport(.1,.5,.8,.04,just=c("left","bottom")))
grid.rect(gp=gpar(col="blue"))
grid.text("Progress",x=.5,y=1.7,gp=gpar(fontsize=15))
progress.box=rectGrob(name="progress.box",0,0,0,1,just=c("left","bottom"),gp=gpar(col="blue",fill="blue"))
grid.draw(progress.box)
upViewport()

table=data.frame(data=data,label=labels)
table=table[order(table$data),]

list=abs(table$data)
list2=table$label

circle.data=data.frame(x=numeric(),y=numeric(),r=numeric(),label=character())
pass=0
#pass2=1

circle.number=0
progress=0
c=0
q=1
zoom = sqrt(sum(list)/pi)*4
item=length(list)
max.separation=(sep/1000)+.0015
min.separation=(sep/1000)+.0005
r=sqrt(list[item]/pi)/zoom


repeat{

x=runif(1,0,1)
y=runif(1,0,1)

if (circle.number>0 ){
	for (p in 1:nrow(circle.data)){
	
	X=circle.data[p,1]
	Y=circle.data[p,2]
	R=circle.data[p,3]
	distance= sqrt( (X-x)^2 + (Y-y)^2 )
	radii=R+r

if ( distance < radii+min.separation | y<r | x<r | 1-y<r | 1-x<r)
		{break}
	else
		{pass=p}

if ( distance < radii+max.separation +.01 ){q=q+1}

				   		 }
	     			 }



if (circle.number>0 & q>c){

x2=x
y2=y
counter=0


repeat{
	
	q=0
	pass2=1
	counter=counter+1
	
		x=runif(1,x2-.003,x2+.005)
		y=runif(1,y2-.005,y2+.005)

	for (p in 1:nrow(circle.data)){
	
	X=circle.data[p,1]
	Y=circle.data[p,2]
	R=circle.data[p,3]
	radii=R+r
	distance= sqrt( (X-x)^2 + (Y-y)^2 )

if ( distance < radii+min.separation | y<r | x<r | 1-y<r | 1-x<r)
		{pass2=0}

if ( distance < radii+max.separation ){q=q+1}
						  }

if((q>c & pass2==1)){break}
if(counter==200){q=0;break}

					}

	 					 }



if (circle.number==0){x=.5;y=.5}

if (pass==nrow(circle.data) & q>c ){
	temp.data=data.frame(x=x,y=y,r=r,label=list2[item])
	circle.data=rbind(circle.data,temp.data)
	circle.number=circle.number+1
	item=item-1
	r=sqrt(list[item]/pi)/zoom
	if (circle.number>1){c=1}
	progress=nrow(circle.data)/length(list)
	grid.edit("progress.box",width=unit(progress,"npc"))
								   
									    }

if (item==0){break}


if (circle.number>0){q=0}

	}



dev.off()
dev.new()
for(d in 1:length(list)){
	color=rainbow(100)[runif(1,1,100)]
	grid.circle(x=circle.data$x[d],y=circle.data$y[d],r=circle.data$r[d],gp=gpar(col=color,fill=color,alpha=0.5))
	grid.text(label=circle.data$label[d],x=circle.data$x[d],y=circle.data$y[d],gp=gpar(fontsize=10))
				}


circle.data = circle.data[order(circle.data$label),]
circle.data <<- circle.data
print(circle.data)
TimeF = round(difftime(Sys.time(),time1,units="secs"),digits=0)
cat("\n","Time elapsed:",TimeF,"seconds","\n","Circle coordinates are saved in circle.data","\n")
decision=readline("\n Export graph as a PDF file ? \n 1. Yes \n 2. No \n")
if (decision==1 | decision=="Yes" | decision=="y"){
			 fn=readline("File name:")
			 dev.copy(pdf,paste(fn,".pdf",sep=""))
			 dev.off()
			 cat("File ",fn,".pdf ","has been saved in the working directory ","\n",sep="") 
			 system(paste("open ",fn,".pdf",sep=""))
		    }

}




