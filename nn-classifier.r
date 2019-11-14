########## Sample generation ##########
x=runif(200)		
y=runif(200)	
o=xor(y>x, y+x>1) #(envelope shape classification)
d=data.frame(input1=x, input2=y, output=as.numeric(o))

b.B=c(0,0,0,0)
accuracies=vector()
library(grid)

########## Function for a 2-3-1 neural network ##########
runn=function(x,y){
	### Input layer ###
	I1= x
	I2= y

	### Hidden layer ###
	A1= max(0, (I1 * I1A1w) + (I2 * I2A1w)  +B[1])
	A2= max(0, (I1 * I1A2w) + (I2 * I2A2w)  +B[2])
	A3= max(0, (I1 * I1A3w) + (I2 * I2A3w)  +B[3])

	### Output layer ###
	OT= (A1 * A1Tw) + (A2 * A2Tw) + (A3 * A3Tw)  +B[4]

I1<<-I1; I2<<-I2; A1<<-A1; A2<<-A2; A3<<-A3; OT<<-OT		
			}

########## 200 trials ##########
dev.new(width=10,height=.5,xpos=250,ypos=300)
box=rectGrob(name="box",0,0,0,1,just=c("left","bottom"),gp=gpar(col=NA,fill="blue"))
text=textGrob(name="text",paste(0,"%"),gp=gpar(col="gray"))
grid.draw(box)
grid.draw(text)

for (mm in 1:200){

E1.graph=graph=v.graph=E.mean.graph= numeric()

### Assigning random weights and biases 
rn=function(x) {sample(c(-2,2),1,rep=T)}
	I1A1w= rn()
	I1A2w= rn()
	I1A3w= rn()
	I2A1w= rn()
	I2A2w= rn()
	I2A3w= rn()
	A1Tw= rn()
	A2Tw= rn()
	A3Tw= rn()

	B= sample(c(-.1,.1),4,rep=T)	

### Assigning learning rate
	rate=  .02

########## Epochs (maximum)##########	
for (Epoch in 1:25){


########## training with the first 100 samples (100 iterations) ##########
E=0
for (S in 1:100){
	
runn(d$input1[S],d$input2[S])


########## Calculating network values ########## 			
A1net= (I1 * I1A1w) + (I2 * I2A1w)
A2net= (I1 * I1A2w) + (I2 * I2A2w)
A3net= (I1 * I1A3w) + (I2 * I2A3w)
OTnet= (A1 * A1Tw) + (A2 * A2Tw) + (A3 * A3Tw)
E1= ((OT-d$output[S])^2)/2

########## Calculating partial derivatives ###############
## partial derivatives of errors in respect to outputs
	der.E1.OT= OT-d$output[S]

## partial derivatives of outputs in respect to net inputs
	der.OT.OTnet= 1 	
	der.A1.A1net= ifelse(A1net<=0,0,1)
	der.A2.A2net= ifelse(A2net<=0,0,1)
	der.A3.A3net= ifelse(A3net<=0,0,1)

## partial derivatives of net inputs in respect to weights
	der.OTnet.A1Tw= A1
	der.OTnet.A2Tw= A2
	der.OTnet.A3Tw= A3
	der.A1net.I1A1w= I1
	der.A1net.I2A1w= I2
	der.A2net.I1A2w= I1
	der.A2net.I2A2w= I2
	der.A3net.I1A3w= I1
	der.A3net.I2A3w= I2

## partial derivatives of net inputs in respect to outputs in prev. layer
	der.OTnet.A1= A1Tw
	der.OTnet.A2= A2Tw
	der.OTnet.A3= A3Tw

## partial derivatives of errors in respect to inputs in last layer
	der.E1.OTnet= der.E1.OT * der.OT.OTnet

## partial derivatives of errors in respect to outputs in hidden layer
	der.E1.A1= der.E1.OTnet * der.OTnet.A1 
	der.E1.A2= der.E1.OTnet * der.OTnet.A2 
	der.E1.A3= der.E1.OTnet * der.OTnet.A3 

## partial derivatives of errors in respect to weights
	der.E1.A1Tw= der.E1.OTnet * der.OTnet.A1Tw
	der.E1.A2Tw= der.E1.OTnet * der.OTnet.A2Tw
	der.E1.A3Tw= der.E1.OTnet * der.OTnet.A3Tw														

	der.E1.I1A1w= der.E1.A1 * der.A1.A1net * der.A1net.I1A1w
	der.E1.I2A1w= der.E1.A1 * der.A1.A1net * der.A1net.I2A1w
	der.E1.I1A2w= der.E1.A2 * der.A2.A2net * der.A2net.I1A2w
	der.E1.I2A2w= der.E1.A2 * der.A2.A2net * der.A2net.I2A2w
	der.E1.I1A3w= der.E1.A3 * der.A3.A3net * der.A3net.I1A3w
	der.E1.I2A3w= der.E1.A3 * der.A3.A3net * der.A3net.I2A3w

## partial derivatives of errors in respect to biases
	der.E1.B1= der.E1.A1 * der.A1.A1net * 1
	der.E1.B2= der.E1.A2 * der.A2.A2net * 1
	der.E1.B3= der.E1.A3 * der.A3.A3net * 1
	der.E1.B4= der.E1.OT * der.OT.OTnet * 1

########### Updating weights and biases #############
	I1A1w= I1A1w - (rate * der.E1.I1A1w)
	I2A1w= I2A1w - (rate * der.E1.I2A1w)
	I1A2w= I1A2w - (rate * der.E1.I1A2w)
	I2A2w= I2A2w - (rate * der.E1.I2A2w)
	I1A3w= I1A3w - (rate * der.E1.I1A3w)
	I2A3w= I2A3w - (rate * der.E1.I2A3w)
	A1Tw= A1Tw - (rate * der.E1.A1Tw)
	A2Tw= A2Tw - (rate * der.E1.A2Tw)
	A3Tw= A3Tw - (rate * der.E1.A3Tw)

	B[1]=B[1] - (rate * der.E1.B1)
	B[2]=B[2] - (rate * der.E1.B2)
	B[3]=B[3] - (rate * der.E1.B3)
	B[4]=B[4] - (rate * der.E1.B4)

### Adding errors to error vectors
E=c(E,E1)
E1.graph=c(E1.graph,E1)

			         }

E.mean.graph=c(E.mean.graph,mean(E))

########## Evaluating the training set ##########
counter=0
for (TS in 1:100){
runn(d$input1[TS],d$input2[TS])
output=ifelse(OT>.5,1,0)
if (output== d$output[TS]) {counter= counter+1}
			 }

graph=c(graph,counter)

########## validation with next 100 samples ##########
v.counter=0
for (VS in 101:200){
runn(d$input1[VS], d$input2[VS])
output=ifelse(OT>.5,1,0)
if (output== d$output[VS]) {v.counter= v.counter+1}
			  }
			    		  
v.graph=c(v.graph,v.counter)

if(length(E.mean.graph)>10 & var(tail(E.mean.graph,10))<1e-4 ){break}

      	   }

####### Best result updating ###########   
accuracies=c(accuracies,v.counter)  			
 if(v.counter >= max(accuracies)){

	b.I1A1w= I1A1w 
	b.I2A1w= I2A1w 
	b.I1A2w= I1A2w 
	b.I2A2w= I2A2w 
	b.I1A3w= I1A3w 
	b.I2A3w= I2A3w 
	b.A1Tw= A1Tw 
	b.A2Tw= A2Tw 
	b.A3Tw= A3Tw 

	b.B[1]=B[1]
	b.B[2]=B[2] 
	b.B[3]=B[3]
	b.B[4]=B[4]

	b.E1.graph= E1.graph
	b.E.mean.graph= E.mean.graph
	b.graph= graph
	b.v.graph= v.graph
	b.v.counter= v.counter

######## testing best neural network (graph No.5) ########
	test=matrix(ncol=3)
	for (rr in 1:2000){
	x=runif(1)
	y=runif(1)
	runn(x,y)
	output=ifelse(OT>.5,1,0)
	test=rbind(test,c(x,y,output))
				}

    					}
	
######## updating progress ########	
grid.edit("box",unit(0,"npc"),unit(0,"npc"),just=c("left","bottom"),width=unit(mm/200,"npc"))
grid.edit("text",label=paste(trunc(mm/2),"%"))

}

dev.off()

########## Gradient descent and accuracy graphs ##########
dev.new(height=35/6,width=7)

layout(heights=c(1,2/3),mat=matrix(c(1,1,1,2,2,2,3,3,4,4,5,5),2,6,byrow=TRUE))

plot(b.E.mean.graph,type="l",lwd=2,col="blue",ylab="Mean error",xlab="Epoch",main="Gradient descent",ylim=c(0,max(b.E.mean.graph)))
plot(b.graph,type="s", xlab="Epoch",ylab="Percentage of correct predictions", col=c("blue","red"), main="Accuracy",ylim=c(0,100),lwd=3)
lines(b.v.graph,type="s",col="red")
legend(unit(1,"npc"),unit(20,"npc"),legend=c("Training set","Validation set"),col=c("blue","red"),lty=c(1,1),lwd=c(3,1),cex=.9)
plot(b.E1.graph,ylim=c(0,1),pch=".",main="Sample error",ylab="1/2 X squared difference", xlab="Iteration")
hist(accuracies,xlab="Accuracy",col="gray",main="Histogram of trial accuracies")
plot(test[,1],test[,2],col=test[,3]+1,main="Test",xlab="input1",ylab="input2",pch=20)

### Print accuracy and kappa statistic
cat("Accuracy= ",b.v.counter/100,"\n")
cat("Kappa statistic= ",(b.v.counter-50)/50,"\n")
