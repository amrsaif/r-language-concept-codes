########## Sample generation with 95% accuracy (logical function can be modified here) ##########
n1=sample(c(0,1),200,rep=T)
n2=sample(c(0,1),200,rep=T)
o=xor(n1,n2)
rand=sample(1:200,10)
o[rand]=!o[rand]
d=data.frame(input1=n1,input2=n2,output=as.numeric(o))

########## Function for a 2-3-2 neural network ##########
runn=function(x,y){
	### Input layer ###
	I1= x
	I2= y

	### Hidden layer with ReLu activation function ###
	A1= max(00, (I1 * I1A1w) + (I2 * I2A1w) +B[1]) 
	A2= max(00, (I1 * I1A2w) + (I2 * I2A2w) +B[2])
	A3= max(00, (I1 * I1A3w) + (I2 * I2A3w) +B[3])

	### Output layer ###
	OT= (A1 * A1Tw) + (A2 * A2Tw) + (A3 * A3Tw) +B[4]
	OF= (A1 * A1Fw) + (A2 * A2Fw) + (A3 * A3Fw) +B[5]

I1<<-I1; I2<<-I2; A1<<-A1; A2<<-A2; A3<<-A3; OT<<-OT; OF<<-OF
			}


########## 50 trials before failing ##########
for (mm in 1:50){
E1.graph= E2.graph= graph= v.graph= E.mean.graph= numeric()

### Assigning random weights and biases 
rn=function(x){runif(1,-2,2)} 
	I1A1w= rn()
	I1A2w= rn()
	I1A3w= rn()
	I2A1w= rn()
	I2A2w= rn()
	I2A3w= rn()
	A1Tw= rn()
	A1Fw= rn()
	A2Tw= rn()
	A2Fw= rn()
	A3Tw= rn()
	A3Fw= rn()

	B= runif(5,-2,2)

### Assigning learning rate
	rate= .01
	

########## 50 Epochs ##########	
for (Epoch in 1:50){


########## Training with the first 100 samples (100 iterations) ##########
E=0
for (S in 1:100){
runn(d$input1[S],d$input2[S])


########## Calculating network values and output errors ########## 			
A1net= (I1 * I1A1w) + (I2 * I2A1w)
A2net= (I1 * I1A2w) + (I2 * I2A2w)
A3net= (I1 * I1A3w) + (I2 * I2A3w)
E1= ((OT-d$output[S])^2)/2
E2= ((OF-!(d$output[S]))^2)/2

########## Calculating partial derivatives ###############
## partial derivatives of errors in respect to outputs
	der.E1.OT= OT-d$output[S]
	der.E2.OF= OF-as.numeric(!d$output[S])

## partial derivatives of node outputs in respect to net node inputs
	der.A1.A1net= ifelse(A1net<=0,0,1)
	der.A2.A2net= ifelse(A2net<=0,0,1)
	der.A3.A3net= ifelse(A3net<=0,0,1)

## partial derivatives of net inputs in respect to weights
	der.OT.A1Tw= A1
	der.OT.A2Tw= A2
	der.OT.A3Tw= A3
	der.OF.A1Fw= A1
	der.OF.A2Fw= A2
	der.OF.A3Fw= A3
	der.A1net.I1A1w= I1
	der.A1net.I2A1w= I2
	der.A2net.I1A2w= I1
	der.A2net.I2A2w= I2
	der.A3net.I1A3w= I1
	der.A3net.I2A3w= I2

## partial derivatives of net inputs in respect to outputs in prev. layer
	der.OT.A1= A1Tw
	der.OF.A1= A1Fw
	der.OT.A2= A2Tw
	der.OF.A2= A2Fw
	der.OT.A3= A3Tw
	der.OF.A3= A3Fw

## partial derivatives of errors in respect to outputs in hidden layer
	der.E1.A1= der.E1.OT * der.OT.A1 
	der.E2.A1= der.E2.OF * der.OF.A1
	der.E1.A2= der.E1.OT * der.OT.A2 
	der.E2.A2= der.E2.OF * der.OF.A2
	der.E1.A3= der.E1.OT * der.OT.A3 
	der.E2.A3= der.E2.OF * der.OF.A3
	der.Etotal.A1= der.E1.A1 + der.E2.A1
	der.Etotal.A2= der.E1.A2 + der.E2.A2
	der.Etotal.A3= der.E1.A3 + der.E2.A3

## partial derivatives of errors in respect to weights
	der.E1.A1Tw= der.E1.OT * der.OT.A1Tw
	der.E1.A2Tw= der.E1.OT * der.OT.A2Tw
	der.E1.A3Tw= der.E1.OT * der.OT.A3Tw														
	der.E2.A1Fw= der.E2.OF * der.OF.A1Fw
	der.E2.A2Fw= der.E2.OF * der.OF.A2Fw
	der.E2.A3Fw= der.E2.OF * der.OF.A3Fw
	der.Etotal.I1A1w= der.Etotal.A1 * der.A1.A1net * der.A1net.I1A1w
	der.Etotal.I2A1w= der.Etotal.A1 * der.A1.A1net * der.A1net.I2A1w
	der.Etotal.I1A2w= der.Etotal.A2 * der.A2.A2net * der.A2net.I1A2w
	der.Etotal.I2A2w= der.Etotal.A2 * der.A2.A2net * der.A2net.I2A2w
	der.Etotal.I1A3w= der.Etotal.A3 * der.A3.A3net * der.A3net.I1A3w
	der.Etotal.I2A3w= der.Etotal.A3 * der.A3.A3net * der.A3net.I2A3w

## partial derivatives of errors in respect to biases 
	der.Etotal.B1= der.Etotal.A1 * der.A1.A1net * 1 # (derivative of net node input in respect to bias = 1)
	der.Etotal.B2= der.Etotal.A2 * der.A2.A2net * 1
	der.Etotal.B3= der.Etotal.A3 * der.A3.A3net * 1
	der.E1.B4= der.E1.OT * 1
	der.E2.B5= der.E2.OF * 1


########### Updating weights and biases #############
	I1A1w= I1A1w - (rate * der.Etotal.I1A1w)
	I2A1w= I2A1w - (rate * der.Etotal.I2A1w)
	I1A2w= I1A2w - (rate * der.Etotal.I1A2w)
	I2A2w= I2A2w - (rate * der.Etotal.I2A2w)
	I1A3w= I1A3w - (rate * der.Etotal.I1A3w)
	I2A3w= I2A3w - (rate * der.Etotal.I2A3w)
	A1Tw= A1Tw - (rate * der.E1.A1Tw)
	A2Tw= A2Tw - (rate * der.E1.A2Tw)
	A3Tw= A3Tw - (rate * der.E1.A3Tw)
	A1Fw= A1Fw - (rate * der.E2.A1Fw)
	A2Fw= A2Fw - (rate * der.E2.A2Fw)
	A3Fw= A3Fw - (rate * der.E2.A3Fw)
	B[1]=B[1] - (rate * der.Etotal.B1)
	B[2]=B[2] - (rate * der.Etotal.B2)
	B[3]=B[3] - (rate * der.Etotal.B3)
	B[4]=B[4] - (rate * der.E1.B4)
	B[5]=B[5] - (rate * der.E2.B5)


########## Adding errors to error vectors (E= total error, E1= output 1 error, E2= output 2 error) ##########
E=c(E,E1+E2)
E1.graph=c(E1.graph,E1)
E2.graph=c(E2.graph,E2)

			         }

E.mean.graph=c(E.mean.graph,mean(E))


########## Evaluating the training set ##########
counter=0
for (TS in 1:100){

runn(d$input1[TS],d$input2[TS])
output=ifelse(OT>OF,1,0)
if (output== d$output[TS]) {counter= counter+1}

			 }

graph=c(graph,counter)


########## Validation with next 100 samples (validation set) ##########
v.counter=0
for (VS in 101:200){

runn(d$input1[VS], d$input2[VS])
output=ifelse(OT>OF,1,0)
if (output== d$output[VS]) {v.counter= v.counter+1}
			  }
			    		  
v.graph=c(v.graph,v.counter)


### break loop when error signal converges
if(length(E.mean.graph)>10 & var(tail(E.mean.graph,10))< 1e-5){break}
      	   }
      			
      				
### assigning acceptable error value
					
if(mean(E)<.1){break}

}


########## Neural network diagrams ##########
library(grid)
dev.new()
vc=0
for (I1 in 0:1){
	for (I2 in 0:1){

runn(I1,I2)

vp=list(viewport(0,1,0.5,0.5,just=c("left","top")),
		    viewport(1,1,0.5,0.5,just=c("right","top")),
		    viewport(0,0,0.5,0.5,just=c("left","bottom")),
        viewport(1,0,0.5,0.5,just=c("right","bottom")))
vc=vc+1
pushViewport(vp[[vc]])
grid.rect()

A.max=max(c(A1,A2,A3))
A.min=min(c(A1,A2,A3))
w.list=c(I1A1w, I2A1w, I1A2w, I2A2w, I1A3w, I2A3w, A1Tw, A2Tw, A3Tw, A1Fw, A2Fw, A3Fw)
c1=ifelse(I1==1,"black","white")
c2=ifelse(I2==1,"black","white")
line.weight=function(x){ ((x-min(w.list)) / (max(w.list)-min(w.list))) * 8 +1 }
circle.thk=function(x){ ((x-min(B)) / (max(B)-min(B)))* 5 }
col.fn=function(x){ heat.colors(20)[((x-A.min)/((A.max-A.min+.0001))) * 19 +1] }

l.crd=matrix(c(0.10, 0.50, 0.60, 0.75, 0.10, 0.50, 0.60, 0.25, 0.10, 0.50, 0.40, 0.50, 0.10, 0.50, 0.60, 0.50, 0.10,
	0.50, 0.40, 0.75, 0.10, 0.50, 0.40, 0.25, 0.50, 0.90, 0.75, 0.60, 0.50, 0.90, 0.50, 0.60, 0.50, 0.90,
	0.25, 0.60, 0.50, 0.90, 0.75, 0.40, 0.50, 0.90, 0.50, 0.40, 0.50, 0.90, 0.25, 0.40),ncol=4,byrow=T)

t.crd=matrix(c("I1",.1,.6,"I2",.1,.4,"A1",.5,.75,"A2",.5,.5,"A3",.5,.25,"1",.9,.6,"0",.9,.4,"Input layer",
	.1,.9,"Hidden layer",0.5,.9,"Output layer",.9,.9),ncol=3,byrow=T)

for (ln in 1:12){grid.lines(c(l.crd[ln,1],l.crd[ln,2]),c(l.crd[ln,3],l.crd[ln,4]),gp=gpar(lwd=line.weight(w.list[ln])))}

grid.circle(.1,.6,.05,gp=gpar(fill=c1))
grid.circle(.1,.4,.05,gp=gpar(fill=c2))
grid.circle(.5,.75,.05,gp=gpar(fill=col.fn(A1),lwd=circle.thk(B[1])))
grid.circle(.5,.5,.05,gp=gpar(fill=col.fn(A2),lwd=circle.thk(B[2])))
grid.circle(.5,.25,.05,gp=gpar(fill=col.fn(A3),lwd=circle.thk(B[3])))
grid.circle(.9,.6,.05,gp=gpar(fill=heat.colors(20)[ifelse(OT>OF,20,1)],lwd=circle.thk(B[4])))
grid.circle(.9,.4,.05,gp=gpar(fill=heat.colors(20)[ifelse(OT>OF,1,20)],lwd=circle.thk(B[5])))

for (tx in 1:10){grid.text(label=t.crd[tx,1],as.numeric(t.crd[tx,2]),as.numeric(t.crd[tx,3]),gp=gpar(col="gray33",cex=.75))}

upViewport()
					}
				}


########## Gradient descent and accuracy graphs ##########
dev.new()
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))

plot(E.mean.graph,type="l",lwd=2,col="blue",ylab="Mean error",xlab="Epoch",main="Gradient descent",ylim=c(0,max(E.mean.graph)))

plot(graph,type="s", xlab="Epoch",ylab="Percentage of correct predictions", col=c("blue","red"), main="Accuracy",ylim=c(0,100),lwd=3)
lines(v.graph,type="s",col="red")
legend(unit(1,"npc"),unit(25,"npc"),legend=c("Training set","Validation set"),col=c("blue","red"),lty=c(1,1),lwd=c(3,1),cex=.9)

plot(E1.graph,ylim=c(0,1),pch=".",main="Output 1 error",ylab="1/2 X squared difference", xlab="Iteration (1 sample per iteration)")

plot(E2.graph,ylim=c(0,1),pch=".",main="Output 2 error",ylab="1/2 X squared difference", xlab="Iteration (1 sample per iteration)")


########## Print number of restarts and accuracy ##########
cat("Number of restarts= ",mm-1,"\n")
cat("Accuracy= ",v.counter,"\n")
