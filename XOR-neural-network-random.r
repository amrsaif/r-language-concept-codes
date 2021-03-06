		### Sample generation with 95% accuracy (logical function can be modified here) ###
n1=sample(c(0,1),200,rep=T)
n2=sample(c(0,1),200,rep=T)
o=xor(n1,n2)
rand=sample(1:200,10)
o[rand]=!o[rand]
d=data.frame(input1=n1,input2=n2,output=o)


		### 1000 restarts before failing ###
cc=0
for (mm in 1:1000){
max=0
graph=numeric()
v.graph=numeric()


		### random weight assigning ###
	I1A1w=runif(1,0,1)
	I1A2w=runif(1,0,1)
	I1A3w=runif(1,0,1)

	I2A1w=runif(1,0,1)
	I2A2w=runif(1,0,1)
	I2A3w=runif(1,0,1)

	A1Tw=runif(1,0,1)
	A1Fw=runif(1,0,1)

	A2Tw=runif(1,0,1)
	A2Fw=runif(1,0,1)

	A3Tw=runif(1,0,1)
	A3Fw=runif(1,0,1)


		### 100 Iterations before restarting ###
rate= sample(c(0.1,-0.1),12,rep=T)
for (it in 1:100){

	I1A1w= I1A1w + rate[1]
	I1A2w= I1A2w + rate[2]
	I1A3w= I1A3w + rate[3]

	I2A1w= I2A1w + rate[4]
	I2A2w= I2A2w + rate[5]
	I2A3w= I2A3w + rate[6]

	A1Tw= A1Tw + rate[7]
	A1Fw= A1Fw + rate[8]

	A2Tw= A2Tw + rate[9]
	A2Fw= A2Fw + rate[10]

	A3Tw= A3Tw + rate[11]
	A3Fw= A3Fw + rate[12]

	counter = 0


		### training with the first 100 samples ###
for (sample in 1:100){

			### Input layer ###
I1= d$input1[sample]
I2= d$input2[sample]

			### Hidden layer ###
A1= max(0.5, (I1 * I1A1w) + (I2 * I2A1w)  )
A2= max(0.5, (I1 * I1A2w) + (I2 * I2A2w)  )
A3= max(0.5, (I1 * I1A3w) + (I2 * I2A3w)  )

			### Output layer ###
OT= max(0.5, (A1 * A1Tw) + (A2 * A2Tw) + (A3 * A3Tw)  )
OF= max(0.5, (A1 * A1Fw) + (A2 * A2Fw) + (A3 * A3Fw)  )

if (OT>OF) {output = 1} else if (OT<OF) {output = 0} else {output = 2}

if (output== d$output[sample]) {counter= counter+1}

			         }


if (counter > max){max=counter} else {
	

	I1A1w= I1A1w - rate[1]
	I1A2w= I1A2w - rate[2]
	I1A3w= I1A3w - rate[3]

	I2A1w= I2A1w - rate[4]
	I2A2w= I2A2w - rate[5]
	I2A3w= I2A3w - rate[6]

	A1Tw= A1Tw - rate[7]
	A1Fw= A1Fw - rate[8]

	A2Tw= A2Tw - rate[9]
	A2Fw= A2Fw - rate[10]

	A3Tw= A3Tw - rate[11]
	A3Fw= A3Fw - rate[12]

	
	rate= sample(c(.1,-.1),12,rep=T)

									 }

graph=c(graph,max)


		### validation with next 100 samples ###
v.counter=0
for (v.sample in 101:200){

I1= d$input1[v.sample]
I2= d$input2[v.sample]

A1= max(0.5,  (I1 * I1A1w) + (I2 * I2A1w)  )
A2= max(0.5,	(I1 * I1A2w) + (I2 * I2A2w)  )
A3= max(0.5,	(I1 * I1A3w) + (I2 * I2A3w)  )

OT= max(0.5,  (A1 * A1Tw) + (A2 * A2Tw) + (A3 * A3Tw)  )
OF= max(0.5,  (A1 * A1Fw) + (A2 * A2Fw) + (A3 * A3Fw)  )

if (OT>OF) {output = 1} else if (OT<OF) {output = 0} else {output = 2}

if (output== d$output[v.sample]) {v.counter= v.counter+1}

			    		  }
			    		  
v.graph=c(v.graph,v.counter)


      				}
      				
      				
		### assigning accuracy cut-off point (90%) ###
cc=cc+1 
if (max > 90){break}


}


		### Testing passed neural network ###
d2=data.frame(Input1=c(0,1,0,1),Input2=c(0,0,1,1))
Output=vector()

for (sample in 1:4){

I1= d2$Input1[sample]
I2= d2$Input2[sample]

A1= max(0.5,  (I1 * I1A1w) + (I2 * I2A1w)  )
A2= max(0.5,	(I1 * I1A2w) + (I2 * I2A2w)  )
A3= max(0.5,	(I1 * I1A3w) + (I2 * I2A3w)  )

OT= max(0.5,  (A1 * A1Tw) + (A2 * A2Tw) + (A3 * A3Tw)  )
OF= max(0.5,  (A1 * A1Fw) + (A2 * A2Fw) + (A3 * A3Fw)  )

if (OT>OF) {output = 1} else {output = 0}

Output=c(Output,output)
test.result=cbind(matrix(c(0,1,0,1,0,0,1,1,Output),ncol=3))

					}


		### Training/validation plot ###
plot(graph,type="s", xlab="Iteration",ylab="Percentage of correct predictions", col=c("blue","red"), main="Training",ylim=c(0,100),lwd=3)
lines(v.graph,type="s",col="red")
legend(60,50,legend=c("Training set","Validation set"),col=c("blue","red"),lty=c(1,1),lwd=c(3,1))
legend(55,35,title="Test",legend=c("Input1",test.result[1:4],"Input2",test.result[5:8],"Output",test.result[9:12]),ncol=3L)


		### Neural network diagrams with different input combinations ###
library(grid)
dev.new()

for (I1 in 0:1){
	for (I2 in 0:1){

if (I1==0 & I2==0) {vp=viewport(0,1,.5,.5,just=c("left","top"))}
if (I1==0 & I2==1) {vp=viewport(1,1,.5,.5,just=c("right","top"))}
if (I1==1 & I2==0) {vp=viewport(0,0,.5,.5,just=c("left","bottom"))}
if (I1==1 & I2==1) {vp=viewport(1,0,.5,.5,just=c("right","bottom"))}

pushViewport(vp)

A1= max(0.5,  (I1 * I1A1w) + (I2 * I2A1w)  )
A2= max(0.5,	(I1 * I1A2w) + (I2 * I2A2w)  )
A3= max(0.5,	(I1 * I1A3w) + (I2 * I2A3w)  )

OT= max(0.5,  (A1 * A1Tw) + (A2 * A2Tw) + (A3 * A3Tw)  )
OF= max(0.5,  (A1 * A1Fw) + (A2 * A2Fw) + (A3 * A3Fw)  )

max=max(c(A1,A2,A3))
min=min(c(A1,A2,A3))

if(I1==1){c1="black"}else{c1="white"}
if(I2==1){c2="black"}else{c2="white"}

grid.rect()

grid.text(label="Input layer",.1,.9,gp=gpar(cex=.7))
grid.text(label="Hidden layer",.5,.9,gp=gpar(cex=.7))
grid.text(label="Output layer",.9,.9,gp=gpar(cex=.7))

grid.lines(c(.1,.5),c(.6,.75),gp=gpar(lwd=(I1A1w+1)*2))
grid.lines(c(.1,.5),c(.6,.5),gp=gpar(lwd=(I1A2w+1)*2))
grid.lines(c(.1,.5),c(.6,.25),gp=gpar(lwd=(I1A3w+1)*2))
grid.lines(c(.1,.5),c(.4,.75),gp=gpar(lwd=(I2A1w+1)*2))
grid.lines(c(.1,.5),c(.4,.5),gp=gpar(lwd=(I2A2w+1)*2))
grid.lines(c(.1,.5),c(.4,.25),gp=gpar(lwd=(I2A3w+1)*2))

grid.lines(c(.5,.9),c(.75,.6),gp=gpar(lwd=(A1Tw+1)*2))
grid.lines(c(.5,.9),c(.5,.6),gp=gpar(lwd=(A2Tw+1)*2))
grid.lines(c(.5,.9),c(.25,.6),gp=gpar(lwd=(A3Tw+1)*2))
grid.lines(c(.5,.9),c(.75,.4),gp=gpar(lwd=(A1Fw+1)*2))
grid.lines(c(.5,.9),c(.5,.4),gp=gpar(lwd=(A2Fw+1)*2))
grid.lines(c(.5,.9),c(.25,.4),gp=gpar(lwd=(A3Fw+1)*2))

grid.circle(.1,.6,.05,gp=gpar(fill=c1))
grid.text(label="I1",.1,.6,gp=gpar(col="gray",cex=1))
grid.circle(.1,.4,.05,gp=gpar(fill=c2))
grid.text(label="I2",.1,.4,gp=gpar(col="gray",cex=1))

grid.circle(.5,.75,.05,gp=gpar(fill=heat.colors(21)[((A1-min)*(20/(max-min+.001)))+1]))
grid.text(label="A1",.5,.75,gp=gpar(col="gray",cex=1))
grid.circle(.5,.5,.05,gp=gpar(fill=heat.colors(21)[((A2-min)*(20/(max-min+.001)))+1]))
grid.text(label="A2",.5,.5,gp=gpar(col="gray",cex=1))
grid.circle(.5,.25,.05,gp=gpar(fill=heat.colors(21)[((A3-min)*(20/(max-min+.001)))+1]))
grid.text(label="A3",.5,.25,gp=gpar(col="gray",cex=1))

grid.circle(.9,.6,.05,gp=gpar(fill=heat.colors(21)[((OT-min(OT,OF))*(20/abs(OT-OF)))+1]))
grid.text(label="1",.9,.6,gp=gpar(col="gray",cex=1))
grid.circle(.9,.4,.05,gp=gpar(fill=heat.colors(21)[((OF-min(OT,OF))*(20/abs(OT-OF)))+1]))
grid.text(label="0",.9,.4,gp=gpar(col="gray",cex=1))

upViewport()
					}
				}
 
cat("Number of restarts= ",cc,"\n")
