glm.train = glm(default~income+balance, data=Default, family = binomial)
glm.pred = rep("No", 10000)
glm.probs = predict(glm.train, Default, type = "response")
glm.pred[glm.probs>.5]= "Yes"
mean((Default$default != glm.pred))

boot.fn = function(data, index){
	glm.train = glm(default~income+balance, data=data, subset=index, family = binomial)
	return(coef(glm.train))
}

df = data.frame(y=y, x=x, x2=x^2,x3=x^3,x4=x^4)
set.seed(11)
summary(glm(y~x, data=df))
score.1 = cv.glm(df, glm.train)$delta[1]
set.seed(11)
glm.train = glm(y~x+x2, data=df)
score.2 = cv.glm(df, glm.train)$delta[1]
set.seed(11)
glm.train = glm(y~x+x2+x3, data=df)
score.3 = cv.glm(df, glm.train)$delta[1]
set.seed(11)
glm.train = glm(y~x+x2+x3+x4, data=df)
score.4 = cv.glm(df, glm.train)$delta[1]

for (i in 1:nrow(Weekly)){
	glm.train = glm(Direction~Lag1+Lag2, data=Weekly, family = binomial, subset = -i)
	pred = predict(glm.train, Weekly[i,], type="response") > 0.5
	if(pred)
		glm.pred[i] = "Up"
}
