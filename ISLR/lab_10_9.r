set.seed(1)
x = matrix(rnorm(20*3*50), ncol=50)
x[1:20, 1] = x[1:20, 1] - 4
x[21:40, 2] = x[21:40, 2] + 2
x[41:60, 5] = x[41:60, 5] + 5
pr.out = prcomp(x)
summary(pr.out)
y = c(rep(1,20),rep(2,20),rep(3,20))
plot(pr.out$x[,1:2], col=y)

k3 = kmeans(x,3)$cluster
table(y,k3)
plot(pr.out$x[,1:2], col=k3)

k2 = kmeans(x,2)$cluster
table(y,k2)
plot(pr.out$x[,1:2], col=k2)

k4 = kmeans(x,4)$cluster
table(y,k4)
plot(pr.out$x[,1:2], col=k4)

k3_2 = kmeans(pr.out$x[,1:2],3)$cluster
table(y,k3_2)
plot(pr.out$x[,1:2], col=k3_2)

k3_scale = kmeans(scale(x),3)$cluster
table(y,k3_scale)
plot(pr.out$x[,1:2], col=k3_scale)