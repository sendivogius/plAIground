pr.out = prcomp(USArrests , scale =TRUE)
biplot (pr.out , scale =0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot (pr.out , scale =0)
pr.var =pr.out$sdev ^2
pve=pr.var/sum(pr.var )
plot(pve , xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1) ,type="b")
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,type="b")