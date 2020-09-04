#
spde = inla.spde2.matern(mesh=mesh,alpha=2)
#
field = mesh$idx$loc
formula = logz ~ w + w2 + f(field,model=spde)
#
result = inla(formula,data=data.frame(data)[data$year==2013,],family="gaussian",verbose=F)
summary(result)
#
result.field = inla.spde2.result(result, "field", spde)
names(result.field)
#
(inla.kappa = inla.emarginal(function(x) x, result.field$marginals.kappa[[1]]))
(inla.psill = inla.emarginal(function(x) x, result.field$marginals.variance.nominal[[1]]))
(inla.range = inla.emarginal(function(x) x, result.field$marginals.range.nominal[[1]]))
(inla.tau   = 1/(sqrt(4*pi)*inla.kappa*inla.psill))
(inla.nugget = 1/summary(result)$hyperpar$mean[1]) # 1/precision
#
plot(result.field$marginals.range.nominal$range.nominal.1,type='l')
range.mode = result.field$marginals.range.nominal$range.nominal.1[
	result.field$marginals.range.nominal$range.nominal.1[,"y"]==
		max(result.field$marginals.range.nominal$range.nominal.1[,"y"]),"x"]
abline(v=range.mode,col=2)
abline(v=inla.range,col=3)
#
plot(result.field$marginals.variance.nominal$variance.nominal.1,type='l')
var.mode = result.field$marginals.variance.nominal$variance.nominal.1[
	result.field$marginals.variance.nominal$variance.nominal.1[,"y"]==
		max(result.field$marginals.variance.nominal$variance.nominal.1[,"y"]),"x"]
abline(v=var.mode,col=2)
abline(v=inla.psill,col=3)
#
red.inla = vgm(model="Mat",psill=inla.psill,range=inla.range,nugget=inla.nugget)
plot(red.vario,red.eye,pch=20,cex=vario.red$np/50000)
