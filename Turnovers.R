
FitTODiff = function(df)
{
  fit = glm(Win ~ TODiff, family=binomial(link="logit"), data = df)
  print(inv.logit (coef(fit)))
  return (fit)
}

fits = lapply(teamList, FitTODiff)

OneGameDrop = function(fit)
{
  drop = inv.logit( coef(fit)[1]) - inv.logit( coef(fit)[1] -coef(fit)[2] )
  return (drop)
}

drops = sapply(fits, OneGameDrop)

plot (drops)

inv.logit( coef(fits[[2]])[1]) - inv.logit( coef(fits[[2]])[1] -coef(fits[[2]])[2] )
inv.logit( coef(fits[[3]])[1]) - inv.logit( coef(fits[[3]])[1] -coef(fits[[3]])[2] )
par(mfrow = c(3,1))

for (iTeam in 1:3)
{
  df = teamList[[iTeam]]
  with (df, plot(jitter(TODiff), jitter(Win, factor=0.5), xlab="Turnover difference", ylab = "Wins", xlim=c(-6,6)))
  fit = glm(df$Win ~ df$TODiff, family=binomial(link="logit"))
  curve( inv.logit(coef(fit)[1] + coef(fit)[2]*x), add=T)
}

par(mfrow = c(1,1))

inv.logit(coef(fit.NE)[1]-2*coef(fit.NE)[2])
inv.logit(coef(fit.NE)[1]-coef(fit.NE)[2])
inv.logit(coef(fit.NE)[1])
inv.logit(coef(fit.NE)[1]+coef(fit.NE)[2])
inv.logit(coef(fit.NE)[1]+coef(fit.NE)[2]*2)
