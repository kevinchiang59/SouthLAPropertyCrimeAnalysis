summary(lm(formula = PartI ~ Liquor + Marijuana + Tobacco + Population, data = popDF))
summary(lm(formula = PartI ~ Liquor*Population + Marijuana*Population + Tobacco*Population, data = popDF))
summary(lm(formula = PartI ~ Liquor * Marijuana * Tobacco + Population, data = popDF))
summary(lm(formula = PartI ~ Liquor * Marijuana * Tobacco * Population, data = popDF))
summary(lm(formula = PartI ~ Liquor + Marijuana + Tobacco, data = popDF))
summary(lm(formula = PartI ~ Liquor * Marijuana * Tobacco, data = popDF))

summary(lm(formula = PartII ~ Liquor + Marijuana + Tobacco + Population, data = popDF))
summary(lm(formula = PartII ~ Liquor*Population + Marijuana*Population + Tobacco*Population, data = popDF))
summary(lm(formula = PartII ~ Liquor * Marijuana * Tobacco + Population, data = popDF))
summary(lm(formula = PartII ~ Liquor * Marijuana * Tobacco * Population, data = popDF))
summary(lm(formula = PartII ~ Liquor + Marijuana + Tobacco, data = popDF))
summary(lm(formula = PartII ~ Liquor * Marijuana * Tobacco, data = popDF))

summary(lm(formula = PartI ~ stdLiquor + stdMarijuana + stdTobacco + stdPopulation, data = popDF))
summary(lm(formula = PartI ~ stdLiquor*stdPopulation + stdMarijuana*stdPopulation + stdTobacco*stdPopulation, data = popDF))
summary(lm(formula = PartI ~ stdLiquor * stdMarijuana * stdTobacco + stdPopulation, data = popDF))
summary(lm(formula = PartI ~ stdLiquor * stdMarijuana * stdTobacco * stdPopulation, data = popDF))
summary(lm(formula = PartI ~ stdLiquor + stdMarijuana + stdTobacco, data = popDF))
summary(lm(formula = PartI ~ stdLiquor * stdMarijuana * stdTobacco, data = popDF))

summary(lm(formula = PartII ~ stdLiquor + stdMarijuana + stdTobacco + stdPopulation, data = popDF))
summary(lm(formula = PartII ~ stdLiquor*stdPopulation + stdMarijuana*stdPopulation + stdTobacco*stdPopulation, data = popDF))
summary(lm(formula = PartII ~ stdLiquor * stdMarijuana * stdTobacco + stdPopulation, data = popDF))
summary(lm(formula = PartII ~ stdLiquor * stdMarijuana * stdTobacco * stdPopulation, data = popDF))
summary(lm(formula = PartII ~ stdLiquor + stdMarijuana + stdTobacco, data = popDF))
summary(lm(formula = PartII ~ stdLiquor * stdMarijuana * stdTobacco, data = popDF))

popDF$cLiquor <- popDF$Liquor-mean(popDF$Liquor)
popDF$cMarijuana <- popDF$Marijuana-mean(popDF$Marijuana)
popDF$cTobacco <- popDF$Tobacco-mean(popDF$Tobacco)
popDF$cPopulation <- popDF$Population-mean(popDF$Population)

summary(lm(formula = PartI ~ cLiquor + cMarijuana + cTobacco + cPopulation, data = popDF))
summary(lm(formula = PartI ~ cLiquor*cPopulation + cMarijuana*cPopulation + cTobacco*cPopulation, data = popDF))
summary(lm(formula = PartI ~ cLiquor * cMarijuana * cTobacco + cPopulation, data = popDF))
summary(lm(formula = PartI ~ cLiquor * cMarijuana * cTobacco * cPopulation, data = popDF))
summary(lm(formula = PartI ~ cLiquor + cMarijuana + cTobacco, data = popDF))
summary(lm(formula = PartI ~ cLiquor * cMarijuana * cTobacco, data = popDF))

summary(lm(formula = PartII ~ cLiquor + cMarijuana + cTobacco + cPopulation, data = popDF))
summary(lm(formula = PartII ~ cLiquor*cPopulation + cMarijuana*cPopulation + cTobacco*cPopulation, data = popDF))
summary(lm(formula = PartII ~ cLiquor * cMarijuana * cTobacco + cPopulation, data = popDF))
summary(lm(formula = PartII ~ cLiquor * cMarijuana * cTobacco * cPopulation, data = popDF))
summary(lm(formula = PartII ~ cLiquor + cMarijuana + cTobacco, data = popDF))
summary(lm(formula = PartII ~ cLiquor * cMarijuana * cTobacco, data = popDF))
