grow=5
sen=0
summer=grow+1
spring=grow-1
fall=sen+1
winter=sen-1

summer.d=cbind(rnorm(100, summer, sd=1), rep('grow', 100))
spring.d=cbind(rnorm(100, spring, sd=1), rep('grow', 100))
fall.d=cbind(rnorm(100, fall, sd=1), rep('sen', 100))
winter.d=cbind(rnorm(100, winter, sd=1), rep('sen', 100))


data=data.frame(summer.d, spring.d, fall.d, winter.d)
colnames(data)=(c('summer', 'summer.growth', 'spring', 'spring.growth', 'fall', 'fall.growth', 'winter', 'winter.growth'))
d1=with(data, data.frame(summer, spring, fall, winter))

d2=gather(d1, key='season', value='biomass')
d2$growth=with(data, c(as.character(summer.growth), as.character(spring.growth), as.character(fall.growth), as.character(winter.growth)))

data.lm=lm(biomass~season, data=d2)
summary(data.lm)

library(emmeans)
emmeans(data.lm, ~season)
contrast(emmeans(data.lm, ~season), method='tukey')

##Let's try with both season and growth (but with interaction which doesn't make sense)
data.lm2=lm(biomass~season*growth, data=d2)
summary(data.lm2) ### a lot of NAs because nonsenscial interactions rightfully so. But the effect of sen is also NA...
##And rightfully so, can't estiamte the interaction between senesce and spring/summer
emmeans(data.lm2, ~season) #interesting note from R 'a nesting structure was detected in the fitted model'
emmeans(data.lm2, ~growth) #Was able to compare the effect of growth vs sen even though sen came out as NA in the summary output...
emmeans(data.lm2, ~growth|season) #This doesn't work
d2.em=emmeans(data.lm2, ~season|growth) #3This sort of works (follow up below)
contrast(d2.em, method='tukey') ##Thiis nicely tells the relative difference between spring vs summer and fall vs winter (allowing us to decude the relative effect of the seasons are +/- 1)


##Without interaction
data.lm2=lm(biomass~growth+season, data=d2)
summary(data.lm2) ##Hmm, still no effect of growthsen in the summary output
					####But this effect changes if you switch the order of growth and season!!! Such that if growth comes first, we now see an effect of sen (but no effect of 							summer)
					####Ah!!! That's because of the arbitrary nature of the lm coefficient estimates!
					###The intercept represents one particular season/growth combination but doesn't realize it has the effect of both: the model thinks the intercept is just a 					particular season or a particular growth. Then when it tries to measure the corresponding counterpart of its season or growth, it can't(!) because it was 					already all explained in the estimate and there aren't any other combinations for it to be measured. All because of the nested structure (that's important)
					####All in all, this tells me to look at the emmeans instead of the model output
anova(data.lm2) ##And no growth seen in the anova (but if the order is switched, then we do see both effects)
emmeans(data.lm2, ~growth+season) ##recovers each individual season
contrast(emmeans(data.lm2, ~growth+season), method='tukey')
emmeans(data.lm2, ~season)
contrast(emmeans(data.lm2, ~season), method='tukey')
emmeans(data.lm2, ~growth)
contrast(emmeans(data.lm2, ~growth), method='tukey')



###Let's try a nested model? ###This shouldn't work or add anything because each season was uniquely identified across all growth range. And if we remembered last time, the '/' just means look at the interaction without the main effects
data.lm2=lm(biomass~season/growth, data=d2)
summary(data.lm2) ###Yep, it's what I expected
anova(data.lm2)
emmeans(data.lm2, ~season)


###So what have we learned from this??
###Well, in a nested question, we have two questions: are the upper levesl different (growth in this case) and are the lower levels different after accounting for upper level growth. Many ways to go about this (look at AIC scores and see if adding the lower levels help explain patterns for one...)
		##But right now, the way to do it is to add both upper and lower levels
				##This will create a nested structure but no point in adding nesting in your models (because you have coded them correctly)
				##Disregard any NA and the output in general from the summary and dive into emmeans
				##Using emmeans, compare upper level first
				##If different, great, if not, you're pretty much done? (NOPE, still the really real possibility that lower level differences are present)
						###So just use tukey to do all pair-wise comparisons. Fair fair
				##If different, then compare the lower levels split up by upper levels to see if there are differences (minimize tukey?)
				##If no difference, you can say it's due to upper level
				##If there are differences, it's both upper and lower differences (since you've already compared upper level differences and any difference left is within the 				upper levels)
######What if there are no upper level differences and just lower level differences????