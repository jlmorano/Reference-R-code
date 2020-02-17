# README

This was part of the Introduction to Modeling course (Spring 2020) where I recreated the model in Rustici et al. 2017 (Fish-Urchin_Model.R) and then modified the model (Modified_Fish-Urchin_Model.R) for a term project.

## Summary of Paper
Rustici M, Ceccherelli G, Piazzi L. 2017. Predator exploitation and sea urchin bistability: Consequence on benthic alternative states. Ecological Modelling. 344:1–5. doi:10.1016/j.ecolmodel.2016.10.021.

In the Mediterranean Sea, when sea urchin density is low, the coastal rocky habitat is a dense algal forest, dominated by a high diversity of erect algae; when sea urchin density is high, the habitat is barren and dominated by encrusting coralline algae and bare rock. The urchin population is not controlled by predators alone, because when fishing pressure is removed, in a Marine Protected Area, there still are barren habitats. Rustici et al. (2017) modeled how fishing on the urchin’s predator affects the urchin population density and the mechanisms driving transitions between the habitat states. 

# Replicating the Model
I replicated the mathematical model in R (see attached R script, “Fish-Urchin_Model.R”). As varied s from 0-6, I generated the phase plot. For each of the 2 stable states, 2 saddle states, and 1 unstable point, I found the x,y point and saved the x, y values at each value of s into “S points.csv”. I then plotted the x values as a function of s to replicate Figures 1 and 2 from the paper.

# Modifying the Model
The authors concluded that fishing urchins under the high steady state (when the habitat is barren and urchin population density is high), would be the most practical and effective approach to changing the habitat from barren to a diverse algal forest. I modeled the effect of removing urchins from the population from fishing (parameter u), and then varied the urchin fishing pressure (0 < u < 5) and the fishing pressure (1 < s < 5), keeping all other parameters constant. 

Adding the urchin fishing pressure eliminates the stable high and low urchin populations, however, it creates unstable states with a narrow condition for a stable urchin population, when s = 1 or 3 and u = 0.5. This means that when fishing on the predator is relatively high (s = 3) or low (s = 1), a low level of urchin harvesting could create a stable population of urchins below the density of predatory fish. 
