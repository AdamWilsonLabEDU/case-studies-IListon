Visualizing the impact of hurricanes on mangroves over time
================
Isabel Liston

# Problem / Question

Mangrove trees are a very important aspect of many coastal ecosystems.
Many of these areas are also impacted by hurricanes, therefore it is
beneficial to understand how hurricanes damage mangrove trees and how
long it takes trees to recover from said damage.

# Inspiring Examples

## Example 1

This plot from Hwang et al., 2016 is serving as inspiration for how I
will visualize the amount of each species of mangrove in each plot over
time. I might also use this format to compare the impacts of various
hurricanes on each species of mangrove.  
![Plot from Hwang et al., 2016
[Link](http://dx.doi.org/10.1080/01426397.2016.1210106)](Example_Plot_Hwang_2016.png)

## Example 2

Doyle et al. 1995 used this data set to analyze the impact of winds
related to Hurricane Andrew on mangroves on the southwest coast of
Florida. I was inspired by this map that they made to illustrate the
study area, specifically the addition of the hurricane’s path. ![Map 1
from Doyle et al., 1995
[Link](https://www.jstor.org/stable/25736006)](Example_Map_Doyle_1995.png)

## Example 3

This example inspired me to think about alternative ways of visualizing
tree attribute over time, specifically relating tree diameter at breast
height (DBH) to the thickness of the lines and having the x-axis
represent time, with notes of when hurricane events happened. ![Figure
showing the abundance of vaious species along a transect.
[Link](https://old-ib.bioninja.com.au/options/option-c-ecology-and-conser/c1-species-and-communities/species-distribution.html)](Example_3.jpeg)

# Proposed data sources

I will be using a data set from the USGS Wetland and Aquatic Research
Center that contains measurements for various species of mangroves in
established plots between 1992 and 2011. I also plan to use data from
the Hurricane Exposure package to cross reference the area of hurricane
impact with the plot areas.

# Proposed methods

I plan to use tidyverse to do my initial data exploration and
manipulation. Then ggplot2 and sf to visualize the spatial distribution
of the plots and the various species of mangroves. The Hurricane
Exposure package and sf package will be used to add the additional
spatial information of hurricanes that have passed through this area to
the map. Finally, biomonitoR will be used to compare the recovery rate
of the different species of mangrove trees.

# Expected results

The results of my analysis will include a map of the study area, a
figure with the path of the hurricanes that passed through the area
during the study period, a bar chart comparing the make-up of each plot
over time and a graph showing the changes in DBH for the trees in each
plot over time.
