# WWI VieweR/

The app is hosted here : https://cdk296.shinyapps.io/WWI_VieweR/ 

## Motivation

The goal of this project was to validate the capstone project of the Data Visualization specialization of the Johns Hopkins University on Coursera.
The code of the application and of the scrapping is accessible here [on this github repo.](https://github.com/Cdk29/Capstone_project)


I started around the 11 November, which gave me the idea for this project.

## Data:

I originally started with the [CDB90](https://github.com/jrnold/CDB90) dataset, planning to recover the latitude and longitude for each battle from Dbpedia. But the URLs links inside the battles.csv where somewhat too old, and query using them were not working.

I ended up querying directly dbpedia, using the code in the file Week_2.Rmd and scrapping_all_ww1.Rmd (the file is named Week_2 because of the segmentation of the capstone project course on Coursera)/

See [the github repo.](https://github.com/Cdk29/Capstone_project)

## Data limitation

Facts in history are not that objective or simple. For exemple, is a battle a victory or not ? Take for example the  [Battle of Jutland](https://en.wikipedia.org/wiki/Battle_of_Jutland), both camps claimed victory, because they were pursuing differents objectives.


Also, some results of battle may vary depending the language used for scrapping.

Regarding the scrapping, I am pretty sure some battles are missings, for exemple in the eastern front of the war. Not sure why.

Also, the map where I mapped the battles is a contemporary map. Too bad there is not a world map of 1914 accessible on Leafleat.
