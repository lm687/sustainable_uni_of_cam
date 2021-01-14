## What are the intiatives for tackling climate change/ making the University of Cambridge more sustainable?

<!--- ![graph of initiatives for sustainibility and climate action at the university of cambridge](figures/Cambridge_initiatives_climate_and_sustainability.png) --->


To see the current version of the interactive plot, click [here](https://lm687.shinyapps.io/code/)

To see the previous version of the interactive plot, click [here](https://lm687.github.io/sustainable_uni_of_cam/html_files.html)


## FAQs
- *What is this map?*

- *Can I add my project to this map?*

- *Some information is incorrect, how can I let you know?*

- *Can I share this map?*

## Technical details
The source files are:

```
dataframe_edges.txt 
metadata.txt
url_figures.txt
```

To deploy the shiny app, from rstudio, and within the `code/` folder, do
```
rsconnect::deployApp()
```

note that there should be no `rstudioapi()` bits in the code
