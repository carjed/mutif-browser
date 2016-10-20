# mutif-browser

## Overview
Point mutation rates in the human genome are highly variable. A substantial portion of this variability is attributable to the sequence of bases surrounding the site of the mutation. We have estimated these motif-specific mutation rates, considering all combinations of bases up to 3 positions upstream and downstream from a mutation site.

This utility was designed to explore and compare these 24,576 unique 7-mer mutation rates. It is built on the R statistical computing environment using [Shiny](http://shiny.rstudio.com/).

<!-- If you use mutif in your research, please cite the following paper:

*citation goes here* -->

### Accessing this utility
Most users will want to use the web interface, available at *http://www.jedidiahcarlson.com/shiny/mutif-browser/*

To run the app on your own system, run the following command from an R session:
`runGitHub("mutif-browser", "carjed")`

Note that the mutif browser requires the following packages:

* `library(shiny)`
* `library(shinyBS) # Used for collapsible sections`
* `library(Cairo)   # For nicer ggplot2 output when deployed on Linux`
* `library(DT)      # Pretty tables`
* `library(ggplot2) # Pretty plots`
* `library(RColorBrewer)`
* `library(dplyr)`

If you have the `dev_tools` package installed, you can quickly install and load the required packages with  `source_gist("https://gist.github.com/carjed/1839dd68e43eda0e4c06")`

### Usage Instructions
To start, choose the single-base mutation type you wish to examine from the "Select Type" dropdown menu on the left.

This will display a heatmap of the 4,096 unique 7-mer mutation rate estimates corresponding to the selected type. These 7-mer subtypes are organized according to the 3 positions upstream (x-axis) and 3 positions downstream (y-axis) from the site of a mutation. Subtypes sharing the same +1/-1 base are grouped together in a large box (256 subtypes each), and subtypes sharing the same +1/-1 and +2/-2 base are grouped together in smaller boxes (16 subtypes each).

### Selecting subtypes of interest
Hovering over a cell in the heatmap will display a text field under the heatmap indicating the 7-mer motif and mutation rate information. Clicking a cell will highlight it on the heatmap.

On the sidebar is a grid of checkboxes corresponding to the 4 possible nucleotides at each position +/-3 bases away from the mutation site. Selecting one or more of these checkboxes will highlight the corresponding 7-mers on the heatmap.

Below the heatmap, you will find a searchable table of the 4,096 7-mer relative mutation rates for the selected type. Clicking on a row will highlight the corresponding 7-mer in the heatmap.

Below this table is another table containing all motifs that have been selected from clicking on the heatmap, selecting from the table, and/or selecting from the checkbox grid. Changing to another mutation type clears all selected subtypes from this list.

### Downloading Data
To download data for your selected motifs, simply click the "Download Selected Data" button.  Alternatively, you can download data for all 24,576 subtypes by clicking the "Download Full Data" button.

----
### Known bugs/quirks/limitations:
* If a motif is first selected from the plot, it can be selected a second time from the table. Has to do with use of rbind() to combine the two sources--some sort of merge function or indexed selection would be better, but neither seems to work.
* When switching between categories, plot briefly shows a blank panel. This only occurs if rows from the table are selected--seems to be due to incorrect clearing of data when reactive dropdown for category selection is changed
* Mouse clicks sometimes fail to select a cell, especially near borders.
* Hovering sometimes returns garbled info for multiple cells
* in narrow windows, the data tables are outside of the frame

### Features under development:

New features are under active development!

#### Selection Summary
Include summary statistics for the mutation rates of selected subytpes:
* Min, Mean, Median, Max, etc.
* Fold-difference over background rate for the category
* Chi-squared test for uniformity

This will dynamically update as the selected subtypes change

#### Display improvements
* Select across different categories
* Brush to select group of cells
* Display motif with IUPAC ambiguity codes next to selection boxes
* Filter and highlight subtypes by range of rates
* Create option to show heatmap for lower-resolution 5-mer or 3-mer subtypes
* Additional tab showing exploratory plots for selected data
* Integration with the [Mr. Eel](www.jedidiahcarlson.com/mr-eel/) utility
