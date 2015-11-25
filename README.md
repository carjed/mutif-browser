# mutif-browser

## Overview
The Sequence Motif Mutation Browser is a tool used for finding and comparing genome-wide relative mutation rates of k-mers in the human genome. It is built on the R statistical computing environment using [Shiny](http://shiny.rstudio.com/).

If you use mutif in your research, please cite the following paper:

*citation goes here*

To select a mutation category to examine, choose from the Category dropdown menu.

The data for each category is organized visually into a heatmap of 4,096 unique 7-base pair motifs.  To assist in locating a specific motif, a gray frame is drawn around each set of 256 motifs that share the same bases in the +/-1 position from the mutation site. For each cell (or group of cells), the base at the +1, +2, and +3 position are given on the bottom, middle, and top level of the x-axis labels, respectively.  Similarly, the bases at the -1, -2, and -3 positions are shown in the corresponding levels of the y-axis label.
 
The left panel displays the following:
* Widgets to select and search for a specific motif or group of motifs.
* A filtered table of the motifs that have been selected.
* Summary statistics for the selected motifs [under development]
* Download buttons.

## Usage Instructions

Most users will want to use the web interface, available at *https://carjed.shinyapps.io/mutif-browser*

This will hopefully be hosted on a dedicated server in the future.

To run the app on your own system, run the following command from an R session:
`runGitHub("mutif-browser", "carjed")`

Note that the mutif browser requires the following packages:

* `library(shiny)`
* `library(shinyBS) # Used for collapsible sections`
* `library(Cairo)   # For nicer ggplot2 output when deployed on Linux`
* `library(DT)      # Pretty tables`
* `library(ggplot2) # Pretty plots`
* `library(RColorBrewer)`

If you have the `dev_tools` package installed, you can quickly install and load the required packages with  `source_gist("https://gist.github.com/carjed/1839dd68e43eda0e4c06")`

### Selecting Motifs
To select a set of k-mers corresponding to a given motif, check the box for the base(s) of interest at each position upstream/downstream from the mutation site. This will mark the corresponding cells of the heatmap and add the data to the Selection Table.

The motif corresponding to your selection is displayed to the right of the checkbox grid. If this motif is degenerate, ambiguous bases are represented in standard IUPAC codes [under development].

Additionally, you can select motifs of interest either by clicking on a cell of the heatmap or selecting a row from the table.  Both of these will also mark the cell on the heatmap and add to the Selection Table.  Hovering over a cell in the heatmap will display the motif and relative rate below the plot.

### Selection Summary
When motifs have been added to the Selection Table, the following statistics are automatically calculated among the selected subset:
* Relative mutation rate (selected motifs)
* Fold-difference over background rate for the category
* Min, Median, Max
* Chi-squared test for uniformity

### Downloading Data
To download data for your selected motifs, simply click the "Download Selected Data" button.  Alternatively, you can download data for all 25476 motifs across all 6 categories by clicking the "Download Full Data" button.

----
### Known bugs/quirks/limitations:
* If a motif is first selected from the plot, it can be selected a second time from the table. Has to do with use of rbind() to combine the two sources--some sort of merge function or indexed selection would be better, but neither seems to work.
* When switching between categories, plot briefly shows a blank panel. This only occurs if rows from the table are selected--seems to be due to incorrect clearing of data when reactive dropdown for category selection is changed
* Mouse clicks sometimes fail to select a cell, especially near borders.
* Hovering sometimes returns garbled info for multiple cells
* in narrow windows, the data tables are outside of the frame

### Features under development:
* Dynamically updated stats summary for selected motifs
* Select across different categories
* Brush to select group of cells
* Display motif with IUPAC codes next to selection boxes
* Create option to show heatmap for 5bp/3bp motifs

