Title: Quantifying anthropogenic disturbance to strand-feeding dolphins
in Captain Sam’s Inlet, SC

**Objective**

The objective of this project is ultimately to create a land-based
viewing guideline that will (1) reduce disturbance to the dolphins at
Captain Sam’s Inlet and (2) maximize the strand-feeding behavior.

The main questions are:

1.  Is the frequency of disturbance behaviors affected by distance
    treatment?
2.  How does distance treatment affect how likely dolphins are to strand
    feed?
3.  Does the number of people present affect where dolphins choose to
    feed?

These questions are addressed by creating several model types to analyze
the effect of several variables, including distance treatment and number
of people present.

**Code Structure**

Code is arranged sequentially by what must be run first (i.e.,
dependencies are at the top, followed by the data, etc.) Related
functions are grouped into blocks. Code comments are above or beside the
lines they are describing. The preliminary section is labeled to
separate it from the analysis.

**Dependencies**

You’ll need to install the following packages to ensure all of the code
runs:

library(pscl) library(psc) library(ggplot2) library(dplyr) library(MASS)

You’ll also need to download the ‘ExpandDF\_Function’ r file and run it
to use the function in the main r file.

**Data**

The data files are in .csv format and can be found in the data folder.
Both data files should be imported. The THESIS.csv file includes all of
the original data collected for this project. The THESIS.update.csv file
is the same data with the addition of several rows including strand
feeding events that were off-effort (not included in the orignial thesis
file).

Data was collected during June-November of 2024 on Kiawah Island in
collaboration with the Lowcountry Marine Mammal Network. A distance
treatment was applied to a crowd on Kiawah Island, and the dolphin
behavior was recorded as the response variables. Data was collected in 5 minute on-effort periods, each followed by a 5 minute off-effort period. Strand feeding events were recorded during on-effort and off-effort periods. Disturbance behaviors were only recorded during on-effort periods.

The data contains many variables, and a few are added on in the preliminary section of the r file (for example, the difference from low tide time). The variable of
interest as they appear in the data file include:

Distance.tested = the distance treatment being applied (0ft, 20ft, 40ft,
60ft, 80ft) 
KI.AVG.total = the average number of people on Kiawah Island
for a 5 minute on-effort period 
SBI.total.AVG = the average number of people on Seabrook Island for a 5 minute on-effort period 
Number_TS = Count of tail slaps (a disturbance behavior) in a 5 minute on-effort
period 
Number\_Chuff = Count of chuffs (a disturbancae behavior) in a 5
minute on-effort period 
Number.Sf.Events = the number of strand feeding events in a 5-minute period (can include on and off effort) 
SF\_Zone = which zone a strand feeding event took place

**How to recreate my results**

Install required dependencies, import all csv files supplied in the data
folder to your working directory, download and run the r file for the
‘expandDF’ function, then run the main script to generate results.

**Acknowledgements**

I’d like to thank my advisor, Dr. Allan Strand, and my committee
members, Dr. Melissa Hughes, Wayne McFee, and Lauren Rust for all their
help on this project. Special thanks to all of the volunteers who helped
collect this data in 2024.
