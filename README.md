### A catalogue of variation resulting in human lethality

An interactive Shiny app displaying a catalogue of genes linked to early lethality
in humans.

The catalogue includes curation of OMIM genes with a reported lethal
phenotype.

The current functionalities of the app include:

* Data table with the list of lethal genes in humans curated from OMIM,
available to search and download.
* Visualization and comparison of LOEUF values, SHET scores, and Gene Effect scores.

Can be found at:

https://lethalphenotypes.research.its.qmul.ac.uk/

OR

https://shiny-whri-01.research.its.qmul.ac.uk/

### Repository contents

All files contained within the `catalogue_lethal_genes_app` pertain to the app itself. 
To run locally, simply navigate to this directory, open an R shell and run the following command:
```
shiny::runApp()
```
Alternatively, the Run App functionality can be used within RStudio. 
