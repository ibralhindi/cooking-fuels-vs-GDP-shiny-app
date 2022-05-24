# cooking-fuels-vs-GDP-shiny-app
R shiny app examining access to cooking fuels and technologies vs GDP per capita. MBAT - S2, 2021

## How to run the app

The gadget consists of two operational tabs: **chart** and **table**

**Chart**

The chart tab displays a plot of countries according to their access to
clean fuels and tools for cooking percentage vs GDP per capita. The plot
can be modified in the following ways:

-   Check **Linearize x-axis** to convert the plot from a log-scale to a
    linear-scale
-   Check **Hide countries &lt; 1 million** to remove from the plot
    countries with a population of less than 1 million people
-   Type country name(s) in the **Select Countries** selector box to
    highlight chosen country(s) in the plot
-   Move the **year slider** to show the information for the year
    selected
-   **Hover** over any point in the plot to display that country’s
    information in a pop-up
-   Click on **continent** to the right of the plot to show only the
    countries from that continent

**Table**

The table tab displays the information for each country across all the
years in a table form. The table can be sorted by any column in
ascending or descending order by clicking on the arrows located in each
column head. The table columns can also be filtered by any value present
in that column.
