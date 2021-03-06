# ShinyTurnips

**ShinyTurnips is an app for logging and visualizing Animal Crossing turnip prices among your friend group.**

## Overview

This app accepts price entries from a pre-determined group of friends and saves them as individual CSV files. It then compiles these CSV files into one data table to generate a tabular and graphical visualization of turnip prices from the most current week.

## How to Use

1. The file friends.txt must be updated to a list of current friends' names and their favorite colors. It would also be possible to change the name field to free-form text entry, but I did not want people to have to retype their names over and over. Free-form entry also creates the opportunity for confusion through typos and random people adding themselves.

## Notes

* Ideally, purchase prices should be recorded as Sunday AM prices. If purchase price is recorded as Sunday PM price, the table will display it in a separate row. The graph handles this better and will collapse both Sunday prices into one x-axis point called "Purchase".

* If multiple prices are entered for the same individual and the same time window, the most recent price will overwrite all previous prices. This means typos and mistakes can be corrected by re-submitting the form.

* Some prediction tools may ask for the previous week's pattern. In the Archive tab, all prices recorded to date are displayed sorted newest to oldest.