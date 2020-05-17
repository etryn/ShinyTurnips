# ShinyTurnips

**ShinyTurnips is an app for logging and visualizing Animal Crossing turnip prices among your friend group.**

## Overview

This app accepts price entries from a pre-determined group of friends and saves them as individual CSV files. It then compiles these CSV files into one data table to generate a tabular and graphical visualization of turnip prices from the most current week.

## Use

1. ceylonMembers in app.R must be updated to a list of current friends' names. It would also be possible to change the name column to free-form text entry, but I did not want people to have to retype their names over and over. Free-form entry also creates the opportunity for confusion through typos and random people adding themselves.

## Notes

* Ideally, purchase prices should be recorded as Sunday AM prices. If purchase price is recorded as Sunday PM price, the table will display it in a separate row. The graph handles this better and will collapse both Sunday prices into one x-axis point called "Purchase".

* If multiple prices are entered for the same individual and the same time window, the most recent price will overwrite all previous prices. This means typos and mistakes can be corrected by re-submitting the form.

## Ideas for Improvements

* Highlight names or otherwise indicate when a spike is beginning
* Ability to scroll through previous weeks in the table or graph view (or in a separate tab?)
* Better implementation of Sunday purchase price entry



