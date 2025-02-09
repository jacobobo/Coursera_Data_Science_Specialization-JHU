---
title       : How the 'How Old Am I?' shiny App works
subtitle    : Final Project for JHSPH Data Science Data Products Course
author      : JBrand
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Introduction

This project is for the Data Products course on Coursera from the Data Science Specialization.

I have created a shiny App and it is being hosted on shinyapps.io

It can be viewed at [https://jacobobo.shinyapps.io/Data_Products/](https://jacobobo.shinyapps.io/Data_Products/)

--- 

## Purpose

The purpose of the project is three-fold:  
1. Create an application in shiny (both the ui.R and server.R)  
2. Create a presentation in Slidify with run-able code  
3. Publish both to the web for viewing  

---

## How Old Am I?

This is a very simple app that calculates how old you are in days and years. You simply put in your birth date.
The math involved is very simple - I will use a sample birth date of 2000-01-01 as an example.

```r
days <- as.numeric(difftime(Sys.Date(),"2000-01-01"))
years <- (as.numeric(difftime(Sys.Date(),"2000-01-01")))/365
print(c(days,years))
```

```
## [1] 5902.79167   16.17203
```

---

## Remarks

Now, this is a very simplified example, as it does not take into account leap years.  However, for the purposes of this project, it is adequate to show use of shiny and Slidify.
