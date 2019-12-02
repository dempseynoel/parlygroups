# parlygroups
parlygroups offers a suite of functions for web-scraping data contained within the House of Commons' "Register of All-Party Parliamentary Groups" (APPGs).

## Overview
According to [Parliament](https://publications.parliament.uk/pa/cm/cmallparty/191105/introduction.htm) an "All-Party Parliamentary Group (APPG) consists of Members of both Houses who join together to pursue a particular topic or interest". The register of APPGs "contains the financial and other information about Groups which the House has decided should be published. The Register is published on the parliamentary website and updated approximately every six weeks". The register is published both as a web page and a PDF document.

## Usage
The package provides one primary function to web scrape the APPGs: ```download_appgs```. The scraped data is stored in a cache within a seperate environment. You need to call ```download_appgs``` first, by providing it with at least the date of the register of interest as a character string in IS0 8601 format ("YYYY-MM-DD"), before calling any secondary function. If your R session is re-started any cached data will be lost and you will need to re-download using ```download_appgs```.

In addition to the primary argument of ```download_appgs``` there are two secondary arguments available: pause and save.

Pause is a non-negative numeric indicating length of time in seconds to pause between accessing APPG pages. It is set to 1 second by default. A longer pause helps minimise burden on the Parliamentary website - see the [ONS web-scraping policy](https://www.ons.gov.uk/aboutus/transparencyandgovernance/lookingafterandusingdataforpublicbenefit/policies/policieswebscrapingpolicy) for further best practice guidance on web-scraping.

Save is a boolean which if TRUE saves the cached data as a series of .csv/.RDS files in your current working directory. This is intended to be a QOL argument allowing users to avoid re-downloading the same APPG register each time in a new session. By default 'save' is FALSE. 

The secondary functions are all prefixed ```fetch_appg_*``` and return tibbles based on the main five types of entry in the Register of All-Party Parliamentary Groups. For example, ```fetch_appg_members``` fetches details on which MPs and Lords are members of APPGs along with their role and party affiliation. 

## Example

## Testing 
The package does not yet have unit tests, but the functions appear to work as intended. You should satisfy yourself that the functions behave in the way that you expect if you wish to use this package for research purposes.

## Installation
Install from GitHub using devtools.
```
install.packages("devtools")
devtools::install_github("dempseynoel/mpinterests")
```
