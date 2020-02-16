# Cycling Results Gathering
Ben and Chris met with Cycling New Zealand in January 2020 to listen to existing reasons for using data across Performance Analysis, HPAD, Coaching and Operations. We came away with a clear picture of how data is used and how Intelligence can assist in this. Primarily this can be in the automation of competition results, from Tissot Timing in the first instance.

## Shared data from CNZ staff
PTA and HPAD shared some of their work with us to familiarise us with their efforts and purpose of using data. These are available in the **data** folder of this repository. These should only be used and viewed in working towards this project and shuold not be shared beyond the Intelligence team.

## Tissot Timing data
Data available at https://www.tissottiming.com/Sport/CTR includes World Cup and World Championship competitions. Webpages appear to be rendered in HTML and hence give us the potential for web scraping. The following notes will summarise progress made to this end.

The following URL structure is used for each results page:
www.tissottiming.com/**year**/**comp_code**/en-us/default/Stage/**event_code**/Results/**round**

## PTA use of data
Anna Higgins uses seperate Excel spreadsheets for each discipline (e.g. Men's Omnium) and records discipline-specific details and metrics for each. She shared some examples of these in order that we can see the output that any automation should work towards. 

In the first instance it is sensible to attempt web scraping in R with the package **rvest**. Ultimately there could be automation functionality through a _shiny_ app available at a secure website. Here the PTA would go to select event, disciplines, etc to automatically spit out results formatted for the existing spreadsheets.

### What data is used after each day/round?
- Omnium race results are used to assess scenarios and competitors __during competition__ i.e. after each race. Anna currently does this work manually.
- All other results are used for analysis __post-competition__. This can ostensibly require up to 1 week of wrangling/formatting after competition finishes.

## HPAD use of data
Graeme Hunn uses seperate Excel spreadsheets to track Junior and Elite world championship times for last 3 years. Average of these times for each discipline are used to "classify" the performance of the NZ cyclists as "world class", "potential", "development", etc. Scraping results in the way outlined above will benefit HPAD too.
