Overview of the Project
=======================

For living or staying in Singapore, a major expenditure item for most people is the expenditure on residential properties. Majority of the Singaporean and permanent residents owned and stayed in non-landed residential properties comprising of HDB developed housing units or privately developed residential properties funded by their monthly mortgage instalment payment. There is also a significant number of Singaporean, permanent residents and foreigners who arrived in Singapore for employments stayed in rented residential properties: HDB or private.

Problem Statements
==================

For a person interested to stay in non-landed private apartment, he or she will face a wide range of choices. Subjected to the budget constraint for the lease, he or she has to select based on the apartment’s location, kind of facilities available and amenities (e.g. nearby MRT, supermarket etc) surrounding the property.

For a person interested to make an investment in non-landed private apartment for passive rental income, subjected to the budget constraint to invest in the property, he or she has a difficult choice to make in terms of whether to invest in newly developed or existing property and the location to search for property which will offer the suitable rental rate.

It is widely acknowledged that the rental price rate per square feet (sq.ft) is largely driven by the location of the property, and to certain extent the economic condition of Singapore which influence the demand (e.g. inflow of foreigners into the country for employment) and supply (e.g. vacancy and newly developed unit available for let) of the private residential units for rental market. However, the expectation of the rental price is usually based on the recently transacted rental price of neighboring apartments.

Project’s Objective
===================

Amongst the driving forces of the rental rate for private residential property, we would like to establish some useful metrics that could help to provide a measure for the expected rental rate in different locations. These analysis result can be used as benchmark information for house hunters during their search through the internet listing to rent their ideal home.

Project's Scope
===============

We will explore the relationship of non-landed private residential properties rental rates using 3 metrices:

-   Age of the properties derived from the TOP year
-   Location of the properties based on District and Region
-   Distance from MRT stations.

If there is a significant correlation for the rental rate, age of the property, its location and distance from MRT, we will go further to establish a model to depict such a relationship.

Dataset(s)
----------

Source of data for private residential properties’ rental transactions is obtained from Urban Redevelopment Authority (“URA”) website. URA publishes URA related data for public use and is available for download via API data service. The data service provides past 36 months rental transactions data with rental contracts submitted to IRAS for Stamp Duty assessment in JSON format.

-   <https://www.ura.gov.sg/maps/api/private-residential-properties-rental-contract>

Data for the private residential properties’ TOP (by year) and MRT proximity is obtained via web scrapping from the SingaporeExpats.com website:

-   <https://condo.singaporeexpats.com>
-   <https://www.singaporeexpats.com/singapore-property-pictures/photos-A-G.htm>

Data Collection
===============

All web scaping routines are consolidated into smur package for easy access in this capstone project.

R source code is also available in github for reference

-   <https://github.com/portergoh/capstone>
