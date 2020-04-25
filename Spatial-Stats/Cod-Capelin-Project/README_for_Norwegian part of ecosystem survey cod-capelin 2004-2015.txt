This README.txt file was generated on 20180514 by Johanna Fall


-------------------
GENERAL INFORMATION
-------------------


1. Title of Datasets

  Ecosystem survey: "Norwegian part of ecosystem survey cod-capelin 2004-2015.txt"
  Winter survey: "Norwegian part of winter survey cod-capelin 2004-2015.txt"


2. Author Information


  Principal Investigator Contact Information
        Name: Johanna Fall
           Institution: Institute of Marine Research
           Address: PO Box 1870 Nordnes, N-5817 Bergen, Norway
           Email: johanna.fall@hi.no


  Co-investigator Contact Information
        Name: Edda Johannesen
           Institution: Institute of Marine Research
           Address: PO Box 1870 Nordnes, N-5817 Bergen, Norway
           Email: edda.johannesen@hi.no


3. Date of data collection

  Ecosystem survey: 2004-08-07 -- 2015-09-24

  Winter survey: 2004-02-05 -- 2015-03-12


4. Geographic location of data collection

  Ecosystem survey: Barents Sea, longitude 8.0194 -- 46.5833 and latitude 70.0583 -- 81.7306 (decimal degrees)

  Winter survey: Barents Sea, longitude 8.3861 -- 51.2583 and latitude 68.6417-- 80.0250 (decimal degrees)


5. Funding sources that supported the collection of the data

  Norwegian Government


--------------------------
SHARING/ACCESS INFORMATION
-------------------------- 


1. Licenses/restrictions placed on the data

  CC0


2. Recommended citation for the data

  Fall J, Ciannelli L, Skaret G, Johannesen E (2018) Data from: Seasonal dynamics of spatial overlap between Northeast Arctic cod
  (Gadus morhua) and capelin (Mallotus villosus) in the Barents Sea. Dryad Digital Repository, [DOI]

---------------------
DATA & FILE OVERVIEW
---------------------


1. File List

   i.  Filename: Norwegian part of ecosystem survey cod-capelin 2004-2015.txt        
       Short description: Data on cod and capelin distribution in autumn.        

   ii. Filename: Norwegian part of winter survey cod-capelin 2004-2015.txt        
       Short description: Data on cod and capelin distribution in winter.        


2. Additional related data collected that was not included in the current data package:
   
   The data was collected during the Joint IMR-PINRO surveys of the Barents Sea, a collaboration between Russian and Norwegian 
   research institutes and funded by the respective governments. Norwegian data used here are freely available (the data sets 
   described here), but the Russian data are not available due to Russian national restrictions on data funded by their government.
   The original data sets that were used for analyses presented in the main paper, contained 4644 observations in the ecosystem 
   survey data set, and 3994 observations in the winter survey data set. The Norwegian data shared here represent 53% of autumn 
   data, and 80 % of winter data.

   Data requests may be sent to Research Director Geir Huse at the Institute of Marine Research, geir.huse@hi.no or the leader
   of the Barents Sea research programme, Maria Fossheim, maria.fossheim@hi.no.
 
   For more information on the history of Norwegian-Russian cooperation in the area, see:

                 Eriksen E, Gjøsæter H, Prozorkevich D, Shamray E, Dolgov A, Skern-Mauritzen M, et al. 
                 From single species surveys towards monitoring of the Barents Sea ecosystem. 
                 Progress in Oceanography. 2017.


--------------------------
METHODOLOGICAL INFORMATION
--------------------------


1. Description of methods used for collection/generation of data: 
  
  Ecosystem survey: 
	Sampling manuals: https://www.imr.no/tokt/okosystemtokt_i_barentshavet/sampling_manual/nb-no
	Survey reports: https://www.imr.no/tokt/okosystemtokt_i_barentshavet/survey_reports/nb-no
  
  Winter survey:
        Survey reports by year: https://www.hi.no/publikasjoner/andre_publikasjoner/imr-pinro_samarbeidsrapporter/2015/nb-no


2. Methods for processing the data:

  See survey reports. In addition, acoustic densities of capelin was interpolated around the trawl stations (see main paper),
  and a swept distance index for capelin caught in the demersal trawl (winter) was calculated by divided the number of capelin
  caught in the trawl with the distance trawled. Temperature measurements from the nearest CTD station were assigned to each
  trawl station. See further the main paper for calculation of julian day and sunheight, and projection of coordinates.

3. Instrument- or software-specific information needed to interpret the data:

  N/A
 
4. Standards and calibration information, if appropriate:


5. Environmental/experimental conditions:

  See survey reports.


6. Describe any quality-assurance procedures performed on the data:

  The spatial distribution of data was compared with the realised sampling design for each year to ensure that all data was present.
  All variables were checked for unrealistic outliers and coding errors.

7. People involved with sample collection, processing, analysis and/or submission:

  See survey reports.



---------------------------------------------------------------------------------------------
DATA-SPECIFIC INFORMATION FOR: "Norwegian part of ecosystem survey cod-capelin 2004-2015.txt"
---------------------------------------------------------------------------------------------


1. Number of variables: 42


2. Number of cases/rows: 2455 


3. Variable List
    A. Name: year
       Description: The year the particular station was sampled [2004-2015].


    B. Name: ship
       Description: Abbreviation of the research vessel ["3YVG" = Christina E, "GS" = G. O. Sars, 
                    "JH" = Johan Hjort, "JM" = Jan Mayen (renamed Helmer Hanssen in 2011, but coded as JM throughout).

    C. Name: lon
       Description: Longitude of sampling location (at the beginning of trawling) in decimal degrees.

    D. Name: lat
       Description: Latitude of sampling location (at the beginning of trawling) in decimal degrees.

    E. Name: x
       Description: Easting of sampling location (at the beginning of trawling) in nautical miles (1 nmi = 1852 m). 
                    Polar stereographic projection with center in 75 N and 35 E.

    F. Name: y
       Description: Northing of sampling location (at the beginning of trawling) in nautical miles (1 nmi = 1852 m). 
                    Polar stereographic projection with center in 75 N and 35 E.

    G. Name: date
       Description: Date of sampling in yyyy-mm-dd format.

    H. Name: month
       Description: Month of sampling [1-12].

    I. Name: day
       Description: Day of sampling [1-31].

    J. Name: julian.day
       Description: Julian day for day of sampling, calculated using the earliest sampling day across years as origin (August 3) [0-60].

    K. Name: time
       Description: Start time of trawling in hhmm format.

    L. Name: start_time
       Description: Start time of trawling in hhmm format (same as K).

    K. Name: stop_time
       Description: Stop time of trawling in hhmm format.

    L. Name: sunheight
       Description: Solar elevation angle calculated based on geographical position, time of year, and time of day.
                    Negative values indicate sun below horizon, zero sun at horizon, positive sun above horizon.

    M. Name: b_depth
       Description: Bottom depth in metres at the beginning of trawling.

    N. Name: b_temp
       Description: Bottom temperature in degrees Celsius from the CTD station taken closest (in time and space) to the 
                    beginning of trawling.

    O. Name: p_temp
       Description: Mean pelagic temperature (50-200 m) in degrees Celsius from the CTD station taken closest (in time and space)
                    to the beginning of trawling.

    P-AG.Names: corrL30_34, corrL35_39, corrL40_44, ..., corrL125_129 
       Description: Swept area density (number of individuals/square nautical mile) of cod of a given length group. The length groups
                    are indicated by the numbers in the variable names; 30_34 = 30-34.99 cm, etc.

    AH.Name: cod.imm
       Description: Total swept area density of all cod in the immature cod group, i.e., row sums of densities for length classes
                    30_34 to 70_74 (see main paper for rationale).

    AI.Name: cod.mat
       Description: Total swept area density of all cod in the mature cod group, i.e., row sums of densities for length classes
                    75_79 to 125_129 (see main paper for rationale).

    AJ.Name: capelinA
       Description: Acoustic backscatter of capelin (nautical area scattering coefficient SA; m^2/nmi^2).

    AK.Name: bio_cod
       Description: Total stock biomass of cod from the 2017 stock assessment (one value per year) in million tonnes (10^9 kg).

    AL.Name: bio_cap
       Description: Total stock biomass of capelin from the yearly stock estimate (one value per year) in million tonnes (10^9 kg).


4. Missing data codes:
        NA	There are no missing data points in this data file.


---------------------------------------------------------------------------------------------
DATA-SPECIFIC INFORMATION FOR: "Norwegian part of winter survey cod-capelin 2004-2015.txt"
---------------------------------------------------------------------------------------------


1. Number of variables: 43


2. Number of cases/rows: 3192


3. Variable List
    A. Name: year
       Description: The year the particular station was sampled [2004-2015].


    B. Name: ship
       Description: Abbreviation of the research vessel ["GS" = G. O. Sars, "JH" = Johan Hjort, 
                    "JM" = Jan Mayen (renamed Helmer Hanssen in 2011, but coded as JM throughout), "LMQI" = Libas.

    C. Name: lon
       Description: Longitude of sampling location (at the beginning of trawling) in decimal degrees.

    D. Name: lat
       Description: Latitude of sampling location (at the beginning of trawling) in decimal degrees.

    E. Name: x
       Description: Easting of sampling location (at the beginning of trawling) in nautical miles (1 nmi = 1852 m). 
                    Polar stereographic projection with center in 75 N and 35 E.

    F. Name: y
       Description: Northing of sampling location (at the beginning of trawling) in nautical miles (1 nmi = 1852 m). 
                    Polar stereographic projection with center in 75 N and 35 E.

    G. Name: date
       Description: Date of sampling in yyyy-mm-dd format.

    H. Name: month
       Description: Month of sampling.

    I. Name: day
       Description: Day of sampling.

    J. Name: julian.day
       Description: Julian day for day of sampling, calculated using the earliest sampling day across years as origin (January 21) [0-53].

    K. Name: time
       Description: Start time of trawling in hhmm format.

    L. Name: start_time
       Description: Start time of trawling in hhmm format (same as K).

    K. Name: stop_time
       Description: Stop time of trawling in hhmm format.

    L. Name: sunheight
       Description: Solar elevation angle calculated based on geographical position, time of year, and time of day.
                    Negative values indicate sun below horizon, zero sun at horizon, positive sun above horizon.

    M. Name: b_depth
       Description: Bottom depth in metres at the beginning of trawling.

    N. Name: b_temp
       Description: Bottom temperature in degrees Celsius from the CTD station taken closest (in time and space) to the 
                    beginning of trawling.

    O. Name: p_temp
       Description: Mean pelagic temperature (50-200 m) in degrees Celsius from the CTD station taken closest (in time and space)
                    to the beginning of trawling.

    P-AG.Names: corrL30_34, corrL35_39, corrL40_44, ..., corrL125_129 
       Description: Swept area density (number of individuals/square nautical mile) of cod of a given length group. The length groups
                    are indicated by the numbers in the variable names; 30_34 = 30-34.99 cm, etc.

    AH.Name: cod.imm
       Description: Total swept area density of all cod in the immature cod group, i.e., row sums of densities for length classes
                    30_34 to 70_74 (see main paper for rationale).

    AI.Name: cod.mat
       Description: Total swept area density of all cod in the mature cod group, i.e., row sums of densities for length classes
                    75_79 to 125_129 (see main paper for rationale).

    AJ.Name: capelinA
       Description: Acoustic backscatter of capelin (nautical area scattering coefficient SA; m^2/nmi^2).

    AK.Name: capelinT
       Description: Swept distance density of capelin from demersal trawl catches (number of individuals/nautical mile).

    AK.Name: bio_cod
       Description: Total stock biomass of cod from the 2017 stock assessment (one value per year) in million tonnes (10^9 kg).

    AL.Name: bio_cap
       Description: Total stock biomass of capelin from the yearly stock estimate (one value per year) in million tonnes (10^9 kg).
                    Since capelin is estimated in autumn, we use the estimate from the year before here, i.e., stock biomass for
                    2004 in this dataset comes from the estimate in 2003, 2005 comes from 2004, etc.

4. Missing data codes:
        NA	There are no missing data points in this data file.

