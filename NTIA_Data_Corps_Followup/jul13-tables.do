*	jul13-tables.do
*	Version 1.0 (May 15, 2015)
*	National Telecommunications and Information Administration
*
*	Contains supporting code for create-ntia-tables.do specific to the named
*	dataset. This Do File recodes variables so that create-ntia-tables.do can be
*	used to extract summary statistics from the dataset in a uniform fashion.
*	
*	Contact: Rafi Goldberg, <rgoldberg@ntia.doc.gov>

* Step 1: List Person, Household, and Respondent Variables being extracted from this dataset.
* Exclude isPerson, isAdult, isHouseholder, and isRespondent from these lists.
global personVars "desktopUser laptopUser tabletUser mobilePhoneUser tvBoxUser homeInternetUser workInternetUser schoolInternetUser altHomeInternetUser internetUser adultInternetUser"
global householdVars "internetAnywhere internetAtHome noInternetAtHome speedMostImportant reliabilityMostImportant affordabilityMostImportant serviceMostImportant mobilityMostImportant dataCapMostImportant mobileAtHome wiredHighSpeedAtHome satelliteAtHome dialUpAtHome ispBundle tvInBundle homePhoneInBundle homeEverOnline noNeedInterestMainReason tooExpensiveMainReason canUseElsewhereMainReason unavailableMainReason noComputerMainReason privSecMainReason computerAtHome"
global respondentVars "emailUser textIMUser videoUser locationServicesUser teleworkUser jobSearchUser financeUser eCommerceUser healthInfoUser healthRecordsUser"

* Step 2: Construct universes for this dataset and standardize weight names (personWeight, householdWeight, respondentWeight). Code is usually the same except in older datasets.
* isPerson and isHouseholder are always required, and isRespondent and isAdult (which are likely the same) are required if pulling from random respondents.
generate isPerson = (prtage >= 3 & prpertyp != 3 & hrmis != 1 & hrmis != 5)
generate isHouseholder = (perrp > 0 & perrp < 3 & hrhtype > 0 & hrhtype < 9 & hrmis != 1 & hrmis != 5)
generate isAdult = (isPerson & prtage >= 15)
generate isRespondent = (puelgflg == 20 & hrmis != 1 & hrmis != 5)
rename pwsupwgt personWeight
rename hwsupwgt householdWeight
rename pwprmwgt respondentWeight

* Step 3: Construct demographic variables. The code can likely be reused except in older datasets.
recode prtage (-1/2 = .) (3/14 = 0 "3-14") (15/24 = 1 "15-24") (25/44 = 2 "25-44") (45/64 = 3 "45-64") (nonmissing = 4 "65+"), gen(ageGroup)
recode pemlr (-1 = .) (1/2 = 0 "Employed") (3/4 = 1 "Unemployed") (5/7 = 2 "Not in Labor Force"), gen(workStatus)
recode hefaminc (-1 = .) (1/7 = 0 "< $25,000") (8/11 = 1 "$25,000-49,999") (12/13 = 2 "$50,000-74,999") (14 = 3 "$75,000-99,999") (15/16 = 4 "$100,000 +"), gen(income)
recode peeduca (-1 = .) (31/38 = 0 "No Diploma") (39 = 1 "High School Diploma") (40/42 = 2 "Some College or AA") (nonmissing = 3 "College Degree or More"), gen(education)
recode pesex (-1 = .) (1 = 0 "Male") (2 = 1 "Female"), gen(sex)
recode ptdtrace (-1 = .) (1 = 0 "White, non-Hispanic") (2 = 1 "Black, non-Hispanic") (99 = 2 "Hispanic") (4 = 3 "Asian, non-Hispanic") (3 = 4 "Am Indian/AK Nat, non-Hispanic") (nonmissing = 5 "Other"), gen(race)
replace race = 2 if pehspnon == 1 // Hispanic ID comes from separate variable
recode prdisflg (-1 = .) (2 = 0 "Not Disabled") (1 = 1 "Disabled"), gen(disability)
recode gtmetsta (2 = 0 "Non-Metropolitan Area") (1 = 1 "Metropolitan Area") (3 = 2 "Unknown"), gen(metro)
bysort qstnum: egen schoolChildrenAtHome = max(prtage <= 17 & prtage >= 6 & perrp >= 4) if hrintsta == 1
label define schoolChildrenAtHome 0 "No" 1 "Yes"
label values schoolChildrenAtHome schoolChildrenAtHome

* Step 4: Construct variables being written to analyze table. Make sure anything outside the specified universe is set to missing.

* - Person Variables
generate desktopUser = (pedesk == 1) if isPerson
generate laptopUser = (pelapt == 1) if isPerson
generate tabletUser = (petabl == 1) if isPerson
generate mobilePhoneUser = (pecell == 1) if isPerson
generate tvBoxUser = (pegame == 1 | petvba == 1) if isPerson
generate homeInternetUser = (pehome == 1) if isPerson
generate workInternetUser = (pewrka == 1) if isPerson
generate schoolInternetUser = (peschl == 1) if isPerson
generate altHomeInternetUser = (peelhs == 1) if isPerson
generate internetUser = (peperscr == 1) if isPerson
generate adultInternetUser = internetUser if isAdult

* - Household Variables
bysort qstnum: egen internetAnywhere = max(peperscr == 1)
replace internetAnywhere = . if isHouseholder != 1
generate internetAtHome = (henet3 == 1) if isHouseholder
generate noInternetAtHome = (internetAtHome == 0) if isHouseholder
generate speedMostImportant = (hesci17 == 1) if internetAtHome == 1
generate reliabilityMostImportant = (hesci17 == 2) if internetAtHome == 1
generate affordabilityMostImportant = (hesci17 == 3) if internetAtHome == 1
generate serviceMostImportant = (hesci17 == 4) if internetAtHome == 1
generate mobilityMostImportant = (hesci17 == 5) if internetAtHome == 1
generate dataCapMostImportant = (hesci17 == 6) if internetAtHome == 1
generate mobileAtHome = (henet45 == 1) if internetAtHome == 1
generate wiredHighSpeedAtHome = (henet42 == 1 | henet43 == 1 | henet44 == 1) if internetAtHome == 1
generate satelliteAtHome = (henet46 == 1) if internetAtHome == 1
generate dialUpAtHome = (henet41 == 1) if internetAtHome == 1
generate ispBundle = (hesci11 == 1) if internetAtHome == 1
generate tvInBundle = (hesci121 == 1 | hesci122 == 1) if ispBundle == 1
generate homePhoneInBundle = (hesci123 == 1) if ispBundle == 1
generate homeEverOnline = (henet3a == 1) if noInternetAtHome == 1
generate noNeedInterestMainReason = (henet6 == 1 | henet7 == 1) if noInternetAtHome == 1
generate tooExpensiveMainReason = (henet6 == 2 | henet7 == 2) if noInternetAtHome == 1
generate canUseElsewhereMainReason = (henet6 == 3 | henet7 == 3) if noInternetAtHome == 1
generate unavailableMainReason = (henet6 == 4 | henet7 == 4) if noInternetAtHome == 1
generate noComputerMainReason = (henet6 == 5 | henet7 == 5) if noInternetAtHome == 1
generate privSecMainReason = (henet6 == 6 | henet7 == 6) if noInternetAtHome == 1
generate computerAtHome = (henet2 == 1) if isHouseholder

* - Respondent Variables
* Note: These questions were asked of peprim1 < 6, which does not overlap completely with peperscr responses.
*       Analysis was undertaken in the below fashion for consistency between datasets and the assumption that
*       rare Internet users aren't undertaking these activities.
generate emailUser = (peprm35 == 1) if isRespondent & adultInternetUser == 1
generate textIMUser = (peprm36 == 1) if isRespondent & adultInternetUser == 1
generate videoUser = (peprm33 == 1) if isRespondent & adultInternetUser == 1
generate locationServicesUser = (peprm313 == 1) if isRespondent & adultInternetUser == 1
generate teleworkUser = (peprm37 == 1) if isRespondent & adultInternetUser == 1
generate jobSearchUser = (peprm38 == 1) if isRespondent & adultInternetUser == 1
generate financeUser = (peprm311 == 1) if isRespondent & adultInternetUser == 1
generate eCommerceUser = (peprm312 == 1) if isRespondent & adultInternetUser == 1
generate healthInfoUser = (peprm314 == 1) if isRespondent & adultInternetUser == 1
generate healthRecordsUser = (peprm315 == 1 | peprm316 == 1) if isRespondent & adultInternetUser == 1
