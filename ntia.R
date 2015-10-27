# library(scidb)
# scidbconnect()

ptm <- proc.time()

library(readr)

# ntia <- as.scidb(read_csv('./jul13-cps.csv'),chunksize=c(1000000000))
ntia <- read_csv('./jul13-cps.csv')
# ntia_x <- as.scidb(ntia)

isPerson = ntia$prtage >= 3 & ntia$prpertyp != 3 & ntia$hrmis != 1 & ntia$hrmis != 5
isHouseholder = ntia$perrp > 0 & ntia$perrp < 3 & ntia$hrhtype > 0 & ntia$hrhtype < 9 & ntia$hrmis != 1 & ntia$hrmis != 5
isAdult = isPerson & ntia$prtage >= 15
isRespondent = ntia$puelgflg == 20 & ntia$hrmis != 1 & ntia$hrmis != 5
personWeight <- ntia$pwsupwgt
householdWeight <- ntia$hwsupwgt
respondentWeight <- ntia$pwprmwgt
personRep <- ntia[,c("pewgt1", "pewgt2", "pewgt3", "pewgt4", "pewgt5", "pewgt6", "pewgt7", "pewgt8", "pewgt9", "pewgt10", "pewgt11", "pewgt12", "pewgt13", "pewgt14", "pewgt15", "pewgt16", "pewgt17", "pewgt18", "pewgt19", "pewgt20", "pewgt21", "pewgt22", "pewgt23", "pewgt24", "pewgt25", "pewgt26", "pewgt27", "pewgt28", "pewgt29", "pewgt30", "pewgt31", "pewgt32", "pewgt33", "pewgt34", "pewgt35", "pewgt36", "pewgt37", "pewgt38", "pewgt39", "pewgt40", "pewgt41", "pewgt42", "pewgt43", "pewgt44", "pewgt45", "pewgt46", "pewgt47", "pewgt48", "pewgt49", "pewgt50", "pewgt51", "pewgt52", "pewgt53", "pewgt54", "pewgt55", "pewgt56", "pewgt57", "pewgt58", "pewgt59", "pewgt60", "pewgt61", "pewgt62", "pewgt63", "pewgt64", "pewgt65", "pewgt66", "pewgt67", "pewgt68", "pewgt69", "pewgt70", "pewgt71", "pewgt72", "pewgt73", "pewgt74", "pewgt75", "pewgt76", "pewgt77", "pewgt78", "pewgt79", "pewgt80", "pewgt81", "pewgt82", "pewgt83", "pewgt84", "pewgt85", "pewgt86", "pewgt87", "pewgt88", "pewgt89", "pewgt90", "pewgt91", "pewgt92", "pewgt93", "pewgt94", "pewgt95", "pewgt96", "pewgt97", "pewgt98", "pewgt99", "pewgt100", "pewgt101", "pewgt102", "pewgt103", "pewgt104", "pewgt105", "pewgt106", "pewgt107", "pewgt108", "pewgt109", "pewgt110", "pewgt111", "pewgt112", "pewgt113", "pewgt114", "pewgt115", "pewgt116", "pewgt117", "pewgt118", "pewgt119", "pewgt120", "pewgt121", "pewgt122", "pewgt123", "pewgt124", "pewgt125", "pewgt126", "pewgt127", "pewgt128", "pewgt129", "pewgt130", "pewgt131", "pewgt132", "pewgt133", "pewgt134", "pewgt135", "pewgt136", "pewgt137", "pewgt138", "pewgt139", "pewgt140", "pewgt141", "pewgt142", "pewgt143", "pewgt144", "pewgt145", "pewgt146", "pewgt147", "pewgt148", "pewgt149", "pewgt150", "pewgt151", "pewgt152", "pewgt153", "pewgt154", "pewgt155", "pewgt156", "pewgt157", "pewgt158", "pewgt159", "pewgt160")]
householdRep <- ntia[,c("hhwgt1", "hhwgt2", "hhwgt3", "hhwgt4", "hhwgt5", "hhwgt6", "hhwgt7", "hhwgt8", "hhwgt9", "hhwgt10", "hhwgt11", "hhwgt12", "hhwgt13", "hhwgt14", "hhwgt15", "hhwgt16", "hhwgt17", "hhwgt18", "hhwgt19", "hhwgt20", "hhwgt21", "hhwgt22", "hhwgt23", "hhwgt24", "hhwgt25", "hhwgt26", "hhwgt27", "hhwgt28", "hhwgt29", "hhwgt30", "hhwgt31", "hhwgt32", "hhwgt33", "hhwgt34", "hhwgt35", "hhwgt36", "hhwgt37", "hhwgt38", "hhwgt39", "hhwgt40", "hhwgt41", "hhwgt42", "hhwgt43", "hhwgt44", "hhwgt45", "hhwgt46", "hhwgt47", "hhwgt48", "hhwgt49", "hhwgt50", "hhwgt51", "hhwgt52", "hhwgt53", "hhwgt54", "hhwgt55", "hhwgt56", "hhwgt57", "hhwgt58", "hhwgt59", "hhwgt60", "hhwgt61", "hhwgt62", "hhwgt63", "hhwgt64", "hhwgt65", "hhwgt66", "hhwgt67", "hhwgt68", "hhwgt69", "hhwgt70", "hhwgt71", "hhwgt72", "hhwgt73", "hhwgt74", "hhwgt75", "hhwgt76", "hhwgt77", "hhwgt78", "hhwgt79", "hhwgt80", "hhwgt81", "hhwgt82", "hhwgt83", "hhwgt84", "hhwgt85", "hhwgt86", "hhwgt87", "hhwgt88", "hhwgt89", "hhwgt90", "hhwgt91", "hhwgt92", "hhwgt93", "hhwgt94", "hhwgt95", "hhwgt96", "hhwgt97", "hhwgt98", "hhwgt99", "hhwgt100", "hhwgt101", "hhwgt102", "hhwgt103", "hhwgt104", "hhwgt105", "hhwgt106", "hhwgt107", "hhwgt108", "hhwgt109", "hhwgt110", "hhwgt111", "hhwgt112", "hhwgt113", "hhwgt114", "hhwgt115", "hhwgt116", "hhwgt117", "hhwgt118", "hhwgt119", "hhwgt120", "hhwgt121", "hhwgt122", "hhwgt123", "hhwgt124", "hhwgt125", "hhwgt126", "hhwgt127", "hhwgt128", "hhwgt129", "hhwgt130", "hhwgt131", "hhwgt132", "hhwgt133", "hhwgt134", "hhwgt135", "hhwgt136", "hhwgt137", "hhwgt138", "hhwgt139", "hhwgt140", "hhwgt141", "hhwgt142", "hhwgt143", "hhwgt144", "hhwgt145", "hhwgt146", "hhwgt147", "hhwgt148", "hhwgt149", "hhwgt150", "hhwgt151", "hhwgt152", "hhwgt153", "hhwgt154", "hhwgt155", "hhwgt156", "hhwgt157", "hhwgt158", "hhwgt159", "hhwgt160")]
respondentRep <- ntia[,c("rewgt1", "rewgt2", "rewgt3", "rewgt4", "rewgt5", "rewgt6", "rewgt7", "rewgt8", "rewgt9", "rewgt10", "rewgt11", "rewgt12", "rewgt13", "rewgt14", "rewgt15", "rewgt16", "rewgt17", "rewgt18", "rewgt19", "rewgt20", "rewgt21", "rewgt22", "rewgt23", "rewgt24", "rewgt25", "rewgt26", "rewgt27", "rewgt28", "rewgt29", "rewgt30", "rewgt31", "rewgt32", "rewgt33", "rewgt34", "rewgt35", "rewgt36", "rewgt37", "rewgt38", "rewgt39", "rewgt40", "rewgt41", "rewgt42", "rewgt43", "rewgt44", "rewgt45", "rewgt46", "rewgt47", "rewgt48", "rewgt49", "rewgt50", "rewgt51", "rewgt52", "rewgt53", "rewgt54", "rewgt55", "rewgt56", "rewgt57", "rewgt58", "rewgt59", "rewgt60", "rewgt61", "rewgt62", "rewgt63", "rewgt64", "rewgt65", "rewgt66", "rewgt67", "rewgt68", "rewgt69", "rewgt70", "rewgt71", "rewgt72", "rewgt73", "rewgt74", "rewgt75", "rewgt76", "rewgt77", "rewgt78", "rewgt79", "rewgt80", "rewgt81", "rewgt82", "rewgt83", "rewgt84", "rewgt85", "rewgt86", "rewgt87", "rewgt88", "rewgt89", "rewgt90", "rewgt91", "rewgt92", "rewgt93", "rewgt94", "rewgt95", "rewgt96", "rewgt97", "rewgt98", "rewgt99", "rewgt100", "rewgt101", "rewgt102", "rewgt103", "rewgt104", "rewgt105", "rewgt106", "rewgt107", "rewgt108", "rewgt109", "rewgt110", "rewgt111", "rewgt112", "rewgt113", "rewgt114", "rewgt115", "rewgt116", "rewgt117", "rewgt118", "rewgt119", "rewgt120", "rewgt121", "rewgt122", "rewgt123", "rewgt124", "rewgt125", "rewgt126", "rewgt127", "rewgt128", "rewgt129", "rewgt130", "rewgt131", "rewgt132", "rewgt133", "rewgt134", "rewgt135", "rewgt136", "rewgt137", "rewgt138", "rewgt139", "rewgt140", "rewgt141", "rewgt142", "rewgt143", "rewgt144", "rewgt145", "rewgt146", "rewgt147", "rewgt148", "rewgt149", "rewgt150", "rewgt151", "rewgt152", "rewgt153", "rewgt154", "rewgt155", "rewgt156", "rewgt157", "rewgt158", "rewgt159", "rewgt160")]

# Demographic variables
ageGroup <- rep('4 65+', length(ntia$prtage))
ageGroup[ntia$prtage >= -1 & ntia$prtage <= 2] = NA
ageGroup[ntia$prtage >= 3 & ntia$prtage <= 9] = '0 3-14'
ageGroup[ntia$prtage >= 10 & ntia$prtage <= 14] = '0 3-14'
ageGroup[ntia$prtage >= 15 & ntia$prtage <= 24] = '1 15-24'
ageGroup[ntia$prtage >= 25 & ntia$prtage <= 44] = '2 25-44'
ageGroup[ntia$prtage >= 45 & ntia$prtage <= 64] = '3 45-64'
workStatus <- ntia$pemlr
workStatus[ntia$pemlr == -1] = NA
workStatus[ntia$pemlr >= 1 & ntia$pemlr <= 2] = '0 Employed'
workStatus[ntia$pemlr >= 3 & ntia$pemlr <= 4] = '1 Unemployed'
workStatus[ntia$pemlr >= 5 & ntia$pemlr <= 7] = '2 Not in Labor Force'
income <- ntia$hefaminc
income[ntia$hefaminc == -1] = NA
income[ntia$hefaminc >= 1 & ntia$hefaminc <= 7] = '0 < $25,000'
income[ntia$hefaminc >= 8 & ntia$hefaminc <= 9] = '1 $25,000-49,999'
income[ntia$hefaminc >= 10 & ntia$hefaminc <= 11] = '1 $25,000-49,999'
income[ntia$hefaminc >= 12 & ntia$hefaminc <= 13] = '2 $50,000-74,999'
income[ntia$hefaminc == 14] = '3 $75,000-99,999'
income[ntia$hefaminc >= 15 & ntia$hefaminc <= 16] = '4 $100,000 +'
education <- rep('3 College Degree or More', length(ntia$peeduca))
education[ntia$peeduca == -1] = NA
education[ntia$peeduca >= 31 & ntia$peeduca <= 38] = '0 No Diploma'
education[ntia$peeduca == 39] = '1 High School Diploma'
education[ntia$peeduca >= 40 & ntia$peeduca <= 42] = '2 Some College or AA'
sex <- ntia$pesex
sex[ntia$psex == -1] = NA
sex[ntia$psex == 1] = '0 Male'
sex[ntia$psex == 2] = '1 Female '
race <- rep('5 Other', length(ntia$ptdtrace))
race[ntia$ptdtrace == -1] = NA
race[ntia$ptdtrace == 1] = '0 White, non-Hispanic'
race[ntia$ptdtrace == 2] = '1 Black, non-Hispanic'
race[ntia$ptdtrace == 99] = '2 Hispanic'
race[ntia$ptdtrace == 4] = '3 Asian, non-Hispanic'
race[ntia$ptdtrace == 3] = '4 Am Indian/AK Nat, non-Hispanic'
race[ntia$pehspnon == 1] = '2 Hispanic'
disability <- ntia$prdisflg
disability[ntia$prdisflg == -1] = NA
disability[ntia$prdisflg == 2] = '0 Not Disabled'
disability[ntia$prdisflg == 1] = '1 Disabled' 
metro <- ntia$gtmetsta
metro[ntia$gtmetsta == 2] = '0 Non-Metropolitan Area'
metro[ntia$gtmetsta == 1] = '1 Metropolitan Area'
metro[ntia$gtmetsta == 3] = '2 Unknown'
schoolChildrenAtHome = ntia$prtage <= 17 & ntia$prtage >= 6 & ntia$perrp >= 4 & ntia$hrintsta == 1
# demoVars <- c(ageGroup, workStatus, income, education, sex, race, disability, metro)

# Person Variables
desktopUser = ntia$pedesk == 1 & isPerson
laptopUser = ntia$pelapt == 1 & isPerson
tabletUser = ntia$petabl == 1 & isPerson
mobilePhoneUser = ntia$pecell == 1 & isPerson
tvBoxUser = (ntia$pegame == 1 | ntia$petvba == 1) & isPerson
homeInternetUser = ntia$pehome == 1 & isPerson
workInternetUser = ntia$pewrka == 1 & isPerson
schoolInternetUser = ntia$peschl == 1 & isPerson
altHomeInternetUser = ntia$peelhs == 1 & isPerson
internetUser = ntia$peperscr == 1 & isPerson
adultInternetUser = internetUser & isAdult
# personVars <= cbind(desktopUser, laptopUser, tabletUser, mobilePhoneUser, tvBoxUser, homeInternetUser, workInternetUser, schoolInternetUser, altHomeInternetUser, internetUser, adultInternetUser)

# Household Variables
internetAnywhere = ntia$peperscr == 1
internetAnywhere[!isHouseholder] = NA
internetAtHome = ntia$henet3 == 1 & isHouseholder
noInternetAtHome = !internetAtHome & isHouseholder
speedMostImportant = ntia$hesci17 == 1 & internetAtHome
reliabilityMostImportant = ntia$hesci17 == 2 & internetAtHome
affordabilityMostImportant = ntia$hesci17 == 3 & internetAtHome
serviceMostImportant = ntia$hesci17 == 4 & internetAtHome
mobilityMostImportant = ntia$hesci17 == 5 & internetAtHome
dataCapMostImportant = ntia$hesci17 == 6 & internetAtHome
mobileAtHome = ntia$henet45 == 1 & internetAtHome
wiredHighSpeedAtHome = ntia$henet42 == 1 | ntia$henet43 == 1 | ntia$henet44 == 1 & internetAtHome
satelliteAtHome = ntia$henet46 == 1 & internetAtHome
dialUpAtHome = ntia$henet41 == 1 & internetAtHome
ispBundle = ntia$hesci11 == 1 & internetAtHome
tvInBundle = ntia$hesci121 == 1 | ntia$hesci122 == 1 & ispBundle
homePhoneInBundle = ntia$hesci123 == 1 & ispBundle
homeEverOnline = ntia$henet3a == 1 & noInternetAtHome
noNeedInterestMainReason = ntia$henet6 == 1 | ntia$henet7 == 1 & noInternetAtHome
tooExpensiveMainReason = ntia$henet6 == 2 | ntia$henet7 == 2 & noInternetAtHome
canUseElsewhereMainReason = ntia$henet6 == 3 | ntia$henet7 == 3 & noInternetAtHome
unavailableMainReason = ntia$henet6 == 4 | ntia$henet7 == 4 & noInternetAtHome
noComputerMainReason = ntia$henet6 == 5 | ntia$henet7 == 5 & noInternetAtHome
privSecMainReason = ntia$henet6 == 6 | ntia$henet7 == 6 & noInternetAtHome
computerAtHome = ntia$henet2 == 1 & isHouseholder
# householdVars <- cbind(internetAnywhere, internetAtHome, noInternetAtHome, speedMostImportant, reliabilityMostImportant, affordabilityMostImportant, serviceMostImportant, mobilityMostImportant, dataCapMostImportant, mobileAtHome, wiredHighSpeedAtHome, satelliteAtHome, dialUpAtHome, ispBundle, tvInBundle, homePhoneInBundle, homeEverOnline, noNeedInterestMainReason, tooExpensiveMainReason, canUseElsewhereMainReason, unavailableMainReason, noComputerMainReason, privSecMainReason, computerAtHome)

# Respondent Variables
emailUser = ntia$peprm35 == 1 & isRespondent & adultInternetUser
textIMUser = ntia$peprm36 == 1 & isRespondent & adultInternetUser
videoUser = ntia$peprm33 == 1 & isRespondent & adultInternetUser
locationServicesUser = ntia$peprm313 == 1 & isRespondent & adultInternetUser
teleworkUser = ntia$peprm37 == 1 & isRespondent & adultInternetUser
jobSearchUser = ntia$peprm38 == 1 & isRespondent & adultInternetUser
financeUser = ntia$peprm311 == 1 & isRespondent & adultInternetUser
eCommerceUser = ntia$peprm312 == 1 & isRespondent & adultInternetUser
healthInfoUser = ntia$peprm314 == 1 & isRespondent & adultInternetUser
healthRecordsUser = ntia$peprm315 == 1 | ntia$peprm316 == 1 & isRespondent & adultInternetUser
# respondentVars <- cbind(emailUser, textIMUser, videoUser, locationServicesUser, teleworkUser, jobSearchUser, financeUser, eCommerceUser, healthInfoUser, healthRecordsUser)

ntia_estimate <- function(feature, basewgt, repwgt){
    mean_feat = mean(feature * basewgt, na.rm=TRUE)
    mean_replicates = colMeans(feature * repwgt, na.rm=TRUE)
    mean_se = ((4/160)*sum((mean_feat - mean_replicates)^2))^.5
    sum_feat = sum(feature * basewgt, na.rm=TRUE)
    sum_replicates = colSums(feature * repwgt, na.rm=TRUE)
    sum_se = ((4/160)*sum((sum_feat - sum_replicates)^2))^.5
    return(cbind(mean_feat,mean_se,sum_feat,sum_se))
}

ntia_demo_est <- function(feature, basewgt, repwgt, demo){
    mean_feat = aggregate(feature * basewgt, by=list(Category=demo), FUN=mean, na.rm=TRUE)
    mean_rep = aggregate(feature * repwgt, by=list(Category=demo), FUN=mean, na.rm=TRUE)
    mean_se = ((4/160) * rowSums(mean_feat[,2] - mean_rep[,2:161])^2)^.5
    sum_feat = aggregate(feature * basewgt, by=list(Category=demo), FUN=sum, na.rm=TRUE)
    sum_rep = aggregate(feature * repwgt, by=list(Category=demo), FUN=sum, na.rm=TRUE)
    sum_se = ((4/160) * rowSums(sum_feat[,2] -sum_rep[,2:161])^2)^.5
    final = cbind(mean_feat, mean_se, sum_feat$x, sum_se)
    colnames(final) = c("category", "mean_feat", "mean_se", "sum_feat", "sum_se")
    return(final)
}

ntia_demo_all <- function(feature, basewgt, repwgt){
    demo_est = list(ntia_demo_est(feature, basewgt, repwgt, ageGroup), 
        ntia_demo_est(feature, basewgt, repwgt, workStatus), 
        ntia_demo_est(feature, basewgt, repwgt, income), 
        ntia_demo_est(feature, basewgt, repwgt, education), 
        ntia_demo_est(feature, basewgt, repwgt, sex), 
        ntia_demo_est(feature, basewgt, repwgt, race), 
        ntia_demo_est(feature, basewgt, repwgt, disability), 
        ntia_demo_est(feature, basewgt, repwgt, metro))
    names(demo_est) = c("ageGroup", "workStatus", "income", "education", "sex", "race", "disability", "metro")
    return(demo_est)
}

# Person Statistics
per_est <- rbind(ntia_estimate(desktopUser, personWeight, personRep), ntia_estimate(laptopUser, personWeight, personRep), ntia_estimate(tabletUser, personWeight, personRep), ntia_estimate(mobilePhoneUser, personWeight, personRep), ntia_estimate(tvBoxUser, personWeight, personRep), ntia_estimate(homeInternetUser, personWeight, personRep), ntia_estimate(workInternetUser, personWeight, personRep), ntia_estimate(schoolInternetUser, personWeight, personRep), ntia_estimate(altHomeInternetUser, personWeight, personRep), ntia_estimate(internetUser, personWeight, personRep), ntia_estimate(adultInternetUser, personWeight, personRep))
row.names(per_est) <- c("desktopUser", "laptopUser", "tabletUser", "mobilePhoneUser", "tvBoxUser", "homeInternetUser", "workInternetUser", "schoolInternetUser", "altHomeInternetUser", "internetUser", "adultInternetUser")

per_demo_est <- list(ntia_demo_all(desktopUser, personWeight, personRep), ntia_demo_all(laptopUser, personWeight, personRep), ntia_demo_all(tabletUser, personWeight, personRep), ntia_demo_all(mobilePhoneUser, personWeight, personRep), ntia_demo_all(tvBoxUser, personWeight, personRep), ntia_demo_all(homeInternetUser, personWeight, personRep), ntia_demo_all(workInternetUser, personWeight, personRep), ntia_demo_all(schoolInternetUser, personWeight, personRep), ntia_demo_all(altHomeInternetUser, personWeight, personRep), ntia_demo_all(internetUser, personWeight, personRep), ntia_demo_all(adultInternetUser, personWeight, personRep))
names(per_demo_est) <- c("desktopUser", "laptopUser", "tabletUser", "mobilePhoneUser", "tvBoxUser", "homeInternetUser", "workInternetUser", "schoolInternetUser", "altHomeInternetUser", "internetUser", "adultInternetUser")

# Household statistics
hou_est <- rbind(ntia_estimate(internetAnywhere, householdWeight, householdRep), ntia_estimate(internetAtHome, householdWeight, householdRep), ntia_estimate(noInternetAtHome, householdWeight, householdRep), ntia_estimate(speedMostImportant, householdWeight, householdRep), ntia_estimate(reliabilityMostImportant, householdWeight, householdRep), ntia_estimate(affordabilityMostImportant, householdWeight, householdRep), ntia_estimate(serviceMostImportant, householdWeight, householdRep), ntia_estimate(mobilityMostImportant, householdWeight, householdRep), ntia_estimate(dataCapMostImportant, householdWeight, householdRep), ntia_estimate(mobileAtHome, householdWeight, householdRep), ntia_estimate(wiredHighSpeedAtHome, householdWeight, householdRep), ntia_estimate(satelliteAtHome, householdWeight, householdRep), ntia_estimate(dialUpAtHome, householdWeight, householdRep), ntia_estimate(ispBundle, householdWeight, householdRep), ntia_estimate(tvInBundle, householdWeight, householdRep), ntia_estimate(homePhoneInBundle, householdWeight, householdRep), ntia_estimate(homeEverOnline, householdWeight, householdRep), ntia_estimate(noNeedInterestMainReason, householdWeight, householdRep), ntia_estimate(tooExpensiveMainReason, householdWeight, householdRep), ntia_estimate(canUseElsewhereMainReason, householdWeight, householdRep), ntia_estimate(unavailableMainReason, householdWeight, householdRep), ntia_estimate(noComputerMainReason, householdWeight, householdRep), ntia_estimate(privSecMainReason, householdWeight, householdRep), ntia_estimate(computerAtHome, householdWeight, householdRep))
row.names(hou_est) <- c("internetAnywhere", "internetAtHome", "noInternetAtHome", "speedMostImportant", "reliabilityMostImportant", "affordabilityMostImportant", "serviceMostImportant", "mobilityMostImportant", "dataCapMostImportant", "mobileAtHome", "wiredHighSpeedAtHome", "satelliteAtHome", "dialUpAtHome", "ispBundle", "tvInBundle", "homePhoneInBundle", "homeEverOnline", "noNeedInterestMainReason", "tooExpensiveMainReason", "canUseElsewhereMainReason", "unavailableMainReason", "noComputerMainReason", "privSecMainReason", "computerAtHome")

hou_demo_est <- list(ntia_demo_all(internetAnywhere, householdWeight, householdRep), ntia_demo_all(internetAtHome, householdWeight, householdRep), ntia_demo_all(noInternetAtHome, householdWeight, householdRep), ntia_demo_all(speedMostImportant, householdWeight, householdRep), ntia_demo_all(reliabilityMostImportant, householdWeight, householdRep), ntia_demo_all(affordabilityMostImportant, householdWeight, householdRep), ntia_demo_all(serviceMostImportant, householdWeight, householdRep), ntia_demo_all(mobilityMostImportant, householdWeight, householdRep), ntia_demo_all(dataCapMostImportant, householdWeight, householdRep), ntia_demo_all(mobileAtHome, householdWeight, householdRep), ntia_demo_all(wiredHighSpeedAtHome, householdWeight, householdRep), ntia_demo_all(satelliteAtHome, householdWeight, householdRep), ntia_demo_all(dialUpAtHome, householdWeight, householdRep), ntia_demo_all(ispBundle, householdWeight, householdRep), ntia_demo_all(tvInBundle, householdWeight, householdRep), ntia_demo_all(homePhoneInBundle, householdWeight, householdRep), ntia_demo_all(homeEverOnline, householdWeight, householdRep), ntia_demo_all(noNeedInterestMainReason, householdWeight, householdRep), ntia_demo_all(tooExpensiveMainReason, householdWeight, householdRep), ntia_demo_all(canUseElsewhereMainReason, householdWeight, householdRep), ntia_demo_all(unavailableMainReason, householdWeight, householdRep), ntia_demo_all(noComputerMainReason, householdWeight, householdRep), ntia_demo_all(privSecMainReason, householdWeight, householdRep), ntia_demo_all(computerAtHome, householdWeight, householdRep))
names(hou_demo_est) <- c("internetAnywhere", "internetAtHome", "noInternetAtHome", "speedMostImportant", "reliabilityMostImportant", "affordabilityMostImportant", "serviceMostImportant", "mobilityMostImportant", "dataCapMostImportant", "mobileAtHome", "wiredHighSpeedAtHome", "satelliteAtHome", "dialUpAtHome", "ispBundle", "tvInBundle", "homePhoneInBundle", "homeEverOnline", "noNeedInterestMainReason", "tooExpensiveMainReason", "canUseElsewhereMainReason", "unavailableMainReason", "noComputerMainReason", "privSecMainReason", "computerAtHome")

# Respondent Statistics
res_est <- rbind(ntia_estimate(emailUser, respondentWeight, respondentRep), ntia_estimate(textIMUser, respondentWeight, respondentRep), ntia_estimate(videoUser, respondentWeight, respondentRep), ntia_estimate(locationServicesUser, respondentWeight, respondentRep), ntia_estimate(teleworkUser, respondentWeight, respondentRep), ntia_estimate(jobSearchUser, respondentWeight, respondentRep), ntia_estimate(financeUser, respondentWeight, respondentRep), ntia_estimate(eCommerceUser, respondentWeight, respondentRep), ntia_estimate(healthInfoUser, respondentWeight, respondentRep), ntia_estimate(healthRecordsUser, respondentWeight, respondentRep))
row.names(res_est) <- c("emailUser", "textIMUser", "videoUser", "locationServicesUser", "teleworkUser", "jobSearchUser", "financeUser", "eCommerceUser", "healthInfoUser", "healthRecordsUser")

res_demo_est <- list(ntia_demo_all(emailUser, respondentWeight, respondentRep), ntia_demo_all(textIMUser, respondentWeight, respondentRep), ntia_demo_all(videoUser, respondentWeight, respondentRep), ntia_demo_all(locationServicesUser, respondentWeight, respondentRep), ntia_demo_all(teleworkUser, respondentWeight, respondentRep), ntia_demo_all(jobSearchUser, respondentWeight, respondentRep), ntia_demo_all(financeUser, respondentWeight, respondentRep), ntia_demo_all(eCommerceUser, respondentWeight, respondentRep), ntia_demo_all(healthInfoUser, respondentWeight, respondentRep), ntia_demo_all(healthRecordsUser, respondentWeight, respondentRep))
names(res_demo_est) <- c("emailUser", "textIMUser", "videoUser", "locationServicesUser", "teleworkUser", "jobSearchUser", "financeUser", "eCommerceUser", "healthInfoUser", "healthRecordsUser")

print(proc.time() - ptm)
