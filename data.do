clear
clear matrix
// cd "C:\Users\Anna Goodman\Dropbox\z_ITHIMfiles\Stats19"

	** DOWNLOAD 2005-2014 AND 2015 DATA FROM https://data.gov.uk/dataset/road-accidents-safety-data. SAVE THESE IN 'Stats19\1a_DataOriginal' FOLDER
	** DOWNLOAD CODEBOOK FROM https://discover.ukdataservice.ac.uk/catalogue?sn=7752
	** MAKE AN EMPTY FOLDER '1b_DataCreated', IN WHICH '0-temp'

	***************************
	* PREPARE DATASETS
	***************************	
	/** MERGE 2005-2014 AND 2015 DATASETS
		foreach x in Accidents Casualties Vehicles {
		import delimited "1a_DataOriginal/`x'_2015.csv", clear
		saveold "1a_DataOriginal/`x'_2015.dta", replace
		import delimited "1a_DataOriginal/`x'0514.csv", clear
		rename ïaccident_index accident_index
		append using "1a_DataOriginal/`x'_2015.dta", 
		saveold "1b_DataCreated/0-temp/`x'0515.dta", replace
		}
		*/
	** MERGE 3 DATASETS, KEEPING REQUIRED VARIABLES
		use "1b_DataCreated/0-temp/Accidents0515.dta", clear
		keep accident_index local_authority_district st_road_class date number_of_vehicles
		merge 1:m accident_index using "1b_DataCreated/0-temp/Vehicles0515.dta", keepus(vehicle_reference vehicle_type sex_of_driver age_of_driver) nogen
		merge 1:m accident_index vehicle_reference using "1b_DataCreated/0-temp/Casualties0515.dta", keepus(casualty_reference casualty_class casualty_severity sex_of_casualty age_of_casualty) nogen
		keep if local_authority_district<=699 // Drop Wales and Scotland
		saveold stopped, replace
	** PREPARE VARIABLES
		* SEVERITY
			rename casualty_severity cas_severity
			label define severitylab 1 "Fatal" 2 "Serious" 3 "Slight"
			label values cas_severity severitylab

		* DATE
			split date,p("/")
			destring date3, gen(year)

		* ROAD CLASS
			recode st_road_class 1/2=1 3=2 4/6=3 , gen(roadtype)
			label def roadtypelab 1 "Motorway/A(M)" 2 "A" 3 "B, C, Unclassified"
			label val roadtype roadtypelab

		* MODE AND SEX OF VEHICLE AND CASUALTY
			recode vehicle_type -1=99 1=2 2/5=3 8/9=4 10/11=6 16/18=99 19=5 20/21=7 22=99 23=3 90=99 97=3 98=7 , gen(veh_mode)
			gen cas_mode=veh_mode
			replace cas_mode=1 if casualty_class==3
			replace cas_mode=. if cas_severity==.
			label define modelab 1 "pedestrian" 2 "cyclist" 3 "motorcycle" 4 "car/taxi" 5 "light goods" 6 "bus" 7 "heavy goods" 8 "No other vehicle" 99 "other or unknown", modify
			label values veh_mode modelab
			label values cas_mode modelab

			recode sex_of_casualty -1=. 2=0, gen(cas_male)
			recode sex_of_driver -1=. 2=0 3=., gen(veh_male)
			
			recode age_of_casualty -1=., gen(cas_age)
			recode age_of_driver -1=., gen(veh_age)
			
			rename vehicle_reference veh_reference

		* SELECT A 'STRIKE VEHICLE' PEDESTRIAN AT RANDOM (NB ONLY KNOW ABOUT THOSE PEDESTRIANS WHO WERE INJURED...NB DON'T NEED TO SPLIT BY VEHICLE AS THIS ONLY BECOMES RELEVANT IF NO OTHER VEHICLE BUT THE PEDESTRIAN)
			gen pedflag=(cas_mode==1)
			bysort accident_index : egen numped=sum(pedflag)			

			set seed 2010
			gen random0=uniform()
			foreach x in male age {
			by accident_index cas_mode (random0), sort: gen littlen_cas`x'=_n
			gen ped_cas_`x'_temp=cas_`x'
			replace ped_cas_`x'_temp=. if cas_mode!=1 | littlen_cas`x'!=1  // pedestrian age/sex set as equal to one randomly selected pedestrian within the accident
			bysort accident_index: egen ped_cas_`x'=max(ped_cas_`x'_temp)
			drop littlen_cas`x' ped_cas_`x'_temp
			}

		* DEFINE LARGEST AND SECOND LARGEST OTHER VEHICLES, TO BECOME STRIKE VEHICLE
			preserve
			keep accident_index veh_mode veh_reference veh_male veh_age numped ped_cas_male ped_cas_age 
			duplicates drop
			gen veh_modei=-1*veh_mode
			recode veh_modei -99=99
			set seed 2011
			gen random1=uniform()
			by accident_index (veh_modei random1), sort: gen littlen=_n
			keep accident_index veh_reference veh_mode veh_male veh_age littlen numped ped_cas_male ped_cas_age  
			reshape wide veh_reference veh_mode veh_male veh_age, i(accident_index) j(littlen)
			foreach x in reference mode male age {
			rename veh_`x'1 veh_`x'_firstlarge
			rename veh_`x'2 veh_`x'_secondlarge
			}
			recode veh_mode_secondlarge .=1 if numped!=0
			recode veh_mode_secondlarge .=8
			replace veh_male_secondlarge=ped_cas_male if numped!=0 & veh_mode_secondlarge==1
			replace veh_age_secondlarge=ped_cas_age if numped!=0 & veh_mode_secondlarge==1
			keep accident_index veh_reference_firstlarge veh_reference_secondlarge veh_mode_firstlarge veh_mode_secondlarge veh_male_firstlarge veh_male_secondlarge veh_age_firstlarge veh_age_secondlarge
			saveold "1b_DataCreated/0-temp/stats19_strikemode.dta", replace
			restore

		* MERGE IN AND DEFINE STRIKE MODE - FOR NON-PEDESTRIANS, THIS IS LARGEST OTHER VEHICLE
			merge m:1 accident_index using "1b_DataCreated/0-temp/stats19_strikemode.dta", nogen
			foreach x in mode male age {
			gen strike_`x'=veh_`x' if cas_mode==1
			replace strike_`x'=veh_`x'_firstlarge if cas_mode!=1
			replace strike_`x'=veh_`x'_secondlarge if veh_reference==veh_reference_firstlarge & cas_mode!=1
			}
			label values strike_mode modelab

		/* IMPUTE AT RANDOM MISSING SEX OF A) CASUALTY AND B) STRIKER, IN PROPORTION TO OBSERVED SEX RATIO OF STRIKER COLLISIONS FOR EACH MODE [not done for age]
			set seed 2012
			gen random3=uniform()		
			tab cas_male , miss 
				* missing data 0.2% casualty sex
			tab strike_male if strike_mode!=8, miss 
				* missing data 6.5% striker sex
			foreach x in cas strike {
			gen `x'_mode_sexratio=.
			foreach i in 1 2 3 4 5 6 7 99 {
			sum `x'_male if `x'_mode==`i'
			replace `x'_mode_sexratio=r(mean) if `x'_mode==`i'
			}
			}
			recode cas_male .=1 if random3<=cas_mode_sexratio & cas_severity!=.
			recode cas_male .=0 if random3>cas_mode_sexratio & cas_severity!=.
			recode strike_male .=1 if random3<=strike_mode_sexratio & strike_mode!=8
			recode strike_male .=0 if random3>strike_mode_sexratio & strike_mode!=8			
			*/

		*SAVE
			keep if cas_severity!=. // drop those not in casualty dataset
			order accident_index year roadtype cas_severity cas_mode cas_male cas_age strike_mode strike_male strike_age
			keep accident_index-strike_age
			foreach var of varlist accident_index-strike_age {
			label variable `var'
			}
			compress
			saveold "data.dta", replace


	***************************
	* ANALYSIS
	***************************		
	use "data.dta", clear
		recode strike_mode 3=5 5=6 6=3 7=7 8=9 99=8, gen(strike_modecat)
		recode cas_mode 3=5 5=6 6=3 7=7 8=9 99=8, gen(cas_modecat)
		label define modecatlab 1 "walk" 2 "cycle" 3 "bus" 4 "car" 5 "mbike" 6 "van" 7 "lorry" 8 "other" 9 "no other vehicle" , modify
		label values strike_modecat modecatlab
		label values cas_modecat modecatlab

	* CASUALTY PERSPECTIVE (only use pedestrian, cyclist, motorcycle, car/taxi)
		foreach i in 1 0 {
		foreach j in 1 2 3 {
		bysort cas_severity: tab cas_modecat strike_modecat if cas_male==`i' & roadtype==`j'
		}
		}

	* STRIKER PERSPECTIVE
		foreach i in 1 0 {
		foreach j in 1 2 3 {
		bysort cas_severity: tab strike_modecat cas_modecat if strike_male==`i' & roadtype==`j'
		}
		}
