insheet using "/Users/Andrey/Dropbox/data/expropriation survey/Expropriation Survey_August 19, 2019_13.49.csv", comma clear names

gen exprop1 = .
replace exprop1 = 0 if exprop == ""
replace exprop1 = 1 if exprop == "Income/Profits" | exprop == "Property/Assets"
replace exprop1 = 2 if exprop == "Property/Assets,Income/Profits"

gen inspect1 = .
replace inspect1 = 0 if inspect_high_1== "Average"
replace inspect1 = -1 if inspect_high_1== "Lower than Average"
replace inspect1 = -2 if inspect_high_1== "Much lower than Average"
replace inspect1 = 1 if inspect_high_1== "Higher than Average"
replace inspect1 = 2 if inspect_high_1== "Much higher than Average"

gen inspect2 = .
replace inspect2 = 0 if inspect_high_2== "Average"
replace inspect2 = -1 if inspect_high_2== "Lower than Average"
replace inspect2 = -2 if inspect_high_2== "Much lower than Average"
replace inspect2 = 1 if inspect_high_2== "Higher than Average"
replace inspect2 = 2 if inspect_high_2== "Much higher than Average"

gen inspect3 = .
replace inspect3 = 0 if inspect_high_3== "Average"
replace inspect3 = -1 if inspect_high_3== "Lower than Average"
replace inspect3 = -2 if inspect_high_3== "Much lower than Average"
replace inspect3 = 1 if inspect_high_3== "Higher than Average"
replace inspect3 = 2 if inspect_high_3== "Much higher than Average"

gen inspect4 = .
replace inspect4 = 0 if inspect_high_4== "Average"
replace inspect4 = -1 if inspect_high_4== "Lower than Average"
replace inspect4 = -2 if inspect_high_4== "Much lower than Average"
replace inspect4 = 1 if inspect_high_4== "Higher than Average"
replace inspect4 = 2 if inspect_high_4== "Much higher than Average"

gen exproprisk = .
replace exproprisk = 0 if exprop_risk =="Very low"
replace exproprisk = 1 if exprop_risk =="Low"
replace exproprisk = 2 if exprop_risk =="Moderate"
replace exproprisk = 3 if exprop_risk =="High"
replace exproprisk = 4 if exprop_risk =="Very high"

gen size = .
replace size = 1 if firm_size == "Less than 5"
replace size = 2 if firm_size == "From 5 to 9"
replace size = 3 if firm_size == "From 10 to 19"
replace size = 4 if firm_size == "From 20 to 50"
replace size = 5 if firm_size == "From 51 to 100"
replace size = 6 if firm_size == "From 101 to 250"
replace size = 7 if firm_size == "From 251 to 500"
replace size = 8 if firm_size == "From 501 to 1000"
replace size = 9 if firm_size == "1001 and over"

gen sales = .
replace sales = 1 if sales_uk == "Less than 26 thousand"
replace sales = 2 if sales_uk == "26 thousand to 260 thousand"
replace sales = 3 if sales_uk == "260 thousand to 2.6 million"
replace sales = 4 if sales_uk == "2.6 million to 13 million"
replace sales = 5 if sales_uk == "13 million to 26 million"
replace sales = 6 if sales_uk == "26 million to 260 million"
replace sales = 7 if sales_uk == "260 million to 2.6 billion"
replace sales = 8 if sales_uk == "More than 2.6 billion"

replace sales = 1 if sales_vn == "Less than 26 thousand"
replace sales = 2 if sales_vn == "26 thousand to 260 thousand"
replace sales = 3 if sales_vn == "260 thousand to 2.6 million"
replace sales = 4 if sales_vn == "2.6 million to 13 million"
replace sales = 5 if sales_vn == "13 million to 26 million"
replace sales = 6 if sales_vn == "26 million to 260 million"
replace sales = 7 if sales_vn == "260 million to 2.6 billion"
replace sales = 8 if sales_vn == "More than 2.6 billion"

gen foreign = 0
replace foreign = 1 if q152 != ""

gen ukr = 0
replace ukr = 1 if userlanguage == "RU"

gen income =.
replace income =1 if income_access == "Yes"
replace income =0 if income_access == "No"

gen court1 =.
replace court1 =1 if court == "Yes"
replace court1 =0 if court == "No"

destring inspect_1_1, gen(inspectnum1) force
destring inspect_2_1, gen(inspectnum2) force
destring inspect_3_1, gen(inspectnum3) force
destring inspect_4_1, gen(inspectnum4) force

gen bribe =.
replace bribe = 0 if bribe_income == "0%"
replace bribe = 1 if bribe_income == "Less than 1%"
replace bribe = 2 if bribe_income == "From 2% to less than 5%"
replace bribe = 3 if bribe_income == "From 5% to less than 10%"
replace bribe = 4 if bribe_income == "From 10% to less than 20%"
replace bribe = 5 if bribe_income == "From 20% to less than 30%"
replace bribe = 6 if bribe_income == "Over 30%"

gen beliefs1 = .
replace beliefs1 = 1 if  man_belief_1 == "Strongly disagree"
replace beliefs1 = 2 if man_belief_1 == "Somewhat disagree"
replace beliefs1 = 3 if man_belief_1 == "Neither agree nor disagree"
replace beliefs1 = 4 if man_belief_1 == "Somewhat agree"
replace beliefs1 = 5 if  man_belief_1 == "Strongly agree"

gen beliefs2 = .
replace beliefs2 = 1 if  man_belief_2 == "Strongly disagree"
replace beliefs2 = 2 if man_belief_2 == "Somewhat disagree"
replace beliefs2 = 3 if man_belief_2 == "Neither agree nor disagree"
replace beliefs2 = 4 if man_belief_2 == "Somewhat agree"
replace beliefs2 = 5 if  man_belief_2 == "Strongly agree"

gen beliefs3 = .
replace beliefs3 = 1 if  man_belief_3 == "Strongly disagree"
replace beliefs3 = 2 if man_belief_3 == "Somewhat disagree"
replace beliefs3 = 3 if man_belief_3 == "Neither agree nor disagree"
replace beliefs3 = 4 if man_belief_3 == "Somewhat agree"
replace beliefs3 = 5 if  man_belief_3 == "Strongly agree"

gen numcon1 = length(pol_connect1_1) - length(subinstr(pol_connect1_1, ",", "", .))
replace numcon1 = 1 if length(pol_connect1_1)>1 & numcon1==0

gen numcon2 = length(pol_connect1_2) - length(subinstr(pol_connect1_2, ",", "", .))
replace numcon2 = 1 if length(pol_connect1_2)>1 & numcon1==0

gen numcon3 = length(pol_connect1_3) - length(subinstr(pol_connect1_3, ",", "", .))
replace numcon3 = 1 if length(pol_connect1_3)>1 & numcon1==0

gen numcon4 = length(pol_connect1_4) - length(subinstr(pol_connect1_4, ",", "", .))
replace numcon4 = 1 if length(pol_connect1_4)>1 & numcon1==0

gen consum = numcon1 + numcon2 + numcon3 + numcon4

reg exprop1 pol_con_1 firm_begin size foreign pol_eff consum, robust 
reg inspect1 pol_con_1 firm_begin size foreign pol_eff consum, robust
reg inspect2 pol_con_1 firm_begin size foreign pol_eff consum, robust 
reg inspect3 pol_con_1 firm_begin size foreign pol_eff consum, robust 
reg inspect4 pol_con_1 firm_begin size foreign pol_eff consum, robust 
reg exproprisk  pol_con_1 firm_begin size foreign pol_eff consum, robust 

logit income pol_con_1 firm_begin size foreign pol_eff consum, robust
logit court1  pol_con_1 firm_begin size foreign pol_eff consum, robust

reg inspectnum1 pol_con_1 firm_begin size foreign pol_eff ukr consum, robust 
reg inspectnum2 pol_con_1 firm_begin size foreign pol_eff ukr consum, robust 
reg inspectnum3 pol_con_1 firm_begin size foreign pol_eff ukr consum, robust 
reg inspectnum4 pol_con_1 firm_begin size foreign pol_eff ukr consum, robust 

reg bribe pol_con_1 firm_begin size foreign pol_eff ukr consum, robust 

reg beliefs1 pol_con_1 firm_begin size foreign pol_eff ukr consum, robust 
reg beliefs2 pol_con_1 firm_begin size foreign pol_eff ukr consum, robust 
reg beliefs3 pol_con_1 firm_begin size foreign pol_eff ukr consum, robust

replace sector_1 = "ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS" if sector_1 == "ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS; UNDIFFERENTIATED GOODS- AND SERVICES-PRODUCING ACTIVITIES OF HOUSEHOLDS FOR OWN USE"

encode sector_1, gen(sector)

hist sector if ukr==1, discrete frequency ytitle("") yla(1/20, valuelabel noticks ang(horizontal) labsize(tiny)) horizontal
hist sector if ukr==0, discrete frequency ytitle("") yla(1/20, valuelabel noticks ang(horizontal) labsize(tiny)) horizontal

*************
*insheet using "D:\Users\Andrey\Dropbox\data\expropriation survey\Expropriation Survey_August 19, 2019_13.49.csv", comma clear names
insheet using "/Users/Andrey/Dropbox/data/expropriation survey/Expropriation Survey_August 19, 2019_13.49.csv", comma clear names

gen id = _n

expand 8
sort id
bysort id: gen id2 = _n

gen alloc =.

replace alloc = experiment1_desk_1 if id2 ==1
replace alloc = experiment2_desk_1 if id2 ==3
replace alloc = experiment3_desk_1 if id2 ==5
replace alloc = experiment4_desk_1 if id2 ==7

replace alloc = 100-experiment1_desk_1 if id2 ==2
replace alloc = 100-experiment2_desk_1 if id2 ==4
replace alloc = 100-experiment3_desk_1 if id2 ==6
replace alloc = 100-experiment4_desk_1 if id2 ==8

local list1 employees profit tfp assets years
local list2 own country sector connections


local profits profita profitb profitc profitd profite profitf profitg profith assetsa assetsb assetsc assetsd assetse assetsf assetsg assetsh 

foreach i of local profits{
replace `i' = subinstr(`i',"$","",.)
replace `i' = subinstr(`i',"'","",.)
replace `i' = subinstr(`i',"M","",.)
replace `i' = subinstr(`i',"k","",.)
replace `i' = subinstr(`i',",","",.)
destring `i', replace
}

foreach i of local list1 {
	
	gen a`i' = .
	replace a`i' =`i'a if id2 ==1
	replace a`i' = `i'b if id2 ==2
	replace a`i' = `i'c if id2 ==3
	replace a`i' = `i'd if id2 ==4
	replace a`i' =`i'e if id2 ==5
	replace a`i' = `i'f if id2 ==6
	replace a`i' = `i'g if id2 ==7
	replace a`i' = `i'h if id2 ==8
	}

foreach i of local list2 {
	
	gen a`i' = ""
	replace a`i' =`i'a if id2 ==1
	replace a`i' = `i'b if id2 ==2
	replace a`i' = `i'c if id2 ==3
	replace a`i' = `i'd if id2 ==4
	replace a`i' =`i'e if id2 ==5
	replace a`i' = `i'f if id2 ==6
	replace a`i' = `i'g if id2 ==7
	replace a`i' = `i'h if id2 ==8
	}


	
	
gen countryeng = ""
replace countryeng = "Domestic" if acountry == "Украина" | acountry == "Venezuela" | acountry == "Venezuela, Bolivarian Republic of..."  | acountry == "Ukraine" 
replace countryeng = "USA"  if acountry == "США" | acountry == "Estados Unidos" | acountry == "United States"
replace countryeng = "South Korea"  if acountry == "Южная Корея" | acountry == "Corea del Sur"
replace countryeng = "Brazil"  if acountry == "Brasil" | acountry == "Бразилия" 
replace countryeng = "Japan"  if acountry == "Япония" | acountry == "Japón"
replace countryeng = "China"  if acountry == "Китай" | acountry == "China"
replace countryeng = "Saudi Arabia"  if acountry == "Саудовская Аравия" | acountry == "Arabia Saudita"
replace countryeng = "Germany"  if acountry == "Alemania" | acountry == "Германия"
replace countryeng = "Russia"  if acountry == "Россия" | acountry == "Rusia"

gen connecteng = ""
replace connecteng = "Family Member" if aconnect == "Сын президента входит в совет директоров компании" | aconnect == "Владелец - племянник премьер министра" | aconnect == "Владелец - племянница премьер министра" | aconnect == "Владелица - невестка президента" | aconnect == "Дочь президента входит в совет директоров компании" | aconnect == "Владелец - зять президента" | aconnect == "El propietario es sobrino del primer ministro" | aconnect == "El propietario está casado con un hijo del presidente" | aconnect == "La propietaria es sobrina del primer ministro" | aconnect == "La propietaria está casado con una hija del presidente" | aconnect == "Un hijo del presidente forma parte de la Junta Directiva de la empresa" | aconnect == "Una hija del presidente forma parte de la Junta Directiva de la empresa" 

replace connecteng = "Government ties" if aconnect == "Чиновник среднего уровня входит в совет директоров компании" | aconnect == "Министр входит в совет директоров компании" | aconnect =="Владелец - бывший член парламента" | aconnect =="Член парламента входит в совет директоров компании" | aconnect =="El propietario es un ex miembro del parlamento" | aconnect =="Un funcionario público de rango intermedio está en la Junta Directiva de la empresa" | aconnect =="Un miembro del parlamento está en la Junta Directiva de la empresa" | aconnect =="Un ministro está en la Junta Directiva de la empresa"| aconnect == "Владелец - одноклассник президента"

replace connecteng = "Party Member" if aconnect == "Владелец - член политической партии президента" | aconnect == "El propietario es miembro del partido político del presidente"

replace connecteng = "Police/Military" if aconnect == "Владелец - бывший генерал"  |aconnect == "Владелец - бывший офицер полиции" | aconnect == "El propietario es un general retirado de las Fuerzas Armadas" | aconnect == "El propietario es ex oficial de policía"

replace connecteng = "No ties" if aconnect == "У владелеца нет интереса к политике" | aconnect == "El propietario no tiene interés en la política"

*replace connecteng = "Classmate" if aconnect == "Владелец - одноклассник президента"

tab countryeng, gen(count)
tab connecteng, gen(connect)
	
gen alloc2 = alloc/100
replace alloc2 = .99 if alloc2 ==1
replace alloc2 = .01 if alloc2 ==0

	
reg alloc aemployees aprofit atfp aassets ayears, cluster(id)	
betareg alloc2 aemployees aprofit atfp aassets ayears, vce(cluster id)

reg alloc count1 count2 count4-count9, cluster(id)
betareg alloc2 count1 count2 count4-count9, vce(cluster id)

reg alloc connect1-connect2 connect4 connect5, cluster(id)
betareg alloc2 connect1-connect2 connect4 connect5, vce(cluster id)

	

