;;;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
                                                                                                                              ||   
;;;Program name:IRAN's Seacraft and Fisheries Expert System                                                                   ||  
                                                                                                                              ||        
;;;Discription:An expert system that suggest best options for fishery process and Maritime trade in iran.                     ||    
                                                                                                                              ||         
;;;Workflow:in every step ,you must answer the a quastion.according to your answers , expert system suggest some solutions.   ||      
                                                                                                                              ||             
;;;Author:Seyed Mohammad Hossein Mosavi  August 12, 2015.                                                                     ||     
                                                                                                                              ||               
;;;Contact:Seyed.Muhammad.Hosein.Mosavi@gmail.com                                                                             ||    
                                                                                                                              ||       
;;;To execute:1.open clp file  2.load the file from File menu   3.choice run from execution menu   (lines number = 500)       ||          
                                                                                                                              ||         
;;;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))
;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "Maritime and Fisheries Expert System")
  (printout t crlf crlf))

(defrule print-answer ""
  (declare (salience 10))
  (doctor ?item)
  =>
  (printout t crlf crlf)
  (printout t "Suggested Opinion:")
  (printout t crlf crlf)
  (format t " %s%n%n%n" ?item))
  
;;;***************
;;;* QUERY RULES *******************************************************************************************************************************
;;;***************

(defrule determine-owner-ship ""
   (not (ship ?))
   (not (Sailing ?))
   =>
   (assert (ship (yes-or-no-p "Does he/she have any ship(yes/no)? "))))

(defrule determine-chassis-and-engine-inspection-Factor""
   (ship yes)
   (not (Sailing ?))
   =>
   (assert (inspection
      (ask-question "it has chassis and engine inspection Factor (yes/no)? "
                    yes no))))
              
(defrule determine-commercial-Fishing ""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
=>
   (assert (shiptype
      (ask-question "What is your ship type (commercial/fishing)? "
                   commercial fishing))))
    
(defrule determine-Fishing-license ""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype fishing)
   =>
   (assert (Fishinglicense (yes-or-no-p "Do you have a Fishing license (yes/no)? "))))
   
(defrule determine-fish-hunting ""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype fishing)
(Fishinglicense yes)
   =>
   (assert (fishhunting (yes-or-no-p "is fish have hunted (yes/no)? "))))
   
(defrule determine-Sturgeon ""
    (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype fishing)
(Fishinglicense yes)
(fishhunting yes)
   =>
   (assert (Sturgeon (yes-or-no-p "is it Sturgeon (yes/no)? "))))
   


(defrule determine-Shrimp-hunting ""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype fishing)
(Fishinglicense yes)
(fishhunting no)
   =>
   (assert (Shrimphunting (yes-or-no-p "is Shrimp have hunted (yes/no)? "))))



   
(defrule determine-lobster-hunting ""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype fishing)
(Fishinglicense yes)
(fishhunting no)
(Shrimphunting yes)
   =>
   (assert (lobsterhunting (yes-or-no-p "is lobster have hunted (yes/no)? "))))

(defrule determine-oyster-hunting ""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype fishing)
(Fishinglicense yes)
(fishhunting no)
(Shrimphunting no)

   =>
   (assert (oysterhunting (yes-or-no-p "is oyster have hunted (yes/no)? "))))

(defrule determine-Pearl-contain ""
(oysterhunting yes)

   =>
   (assert (Pearlcontain (yes-or-no-p "is oyster have Pearl (yes/no)? "))))

(defrule determine-Sea-cucumber-hunting ""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype fishing)
(Fishinglicense yes)
(fishhunting no)
(Shrimphunting no)
(oysterhunting no)

   =>
   (assert (Seacucumber (yes-or-no-p "is Sea cucumber have hunted (yes/no)? "))))

(defrule determine-othert-sea-animals-hunting ""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype fishing)
(Fishinglicense yes)
(fishhunting no)
(Shrimphunting no)
(oysterhunting no)
(Seacucumber no)
   =>
   (assert (otherseaanimals (yes-or-no-p "is other Sea animals have hunted (yes/no)? "))))

(defrule determine-Trade-license""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype commercial)

   =>
   (assert (Tradelicense (yes-or-no-p "Do you have a Trade license (yes/no)? "))))


(defrule determine-fuel-export""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype commercial)
(Tradelicense yes)

   =>
   (assert (fuelexport (yes-or-no-p "Does it export fuel (yes/no)? "))))

(defrule determine-tanker-ship""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype commercial)
(Tradelicense yes)
(fuelexport yes)

   =>
   (assert (tanker (yes-or-no-p "is it tanker (yes/no)? "))))

(defrule determine-oil-export-type ""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype commercial)
(Tradelicense yes)
(fuelexport yes)
(tanker yes)
=>
   (assert (oilexporttype
      (ask-question "What is going to export (petroleum/kerosene)? "
                   petroleum kerosene))))




(defrule determine-food-export""
(ship yes)
(inspection yes)
(not (Sailing ?))
(shiptype commercial)
(Tradelicense yes)
(fuelexport no)

   =>
   (assert (foodexport (yes-or-no-p "Does it export food (yes/no)? "))))

(defrule determine-food-export-type ""
(ship yes)
(inspection yes)
(not (Sailing ?))
(shiptype commercial)
(Tradelicense yes)
(fuelexport no)
(foodexport yes)

=>
   (assert (foodexporttype
      (ask-question "What is going to export (watermelon-and-lettuce/saffron-and-pistachios)? "
                   watermelon-and-lettuce saffron-and-pistachios))))

(defrule determine-food-export-region ""
(ship yes)
(inspection yes)
(not (Sailing ?))
(shiptype commercial)
(Tradelicense yes)
(fuelexport no)
(foodexport yes)
(foodexporttype watermelon-and-lettuce)

=>
   (assert (foodexportregion
      (ask-question "where you want to export (persian-gulf-countries/other-continent's)? "
                   persian-gulf-countries other-continent's))))

(defrule determine-food-export-region-2 ""
(ship yes)
(inspection yes)
(not (Sailing ?))
(shiptype commercial)
(Tradelicense yes)
(fuelexport no)
(foodexport yes)
(foodexporttype saffron-and-pistachios)

=>
   (assert (foodexportregion
      (ask-question "where you want to export (persian-gulf-countries/other-continent's)? "
                   persian-gulf-countries other-continent's))))

(defrule determine-medication-export""
(ship yes)
(inspection yes)
(not (Sailing ?))
(shiptype commercial)
(Tradelicense yes)
(fuelexport no)
(foodexport no)

   =>
   (assert (medicationexport (yes-or-no-p "Does it export medication (yes/no)? "))))

(defrule determine-medication-export-type ""
(ship yes)
(inspection yes)
(not (Sailing ?))
(shiptype commercial)
(Tradelicense yes)
(fuelexport no)
(foodexport no)
(medicationexport yes)
=>
   (assert (medicationexporttype
      (ask-question "is it essential or not essential medication (essential/not-essential)? "
                   essential not-essential))))

(defrule determine-medication-export-needy ""
(medicationexporttype essential)
=>
   (assert (medicationexportneedy
      (ask-question "Does it send to the needy or not needy countries (needy/not-needy)? "
                   needy not-needy))))

(defrule determine-medication-export-needy-2 ""
(medicationexporttype not-essential)
=>
   (assert (medicationexportneedy
      (ask-question "Does it send to the needy or not needy countries (needy/not-needy)? "
                   needy not-needy))))

(defrule determine-industrial-machinery-export""
(ship yes)
(inspection yes)
(not (Sailing ?))
(shiptype commercial)
(Tradelicense yes)
(fuelexport no)
(foodexport no)
(medicationexport no)

   =>
   (assert (industrialexport (yes-or-no-p "Does it export industrial machinery (yes/no)? "))))

(defrule determine-industrial-machinery-export-country-situation ""
(industrialexport yes)
=>
   (assert (countrysituation
      (ask-question "where you want to export (underdeveloped-country/developing-country)? "
                   underdeveloped-country developing-country))))


(defrule determine-industrial-machinery-export-country-situation-underdeveloped ""
(industrialexport yes)
(countrysituation underdeveloped-country)
=>
   (assert (exchangevolume
      (ask-question "How much is the amount of Exchanges Volume (100ton/more-than-100ton)? "
                   100ton more-than-100ton))))

(defrule determine-industrial-machinery-export-country-situation-developing ""
(industrialexport yes)
(countrysituation developing-country)
=>
   (assert (exchangevolume
      (ask-question "How much is the amount of Exchanges Volume (100ton/more-than-100ton)? "
                   100ton more-than-100ton))))



   

   
;;;****************
;;;* Sailing RULES ***************************************************************************************************************************
;;;****************

(defrule owning-ship-conclusions ""
   (ship no)
   (not (Sailing ?))
   =>
   (assert (Sailing "go to General Directorate of Ports and Maritime")))
  
(defrule ship-inspection-conclusions ""
   (inspection no)
   (not (Sailing ?))
   =>
   (assert (Sailing " go to Administration airframe and engine inspection.")))



(defrule fishing-license-conclusion ""
   (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype fishing)
(Fishinglicense no)
   =>
   (assert (Sailing "Go to Watershed management and Ports office.")))

(defrule determine-Sturgeon-suggestion ""
     (ship yes)
(inspection yes)
   (not (Sailing ?))
(shiptype fishing)
(Fishinglicense yes)
(fishhunting yes)
(Sturgeon yes)
   =>
      (assert (Sailing " 1.send to Caviar canning factory   2.send to market   3.send for export."))
(printout t "1.send to Caviar canning factory" crlf)
(printout t "2.send to market" crlf)
(printout t "3.send for export" crlf))

(defrule determine-is-Sturgeon ""
(Sturgeon no)
 =>
(assert (Sailing " 1.send to Tuna canning factory   2.send to market   "))
(printout t "1.send to Tuna canning factory" crlf)
(printout t "2.send to market" crlf))


   
(defrule lobster-hunting-conclusion ""
(lobsterhunting yes)
   =>
 (assert (Sailing "send for export.")))

(defrule lobster-hunting-conclusion-2 ""
(lobsterhunting no)
   =>
 (assert (Sailing "1.Go to the  market  2.go to the Shrimp canning factory."))
(printout t "1.Go to the  market" crlf)
(printout t "2.go to the Shrimp canning factory" crlf)))


(defrule pearl-contain-conclusion ""
(Pearlcontain yes)
   =>
 (assert (Sailing "send for gold market.")))


(defrule pearl-contain-conclusion-2 ""
(Pearlcontain no)
   =>
 (assert (Sailing "1.send for Canning factory oyster   2.send to the market."))
(printout t "1.send for Canning factory oyster" crlf)
(printout t "2.send to the market" crlf)))

(defrule Sea-cucumber-conclusion ""
(Seacucumber yes)
   =>
 (assert (Sailing "send for export.")))

(defrule other-sea-animals-conclusion ""
(otherseaanimals yes)
   =>
 (assert (Sailing "1.send back to the sea  2.export to the china and East Asia countries"))
(printout t "1.send back to the sea " crlf)
(printout t "2.export to the china and East Asia countries" crlf)))

(defrule other-sea-animals-conclusion-2 ""
(otherseaanimals no)
   =>
 (assert (Sailing "set fishing net again")))

(defrule other-trade-license-conclusion ""
(Tradelicense no)
   =>
 (assert (Sailing "go to Maritime Customs office")))

(defrule oil-export-type-conclusion ""
(oilexporttype petroleum)
   =>
 (assert (Sailing "Export Success"))
(printout t "Export Success" crlf)))

(defrule oil-export-type-conclusion-2 ""
(oilexporttype kerosene)
   =>
 (assert (Sailing "Export Success"))
(printout t "Export Success" crlf)))

(defrule tanker-ship-conclusion ""
(tanker no)
   =>
 (assert (Sailing "Go to the oil refinery"))
(printout t "Go to the oil refinery" crlf)))


(defrule food-export-region-conclusion ""
(foodexportregion persian-gulf-countries)
   =>
 (assert (Sailing "Export Success"))
(printout t "Export Success" crlf)))


(defrule food-export-region-conclusion-2 ""
(foodexportregion other-continent's)
   =>
 (assert (Sailing "Export Success"))
(printout t "Export Success" crlf)))


(defrule medication-export-needy-conclusion ""
(medicationexportneedy needy)
   =>
 (assert (Sailing "Export Success"))
(printout t "Export Success" crlf)))

(defrule medication-export-needy-conclusion-2 ""
(medicationexportneedy not-needy)
   =>
 (assert (Sailing "Export Success"))
(printout t "Export Success" crlf)))

(defrule industrial-machinery-export-country-situation-developing-conclusion ""
(exchangevolume 100ton)
   =>
 (assert (Sailing "Export Success"))
(printout t "Export Success" crlf)))

(defrule industrial-machinery-export-country-situation-developing-conclusion-2 ""
(exchangevolume more-than-100ton)
   =>
 (assert (Sailing "Export Success"))
(printout t "Export Success" crlf)))

(defrule industrial-machinery-export-conclusion ""
(industrialexport no)
   =>
 (assert (Sailing "Wait for Closing contracts or increasing Request"))
(printout t "Wait for Closing contracts or increasing Request" crlf)))

