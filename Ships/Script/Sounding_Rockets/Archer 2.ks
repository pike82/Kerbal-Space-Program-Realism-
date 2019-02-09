// Get Mission Values

LOCAL gui is gui(200).
LOCAL label is gui:ADDLABEL("Enter destruct height in km").
SET label:STYLE:ALIGN TO "CENTER".
SET label:STYLE:HSTRETCH TO True. // Fill horizontally
LOCAL destvalue is gui:ADDTEXTFIELD("").
SET destvalue:STYLE:ALIGN TO "CENTER".
SET destvalue:STYLE:HSTRETCH TO True. // Fill horizontally
set destvalue:style:width to 300.
set destvalue:style:height to 18.
// Show the GUI.
gui:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
    set destvalue:onconfirm to { 
		parameter val.
		set val to val:tonumber(0).
		set destructheight to val*1000.
		SET isDone TO TRUE.
	}.
	WAIT 0.5.
}
gui:HIDE().
Print "Will destruct at: " + destructheight + "m". //Range Safety height
Local sv_ClearanceHeight is 15. //tower clearance height

//Prelaunch

Wait 1. //Alow Variables to be set and Stabilise pre launch
PRINT "Prelaunch.".
Lock Throttle to 1.
SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
LOCK STEERING TO r(up:pitch,up:yaw,facing:roll). //this is locked 90,90 only until the clamps are relased
//SAS on.

//Liftoff
	
STAGE. //Ignite main engines
Print "Starting engines".
Local EngineStartTime is TIME:SECONDS.
Local MaxEngineThrust is 0. 
Local englist is List().
List Engines.
LIST ENGINES IN engList. //Get List of Engines in the vessel
FOR eng IN engList {  //Loops through Engines in the Vessel
	Print "eng:STAGE:" + eng:STAGE.
	Print STAGE:NUMBER.
	IF eng:STAGE >= STAGE:NUMBER { //Check to see if the engine is in the current Stage
		SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
		Print "Stage Full Engine Thrust:" + MaxEngineThrust. 
	}
}
Print "Checking thrust ok".
Local CurrEngineThrust is 0.
Local EngineStartFalied is False.
until CurrEngineThrust > 0.99*MaxEngineThrust{ // until upto thrust or the engines have attempted to get upto thrust for more than 5 seconds.
	Set CurrEngineThrust to 0.
	FOR eng IN engList {  //Loops through Engines in the Vessel
		IF eng:STAGE >= STAGE:NUMBER { //Check to see if the engine is in the current Stage
			SET CurrEngineThrust TO CurrEngineThrust + eng:THRUST. //add thrust to overall thrust
		}
	}
	if (TIME:SECONDS - EngineStartTime) > 5 {
		Lock Throttle to 0.
		Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
		Print "Engine Start up Failed...Making Safe".
		Shutdown. //ends the script
	}
}
Print "Releasing Clamps".
Wait until Stage:Ready. // this ensures time between staging engines and clamps so they do not end up being caught up in the same physics tick
STAGE. // Relase Clamps
PRINT "Lift off!!".
wait 5.
Unlock Steering. //keeping this makes the wings try to provide input

Until AVAILABLETHRUST < 1{
	Wait 0.1.
}
Until SHIP:Q < 0.08{///wait until low Q for next stage restart set to 0.2 for no sounding payload
	Wait 0.2.
}
Stage.//Release first stage
Wait until Stage:Ready.
Stage.//Start ullage Engines
Wait until Stage:Ready.
Wait 0.5.
Stage.//Start Engines
//used for non-return flights
Until ship:altitude > destructheight or ((ship:verticalspeed < 0) and (ship:altitude < 2000)){
	wait 0.1.	
}
wait 1.
Local P is SHIP:PARTSNAMED(core:part:Name)[0].
Local M is P:GETMODULE("ModuleRangeSafety").
M:DOEVENT("Range Safety").


// SET P TO SHIP:PARTSNAMED("sensorBarometer")[0].
// SET M TO P:GETMODULE("ModuleScienceExperiment").
// M:DEPLOY.
// WAIT UNTIL M:HASDATA.
// M:TRANSMIT.

// SET P TO SHIP:PARTSNAMED("sensorThermometer")[0].
// SET M TO P:GETMODULE("ModuleScienceExperiment").
// M:DEPLOY.
// WAIT UNTIL M:HASDATA.
// M:TRANSMIT.