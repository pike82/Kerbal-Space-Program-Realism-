// Get Mission Values

local wndw is gui(300).
set wndw:x to 400. //window start position
set wndw:y to 120.

local label is wndw:ADDLABEL("Enter Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_alt is wndw:addhlayout().
	local alt_label is box_alt:addlabel("Destruct altitude (km)").
	local destvalue is box_alt:ADDTEXTFIELD("900").
	set destvalue:style:width to 100.
	set destvalue:style:height to 18.

local box_azi is wndw:addhlayout().
	local azi_label is box_azi:addlabel("Heading").
	local azivalue is box_azi:ADDTEXTFIELD("270").
	set azivalue:style:width to 100.
	set azivalue:style:height to 18.

local box_pitch is wndw:addhlayout().
	local pitch_label is box_pitch:addlabel("Start Pitch").
	local pitchvalue is box_pitch:ADDTEXTFIELD("55").
	set pitchvalue:style:width to 100.
	set pitchvalue:style:height to 18.

local somebutton is wndw:addbutton("Confirm").
set somebutton:onclick to Continue@.

// Show the GUI.
wndw:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
	WAIT 1.
}

Function Continue {
		set val to destvalue:text.
		set val to val:tonumber(0).
		set destructheight to val*1000.

		set val to azivalue:text.
		set val to val:tonumber(0).
		set sv_intAzimith to val.

		set val to pitchvalue:text.
		set val to val:tonumber(0).
		set sv_anglePitchover to val.

	wndw:hide().
  	set isDone to true.
}

Print "Will destruct at: " + destructheight + "m". //Range Safety height
Print "Start Heading: " + sv_intAzimith.
Print "Start Pitch: " + sv_anglePitchover. //flight pitch
Local sv_ClearanceHeight is 30. //tower clearance height

//Prelaunch

Wait 1. //Alow Variables to be set and Stabilise pre launch
PRINT "Prelaunch.".
Lock Throttle to 1.
SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
LOCK STEERING TO r(up:pitch,up:yaw,facing:roll). //this is locked 90,90 only until the clamps are relased

//Liftoff
	
STAGE. //Ignite main engines
Print "Starting engines". 
Local EngineStartTime is TIME:SECONDS.
Local MaxEngineThrust is 0. 
Local englist is List().
List Engines.
LIST ENGINES IN engList. 
FOR eng IN engList {  
	Print "eng:STAGE:" + eng:STAGE.
	Print STAGE:NUMBER.
	IF eng:STAGE >= STAGE:NUMBER { 
		SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
		Print "Stage Full Engine Thrust:" + MaxEngineThrust. 
	}
}
Print "Checking thrust ok".
Local CurrEngineThrust is 0.
Local EngineStartFalied is False.
until CurrEngineThrust > 0.99*MaxEngineThrust{ 
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
//Wait until Stage:Ready .
//SAS on.
Print "Releasing Clamps".
Wait until Stage:Ready . 
STAGE. // Relase Clamps
PRINT "Lift off!!".
local LchAlt is ALT:RADAR.

// Clear tower
Wait UNTIL ALT:RADAR > sv_ClearanceHeight + LchAlt.
Wait UNTIL SHIP:Q > 0.015. //Ensure past clearance height and airspeed 0.015 equates to approx 50m/s or 1.5kpa which is high enough to ensure aero stability for most craft small pitching	
LOCK STEERING TO HEADING(sv_intAzimith, sv_anglePitchover).
Wait 20.
Until SHIP:Q < 0.15{
	Wait 0.2.
}
Stage. // realese fairings
Print "Waiting".
Until EngineStartTime + 105 < time:seconds{
	wait 0.1.
}
unlock steering.
sas on.
Print (TIME:SECONDS - EngineStartTime).
set ship:control:roll to 1.//spin stabilise
Until AVAILABLETHRUST < 1{
	Wait 0.1.
}

Stage.//Release first stage and start ullage
Wait until Stage:Ready.
wait 0.5.
Stage.//Start Engines

Until ship:altitude > destructheight or ((ship:verticalspeed < 0) and (ship:altitude < 200)){
	wait 2.	
}
wait 1.
Local P is SHIP:PARTSNAMED(core:part:Name)[0].
Local M is P:GETMODULE("ModuleRangeSafety").
M:DOEVENT("Range Safety").
