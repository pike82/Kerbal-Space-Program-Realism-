// Get Booster Values

local wndw is gui(300).
set wndw:x to 400. //window start position
set wndw:y to 120.

local label is wndw:ADDLABEL("Enter Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally


local box_azi is wndw:addhlayout().
	local azi_label is box_azi:addlabel("Heading").
	local azivalue is box_azi:ADDTEXTFIELD("90").
	set azivalue:style:width to 100.
	set azivalue:style:height to 18.

local box_pitch is wndw:addhlayout().
	local pitch_label is box_pitch:addlabel("Start Pitch").
	local pitchvalue is box_pitch:ADDTEXTFIELD("80").
	set pitchvalue:style:width to 100.
	set pitchvalue:style:height to 18.

local box_LVL is wndw:addhlayout().
	local LVL_label is box_LVL:addlabel("Level angle").
	local LVLvalue is box_LVL:ADDTEXTFIELD("-10").
	set LVLvalue:style:width to 100.
	set LVLvalue:style:height to 18.

local box_WAIT is wndw:addhlayout().
	local WAIT_label is box_WAIT:addlabel("AP WAIT").
	local WAITvalue is box_WAIT:ADDTEXTFIELD("120").
	set WAITvalue:style:width to 100.
	set WAITvalue:style:height to 18.

local box_END is wndw:addhlayout().
	local END_label is box_END:addlabel("PE END (km)").
	local ENDvalue is box_END:ADDTEXTFIELD("160").
	set ENDvalue:style:width to 100.
	set ENDvalue:style:height to 18.

local somebutton is wndw:addbutton("Confirm").
set somebutton:onclick to Continue@.

// Show the GUI.
wndw:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
	WAIT 1.
}

Function Continue {

		set val to azivalue:text.
		set val to val:tonumber(0).
		set sv_intAzimith to val.

		set val to pitchvalue:text.
		set val to val:tonumber(0).
		set sv_anglePitchover to val.

		set val to LVLvalue:text.
		set val to val:tonumber(0).
		set pitchdown to val.

		set val to WAITvalue:text.
		set val to val:tonumber(0).
		set apwait to val.

		set val to ENDvalue:text.
		set val to val:tonumber(0).
		set endheight to val*1000.

	wndw:hide().
  	set isDone to true.
}

Print "Start Heading: " + sv_intAzimith.
Print "Start Pitch: " + sv_anglePitchover. 
Print "Pitching at: " + pitchdown.
Print "Restart before AP: " + apwait + "s".
Print "Stop burn at: " + endheight + "m".
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
//Wait until Stage:Ready .
//SAS on.
Print "Releasing Clamps".
Wait until Stage:Ready . // this ensures time between staging engines and clamps so they do not end up being caught up in the same physics tick
STAGE. // Relase Clamps
PRINT "Lift off!!".
local LchAlt is ALT:RADAR.

// Clear tower
Wait UNTIL ALT:RADAR > sv_ClearanceHeight + LchAlt.
Wait UNTIL SHIP:Q > 0.015. //Ensure past clearance height and airspeed 0.015 equates to approx 50m/s or 1.5kpa which is high enough to ensure aero stability for most craft small pitching	

// Pitchover
LOCK STEERING TO HEADING((sv_intAzimith-0), sv_anglePitchover).
Print "Pitchover: " + (TIME:SECONDS - EngineStartTime).
Wait 10.

// Gravity Turn
LOCK STEERING TO SRFPROGRADE.
Print "Gravity Turn: " + (TIME:SECONDS - EngineStartTime).
until (TIME:SECONDS - EngineStartTime) > 105{
	wait 0.5.
}
Stage. //Relase boosters
Wait 1.0.
Until SHIP:Q < 0.05{
	Wait 0.2.
}
Print "Fairings Delpolyed: " + (TIME:SECONDS - EngineStartTime).
Stage.
Wait 1.
// unguided setup
until (TIME:SECONDS - EngineStartTime) > 108{
	wait 0.5.
}
LOCK STEERING TO HEADING(sv_intAzimith, pitchdown).
until (TIME:SECONDS - EngineStartTime) > 120{
	wait 0.5.
}
Print "Starting Stabilisation: " + (TIME:SECONDS - EngineStartTime).
unlock steering.
SAS on.
wait 0.5.
set ship:control:roll to 1.
Until AVAILABLETHRUST < 1{
	Wait 0.1.
}
Print "MECO and Release: " + (TIME:SECONDS - EngineStartTime).
Stage.//Release First stage
Wait until Stage:Ready.
wait 0.5.
Print (TIME:SECONDS - EngineStartTime).
Print "Apo: " + ship:apoapsis.
wait apwait.//this determines coast and periapsis
Stage.//start ullage
Wait until Stage:Ready.
wait 0.5.
Stage.//start engine
wait 0.5.
Stage.//start engine backup
until ship:periapsis > endheight{
	Wait 0.1.
}
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
Wait 1.0.
Shutdown.
