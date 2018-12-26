// Get Mission Values

local wndw is gui(300).
set wndw:x to 700. //window start position
set wndw:y to 120.

local label is wndw:ADDLABEL("Enter Mission Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_WAIT is wndw:addhlayout().
	local WAIT_label is box_WAIT:addlabel("AP WAIT").
	local WAITvalue is box_WAIT:ADDTEXTFIELD("45").
	set WAITvalue:style:width to 100.
	set WAITvalue:style:height to 18.

local box_END is wndw:addhlayout().
	local END_label is box_END:addlabel("PE END (km)").
	local ENDvalue is box_END:ADDTEXTFIELD("3000").
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

		set val to WAITvalue:text.
		set val to val:tonumber(0).
		set apwait to val.

		set val to ENDvalue:text.
		set val to val:tonumber(0).
		set endheight to val*1000.

	wndw:hide().
  	set isDone to true.
}

Global boosterCPU is "Aethon2".

Print "Restart before AP: " + apwait + "s".
Print "Stop burn at: " + endheight + "m".
Print "Waitng for activation".
//wait for active
Local holdload is false. 
until holdload = true {
	Set holdload to true. //reset to true and rely on previous stage to turn false
	local PROCESSOR_List is list().
	LIST PROCESSORS IN PROCESSOR_List. // get a list of all connected cores
	for Processor in PROCESSOR_List {
		if Processor:TAG = boosterCPU{ //checks to see if previous stage is present
			Set holdload to false.
		}
	}
	wait 0.2.
}
Print "Comm active".
LOCK STEERING TO PROGRADE.
RCS on.
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
ff_COMMS().

//Circularise burn
Lock Horizon to VXCL(UP:VECTOR, VELOCITY:SURFACE). //negative velocity makes it retrograde
LOCK STEERING TO LOOKDIRUP(ANGLEAXIS(0,
			VCRS(horizon,BODY:POSITION))*horizon,
			FACING:TOPVECTOR).//lock to prograde along horizon

until (ETA:apoapsis) < apwait{
	wait 0.5.
}
SET SHIP:CONTROL:FORE TO 0.9.//start ullage using RCS
wait 6.
Lock Throttle to 1.
SET SHIP:CONTROL:FORE to 0.
Stage.//start engine
until ship:periapsis > endheight{
	Wait 0.01.
}
Lock Throttle to 0.
RCS off.
wait 10.
Stage.
wait 20.
until (ETA:apoapsis) < 150{
	wait 0.5.
}
rcs on.
wait 100.
stage.
Lock Throttle to 1.
until ship:orbit:eccentricity < 0.004{
	Wait 0.01.
}
Lock Throttle to 0.
wait 400.
Shutdown.

FUNCTION ff_COMMS {
	PARAMETER event is "activate", stagewait IS 0.1, ShipQtgt is 0.0045.
	// "deactivate"
	IF SHIP:Q < ShipQtgt {
		FOR antenna IN SHIP:MODULESNAMED("ModuleRTAntenna") {
			IF antenna:HASEVENT(event) {
				antenna:DOEVENT(event).
				PRINT event + " Antennas".
				WAIT stageWait.
			}	
		}.
	}
} // End of Function