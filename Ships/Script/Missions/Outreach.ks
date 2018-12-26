// Get Mission Values

local wndw is gui(300).
set wndw:x to 700. //window start position
set wndw:y to 120.

local label is wndw:ADDLABEL("Enter Mission Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_WAIT is wndw:addhlayout().
	local WAIT_label is box_WAIT:addlabel("AP WAIT").
	local WAITvalue is box_WAIT:ADDTEXTFIELD("50").
	set WAITvalue:style:width to 100.
	set WAITvalue:style:height to 18.

local box_END is wndw:addhlayout().
	local END_label is box_END:addlabel("PE END (km)").
	local ENDvalue is box_END:ADDTEXTFIELD("150").
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

Global boosterCPU is "Zu".

Print "Restart before AP: " + apwait + "s".
Print "Stop burn at: " + endheight + "m".
Local holdload is false. 
until holdload = true {
	print holdload.
	Print holdload = true.
	Set holdload to true. //reset to true and rely on previous stage to turn false
	print holdload.
	local PROCESSOR_List is list().
	LIST PROCESSORS IN PROCESSOR_List. // get a list of all connected cores
	Print PROCESSOR_List:length.
	for Processor in PROCESSOR_List {
		if Processor:TAG = boosterCPU{ //checks to see if previous stage is present
			Set holdload to false.
		}
	}
	print holdload.
	Print holdload = true.
	wait 2.
}
Print "Outreach active".
//Complete stage into orbit
//ensure throttle is still at 1 and to maintain booster setting
Lock Throttle to 1. 
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
Print "Apo: " + ship:apoapsis.
until (ETA:apoapsis) < apwait{
	wait 0.5.
}
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
Print "Mission Finshed".
Shutdown. //ends the script

