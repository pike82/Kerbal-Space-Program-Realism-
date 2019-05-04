// Get Mission Values

local wndw is gui(300).
set wndw:x to 700. //window start position
set wndw:y to 120.

local label is wndw:ADDLABEL("Enter Mission Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_TAR is wndw:addhlayout().
	local TAR_label is box_TAR:addlabel("Landing Target longitude").
	local TARvalue is box_TAR:ADDTEXTFIELD("155").
	set TARvalue:style:width to 100.
	set TARvalue:style:height to 18.

local box_End is wndw:addhlayout().
	local End_label is box_End:addlabel("Mission end (hours)").
	local Endvalue is box_End:ADDTEXTFIELD("12.00").
	set Endvalue:style:width to 100.
	set Endvalue:style:height to 18.

local box_Res is wndw:addhlayout().
	local Res_label is box_Res:addlabel("Restart Location").
	local Resvalue is box_Res:ADDTEXTFIELD("0").
	set Resvalue:style:width to 100.
	set Resvalue:style:height to 18.

local somebutton is wndw:addbutton("Confirm").
set somebutton:onclick to Continue@.

// Show the GUI.
wndw:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
	WAIT 1.
}

Function Continue {

		set val to TARvalue:text.
		set val to val:tonumber(0).
		set L_TAR to val.

		set val to Endvalue:text.
		set val to val:tonumber(0).
		set Endtime to val*3600.

		set val to Resvalue:text.
		set val to val:tonumber(0).
		set runmode to val.

	wndw:hide().
  	set isDone to true.
}
wait 0.1.
Global boosterCPU is "Aethon".
If runmode = 0{

	Print "Waiting for activation".
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
		ff_CheckAbort().
		wait 0.2.

		// if (SHIP:Q > 0.35) and (ship:airspeed > 5) { //max Q abort test
		// 	Print "Max Q abort".
		// 	ff_Abort().
		// }
		
		// if (ship:AVAILABLETHRUST/(ship:mass*9.8)) > 5 { //high G abort test
		// 	Print "High G abort".
		// 	ff_Abort().
		// }

	}
	Print "Atlas active".
	Lock Throttle to 0.
	Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
	RCS off.
	wait 10.
	Set runmode to 1.	
}
If runmode = 1{
	
	until MISSIONTIME > endtime {
		Clearscreen.
		Print "Waitng for mission to finish : " + (endtime - MISSIONTIME).
		Wait 1.0.
	}
	Set runmode to 2.
}

If runmode = 2{
	Local transnode is ff_transfer(L_TAR).
	local transmnv is node(hf_unfreeze(transnode[0]), hf_unfreeze(transnode[1]), hf_unfreeze(transnode[2]), hf_unfreeze(transnode[3])).
	add transmnv.
	wait 1.
	Set runmode to 2.5.
}

If runmode = 2.5{
	Local counter is 0.
	Until counter > 300{
		Clearscreen.
		Print "Refine Node before: " + (300-counter).
		wait 1.
		Set Counter to counter +1.
	}
	Set runmode to 3.
}

If runmode = 3{
	local startTime is time:seconds + nextnode:eta.
	Print "burn starts at: " + startTime.
	wait 5.
	wait until time:seconds > startTime - 90.
	RCS on.
	lock steering to nextnode:burnvector.
	wait until time:seconds > startTime.
	lock throttle to 1.
	Wait until Stage:Ready.
	stage.//Start retro rockets
	Wait 5.
	lock throttle to 0.
	unlock steering.
	RCS off.
	stage. //release retro rockets
	remove nextnode.
	Set runmode to 3.5.
}

If runmode = 3.5{
	until altitude < 121000{
		Wait 2.
	}
	Set runmode to 4.
}

If runmode = 4{
	Lock steering to retrograde.
	RCS on.
	wait 60.
	until ALT:RADAR < 10000{
		Wait 2.
	}
	Stage.
	wait 1.
	ff_Para_activate().
	RCS off.
	wait 5.
}
wait 400.
Shutdown.

Function ff_Para_activate{
	for RealChute in ship:modulesNamed("RealChuteModule") {
		RealChute:doevent("arm parachute").
		Print "Parchute armed enabled.".
	}
}

function ff_CheckAbort{
	If (verticalspeed < 0) and (altitude < 500) and (ship:airspeed > 5){
		Print"Low Airspeed and altitude abort".
		ff_Abort().
	}
	If (SHIP:Q > 0.05) and (ship:airspeed > 5) {
		if vang(SHIP:FACING:FOREVECTOR, srfprograde:vector) > 5{
			Print"AoA abort".
			ff_Abort().
		}
	}
	Local englist is List().
	FOR eng IN engList { 
		If eng:IGNITION and (ship:airspeed > 5){
			if eng:THRUST < 0.95 * eng:AVAILABLETHRUST{
				Print"Engine Failure abort".
				ff_Abort().
			}
		}
	}
}

Function ff_Abort {
	Print "Engine Shutdown!!!".
	local PROCESSOR_List is list().
	LIST PROCESSORS IN PROCESSOR_List. // get a list of all connected cores
	for Processor in PROCESSOR_List {
		if Processor:TAG = boosterCPU{ //checks to see if previous stage is present
			Processor:DEACTIVATE().
		}
	}
	lock throttle to 0.
	lock PILOTMAINTHROTTLE to 0.
	Local englist is List().
	FOR eng IN engList { 
		If eng:IGNITION {
			eng:shutdown.
		}
	}
	wait 0.1.
	Abort on.
	until (verticalspeed < 1){
		wait 1.
	}
	brakes on.
}

function ff_Transfer {
Parameter targ. //
	Local startSearchTime is time:seconds + 120.
	Global Scorebound is hf_LandOnPlanetscore@:bind(targ).
  	set transfer to ff_seek(startSearchTime, ff_freeze(0), ff_freeze(0), ff_freeze(-55), Scorebound). 
  	return transfer.
}

function hf_LandOnPlanetscore{
parameter targ, mnv.
Local result is 0.
	if ADDONS:TR:AVAILABLE {
		if ADDONS:TR:HASIMPACT {
			PRINT ADDONS:TR:IMPACTPOS.
			Set result to abs(ADDONS:TR:IMPACTPOS:lng - Targ).
			wait 1.
		} else {
			set result to -nextnode:periapsis.
		} 
	}else {
    	PRINT "Trajectories is not available.".
	}
  return result.
}

function ff_freeze {
	parameter n. 
	return lex("frozen", n).
}/// End Function

function ff_seek {
	parameter t, r, n, p, fitness, fine is False,
			  data is list(t, r, n, p),
			  fit is hf_orbit_fitness(fitness@).  
	Print "Seek 10".
	set data to ff_optimize(data, fit, 1). 
	Print "Seek 1".
	If Fine{
		set data to ff_optimize(data, fit, 0.1). 
		Print "Seek 0.1".
	}
	fit(data). 
	wait 0. 
	return data. 
}/// End Function

function ff_optimize {
	parameter data, fitness, step_size,
	winning is list(fitness(data), data),
	improvement is hf_best_neighbor(winning, fitness, step_size).
	until improvement[0] <= winning[0] { 
	  set winning to improvement. 
	  set improvement to hf_best_neighbor(winning, fitness, step_size). 
	}
	return winning[1]. 
 }/// End Function

// identifies if the paramter is frozen
function hf_frozen {
	parameter v. 
	return (v+""):indexof("frozen") <> -1.
}/// End Function

// Returns paramters from the frozen lexicon
function hf_unfreeze {
	parameter v. 
	if hf_frozen(v) return v["frozen"]. 
	else return v.
}/// End Function
	
function hf_orbit_fitness {
	parameter fitness. // the parameter used to evaluate fitness
	return {
		parameter data.
		until not hasnode { 
			remove nextnode. // Used to remove any existing nodes
			wait 0. 
		} 
		Print "orb fit create node".
		local new_node is node(
		hf_unfreeze(data[0]), hf_unfreeze(data[1]),
		hf_unfreeze(data[2]), hf_unfreeze(data[3])). 
		add new_node. 
		wait 0.
		return fitness(new_node). 
	}.
}/// End Function
	
function hf_best_neighbor {
	parameter best, fitness, step_size. 
	for neighbor in hf_neighbors(best[1], step_size) { 
		local score is fitness(neighbor). 
		if score > best[0] set best to list(score, neighbor). 
		//Print "score" + score + "best" + Best[0].
	}
	//Print "best:" + best.
	return best. //return the best result of all the neighbours
}/// End Function

function hf_neighbors {
	parameter data, step_size, results is list().
	for i in range(0, data:length) if not hf_frozen(data[i]) { 
		local increment is data:copy.
		local decrement is data:copy.
		set increment[i] to increment[i] + step_size. 
		set decrement[i] to decrement[i] - step_size. 
		results:add(increment).
		results:add(decrement).
	}
	return results. // Return the list of neighbours for the data that can be changed (i.e. unfrozen)
}  /// End Function	

function hf_isManeuverComplete {
	parameter mnv.
	if not(defined originalVector) or originalVector = -1 {
		declare global originalVector to mnv:burnvector.
	}
	if vang(originalVector, mnv:burnvector) > 45 {
		declare global originalVector to -1.
		return true.
	}
	return false.
}

