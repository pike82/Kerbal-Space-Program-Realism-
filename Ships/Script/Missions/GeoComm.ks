/// Get Mission Values

local wndw is gui(300).
set wndw:x to 700. //window start position
set wndw:y to 120.

local label is wndw:ADDLABEL("Enter Mission Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_WAIT is wndw:addhlayout().
	local WAIT_label is box_WAIT:addlabel("AP WAIT").
	local WAITvalue is box_WAIT:ADDTEXTFIELD("65").
	set WAITvalue:style:width to 100.
	set WAITvalue:style:height to 18.

local box_END is wndw:addhlayout().
	local END_label is box_END:addlabel("PE END (km)").
	local ENDvalue is box_END:ADDTEXTFIELD("150").
	set ENDvalue:style:width to 100.
	set ENDvalue:style:height to 18.

local box_APEND is wndw:addhlayout().
	local APEND_label is box_APEND:addlabel("Trans AP END (km)").
	local APENDvalue is box_APEND:ADDTEXTFIELD("250000").
	set APENDvalue:style:width to 100.
	set APENDvalue:style:height to 18.

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

		set val to APENDvalue:text.
		set val to val:tonumber(0).
		set APheight to val*1000.

	wndw:hide().
  	set isDone to true.
}
Global boosterCPU is "Aethon3".

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
Print "GeoComm active".
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
RCS on.

//Circularise burn
Lock Horizon to VXCL(UP:VECTOR, VELOCITY:SURFACE). //negative velocity makes it retrograde
LOCK STEERING TO LOOKDIRUP(ANGLEAXIS(0,
			VCRS(horizon,BODY:POSITION))*horizon,
			FACING:TOPVECTOR).//lock to prograde along horizon

until (ETA:apoapsis) < apwait{
	wait 0.5.
}
SET SHIP:CONTROL:FORE TO 0.9.//start ullage using RCS
wait 5.
Lock Throttle to 1.
SET SHIP:CONTROL:FORE to 0.
Stage.//start engine
until ship:periapsis > endheight{
	Wait 0.01.
}
Lock Throttle to 0.
RCS off.

//Unfurl things
panels on.
ff_COMMS().
ff_avionics("shutdown-avionics").

/////High tranfer burn
Local transnode is ff_Transfer (APheight).
local startTime is time:seconds + nextnode:eta - (ff_Burn_Time(nextnode:deltaV:mag/ 2, 278, 35.1,1)).
Print (ff_Burn_Time(nextnode:deltaV:mag/ 2, 278, 35.1,1)).
Print "burn starts at: " + startTime.
wait 5.
warpto(startTime - 75).
wait until time:seconds > startTime - 70.
lock steering to nextnode:deltav. //burnvector
RCS on.
wait 60.
SET SHIP:CONTROL:FORE TO 0.9.//start ullage using RCS
wait 10. //RCS ullage
wait until time:seconds > startTime.
Lock Throttle to 1.
SET SHIP:CONTROL:FORE to 0.
until hf_isManeuverComplete(nextnode) {
		if ship:orbit:HASNEXTPATCH {
			if ship:orbit:nextPatch:periapsis > APheight {
				Break.
			}
	}
	wait 0.001.
}
lock throttle to 0.
unlock steering.
RCS off.
remove nextnode.
ff_avionics("shutdown-avionics").
wait 1.0.
Print "Waiting for AP".
wait 15.

/////AP tranfer burn to low
Local transnode is ff_LowerTransfer ().
Local counter is 0.
Until counter > 120{
	Clearscreen.
	Print "Refine Node before: " + (120-counter).
	wait 1.
	Set Counter to counter +1.
}
local startTime is time:seconds + nextnode:eta - (ff_Burn_Time(nextnode:deltaV:mag/ 2, 278, 35.1,1)).
Print (ff_Burn_Time(nextnode:deltaV:mag/ 2, 278, 35.1,1)).
Print "burn starts at: " + startTime.
wait 5.
warpto(startTime - 75).
wait until time:seconds > startTime - 70.
ff_avionics().
lock steering to nextnode:deltav.
RCS on.
wait 60.
SET SHIP:CONTROL:FORE TO 0.9.//start ullage using RCS
wait 10. //RCS ullage
wait until time:seconds > startTime.
Lock Throttle to 1.
SET SHIP:CONTROL:FORE to 0.
until hf_isManeuverComplete(nextnode) {
		if ship:orbit:HASNEXTPATCH {
			if ship:orbit:nextPatch:periapsis < 36793172 {
				Break.
			}
	}
	wait 0.001.
}
lock throttle to 0.
unlock steering.
RCS off.
ff_avionics("shutdown-avionics").
remove nextnode.

//Geo Circularise burn
Local transnode is ff_GeoTransferlow ().
Local counter is 0.
Until counter > 120{
	Clearscreen.
	Print "Refine Node before: " + (120-counter).
	wait 1.
	Set Counter to counter +1.
}
local startTime is time:seconds + nextnode:eta - (ff_Burn_Time(nextnode:deltaV:mag/ 2, 258, 1.5,1)).
Print (ff_Burn_Time(nextnode:deltaV:mag/ 2, 258, 1.5,1)).
Print "burn starts at: " + startTime.
wait 5.
warpto(startTime - 75).
Stage. //stop second stage debris in GEO orbit
wait 1.
Stage. // activate RCS engine
wait until time:seconds > startTime - 70.
ff_avionics().
lock steering to nextnode:deltav.
RCS on.
wait 70.
wait until time:seconds > startTime.
Lock Throttle to 1.
SET SHIP:CONTROL:FORE to 0.
until hf_isManeuverComplete(nextnode) {
		if ship:orbit:HASNEXTPATCH {
			if ship:orbit:nextPatch:periapsis < 35793172 {
				Break.
			}
	}
	wait 0.001.
}
lock throttle to 0.
unlock steering.
RCS off.
ff_avionics("shutdown-avionics").
remove nextnode.

//Geo Orbit refine
Local transnode is ff_GeoTransferhigh ().
Local counter is 0.
Until counter > 120{
	Clearscreen.
	Print "Refine Node before: " + (120-counter).
	wait 1.
	Set Counter to counter +1.
}
local startTime is time:seconds + nextnode:eta - (ff_Burn_Time(nextnode:deltaV:mag/ 2, 258, 1.5,1)).
Print "burn starts at: " + startTime.
wait 5.
warpto(startTime - 75).
wait until time:seconds > startTime - 70.
ff_avionics().
lock steering to nextnode:deltav.
RCS on.
wait 70.
wait until time:seconds > startTime.
Lock Throttle to 1.
until hf_isManeuverComplete(nextnode) {
		if ship:orbit:HASNEXTPATCH {
			if ship:orbit:nextPatch:periapsis < 35793172 {
				Break.
			}
	}
	wait 0.001.
}
lock throttle to 0.
unlock steering.
RCS off.
ff_avionics("shutdown-avionics").
remove nextnode.

//Geo inclination refine

Local transnode is ff_GeoTransfer ().
Local counter is 0.
Until counter > 120{
	Clearscreen.
	Print "Refine Node before: " + (120-counter).
	wait 1.
	Set Counter to counter +1.
}
local startTime is time:seconds + nextnode:eta - (ff_Burn_Time(nextnode:deltaV:mag/ 2, 258, 1.5,1)).
Print "burn starts at: " + startTime.
wait 5.
warpto(startTime - 75).
wait until time:seconds > startTime - 70.
ff_avionics().
lock steering to nextnode:deltav.
RCS on.
wait 70.
wait until time:seconds > startTime.
Lock Throttle to 1.
until hf_isManeuverComplete(nextnode) {
		if ship:orbit:HASNEXTPATCH {
			if ship:orbit:nextPatch:periapsis < endPE {
				Break.
			}
	}
	wait 0.001.
}
lock throttle to 0.
unlock steering.
RCS off.
ff_avionics("shutdown-avionics").
remove nextnode.

wait 400.
Shutdown.

FUNCTION ff_COMMS {
	PARAMETER event is "activate", stagewait IS 0.1, ShipQtgt is 0.0045.
	// "deactivate"
	IF SHIP:Q < ShipQtgt {
		FOR antenna IN SHIP:MODULESNAMED("ModuleRTAntenna") {
			IF antenna:HASEVENT(event) {
				antenna:DOEVENT(event).
				if antenna:HASFIELD("target"){
					antenna:Setfield("target", "active-vessel").
				}
				PRINT event + " Antennas".
				WAIT stageWait.
			}	
		}.
	}
} // End of Function


function ff_Transfer {
  Parameter endheight. //
	Local start is time:seconds + 60.
	Local end is orbit:period + time:seconds + 60.

	local startSearchTime is hf_ternarySearch(
		hf_LatScore@,
		start, end, 1, false
	).
	local transfer is ff_seek(ff_freeze(startSearchTime), 0, 0, 3000, hf_APScore@).
	return transfer.
}

function ff_LowerTransfer {
	Local start is time:seconds + eta:apoapsis.
	local transfer is ff_seek(ff_freeze(Start), ff_freeze(0), 0, 0, hf_LowScore@).
	return transfer.
}

function ff_GeoTransfer {
	Local start is time:seconds + eta:periapsis.
	local transfer is ff_seek(ff_freeze(start), ff_freeze(0), 0, 0, hf_IncScore@, true).
	return transfer.
}

function ff_GeoTransferlow {
	Local start is time:seconds + eta:periapsis.
	local transfer is ff_seek(ff_freeze(start), ff_freeze(0), 0, 0, hf_GeoScore@, true).
	return transfer.
}

function ff_GeoTransferHigh {
	Local start is time:seconds + eta:apoapsis.
	local transfer is ff_seek(ff_freeze(start), ff_freeze(0), 0, 0, hf_GeoScore@, true).
	return transfer.
}

function hf_LatScore{
  parameter t.
	Local result is abs(Body:GEOPOSITIONOF(positionAT(ship, t)):lat).
	Print "Lat: " +result.
  	return result.
}

function hf_APScore{
  parameter mnv.
	Local result is -(abs(Body:GEOPOSITIONOF(positionAT(ship, hf_TimeToApoapsis(mnv))):lat)*10000) - abs(mnv:orbit:apoapsis -APheight).// - nextnode:deltav:mag.
	Print result.
  return result.
}

function hf_TimeToApoapsis {
  parameter mnv.
	local endtime is 1^9.
	if mnv:orbit:eccentricity < 1 {
		Set endtime to (mnv:orbit:period / 2).
	}
  local apoapsisTime is hf_ternarySearch(
    hf_altitudeAt@,
    time:seconds + mnv:eta, 
    time:seconds + mnv:eta + endtime,
    1
  ).
  return apoapsisTime.
}

function hf_TimeToPeriapsis {
  parameter mnv.
  local periapsisTime is hf_ternarySearch(
    hf_altitudeAt@,
    time:seconds + mnv:eta, 
    time:seconds + mnv:eta + (mnv:orbit:period / 2),
    1, false
  ).
  return periapsisTime.
}

function hf_altitudeAt {
  parameter t.
  return ship:body:altitudeOf(positionAt(ship, t)).
}

function hf_lowScore{
  parameter mnv, endPE is 36793172.// set above actual height as keeping second satge out of GEO Orbit.
	Local result is -(abs(0-mnv:orbit:inclination)*1000000) - abs(mnv:orbit:periapsis - endPE).
	//Print result.
  	return result.
}

function hf_incScore{
  parameter mnv.
	Local result is - (abs(0-mnv:orbit:inclination)*10000) .
	//Print result.
  	return result.
}

function hf_GeoScore{
  parameter mnv, endperiod is 86164, endPE is 35793172.
	Local result is -abs(mnv:orbit:period - endperiod) - abs(mnv:orbit:periapsis -endPE).// - abs(mnv:deltav:mag).
	//Print result.
  	return result.
}

function hf_ternarySearch {
  parameter f, left, right, absolutePrecision, maxVal is true.
  until false {
    if abs(right - left) < absolutePrecision {
      return (left + right) / 2.
    }
    local leftThird is left + (right - left) / 3.
    local rightThird is right - (right - left) / 3.
    if maxval = true{
			if f(leftThird) < f(rightThird) {
				set left to leftThird.
			} else {
				set right to rightThird.
			}
		}
		if maxval = false{
			if f(leftThird) > f(rightThird) {
				set left to leftThird.
			} else {
				set right to rightThird.
			}
		}
  }
}

function ff_freeze {
	parameter n. 
	return lex("frozen", n).
}/// End Function

function ff_seek {
	parameter t, r, n, p, fitness, fine is False,
			  data is list(t, r, n, p),
			  fit is hf_orbit_fitness(fitness@).  // time, radial, normal, prograde, fitness are the parameters passed in for the node to be found. passes fitness through as a delegate to orbital fitness in this case { parameter mnv. return -mnv:orbit:eccentricity. } is passed through as a local function but any scorring evaluation can be passed through
	set data to ff_optimize(data, fit, 100). // search in 100m/s incriments
	Print "Seek 100".
	set data to ff_optimize(data, fit, 10). // search in 10m/s incriments
	Print "Seek 10".
	set data to ff_optimize(data, fit, 1). // search in 1m/s incriments
	Print "Seek 1".
	If Fine{
		set data to ff_optimize(data, fit, 0.1). // search in 0.1m/s incriments
		Print "Seek 0.1".
	}
	fit(data). //sets the final manuver node and returns its parameters
	wait 0. 
	return data. // returns the manevour node parameters to where the function was called
}/// End Function

function ff_optimize {
	parameter data, fitness, step_size,
	winning is list(fitness(data), data),
	improvement is hf_best_neighbor(winning, fitness, step_size). // collect current node info, the parameter to evaluate, and the incriment size(note: there was a comma here not a full stop if something goes wrong)// a list of the fitness score and the data, sets the first winning node to the original data passed through(note: there was a comma here not a full stop if something goes wrong)// calculates the first improvement node to make it through the until loop
	until improvement[0] <= winning[0] { // this loops until the imporvment fitness score is lower than the current winning value fitness score (top of the hill is reached)
	  set winning to improvement. // sets the winning node to the improvement node just found
	  set improvement to hf_best_neighbor(winning, fitness, step_size). // runs the best neighbour function to find a better node using the current node that is winning
	}
	return winning[1]. // returns the second column of the winning list "(data)", instead of "fitness(data)"
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
		local new_node is node(
		hf_unfreeze(data[0]), hf_unfreeze(data[1]),
		hf_unfreeze(data[2]), hf_unfreeze(data[3])). //Collects Node parameters from the Frozen Lexicon, presented in time, radial, normal, prograde.
		add new_node. // produces new node in the game
		//Print new_node.
		wait 0.
		return fitness(new_node). // returns the manevour node parameters to where the function was called
	}.
}/// End Function
	
function hf_best_neighbor {
	parameter best, fitness, step_size. // best is the winning list and contains two coloumns
	for neighbor in hf_neighbors(best[1], step_size) { //send to neighbours function the node information and the step size to retune a list of the neighbours
		local score is fitness(neighbor). // Set up for the score to analyse what is returned by neigbour. This is what finds the fitness score by looking at the mnv node orbit eccentricity that was passed through as delegate into fitness
		if score > best[0] set best to list(score, neighbor). //if the eccentricity score of the neighbour is better save the mnv result to best
		//Print "score" + score + "best" + Best[0].
	}
	//Print "best:" + best.
	return best. //return the best result of all the neighbours
}/// End Function

function hf_neighbors {
	parameter data, step_size, results is list().
	for i in range(0, data:length) if not hf_frozen(data[i]) { // for each of the data points sent through check if the data is frozen (i.e. is a value that should not be changed). 
		local increment is data:copy.
		local decrement is data:copy.
		set increment[i] to increment[i] + step_size. //for each of the data points allowed to be changed increment up by the step size
		set decrement[i] to decrement[i] - step_size. //for each of the data points allowed to be changed increment up by the step size
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
  if vang(originalVector, mnv:burnvector) > 90 {
    declare global originalVector to -1.
    return true.
  }
  return false.
}

function ff_burn_time {
parameter dV, isp is 0, thrust is 0, engine_count is 0. // For RSS/RO engine values must be given unless they are actually burning.
lock throttle to 0.
Print "Burntime".
	local g is 9.80665.  // Gravitational acceleration constant used in game for Isp Calculation (m/s²)
	local m is ship:mass * 1000. // Starting mass (kg)
	local e is constant():e. // Base of natural log
	
	//TODO: look at comapring the dv with the ff_stage_delta_v. If less look at the engine in the next stage and determine the delta_v and time to burn until the dv has been meet.
	If engine_count = 0{ // only evaluate is figures not given
		list engines in all_engines.
		for en in all_engines {
			if en:ignition and not en:flameout {
				set thrust to thrust + en:availablethrust.
				set isp to isp + en:isp.
				set engine_count to engine_count + 1.
			}
		}
	}
	if engine_count = 0{
		return 1. //return something to prevent error.
	}
	set isp to isp / engine_count. //assumes only one type of engine in cluster
	set thrust to thrust * 1000. // Engine Thrust (kg * m/s²)
	Print isp.
	Print Thrust.
	return g * m * isp * (1 - e^(-dV/(g*isp))) / thrust.
}/// End Function


FUNCTION hf_mAngle{
PARAMETER a.
  UNTIL a >= 0 { SET a TO a + 360. }
  RETURN MOD(a,360).
  
}


FUNCTION ff_avionics {
	PARAMETER event is "activate-avionics".
	// "deactivate"
		FOR avionic IN SHIP:MODULESNAMED("Avionics") {
			IF avionic:HASEVENT(event) {
				avionic:DOEVENT(event).
				PRINT event + " avionics".
				WAIT stageWait.
			}	
		}.
} // End of Function