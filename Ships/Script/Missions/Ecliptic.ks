// Get Mission Values

local wndw is gui(300).
set wndw:x to 700. //window start position
set wndw:y to 120.

local label is wndw:ADDLABEL("Enter Mission Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_WAIT is wndw:addhlayout().
	local WAIT_label is box_WAIT:addlabel("AP WAIT").
	local WAITvalue is box_WAIT:ADDTEXTFIELD("35").
	set WAITvalue:style:width to 100.
	set WAITvalue:style:height to 18.

local box_END is wndw:addhlayout().
	local END_label is box_END:addlabel("PE END (km)").
	local ENDvalue is box_END:ADDTEXTFIELD("260").
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

Global boosterCPU is "Aethon".

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
Print "Aztec active".
Lock Horizon to VXCL(UP:VECTOR, VELOCITY:SURFACE). //negative velocity makes it retrograde
LOCK STEERING TO LOOKDIRUP(ANGLEAXIS(0,
            VCRS(horizon,BODY:POSITION))*horizon,
			FACING:TOPVECTOR).//lock to prograde along horizon
RCS on.
Lock Throttle to 1.
Wait 1.//move away from booster
Lock Throttle to 0.
ff_COMMS().
//Circularise burn

until (ETA:apoapsis) < apwait{
	wait 0.5.
}
Lock Throttle to 1.
Wait until Stage:Ready.
wait 5.
Stage.//start engine

until ship:periapsis > endheight{
	Wait 0.1.
}
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
RCS off.
stage.//release
//Adjust orbit to ecliptic

local start is 0.
local startnode is 0.
local transnode is 0.
local transmnv is 0.
local startTime is 0.
//find highest LAT for LAN change
set start to orbit:period/2 + time:seconds + 60.
set startnode to ff_seek(start, ff_freeze(0), ff_freeze(0), ff_freeze(0), ff_LANTimeScore@).
remove nextnode.

// find manv deltav at that time.
Set start to startnode[0].
set transnode to ff_seek(ff_freeze(start), 0, 0, ff_freeze(0), ff_ElipticLANScore@).
Print "transnode complete".
set transmnv to node(hf_unfreeze(transnode[0]), hf_unfreeze(transnode[1]), hf_unfreeze(transnode[2]), hf_unfreeze(transnode[3])).
add transmnv.
set startTime to time:seconds + nextnode:eta - ff_Burn_Time(nextnode:deltaV:mag/ 2, 198, 1, 1).
Print "burn starts at: " + startTime.
Set warp to 0.
wait until time:seconds > startTime - 60.
RCS on.
lock steering to nextnode:burnvector.
wait until time:seconds > startTime.//RCS ullage Start
stage.//start engine
lock throttle to 1.
until (hf_isManeuverComplete(nextnode) = true) or (nextnode:burnvector:mag < 0.25) {
}
lock throttle to 0.
unlock steering.
RCS off.
remove nextnode.

//Change inclination but not LAN
//find highest LAT for LAN change
Print "Commencing LAN Change". 
Set start to orbit:period/2 + time:seconds + 60.
set startnode to ff_seek(start, ff_freeze(0), ff_freeze(0), ff_freeze(0), ff_IncTimeScore@).
remove nextnode.

// find manv deltav at that time.
Set start to startnode[0].
set transnode to ff_seek(ff_freeze(start), 0, 0, 0, ff_ElipticIncScore@).
set transmnv to node(hf_unfreeze(transnode[0]), hf_unfreeze(transnode[1]), hf_unfreeze(transnode[2]), hf_unfreeze(transnode[3])).
add transmnv.
set startTime to time:seconds + transmnv:eta - ff_Burn_Time(nextnode:deltaV:mag/ 2, 198).
Print "burn starts at: " + startTime.
Set warp to 0.
wait until time:seconds > startTime - 60.
RCS on.
lock steering to nextnode:burnvector.
wait until time:seconds > startTime.
Print "burn Start".
Set originalVector to -1.///ensures reset from before.
lock throttle to 1.
until (hf_isManeuverComplete(nextnode) = true) or (nextnode:burnvector:mag < 0.25) {
}
lock throttle to 0.
unlock steering.
RCS off.
remove nextnode.
wait 1.
Print "burns complete shutting down".
shutdown.

function ff_LANTimeScore {
	parameter mnv.
	local result is 0.
	Local t is mnv:eta + time:seconds.
	set result to Body:GEOPOSITIONOF(positionAT(ship, t)).
	set result to result:lat. //want highest lat.
	return result.
}

function ff_IncTimeScore {
	parameter mnv.
	local result is 0.
	Local t is mnv:eta + time:seconds.
	set result to Body:GEOPOSITIONOF(positionAT(ship, t)).
	set result to abs(result:lat). //want lowest lat.
	//Print "inc time:" +result.
	return -1 *result.
}

function ff_ElipticLANScore {
	parameter mnv.
	local result is 0.
	set result to hf_360AngDiff(359.9965004168758, mnv:orbit:Lan).
	return (-1 * result)-abs(mnv:deltav:mag/100).
}

function ff_ElipticIncScore {
  parameter mnv.
  	local result is 0.
  	set result to hf_180AngDiff(23.44603795469773, mnv:orbit:inclination) + (hf_360AngDiff(359.9965004168758, mnv:orbit:Lan)*5) + (mnv:orbit:eccentricity*500).
	Set result to (-1 * result).
	//Print "Result: " + result.
	Set result to result-abs(nextnode:deltav:mag/10000).
  	return result.
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
	set data to ff_optimize(data, fit, 0.1). // search in 0.1m/s incriments
	Print "Seek 0.1".
	If Fine{
		set data to ff_optimize(data, fit, 0.01). // search in 0.01m/s incriments
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
		Print "orb fit create node".
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

	
///////////////////////////////////////////////////////////////////////////////////		  

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
	if vang(originalVector, mnv:burnvector) > 45 {
		declare global originalVector to -1.
		return true.
	}
	return false.
}

Function ff_stage_delta_v {
	Parameter RSS_partlist is list().
	//Calculates the amount of delta v for the current stage    
	local m is ship:mass * 1000. // Starting mass (kg)
	local g is 9.80665.
	local engine_count is 0.
	local isp is 0. // Engine ISP (s)
	local RSS is False.
	local fuelmass is 0.
	// obtain ISP
	LIST engines IN engList.
	for en in engList 
	if en:ignition and not en:flameout {
	  set isp to isp + en:isp.
	  set engine_count to engine_count + 1.
	}
	set isp to isp / engine_count.
	
	// obtain RSS yes or no.
	for res IN Stage:Resources{
		if res:name = "HTP"{
			Set RSS to true.
		}
	}
	Print "RSS" + RSS.
	If RSS = true{
	//for real fuels 
		local fuels is list("LQDOXYGEN", "LQDHYDROGEN", "KEROSENE", "Aerozine50", "UDMH", "NTO", "MMH", 
			"HTP", "IRFNA-III", "NitrousOxide", "Aniline", "Ethanol75", "LQDAMMONIA", "LQDMETHANE", 
			"CLF3", "CLF5", "DIBORANE", "PENTABORANE", "ETHANE", "ETHYLENE", "OF2", "LQDFLUORINE", 
			"N2F4", "FurFuryl", "UH25", "TONKA250", "TONKA500", "FLOX30", "FLOX70", "", "FLOX88", 
			"IWFNA", "IRFNA-IV", "AK20", "AK27", "CaveaB", "MON1", "MON3", "MON10", "MON15", "MON20", "Hydyne", "TEATEB").
		for tankPart in RSS_partlist{
			for res in tankpart:RESOURCES{
				for f in fuels{
					if f = res:NAME{
						SET fuelMass TO fuelMass + ((res:DENSITY*res:AMOUNT)*1000).
						Print "fuel mass" + fuelmass.
					}
				}
			}
		}
	} Else {
	//for stock fuels
		local fuels is list("LiquidFuel", "Oxidizer", "SolidFuel", "MonoPropellant").
		for res in STAGE:RESOURCES{
			for f in fuels{
				if f = res:NAME{
					SET fuelMass TO fuelMass + res:DENSITY*res:AMOUNT.
				}
			}
		}
	}
	//TODO:Think about removing RCS components or making it an input term as this could be a significant proportion of the deltaV which is not used.
	return (isp * g * ln(m / (m - fuelMass))).
}./// End Function

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

Function hf_360AngDiff{
	Parameter a, b.
	return 180 - abs(abs(a-b)-180). 
}
Function hf_180AngDiff{
	Parameter a, b.
	return 90 - abs(abs(a-b)-90). 
}
