<?php
if ($_SERVER['REQUEST_METHOD'] == "OPTIONS") {
	header('Access-Control-Allow-Origin: *');
	header('Access-Control-Allow-Methods: POST, GET, OPTIONS');
	header('Access-Control-Max-Age: 1000');
	header('Access-Control-Allow-Headers: Origin, X-Requested-With, Content-Type, Accept');
	header("Content-Length: 0");
	header("Content-Type: text/plain");
} elseif ($_SERVER['REQUEST_METHOD'] == "GET") {
	error_reporting(E_ALL);
	ini_set('display_errors', '1');
	ini_set( 'default_charset', 'UTF-8' );
	date_default_timezone_set("Europe/Amsterdam");
	header('Content-type: application/json; charset=utf-8');
	header('Access-Control-Allow-Origin: *');
	header('Access-Control-Allow-Methods: POST, GET, OPTIONS');
	header('Access-Control-Max-Age: 1000');
	header('Access-Control-Allow-Headers: Origin, X-Requested-With, Content-Type, Accept');
	
	main();
}

function main()
{
	//codes
	$codes = array();
	$codes["eigen"] = isset( $_GET["eigen"] ) ? $_GET["eigen"] : "";
	$codes["gemeenschappelijk"] = isset( $_GET["gemeenschappelijk"] ) ? $_GET["gemeenschappelijk"] : "";
	$codes["vandedag"] = isset( $_GET["vandedag"] ) ? $_GET["vandedag"] : "";
	
	//paginanummers ophalen
	$paginanummers = dir_paginanummers($codes);
	echo json_encode( $paginanummers );
}

/*	Functie geeft JSON van paginanummers
	op basis van de codes (eigen, gemeenschappelijk, van de dag) en eventueel de liturgische rang.
*/
function dir_paginanummers($codes, $rang = 0)
{
	//paginanummers uit database halen
	$paginanummers = array();
	$db = new SQLite3("data/calendar.sqlite3", SQLITE3_OPEN_READONLY);
	
	$sql = "SELECT paginanummers FROM calendar WHERE code_eigen = '" . $codes["eigen"] . "'";
	$result = $db->query($sql);
	$result_array = $result->fetchArray(SQLITE3_ASSOC);
	if ( $result === false || $result_array === false )
	{
		$paginanummers["eigen"] = null;
	} else {
		$paginanummers["eigen"] = json_decode( $result_array["paginanummers"], true );
	}
	
	//tijdelijke if
	if ($codes["gemeenschappelijk"] == null)
	{
		//TODO: rekening houden met meerdere gemeenschappelijke!
		$sql = "SELECT paginanummers FROM gemeenschappelijke WHERE code_gem = '" . $codes["gemeenschappelijk"] . "'"; //dit moet het uiteindelijk worden
	} else {
		$sql = "SELECT paginanummers FROM gemeenschappelijke WHERE code_gem = '901'";
	}
	$result = $db->query($sql);
	$result_array = $result->fetchArray(SQLITE3_ASSOC);
	if ( $result === false || $result_array === false )
	{
		$paginanummers["gemeenschappelijk"] = null;
	} else {
		$paginanummers["gemeenschappelijk"] = json_decode( $result_array["paginanummers"], true );
	}
	
	$sql = "SELECT paginanummers FROM calendar WHERE code_dag = '" . $codes["vandedag"] . "'";
	$result = $db->query($sql);
	$result_array = $result->fetchArray(SQLITE3_ASSOC);
	if ( $result === false || $result_array === false || $result_array["paginanummers"] == "[]" )
	{
		$paginanummers["vandedag"] = null;
	} else {
		$paginanummers["vandedag"] = json_decode( $result_array["paginanummers"], true );
	}
	
	$db->close();
	
	//paginanummers combineren en teruggeven
	$paginanummers["output"] = array();
	if ($paginanummers["eigen"] != null && $paginanummers["gemeenschappelijk"] == null && $paginanummers["vandedag"] == null)
	{
		$paginanummers["eigen"]["ui"] = null; //tijdelijk
		return $paginanummers["eigen"];
	}
	elseif ($paginanummers["eigen"] == null && $paginanummers["gemeenschappelijk"] == null && $paginanummers["vandedag"] != null)
	{
		if ( array_key_exists( "le", $paginanummers["vandedag"]["lz"] ) )
		{
			$keys = array( "l1", "l2" );
			foreach ( $keys as $key )
			{
				if ( array_key_exists( $key, $paginanummers["vandedag"]["lz"] ) )
				{
					unset($paginanummers["vandedag"]["lz"][$key]);
				}
			}
		}
		$paginanummers["vandedag"]["ui"] = null; //tijdelijk
		return $paginanummers["vandedag"];
	}
	elseif ($paginanummers["eigen"] != null && $paginanummers["vandedag"] != null)
	{
		
	}
	else
	{
		//tijdelijk? Dit mag uiteindelijk niet voorkomen.
		if ($paginanummers["vandedag"] == null)
		{
			$paginanummers["vandedag"]["ui"] = null;
			$paginanummers["vandedag"]["lz"] = null;
			$paginanummers["vandedag"]["mo"] = null;
			$paginanummers["vandedag"]["mi"] = null;
			$paginanummers["vandedag"]["av"] = null;
			$paginanummers["vandedag"]["ds"] = null;
		}
		return $paginanummers["vandedag"];
	}
}

/*	Functie geeft array van paginanummers van de uitnodiging
	op basis van de paginanummers (eigen, gemeenschappelijk, van de dag) en de liturgische rang.
*/
function dir_combine_ui($pp_eigen, $pp_gemeenschappelijk, $pp_vandedag, $rang)
{
	$pp_ui = array();
	
	if (isset($pp_eigen["a"]))
	{
		$pp_ui["getijdenboek"]["a"] = $pp_eigen["a"];
	}
	else
	{
		if ($rang == "g")
		{
			
		}
		else
		{
			
		}
	}
	
	return $pp_ui;
}

?>