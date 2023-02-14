<?php

if (main())
{
	echo "Resetten geslaagd.";
} else {
	echo "Resetten mislukt.";
}

function main()
{
	//1. kopieer calendar.sqlite3 naar paginanummers.sqlite3
	/*
	$calendarDB = "calendar.sqlite3";
	$paginanummersDB = "paginanummers.sqlite3";
	*/
	$paginanummersDB = "data/calendar.sqlite3";
	
	/*
	if (copy($calendarDB, $paginanummersDB))
	{
		//2. rijen tijd door het jaar toevoegen
		//Dit wordt op termijn overbodig, omdat dit standaard in de kalenderdatabase zit
		if (addTijddoorjaar($paginanummersDB))
		{
			*/
			//3. paginanummers uit excelbestanden toevoegen
			$db = new SQLite3($paginanummersDB);
			/*
			$sql = "ALTER TABLE calendar ADD paginanummers varchar(255)";
			$db->exec($sql);
			*/
			
			$hymnenFiles = [ "Hymnen tijd door het jaar.txt", "Hymnen vierendertigste week.txt" ];
			$psalmboekFiles = [ "Psalmboek eerste week.txt", "Psalmboek tweede week.txt", "Psalmboek derde week.txt", "Psalmboek vierde week.txt" ];
			$zondagenDoorHetJaarFile = "Tijdeigen zondagen dhj.txt";
			$gemeenschappelijkeFile = "Gemeenschappelijke.txt";
			$heiligenFile = "Heiligen.txt";
			addPaginanummers($db, $hymnenFiles, $psalmboekFiles, $zondagenDoorHetJaarFile, $heiligenFile, $gemeenschappelijkeFile);
			
			$db->close();
		/*
		} else {
			echo "Fout bij het toevoegen van de tijd door het jaar.";
			return false;
		}
	} else {
		echo "Fout bij het aanmaken van de database.";
		return false;
	}
	*/
	return true;
}

function addTijddoorjaar($filename)
{
	$db = new SQLite3($filename);

	for($w=0;$w<34;$w++)
	{
		insertRow($db, 7*$w+0,60,strval($w+1)."e zondag door het jaar","","d","g",NULL,600+7*$w);
		insertRow($db, 7*$w+1,130,"Maandag in de ".strval($w+1)."e week door het jaar","","d","g",NULL,600+7*$w+1);
		insertRow($db, 7*$w+2,130,"Dinsdag in de ".strval($w+1)."e week door het jaar","","d","g",NULL,600+7*$w+2);
		insertRow($db, 7*$w+3,130,"Woensdag in de ".strval($w+1)."e week door het jaar","","d","g",NULL,600+7*$w+3);
		insertRow($db, 7*$w+4,130,"Donderdag in de ".strval($w+1)."e week door het jaar","","d","g",NULL,600+7*$w+4);
		insertRow($db, 7*$w+5,130,"Vrijdag in de ".strval($w+1)."e week door het jaar","","d","g",NULL,600+7*$w+5);
		insertRow($db, 7*$w+6,130,"Zaterdag in de ".strval($w+1)."e week door het jaar","","d","g",NULL,600+7*$w+6);
		insertRow($db, 7*$w+6,120,"Maria op zaterdag","Maria op zaterdag","","w",599,NULL);
		
		/* UIT server/classes/jaarkalender.php
		$data[]=array("D",	7*$w+0,	60,	"",	strval($w+1)."e zondag door het jaar",					"",					"d","g","",	"",600+7*$w,	950);
		$data[]=array("D",	7*$w+1,	130,"",	"Maandag in de ".strval($w+1)."e week door het jaar",	"",					"d","g","",	"",600+7*$w+1,	950);
		$data[]=array("D",	7*$w+2,	130,"",	"Dinsdag in de ".strval($w+1)."e week door het jaar",	"",					"d","g","",	"",600+7*$w+2,	950);
		$data[]=array("D",	7*$w+3,	130,"",	"Woensdag in de ".strval($w+1)."e week door het jaar",	"",					"d","g","",	"",600+7*$w+3,	950);
		$data[]=array("D",	7*$w+4,	130,"",	"Donderdag in de ".strval($w+1)."e week door het jaar",	"",					"d","g","",	"",600+7*$w+4,	950);
		$data[]=array("D",	7*$w+5,	130,"",	"Vrijdag in de ".strval($w+1)."e week door het jaar",	"",					"d","g","",	"",600+7*$w+5,	950);
		$data[]=array("D",	7*$w+6,	130,"",	"Zaterdag in de ".strval($w+1)."e week door het jaar",	"",					"d","g","",	"",600+7*$w+6,	950);
		$data[]=array("D",	7*$w+6,	120,"",	"Maria op zaterdag",									"Maria op zaterdag","",	"w",599,"","",			950);
		*/
	}
	$db->close();
	return true;
}

function addPaginanummers($db, $hymnenFiles, $psalmboekFiles, $zondagenDoorHetJaarFile, $heiligenFile, $gemeenschappelijkeFile)
{
	//elke rij van de db doorlopen: codes ophalen
	//paginanummers toevoegen
	
	//checken of tekstbestanden bestaan?
	$folder = "data/paginanummers/";
	
	//tekstbestanden inladen
	$hymnen = [];
	foreach ($hymnenFiles as $nr => $hymnenFile)
	{
		if ($file = fopen($folder.$hymnenFile, "r"))
		{
			while(!feof($file)) 
			{
				$hymnen[$nr][] = explode("\t", fgets($file));
			}
			fclose($file);
			//var_dump($hymnen);
			/* Kolommen hymnen door het jaar & vierendertigste week (nrs. 0 en 1)
			=> eerste regel: koppen
			=> tweede en derde regel: zondag
			=> daarna overige dagen
			A	0	dag (string)
			B	1	psalmboek weken
			C	2	vooravond
			D	3	lezingendienst
			E	4	morgengebed
			F	5	middaggebed
			G	6	avondgebed
			H	7	dagsluiting
			*/
		} else {
			echo "Niet gelukt bestand te openen: ".$hymnenFile;
		}
	}
	
	$psalmboek = [];
	foreach ($psalmboekFiles as $nr => $psalmboekFile)
	{
		if ($file = fopen($folder.$psalmboekFile, "r"))
		{
			while(!feof($file)) 
			{
				$psalmboek[$nr][] = explode("\t", fgets($file));
			}
			fclose($file);
			
			/* Kolommen psalmboek
			A	0	
			B	1	
			C	2	va	alles
			D	3	va	psalmodie
			E	4	va	na psalmodie
			F	5	
			G	6	ui	alles
			H	7	ui	antifoon
			I	8	
			J	9	lz	alles
			K	10	lz	psalmodie en vers
			L	11	
			M	12	mo	alles
			N	13	mo	psalmodie
			O	14	mo	na psalmodie
			P	15	
			Q	16	mi	alles
			R	17	mi	psalmodie
			S	18	mi	na psalmodie
			T	19	
			U	20	av	alles
			V	21	av	psalmodie
			W	22	av	na psalmodie
			X	23	
			Y	24	
			Z	25	
			*/
			
		} else {
			echo "Niet gelukt bestand te openen: ".$psalmboekFile;
		}
	}
	//var_dump($psalmboek);
	$zondagenDHJ = [];
	if ($file = fopen($folder.$zondagenDoorHetJaarFile, "r"))
	{
		while(!feof($file)) 
		{
			$zondagenDHJ[] = explode("\t", fgets($file));
		}
		fclose($file);
		
		/* Kolommen zondagen door het jaar
		A	0	
		B	1	nummer van de zondag: 3 = eerste zondag
		C	2	vooravond antifonen
		D	3	morgengebed antifonen
		E	4	afsluitend gebed
		F	5	avondgebed antifonen
		*/
	} else {
		echo "Niet gelukt bestand te openen: ".$psalmboekFile;
	}
	$heiligen = [];
	if ($file = fopen($folder.$heiligenFile, "r"))
	{
		while(!feof($file)) 
		{
			$line = explode("\t", fgets($file));
			if(array_key_exists(3, $line))
			{
				$heiligen[$line[3]] = $line;
			}
		}
		fclose($file);
		
		/* Kolommen heiligen
		A	0	
		B	1	
		C	2	
		D	3	code
		E	4	
		F	5	afsluitend gebed
		G	6	morgengebed antifoon lofzang
		H	7	avondgebed antifoon lofzang
		*/
	} else {
		echo "Niet gelukt bestand te openen: ".$heiligenFile;
	}
	
	//database rijen inladen
	$sql = "SELECT code_eigen, code_gem, code_dag FROM calendar";
	$db_codes = $db->query($sql);
	
	$kalender_entries = [];
	while ($row = $db_codes->fetchArray(SQLITE3_ASSOC))
	{
		$c = [ "eigen" => $row["code_eigen"], "gemeenschappelijk" => $row["code_gem"], "vandedag" => $row["code_dag"] ];
		//var_dump($c);
		//print "<br />";
		$kalender_entries[] = $c;
	}
	//var_dump($kalender_entries);
	
	foreach ($kalender_entries as $codes)
	{
		$paginanummers = [];
		//Tijd door het jaar: 601 t/m 837
		if ($codes["vandedag"] >= 601 and $codes["vandedag"] <= 837)
		{
			//welke dag van de week?
			$weekdag = ($codes["vandedag"] - 600) % 7; //zondag = 0
			//welke week door het jaar?
			$weeknummer = intdiv(($codes["vandedag"]-600), 7); //van 0 t/m 33
			//welke week in het psalmboek?
			$psalmboekWeek = getPsalmboekWeek($codes); //van 1 t/m 4
			//paginanummers opbouwen als array
			$pb = $psalmboek[$psalmboekWeek - 1][$weekdag + 2];
			//vooravond
			if ($weekdag == 0)
			{
				$paginanummers["va"]["a"] = $pb[2];
				$paginanummers["va"]["b"]["hy"] = getHymne($hymnen, $weekdag, $weeknummer, $psalmboekWeek, $codes, "va");
				$paginanummers["va"]["b"]["ag"] = $zondagenDHJ[$weeknummer + 3][4];
				$paginanummers["va"]["b"]["ag"] = $zondagenDHJ[$weeknummer + 3][4];
			}
			//uitnodiging
			$paginanummers["ui"]["an"] = $pb[6];
			//lezingendienst
			//$paginanummers["lz"]["a"] = $pb[9];
			$paginanummers["lz"]["hy"] = getHymne($hymnen, $weekdag, $weeknummer, $psalmboekWeek, $codes, "lz");
			$paginanummers["lz"]["ps"] = $pb[10];
			$paginanummers["lz"]["le"] = "onbekend";
			$paginanummers["lz"]["l1"] = "onbekend";
			$paginanummers["lz"]["l2"] = "onbekend";
			$paginanummers["lz"]["ag"] = $zondagenDHJ[$weeknummer + 3][4];
			//morgengebed
			$paginanummers["mo"]["a"] = $pb[12];
			$paginanummers["mo"]["hy"] = getHymne($hymnen, $weekdag, $weeknummer, $psalmboekWeek, $codes, "mo");
			$paginanummers["mo"]["ps"] = $pb[13];
			$paginanummers["mo"]["np"] = $pb[14];
			if ($weekdag == 0)
			{
				$paginanummers["mo"]["b"]["al"] = $zondagenDHJ[$weeknummer + 3][4];
				$paginanummers["mo"]["b"]["ag"] = $zondagenDHJ[$weeknummer + 3][4];
			}
			//middaggebed
			$paginanummers["mi"]["a"] = $pb[16];
			$paginanummers["mi"]["hy"] = getHymne($hymnen, $weekdag, $weeknummer, $psalmboekWeek, $codes, "mi");
			$paginanummers["mi"]["ps"] = $pb[17];
			$paginanummers["mi"]["np"] = $pb[18];
			if ($weekdag == 0)
			{
				$paginanummers["mi"]["b"]["ag"] = $zondagenDHJ[$weeknummer + 3][4];
			}
			//avondgebed
			if ($weekdag != 6)
			{
				$paginanummers["av"]["a"] = $pb[20];
				$paginanummers["av"]["hy"] = getHymne($hymnen, $weekdag, $weeknummer, $psalmboekWeek, $codes, "av");
				$paginanummers["av"]["ps"] = $pb[21];
				$paginanummers["av"]["np"] = $pb[22];
				if ($weekdag == 0)
				{
					$paginanummers["av"]["b"]["al"] = $zondagenDHJ[$weeknummer + 3][4];
					$paginanummers["av"]["b"]["ag"] = $zondagenDHJ[$weeknummer + 3][4];
				}
			}
			//dagsluiting
			$paginanummers["ds"]["a"] = "onbekend";
		} elseif ($codes["eigen"]) {
			if(array_key_exists($codes["eigen"], $heiligen))
			{
				if($heiligen[$codes["eigen"]][5] != "")
				{
					$paginanummers["lz"]["ag"] = $heiligen[$codes["eigen"]][5];
					$paginanummers["mo"]["ag"] = $heiligen[$codes["eigen"]][5];
					$paginanummers["mi"]["ag"] = $heiligen[$codes["eigen"]][5];
					$paginanummers["av"]["ag"] = $heiligen[$codes["eigen"]][5];
				}
				if($heiligen[$codes["eigen"]][6] != "")
				{
					$paginanummers["mo"]["al"] = $heiligen[$codes["eigen"]][6];
				}
				if($heiligen[$codes["eigen"]][7] != "")
				{
					$paginanummers["av"]["al"] = $heiligen[$codes["eigen"]][7];
				}
			}
		}
		$sql = "UPDATE calendar ";
		$sql .= "SET paginanummers = '".(json_encode($paginanummers))."' ";
		$sql .= "WHERE code_eigen ".sqlConditionalstringOfNULL($codes["eigen"])." ";
		$sql .= "AND code_gem ".sqlConditionalstringOfNULL($codes["gemeenschappelijk"])." ";
		$sql .= "AND code_dag ".sqlConditionalstringOfNULL($codes["vandedag"]);
		print $sql;
		print "<br />";
		$db->exec($sql);
		print "<br />";
	}
	
	//tabel met paginanummers gemeenschappelijke
	//bestand inlezen
	$gemeenschappelijke = [];
	if ($file = fopen($folder.$gemeenschappelijkeFile, "r"))
	{
		while(!feof($file)) 
		{
			$gemeenschappelijke[] = explode("\t", fgets($file));
		}
		fclose($file);
		
		/* Kolommen gemeenschappelijke teksten
		A	0	
		B	1	
		C	2	code
		D	3	Vooravond		begin-eind
		E	4	Vooravond		hymne
		F	5	Vooravond		psalmodie
		G	6	Vooravond		lezing en beurtzang
		H	7	Vooravond		antifoon lofzang
		I	8	Vooravond		slotgebeden
		J	9	Vooravond		afsluitend gebed
		K	10	Uitnodiging		antifoon
		L	11	Lezingendienst	hymne
		M	12	Lezingendienst	psalmodie en vers
		N	13	Lezingendienst	lezingen
		O	14	Lezingendienst	lezing 1
		P	15	Lezingendienst	lezing 2
		Q	16	Lezingendienst	afsluitend gebed
		R	17	Morgengebed		begin-eind
		S	18	Morgengebed		hymne
		T	19	Morgengebed		antifonen
		U	20	Morgengebed		psalmen
		V	21	Morgengebed		lezing en beurtzang
		W	22	Morgengebed		antifoon lofzang
		X	23	Morgengebed		slotgebeden
		Y	24	Morgengebed		afsluitend gebed
		Z	25	Middaggebed		antifoon
		AA	26	Middaggebed		lezing en vers
		AB	27	Middaggebed		afsluitend gebed
		AC	28	Avondgebed		begin-eind
		AD	29	Avondgebed		hymne
		AE	30	Avondgebed		psalmodie
		AF	31	Avondgebed		lezing en beurtzang
		AG	32	Avondgebed		antifoon lofzang
		AH	33	Avondgebed		slotgebeden
		AI	34	Avondgebed		afsluitend gebed
		*/
	} else {
		echo "Niet gelukt bestand te openen: ".$gemeenschappelijkeFile;
	}
	
	//gemeenschappelijke invoeren
	$sql = "CREATE TABLE IF NOT EXISTS gemeenschappelijke ( code_gem varchar(255), paginanummers varchar(255) )";
	$db->exec($sql);
	$sql = "DELETE FROM gemeenschappelijke";
	$db->exec($sql);
	
	for ($i = 2; $i < count($gemeenschappelijke); $i++)
	{
		if (array_key_exists(2, $gemeenschappelijke[$i]) && $gemeenschappelijke[$i][2])
		{
			$code = $gemeenschappelijke[$i][2];
			$paginanummers = array();
			$paginanummers["va"]["hy"] = $gemeenschappelijke[$i][4];
			$paginanummers["va"]["ps"] = $gemeenschappelijke[$i][5];
			$paginanummers["va"]["lz"] = $gemeenschappelijke[$i][6];
			$paginanummers["va"]["al"] = $gemeenschappelijke[$i][7];
			$paginanummers["va"]["vb"] = $gemeenschappelijke[$i][8];
			$paginanummers["va"]["ag"] = $gemeenschappelijke[$i][9];
			
			$paginanummers["ui"]["an"] = $gemeenschappelijke[$i][10];
			
			$paginanummers["lz"]["hy"] = $gemeenschappelijke[$i][11];
			$paginanummers["lz"]["ps"] = $gemeenschappelijke[$i][12];
			$paginanummers["lz"]["le"] = $gemeenschappelijke[$i][13];
			$paginanummers["lz"]["l1"] = $gemeenschappelijke[$i][14];
			$paginanummers["lz"]["l2"] = $gemeenschappelijke[$i][15];
			$paginanummers["lz"]["ag"] = $gemeenschappelijke[$i][16];
			
			$paginanummers["mo"]["a"] = $gemeenschappelijke[$i][17];
			$paginanummers["mo"]["hy"] = $gemeenschappelijke[$i][18];
			$paginanummers["mo"]["an"] = $gemeenschappelijke[$i][19];
			$paginanummers["mo"]["ps"] = $gemeenschappelijke[$i][20];
			$paginanummers["mo"]["lz"] = $gemeenschappelijke[$i][21];
			$paginanummers["mo"]["al"] = $gemeenschappelijke[$i][22];
			$paginanummers["mo"]["vb"] = $gemeenschappelijke[$i][23];
			$paginanummers["mo"]["ag"] = $gemeenschappelijke[$i][24];
			
			$paginanummers["mi"]["an"] = $gemeenschappelijke[$i][25];
			$paginanummers["mi"]["lz"] = $gemeenschappelijke[$i][26];
			$paginanummers["mi"]["ag"] = $gemeenschappelijke[$i][27];
			
			$paginanummers["av"]["a"] = $gemeenschappelijke[$i][28];
			$paginanummers["av"]["hy"] = $gemeenschappelijke[$i][29];
			$paginanummers["av"]["ps"] = $gemeenschappelijke[$i][30];
			$paginanummers["av"]["lz"] = $gemeenschappelijke[$i][31];
			$paginanummers["av"]["al"] = $gemeenschappelijke[$i][32];
			$paginanummers["av"]["vb"] = $gemeenschappelijke[$i][33];
			$paginanummers["av"]["ag"] = trim($gemeenschappelijke[$i][34]);
			
			$sql = "INSERT INTO gemeenschappelijke (code_gem, paginanummers) VALUES ('";
			$sql .= $code;
			$sql .= "', '";
			$sql .= json_encode($paginanummers);
			$sql .= "')";
			print $sql;
			$db->exec($sql);
		}
	}
}

function sqlConditionalstringOfNULL($code)
{
	if($code === NULL)
	{
		return "IS NULL";
	} else {
		return "= '".$code."'";
	}
}

function insertRow($db, $dag, $prioriteit, $naam_lang, $naam_kort, $liturgische_tijd, $liturgische_kleur, $code_eigen, $code_dag)
{
	//Deze functie is mogelijk niet af, omdat deze rijen standaard in de database komen
	
	//print "<br />".$naam_lang."<br />";
	$sql = "INSERT INTO calendar(maand, dag, prioriteit, naam_lang, naam_kort, liturgische_tijd, liturgische_kleur, code_eigen, code_dag) ";
	$sql .= "VALUES ('D', $dag, $prioriteit, '$naam_lang', ";
	if ($naam_kort)
	{
		$sql .= "'$naam_kort', ";
	} else {
		$sql .= "NULL, ";
	}
	if ($liturgische_tijd)
	{
		$sql .= "'$liturgische_tijd', ";
	} else {
		$sql .= "NULL, ";
	}
	$sql .= "'$liturgische_kleur', ";
	if ($code_eigen)
	{
		$sql .= "$code_eigen, ";
	} else {
		$sql .= "NULL, ";
	}
	if ($code_dag)
	{
		$sql .= "$code_dag)";
	} else {
		$sql .= "NULL)";
	}
	//print $sql."<br />";
	$db->exec($sql);
}

function getPsalmboekWeek($codes)
{
	if ($codes["vandedag"] >= 1 and $codes["vandedag"] <= 20)
	{
		$weeknummer = floor(($codes["vandedag"] - 1) / 7);
		$psalmboek = $weeknummer % 4 + 1;
		return $psalmboek;
	}
	//vierde zondag van de advent: code dag 021
	elseif ($codes["vandedag"] == 21)
	{
		$psalmboek = 4;
		return $psalmboek;
	}
	//tweede deel: code dag 159 t/m 187
	elseif ($codes["vandedag"] >= 159 and $codes["vandedag"] <= 187)
	{
		$weeknummer = floor(($codes["vandedag"] - 160) / 7) + 3;
		$psalmboek = $weeknummer % 4 + 1;
		return $psalmboek;
	}
	
	//2. veertigdagentijd: donderdag na aswoensdag t/m zaterdag voor palmzondag
	//code dag 054 t/m 091
	elseif ($codes["vandedag"] >= 54 and $codes["vandedag"] <= 91)
	{
		$weeknummer = floor(($codes["vandedag"]-57) / 7); //van -1 tot 6
		$psalmboek = ($weeknummer + 4) % 4 + 1; //+4 om -1 in de berekening te voorkomen
		return $psalmboek;
	}
	
	
	
	//1. tijd door het jaar: code dag 600 t/m 837
	//(weeknummer % 4 + 1) // weeknummer = intdiv( (n-600), 7 ) + 1
	elseif ($codes["vandedag"] >= 600 and $codes["vandedag"] <= 837)
	{
		$weeknummer = intdiv(($codes["vandedag"]-600), 7); //van 0 tot 33
		$psalmboek = $weeknummer % 4 + 1;
		return $psalmboek;
	}
}

function getHymne($files, $weekdag, $weeknummer, $psalmboekWeek, $codes, $gebedsuur)
{
	//geeft string terug met paginanummers gevraagde hymne
	$hymne = "";
	
	//Tijd door het jaar: 601 t/m 837
	if ($codes["vandedag"] >= 601 and $codes["vandedag"] <= 837)
	{
		//uit welk bestand: door het jaar of vierendertigste week?
		if ( $weeknummer == 33 ) //34e week door het jaar
		{
			$file = $files[1];
		} else {
			$file = $files[0];
		}
		
		//welke rij in het bestand?
		if ( $weekdag == 0 and ( $psalmboekWeek == 1 or $psalmboekWeek == 3 ) )
		{
			$rij = 1;
		} elseif ( $weekdag == 0 and ( $psalmboekWeek == 2 or $psalmboekWeek == 4 ) )
		{
			$rij = 2;
		} else {
			$rij = $weekdag + 2;
		}
		
		//welke kolom?
		switch ($gebedsuur)
		{
			case "va":
				$kolom = 2; break;
			case "lz":
				$kolom = 3; break;
			case "mo":
				$kolom = 4; break;
			case "mi":
				$kolom = 5; break;
			case "av":
				$kolom = 6; break;
		}
		
		$hymne = $file[$rij][$kolom];
		
	} else {
		$hymne = "onbekend";
	}
	return $hymne;
}
?>