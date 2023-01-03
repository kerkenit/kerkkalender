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

    include 'lib/kerkkalender.php';

    $start = time();
    if (isset($_GET['start']))
        $start = strtotime($_GET['start']);
    $end = $start;
    if (isset($_GET['end']))
        $end = strtotime($_GET['end']);
    $cal = kerkkalender($start, $end);

    print $cal;
}
