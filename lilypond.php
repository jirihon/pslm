#!/usr/bin/env php
<?php
require_once 'pslm.php';

$pslm_f = $argv[count($argv)-1];

if (preg_match('#\.pslm$#', $pslm_f)) {
    // convert .pslm to .ly before running lilypond
    $psalm = file_get_contents($pslm_f);
    $psalm = pslm_parse_psalm($psalm);
    $lily = pslm_lilypond($psalm, 10, false);

    $lily_f = preg_replace('#\.pslm$#', '.ly', $pslm_f);
    file_put_contents($lily_f, $lily);

    $passed_args = array_slice($argv, 1, -1);
    $passed_args[] = $lily_f;
} else {
    // fallback to basic lilypond
    $passed_args = array_slice($argv, 1);
}
$passed_args = array_map(function($arg) { return escapeshellarg($arg); }, $passed_args);
$passed_args = implode(' ', $passed_args);
$cmd = "lilypond $passed_args";
system($cmd, $retcode);
exit($retcode);
