<?php

require 'db/occasions.php';

$alldefs = json_decode(file_get_contents('db/romcal_alldefinitions.json'), true);

var_dump(array_keys($alldefs));
var_dump(array_keys($occasion_psalm));