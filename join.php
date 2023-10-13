<?php
require_once dirname(__FILE__).'/pslm.php';

$id = $argv[count($argv)-1];

pslm_join_svgs($id, 'html/svg');
