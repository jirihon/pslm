<?php

$psalm_files = glob('pslm/pregenerated/*.pslm');
$psalms = [];

foreach ($psalm_files as $psalm_file) {
    $psalm = file_get_contents($psalm_file);

    preg_match('#/([^/]+)\.pslm$#', $psalm_file, $id_m);
    $id = $id_m[1];

    preg_match('#%% part: responsum.*?%% part#s', $psalm, $responsum_m);
    if (!$responsum_m) {
        //echo "No responsum in $id\n";
        continue;
    }
    preg_match('#%% part: verse.*#s', $psalm, $verse_m);
    if (!$verse_m) {
        //echo "No verse in $id\n";
        continue;
    }
    $psalms[$id] = [$responsum_m[0], $verse_m[0]];
}

$responsums = [];
$verses = [];

foreach ($psalms as $a_id => $a) {
    foreach ($psalms as $b_id => $b) {
        if ($a_id === $b_id) {
            continue;
        }
        if ($a[0] === $b[0]) {
            //echo "$a_id and $b_id have the same responsums\n";
            if (isset($responsums[$a_id])) {
                $responsums[$a_id][] = $b_id;
            } else {
                $responsums[$a_id] = [$b_id];
            }
        }
        if ($a[1] === $b[1]) {
            //echo "$a_id and $b_id have the same verses\n";
            if (isset($verses[$a_id])) {
                $verses[$a_id][] = $b_id;
            } else {
                $verses[$a_id] = [$b_id];
            }
        }
    }
}

function print_equal($id, $responsums, $verses) {
    $r = [];
    if (isset($responsums[$id])) {
        $r = $responsums[$id];
    }
    usort($r, function($a, $b) {
        return intval(substr($a, 2)) > intval(substr($b, 2));
    });
    $v = [];
    if (isset($verses[$id])) {
        $v = $verses[$id];
    }
    usort($v, function($a, $b) {
        return intval(substr($a, 2)) > intval(substr($b, 2));
    });
    echo sprintf("%s,'%s','%s'\n", $id, implode(', ', $r), implode(', ', $v));
}

for ($i = 1; $i <= 344; $i++) {
    $id = "OL$i";
    print_equal($id, $responsums, $verses);

    if ($i == 99) {
        print_equal('OL99a', $responsums, $verses);
    }
}


for ($i = 401; $i <= 728; $i++) {
    $id = "OL$i";
    print_equal($id, $responsums, $verses);
}

file_put_contents('db/similarities.php',
    sprintf(
        "<?php\n\n\$PSLM_SAME_RESPONSUMS = %s;\n\n\$PSLM_SAME_VERSES = %s;\n",
        var_export($responsums, true),
        var_export($verses, true)
    )
);
