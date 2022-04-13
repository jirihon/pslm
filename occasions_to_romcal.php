<?php

require 'db/occasions.php';
require 'db/romcal_names.php';

$alldefs = json_decode(file_get_contents('db/romcal_alldefinitions.json'), true);

$dow = [
    'Pondělí' => 'monday',
    'Úterý' => 'tuesday',
    'Středa' => 'wednesday',
    'Čtvrtek' => 'thursday',
    'Pátek' => 'friday',
    'Sobota' => 'saturday',
];
$nday = [
    'První' => 1,
    'Druhý' => 2,
    'Třetí' => 3,
    'Čtvrtý' => 4,
    'Pátý' => 5,
    'Šestý' => 6,
    'Sedmý' => 7,
];
$month = [
    'ledna' => 1,
    'února' => 2,
    'března' => 3,
    'dubna' => 4,
    'května' => 5,
    'června' => 6,
    'července' => 7,
    'srpna' => 8,
    'září' => 9,
    'října' => 10,
    'listopadu' => 11,
    'prosince' => 12,
];
$week_rx = '(?<week>\d+)';
$day_rx = '(?<day>\d+)';
$dow_rx = '(?<dow>'.implode('|', array_keys($dow)).')';
$cycle_rx = '(?<cycle>[ABC12][ABC12\(\)]*)?';
$month_rx = '(?<month>'.implode('|', array_keys($month)).')';
$nday_rx = '(?<nday>'.implode('|', array_keys($nday)).')';

$name_map = [];

foreach ($names as $name => $id) {
    $name = preg_quote($name);
    $rx = "#^$name ?$cycle_rx#u";
    $name_map[$rx] = $id;
}

$time_map = [
    "#^$week_rx\. neděle adventní ?$cycle_rx#u" => 'advent_{week}_sunday',
    "#^$dow_rx po $week_rx\. neděli adventní#u" => 'advent_{week}_{dow}',

    "#^Předvánoční týden \($day_rx\. prosince#u" => 'advent_december_{day}',
    "#^2. neděle po Narození Páně#u" => 'second_sunday_after_christmas',
    "#^$dow_rx po Zjevení Páně#u" => '{dow}_after_epiphany',
    "#^$nday_rx den v oktávu Narození Páně#u" => 'christmas_octave_day_{nday}',
    "#^Týden před Zjevením Páně \($day_rx\. ledna#u" => 'christmas_time_january_{day}',
    
    "#^$dow_rx po Popeleční středě#u" => '{dow}_after_ash_wednesday',
    "#^$week_rx\. neděle postní ?$cycle_rx#u" => 'lent_{week}_sunday',
    "#^$dow_rx po $week_rx\. neděli postní#u" => 'lent_{week}_{dow}',
    "#^$dow_rx Svatého týdne#u" => 'holy_{dow}',

    "#^$dow_rx v oktávu velikonočním#u" => 'easter_{dow}',
    "#^2\. neděle velikonoční ?$cycle_rx#u" => 'divine_mercy_sunday',
    "#^$week_rx\. neděle velikonoční ?$cycle_rx#u" => 'easter_time_{week}_sunday',
    "#^$dow_rx po $week_rx\. neděli velikonoční#u" => 'easter_time_{week}_{dow}',

    "#^3\. neděle v mezidobí ?$cycle_rx#u" => 'sunday_of_the_word_of_god',
    "#^$week_rx\. neděle v mezidobí ?$cycle_rx#u" => 'ordinary_time_{week}_sunday',
    "#^$dow_rx $week_rx\. týdne ?$cycle_rx#u" => 'ordinary_time_{week}_{dow}',

    "#$day_rx\. $month_rx#u" => '{day}/{month}',
];

$map = array_merge($name_map, $time_map);

$occasions = array_keys($occasion_psalm);

$to_romcal = [];

foreach ($occasions as $occasion) {
    foreach ($map as $regex => $template) {
        if (preg_match($regex, $occasion, $m)) {
            $search = [];
            $replace = [];
            $info = [];

            foreach ($m as $key => $value) {
                if (is_string($key)) {
                    if ($key === 'dow') {
                        $value = $dow[$value];
                    } elseif ($key === 'month') {
                        $value = $month[$value];
                    } elseif ($key === 'nday') {
                        $value = $nday[$value];
                    }
                    $search[] = sprintf('{%s}', $key);
                    $replace[] = $value;
                    $info[$key] = $value;
                }
            }
            $id = str_replace($search, $replace, $template);
            if (strpos($id, '/') === false && !isset($alldefs[$id])) {
                echo "ERROR: $id does not exist in romcal\n";
            }
            $info['id'] = $id;
            $to_romcal[$occasion] = $info;
            break;
        }
    }
}

$fp = fopen('db/occasion_romcal.csv', 'w');
fputcsv($fp, ['occasion','id','cycle']);

foreach ($occasions as $occasion) {
    $info = $to_romcal[$occasion] ?? [];

    fputcsv($fp, [$occasion, $info['id'] ?? '', $info['cycle'] ?? '']);
}
fclose($fp);

$to_occasions = [];
foreach ($to_romcal as $occasion => $info) {
    if (!isset($info['id'])) {
        continue;
    }
    $id = $info['id'];
    preg_match_all('#[ABC12]#', $info['cycle'] ?? '', $m);
    $cycles = $m[0];

    if (empty($cycles)) {
        if (isset($to_occasions[$id])) {
            $to_occasions[$id][] = $occasion;
        } else {
            $to_occasions[$id] = [$occasion];
        }
    }
    foreach ($cycles as $cycle) {
        $key = "$id|$cycle";
        if (isset($to_occassion[$key])) {
            echo "ERROR: $key already set\n";
        }
        $to_occasions[$key] = [$occasion];
    }
}
file_put_contents('db/romcal_to_occasions.json', json_encode($to_occasions, JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
