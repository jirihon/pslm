<?php

use Symfony\Component\Yaml\Yaml;

require_once dirname(__FILE__).'/pslm.php';
require_once dirname(__FILE__).'/db/similarities.php';

define('PSLM_SVG_SIZES', [7, 8, 9, 10, 11, 12, 13, 14, 15]);
//define('PSLM_SVG_SIZES', [15]);
//define('PSLM_SVG_SIZES', [7, 9, 11, 13, 15]);
define('PSLM_CACHE', true);

define('PSLM_PX_PER_STEP', 50);
define('PSLM_MAX_WIDTH', 732);

$PSLM_AUTHORS = Yaml::parseFile(dirname(__FILE__).'/db/authors.yml');
$PSLM_SOURCES = Yaml::parseFile(dirname(__FILE__).'/db/sources.yml');
$PSLM_PSALMS = [];

pslm_render_sizes_css();
pslm_render_listing();
pslm_update_pregenerated();
pslm_save_occasions();
pslm_occasions_to_romcal();
pslm_render_index();


if (file_exists('upload.sh')) {
    system('./upload.sh');
}

function pslm_occasions_to_romcal() {
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
        /** @var array $cycles */
        foreach ($cycles as $cycle) {
            $key = "$id|$cycle";
            if (isset($to_occassion[$key])) {
                echo "ERROR: $key already set\n";
            }
            $to_occasions[$key] = [$occasion];
        }
    }
    file_put_contents('db/romcal_to_occasions.json', json_encode($to_occasions, JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
}

function pslm_save_occasions() {
    global $PSLM_PSALMS;
    $id_psalm = [];

    foreach ($PSLM_PSALMS as $id => $psalm) {
        $id_psalm[$id] = [$id];
    }

    $occasion_psalm = [];
    foreach ($PSLM_PSALMS as $id => $psalm) {
        if (!isset($psalm['opts']['occasion'])) {
            echo "ERROR: $id does not have any occasion\n";
        }
        $occasions = $psalm['opts']['occasion'];
        if (!is_array($occasions)) {
            $occasions = [$occasions];
        }
        foreach ($occasions as $occasion) {
            $occasion = trim($occasion);
            if (isset($occasion_psalm[$occasion])) {
                $occasion_psalm[$occasion][] = $id;
            } else {
                $occasion_psalm[$occasion] = [$id];
            }
        }
    }
    file_put_contents(dirname(__FILE__).'/db/occasions.php', '<?php $occasion_psalm = '.var_export($occasion_psalm, true).';');

    $responsum_psalm = [];
    $psalm_titles = [];
    foreach ($PSLM_PSALMS as $id => $psalm) {
        $responsum = implode(' ', $psalm['original_text']['responsum']);

        if (isset($responsum_psalm[$responsum])) {
            $responsum_psalm[$responsum][] = $id;
        } else {
            $responsum_psalm[$responsum] = [$id];
        }
        $psalm_titles[$id] = pslm_psalm_title($id, $psalm);
    }
    file_put_contents(dirname(__FILE__).'/db/psalms.json', json_encode(array_merge($id_psalm, $occasion_psalm, $responsum_psalm), JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
    file_put_contents(dirname(__FILE__).'/db/psalm_titles.json', json_encode($psalm_titles, JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
}


function pslm_html_page($title, $body, $head = '') {
    ob_start();
?><!DOCTYPE html>
<html lang="cs" prefix="og: http://ogp.me/ns#">
<head>
    <meta charset="UTF-8">
    <?php if (!empty($title)): ?>
        <title><?= $title ?> – Žaltář</title>
    <?php else: ?>
        <title>Žaltář</title>
    <?php endif ?>
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <link rel="stylesheet" href="css/style.css?ver=<?= time() ?>" media="all" />
    <?= $head ?>
</head>
<body>
    <div class="main">
        <?php if (!empty($title)): ?>
            <a onclick="history.back(); return false;" href="./" class="back-button"><svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="chevron-left" class="svg-inline--fa fa-chevron-left fa-w-10" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512"><path fill="currentColor" d="M34.52 239.03L228.87 44.69c9.37-9.37 24.57-9.37 33.94 0l22.67 22.67c9.36 9.36 9.37 24.52.04 33.9L131.49 256l154.02 154.75c9.34 9.38 9.32 24.54-.04 33.9l-22.67 22.67c-9.37 9.37-24.57 9.37-33.94 0L34.52 272.97c-9.37-9.37-9.37-24.57 0-33.94z"></path></svg></a>
            <h1><?= $title ?></h1>
        <?php else: ?>
            <h1>Žaltář</h1>
        <?php endif ?>
        <?= $body ?>
    </div>
</body>
</html>
<?php
     return ob_get_clean();
}

function pslm_psalm_title($id, $psalm) {
    return sprintf(
        '%s – %s – %s',
        $id,
        implode(' ', $psalm['original_text']['responsum']),
        $psalm['opts']['verse_reference']
    );
}

function pslm_render_listing() {
    global $PSLM_SOURCES, $PSLM_PSALMS;
    $html = '';


    foreach ($PSLM_SOURCES as $source) {
        if (!isset($source['ids'])) {
            continue;
        }
        $html .= sprintf('<p>%s</p>', $source['reference']);
        $item_html = [];
        $done = [];

        foreach ($source['ids'] as $id) {
            if (file_exists(sprintf('%s/pslm/%s.pslm', dirname(__FILE__), $id))) {
                $done[] = $id;
                if (!isset($PSLM_PSALMS[$id])) {
                    $PSLM_PSALMS[$id] = pslm_render_psalm_html($id);
                }
                $item_html[] = sprintf(
                    '<li><a href="%s.html">%s</a></li>',
                    $id,
                    pslm_psalm_title($id, $PSLM_PSALMS[$id])
                );
            }
        }
        $total = count($source['ids']);
        $done = count($done);
        $html .= sprintf('<p style="margin-left: 1em">Přepsáno %d z %d žalmů (%s %%).</p>', $done, $total, round($done/$total * 100));
        $html .= sprintf('<ul>%s</ul>', implode('', $item_html));
    }
    $html = pslm_html_page('Rejstřík', $html);
    file_put_contents("html/rejstrik.html", $html);
}

function pslm_render_index() {
    global $PSLM_PSALMS;
    /*
    $cal = Yaml::parseFile('db/calendar.yml');

    usort($cal, function($a, $b) {
        return ($a['year'] < $b['year']) ? -1 : 1;
    });

    $month_to_text = [
        1 => 'Leden',
        2 => 'Únor',
        3 => 'Březen',
        4 => 'Duben',
        5 => 'Květen',
        6 => 'Červen',
        7 => 'Červenec',
        8 => 'Srpen',
        9 => 'Září',
        10 => 'Říjen',
        11 => 'Listopad',
        12 => 'Prosinec',
    ];

    $c_year = intval(date('Y'));
    $c_month = intval(date('n'));
    $c_day = intval(date('j'));
    */

    $html = '';
    $html .= '<p><i>„Zpěvem se Boží slovo ukládá do srdce, aby se nám vynořilo v pravý čas, kdy budeme plni radosti, bolesti, starosti, úzkosti nebo vděčnosti. Tak se zpívané Boží slovo žalmů stane útěchou, posilou a světlem v našem putování do věčného domova.“</i> P. Josef Olejník</p>';
    $html .= '<p><a href="rejstrik.html">Rejstřík</a> | <a href="o-projektu.html">O projektu</a></p>';

    $html .= '<p><input type="search" class="search-field" placeholder="Hledat..." incremental /></p>';
    $html .= '<div class="search"></div>';
    $html .= '<div class="calendar"></div>';

    /*
    foreach ($cal as &$year) {
        usort($year['months'], function($a, $b) {
            return ($a['month'] < $b['month']) ? -1 : 1;
        });
        if ($year['year'] < $c_year) {
            continue;
        }
        $html .= sprintf('<h2>%s</h2>', $year['year']);

        foreach ($year['months'] as &$month) {
            usort($month['days'], function($a, $b) {
                return ($a['day'] < $b['day']) ? -1 : 1;
            });
            if ($year['year'] == $c_year && $month['month'] < $c_month) {
                continue;
            }
            $html .= sprintf('<h3>%s</h3><ul>', $month_to_text[$month['month']]);

            foreach ($month['days'] as $day) {
                if ($year['year'] == $c_year && $month['month'] == $c_month && $day['day'] < $c_day) {
                    continue;
                }
                $weekday = date('w', mktime(0, 0, 0, $month['month'], $day['day'], $year['year']));

                $day_html = [];
                if ($weekday == '0') {
                    $day_html[] = sprintf('<strong>%s. %s. – %s</strong>', $day['day'], $month['month'], $day['name']);
                } else {
                    $day_html[] = sprintf('<strong>%s. %s.</strong> – %s', $day['day'], $month['month'], $day['name']);
                }
                foreach ($day['psalms'] as $id) {
                    if (!isset($PSLM_PSALMS[$id])) {
                        $PSLM_PSALMS[$id] = pslm_render_psalm_html($id);
                    }
                    $day_html[] = sprintf(
                        '<a href="%s.html">%s</a>',
                        $id,
                        pslm_psalm_title($id, $PSLM_PSALMS[$id])
                    );
                }
                $html .= sprintf('<li>%s</li>', implode('<br />', $day_html));
            }
            $html .= '</ul>';
        }
    } */
    $head = sprintf('
    <script>
        let pslm_psalms = %s;
        let pslm_romcal_to_occasions = %s;
        let pslm_titles = %s;
    </script>
    <script src="js/romcal.js?ver=%s"></script>
    <script src="js/cs.js?ver=%s"></script>
    <script src="js/calendar.js?ver=%s"></script>
    <script src="js/diacritics.js?ver=%s"></script>
    <script src="js/search.js?ver=%s"></script>
    ',
        file_get_contents(dirname(__FILE__).'/db/psalms.json'),
        file_get_contents(dirname(__FILE__).'/db/romcal_to_occasions.json'),
        file_get_contents(dirname(__FILE__).'/db/psalm_titles.json'),
        time(),
        time(),
        time(),
        time(),
        time()
    );

    $html = pslm_html_page('', $html, $head);
    file_put_contents("html/index.html", $html);
}


function pslm_update_pregenerated() {
    global $PSLM_PSALMS, $PSLM_SAME_RESPONSUMS, $PSLM_SAME_VERSES;
    
    $updated_responsums = [];
    $done_psalms = [];

    foreach ($PSLM_PSALMS as $id => $psalm) {
        if (isset($PSLM_SAME_RESPONSUMS[$id])) {
            foreach ($PSLM_SAME_RESPONSUMS[$id] as $template_id) {
                if (!isset($updated_responsums[$template_id])) {
                    $key = implode(' ', $psalm['original_music'][0]);
                    $music = implode(' ', $psalm['original_music']['responsum']);

                    $pslm_file = dirname(__FILE__).'/pslm/pregenerated/'.$template_id.'.pslm';
                    $pslm = file_get_contents($pslm_file);
                    $pslm = preg_replace(
                        '#m:[^\n]*\n*%% part: responsum\n*m:[^\n]*#s',
                        "m: $key\n\n%% part: responsum\n\nm: $music",
                        $pslm
                    );
                    file_put_contents($pslm_file, $pslm);
                    $updated_responsums[$template_id] = true;
                }
            }
        }
        if (isset($PSLM_SAME_VERSES[$id])) {
            foreach ($PSLM_SAME_VERSES[$id] as $template_id) {
                if (isset($done_psalms[$template_id])) {
                    $done_psalms[$template_id][] = $id;
                } else {
                    $done_psalms[$template_id] = [$id];
                }
            }
        }
    }
    foreach ($done_psalms as $template_id => $done) {
        $done = implode(', ', $done);
        $pslm_file = dirname(__FILE__).'/pslm/pregenerated/'.$template_id.'.pslm';
        $pslm = file_get_contents($pslm_file);
        $pslm = preg_replace('#(%% page: [^\n]*).*?m: #s', "\\1\n\n% Tento žalm má stejné texty veršů jako publikovaný $done\n\nm: ", $pslm);
        file_put_contents($pslm_file, $pslm);
    }
}

function pslm_to_meta_description($psalm) {
    $desc = [];

    foreach ($psalm['original_text'] as $part => $text) {
        $text = trim(implode(' ', $text));
        $text = str_replace(' -- ', '', $text);
        $text = preg_replace('#\s+#', ' ', $text);

        if (preg_match('#^responsum#', $part)) {
            $desc[] = sprintf('R. %s', $text);
        } elseif (preg_match('#^verse_(\d+)#', $part, $m)) {
            $desc[] = sprintf('%s. %s', $m[1], $text);
        }
    }
    $desc = implode(' / ', $desc);
    return $desc;
}

function pslm_render_psalm_html($id) {
    global $PSLM_SOURCES;
    $psalm = pslm_engrave($id, 'svg');

    $opts = $psalm['opts'];

    if (!is_array($opts['occasion'])) {
        $occasions = [$opts['occasion']];
    } else {
        $occasions = $opts['occasion'];
    }
    $source = isset($opts['source']) && isset($PSLM_SOURCES[$opts['source']]) ? $PSLM_SOURCES[$opts['source']]['reference'] : '';

    $desc = pslm_to_meta_description($psalm);

    ob_start();
?><!DOCTYPE html>
<html lang="cs" prefix="og: http://ogp.me/ns#">
<head>
	<meta charset="UTF-8">
	<title><?= pslm_psalm_title($id, $psalm) ?> – Žaltář</title>
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <meta name="description" content="<?= $desc ?>" />
	<link rel="stylesheet" href="css/sizes.css?ver=<?= time() ?>" media="all" />
	<link rel="stylesheet" href="css/style.css?ver=<?= time() ?>" media="all" />
    <script>
        let pslm_svg_sizes = [<?= implode(', ', PSLM_SVG_SIZES) ?>];
    </script>
    <script src="js/zoom.js?ver=<?= time() ?>"></script>
</head>
<body class="zoom-0 psalm">
    <div class="main">
        <a onclick="history.back(); return false;" href="./" class="back-button"><svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="chevron-left" class="svg-inline--fa fa-chevron-left fa-w-10" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512"><path fill="currentColor" d="M34.52 239.03L228.87 44.69c9.37-9.37 24.57-9.37 33.94 0l22.67 22.67c9.36 9.36 9.37 24.52.04 33.9L131.49 256l154.02 154.75c9.34 9.38 9.32 24.54-.04 33.9l-22.67 22.67c-9.37 9.37-24.57 9.37-33.94 0L34.52 272.97c-9.37-9.37-9.37-24.57 0-33.94z"></path></svg></a>
        <h1><?= $id ?></h1>
        <?php foreach($occasions as $occasion): ?>
        <h3><?= $occasion ?></h3>
        <?php endforeach ?>

        <p><audio controls src="mp3/<?= $id ?>.mp3"></audio></p>
        <p><a href="#" id="zoom-in-button">Zvětšit</a> – <a href="#" id="zoom-out-button">Zmenšit</a> – <a href="#" id="zoom-reset-button">Resetovat</a></p>

        <div class="score">
        <?php foreach (PSLM_SVG_SIZES as $size): ?>
            <div class="size-<?= $size ?>"><?= file_get_contents("svg/$id-$size.svg") ?></div>
        <?php endforeach ?>
        </div>
        
        <p>Odpověď: <?= $opts['responsum_reference'] ?></p>
        <p>Verše: <?= $opts['verse_reference'] ?></p>
        
        <?php if (!empty($source)): ?>
        <p><?= $source ?></p>
        <p>Zdrojový kód: <a href="pslm/<?= $id ?>.pslm">PSLM</a>,
        <?php foreach (PSLM_SVG_SIZES as $size): ?>
        <a class="size-<?= $size ?>" href="ly/<?= $id ?>-<?= $size ?>.ly">LilyPond</a>
        <?php endforeach ?></p>
        <?php endif ?>
        
        <p><a href="mailto:jiri.hon@gmail.com?subject=Chyba v žalmu <?= $id ?>">Nahlásit chybu</a></p>
    </div>
</body>
</html>
<?php
    $html = ob_get_clean();
    file_put_contents("html/$id.html", $html);
    return $psalm;
}

function pslm_render_sizes_css() {
    $css = '';
    $n_sizes = count(PSLM_SVG_SIZES);
    $min_width = PSLM_MAX_WIDTH - ($n_sizes) * PSLM_PX_PER_STEP;
    
    for ($i = 0; $i < $n_sizes; ++$i) {
        if ($i > 0) {
            $css .= "@media (min-width: ${min_width}px) {\n";
        }
        $show_selectors = [];
        $hide_selectors = [];
        $zoom_enable_selectors = [];
        $zoom_disable_selectors = [];

        for ($z = -$n_sizes + 1; $z < $n_sizes; ++$z) {
            $k = $i - $z; // move size by zoom
            if ($k >= $n_sizes) {
                $k = $n_sizes - 1;
            } elseif ($k < 0) {
                $k = 0;
            }
            $show_selectors[] = sprintf('.zoom-%d .size-%d', $z, PSLM_SVG_SIZES[$k]);

            if ($k > 0) {
                $zoom_enable_selectors[] = sprintf('.zoom-%d #zoom-in-button', $z);
            } else {
                $zoom_disable_selectors[] = sprintf('.zoom-%d #zoom-in-button', $z);
            }
            if ($k < $n_sizes - 1) {
                $zoom_enable_selectors[] = sprintf('.zoom-%d #zoom-out-button', $z);
            } else {
                $zoom_disable_selectors[] = sprintf('.zoom-%d #zoom-out-button', $z);
            }

            // hide everything other than what is showed
            for ($m = 0; $m < $n_sizes; ++$m) {
                if ($m != $k) {
                    $hide_selectors[] = sprintf('.zoom-%d .size-%d', $z, PSLM_SVG_SIZES[$m]);
                }
            }
        }
        $css .= sprintf("%s {\n    display: inline-block;\n}\n", implode(",\n", $show_selectors));
        $css .= sprintf("%s {\n    display: none;\n}\n", implode(",\n", $hide_selectors));
        $css .= sprintf("%s {\n    color: blue;\n}\n", implode(",\n", $zoom_enable_selectors));
        $css .= sprintf("%s {\n    color: grey;\n}\n", implode(",\n", $zoom_disable_selectors));
        if ($i > 0) {
            $css .= "}\n";
        }
        $min_width += PSLM_PX_PER_STEP;
    }
    file_put_contents('html/css/sizes.css', $css);
}
