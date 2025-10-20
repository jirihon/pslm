<?php

use Symfony\Component\Yaml\Yaml;

require_once dirname(__FILE__).'/pslm.php';
require_once dirname(__FILE__).'/db/similarities.php';

//define('PSLM_SVG_SIZES', [15]);
//define('PSLM_SVG_SIZES', [7, 9, 11, 13, 15]);
define('PSLM_CACHE', true);

$PSLM_AUTHORS = Yaml::parseFile(dirname(__FILE__).'/db/authors.yml');
$PSLM_SOURCES = Yaml::parseFile(dirname(__FILE__).'/db/sources.yml');
$PSLM_PSALMS = [];

pslm_render_listing();
pslm_export_responsums();
pslm_render_sizes_css();
pslm_render_about();
pslm_update_pregenerated();
pslm_save_occasions();
pslm_occasions_to_romcal();
pslm_render_index();
pslm_render_sitemap();


function pslm_export_responsums() {
    global $PSLM_PSALMS;

    $fh = fopen('db/responsums.csv', 'w');
    fputcsv($fh, ['psalm', 'responsum']);

    foreach ($PSLM_PSALMS as $id => $psalm) {
        $responsum = implode(" ", $psalm['original_text']['responsum']);
        if (empty($responsum)) {
            echo "ERROR: No responsum for psalm $id!\n";
            exit;
        }
        fputcsv($fh, [$id, $responsum]);
    }
    fclose($fh);
}


// if (file_exists('upload.sh')) {
    //system('./upload.sh');
// }

function pslm_render_sitemap() {
    global $PSLM_PSALMS;

    $home_url = 'https://www.zaltar.cz';

    $pages = [
        'rejstrik.html',
        'o-projektu.html',
    ];
    foreach ($PSLM_PSALMS as $id => $psalm) {
        $pages[] = sprintf('%s.html', $id);
    }
    $urls = [];
    foreach ($pages as $page) {
        $urls[] = sprintf('<url><loc>%s/%s</loc></url>', $home_url, $page);
    }
    $sitemap = sprintf(
        '<?xml version="1.0" encoding="UTF-8"?><urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">%s</urlset>',
        implode($urls)
    );
    file_put_contents('html/sitemap.xml', $sitemap);
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
        "#^Výročí posvěcení kostela( \(i v době velikonoční\))?$#u" => 'dedication_of_consecrated_churches_on_october_25',
        
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
                    echo "ERROR: $id does not exist in romcal, continue to match at least by date\n";
                    continue;
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
            if (isset($to_occasions[$key])) {
                $to_occasions[$key][] = $occasion;
            } else {
                $to_occasions[$key] = [$occasion];
            }
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
    $psalm_occasions = [];
    foreach ($PSLM_PSALMS as $id => $psalm) {
        if (!isset($psalm['opts']['occasion'])) {
            echo "ERROR: $id does not have any occasion\n";
        }
        $occasions = $psalm['opts']['occasion'];
        if (!is_array($occasions)) {
            $occasions = [$occasions];
        }
        $psalm_occasions[$id] = [];
        foreach ($occasions as $occasion) {
            $occasion = trim($occasion);
            if (isset($occasion_psalm[$occasion])) {
                $occasion_psalm[$occasion][] = $id;
            } else {
                $occasion_psalm[$occasion] = [$id];
            }
            $psalm_occasions[$id][] = $occasion;
        }
    }
    file_put_contents(dirname(__FILE__).'/db/occasions.php', '<?php $occasion_psalm = '.var_export($occasion_psalm, true).';');

    $responsum_psalm = [];
    $psalm_titles = [];
    $number_psalm = [];
    foreach ($PSLM_PSALMS as $id => $psalm) {
        $responsum = implode(' ', $psalm['original_text']['responsum']);
        if (isset($responsum_psalm[$responsum])) {
            $responsum_psalm[$responsum][] = $id;
        } else {
            $responsum_psalm[$responsum] = [$id];
        }
        $number = pslm_psalm_number($psalm);
        if (isset($number_psalm[$number])) {
            $number_psalm[$number][] = $id;
        } else {
            $number_psalm[$number] = [$id];
        }
        $psalm_titles[$id] = pslm_psalm_title($id, $psalm);
    }
    file_put_contents(dirname(__FILE__).'/db/psalms.json', json_encode(array_merge($id_psalm, $occasion_psalm, $number_psalm, $responsum_psalm), JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
    file_put_contents(dirname(__FILE__).'/db/psalm_titles.json', json_encode($psalm_titles, JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
    file_put_contents(dirname(__FILE__).'/db/psalm_occasions.json', json_encode($psalm_occasions, JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT));
}

function pslm_footer() {
?>
<div class="footer">© <a target="_blank" rel="noopener noreferrer" href="https://www.ado.cz/">Arcibiskupství olomoucké</a></div>
<?php
}


function pslm_html_page($title, $body, $head = '') {
    ob_start();
?><!DOCTYPE html>
<html lang="cs" prefix="og: http://ogp.me/ns#" class="os-default">
<head>
    <?php pslm_render_head($title, 'Noty k Olejníkovým žalmům pro každý den liturgického kalendáře.') ?>
    <?= $head ?>
</head>
<body class="page">
    <div class="main">
        <?php if (!empty($title)): ?>
            <a onclick="history.back(); return false;" href="./" class="back-button"><svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="chevron-left" class="svg-inline--fa fa-chevron-left fa-w-10" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512"><path fill="currentColor" d="M34.52 239.03L228.87 44.69c9.37-9.37 24.57-9.37 33.94 0l22.67 22.67c9.36 9.36 9.37 24.52.04 33.9L131.49 256l154.02 154.75c9.34 9.38 9.32 24.54-.04 33.9l-22.67 22.67c-9.37 9.37-24.57 9.37-33.94 0L34.52 272.97c-9.37-9.37-9.37-24.57 0-33.94z"></path></svg></a>
            <h1><?= $title ?></h1>
        <?php else: ?>
            <h1>Olejníkův žaltář</h1>
        <?php endif ?>
        <?= $body ?>
        <?php pslm_footer() ?>
    </div>
</body>
</html>
<?php
     return ob_get_clean();
}

function pslm_psalm_number($psalm) {
    $psalm_number = preg_replace('#^Ž\s+([^\s]+).*$#', 'Žalm $1', $psalm['opts']['verse_reference']);
    return $psalm_number;
}

function pslm_psalm_title($id, $psalm) {
    return sprintf(
        '%s – %s – %s',
        $id,
        pslm_original_text_to_plain_text($psalm['original_text']['responsum']),
        pslm_psalm_number($psalm),
    );
}

function pslm_render_listing() {
    global $PSLM_SOURCES, $PSLM_PSALMS;
    $html = '';

    file_put_contents("all.sh", "");

    foreach ($PSLM_SOURCES as $source) {
        if (!isset($source['ids'])) {
            continue;
        }
        $html .= sprintf('<p>%s</p>', $source['reference']);
        $item_html = [];
        $done = [];

        foreach ($source['ids'] as $id) {
            echo "Processing $id\n";
            if (file_exists(sprintf('%s/pslm/%s.pslm', dirname(__FILE__), $id))) {
                file_put_contents("all.sh", "bash sh/$id.sh\n", FILE_APPEND);
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
        //$html .= sprintf('<p style="margin-left: 1em">Přepsáno %d z %d žalmů (%s %%).</p>', $done, $total, round($done/$total * 100));
        $html .= sprintf('<ul>%s</ul>', implode('', $item_html));
    }
    $html = pslm_html_page('Rejstřík', $html);
    file_put_contents("html/rejstrik.html", $html);
}

function pslm_render_about() {
    $html = '
    <p>Tento mobilní žaltář je věrnou elektronickou kopií nejnovějšího knižního vydání Olejníkových žalmů. Obsahuje všechny žalmy z publikací <em>Žaltář I: Responzoriální žalmy pro neděle, svátky, obřady a všední dny adventní, vánoční, postní a velikonoční</em> a <em>Žaltář II: Responzoriální žalmy pro všední dny liturgického mezidobí, cyklus 1 a 2</em> vydaných Arcibiskupstvím olomouckým v roce 2021.</p>

    <p>Cílem není nahrazení papírové formy žaltářů, avšak vhodné doplnění tam, kde papírová forma není dostupná.</p>

    <p>Knižní vydání žaltářů je možné si objednat na e-mailu <a href="mailto:svecova.marta@ado.cz">svecova.marta@ado.cz</a>. Více informací k žaltářům lze nalézt na stránce <a target="_blank" rel="noopener noreferrer" href="https://josefolejnik.cz/publikace/vydane/zaltar-2dilny/">josefolejnik.cz</a>.</p>

    <p>Mobilní žaltář vznikl pod záštitou webu <a target="_blank" rel="noopener noreferrer" href="https://www.liturgie.cz/">Liturgie.cz</a> a se svolením <a target="_blank" rel="noopener noreferrer" href="https://www.ado.cz/">Arcibiskupství olomouckého</a>, které je držitelem autorských práv k dílu P. Josefa Olejníka.</p>
    
    <p>Zdrojový kód projektu je k dispozici na platformě <a target="_blank" rel="noopener noreferrer" href="https://github.com/jirihon/pslm/">GitHub</a>. Mobilní žaltář je dokončený a notový zápis prošel korekturou. Naleznete-li chybu, dejte nám prosím vědět na adresu <a href="mailto:jiri.hon@gmail.com">jiri.hon@gmail.com</a>, rádi ji opravíme.</p>

    <p>Tým Žaltář.cz:</p>
    <ul>
        <li><a href="mailto:jiri.hon@gmail.com">Jiří Hon</a> – programování, sazba (farnost <a target="_blank" rel="noopener noreferrer" href="https://www.farnostjaktar.cz/">Opava–Jaktař</a>)</li>
        <li>František Sovadina – sazba (farnost <a target="_blank" rel="noopener noreferrer" href="https://www.farnostopava.cz/">Opava</a>)</li>
        <li>Antonín Salzmann – sazba (farnost <a target="_blank" rel="noopener noreferrer" href="https://www.farnostjaktar.cz/">Opava–Jaktař</a>)</li>
        <li>Vladimír Pavlík – korektury (farnost <a target="_blank" rel="noopener noreferrer" href="http://www.farnostvelkapolom.cz/">Velká Polom</a>)</li>
        <li>P. Ondřej Talaš – korektury, duchovní podpora (vedoucí <a target="_blank" rel="noopener noreferrer" href="https://www.ado.cz/cemu-se-venujeme/pastorace/centrum-pro-liturgickou-hudbu/">Centra pro liturgickou hudbu Arcibiskupství olomouckého</a>)</li>
    </ul>
    ';
    $html = pslm_html_page('O projektu', $html, '');
    file_put_contents("html/o-projektu.html", $html);
}

function pslm_render_index() {
    $html = '
    <h2 class="subtitle">Noty k Olejníkovým žalmům pro každý den liturgického kalendáře.</h2>
    <p><i>„Zpěvem se Boží slovo ukládá do srdce, aby se nám vynořilo v pravý čas, kdy budeme plni radosti, bolesti, starosti, úzkosti nebo vděčnosti. Tak se zpívané Boží slovo žalmů stane útěchou, posilou a světlem v našem putování do věčného domova.“</i> P. Josef Olejník</p>
    <p><a href="rejstrik.html">Rejstřík</a> | <a href="o-projektu.html">O projektu</a></p>

    <p><input type="search" class="search-field" placeholder="Hledat..." incremental /></p>
    <div class="search"></div>
    <p>Režim: <a href="#" id="light-theme">Světlý</a> – <a href="#" id="dark-theme">Tmavý</a> – <a href="#" id="os-default-theme">Výchozí</a></p>
    <div class="calendar"><div class="today"></div><div class="sunday"></div><div class="week"></div></div>
    ';

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
        filemtime('html/js/romcal.js'),
        filemtime('html/js/cs.js'),
        filemtime('html/js/calendar.js'),
        filemtime('html/js/diacritics.js'),
        filemtime('html/js/search.js')
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
                    if (!isset($psalm['original_music']['responsum'])) {
                        echo "ERROR: missing responsum for $id\n";
                    }

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

function pslm_original_text_to_plain_text($text) {
    $text = trim(implode(' ', $text));
    $text = str_replace(' -- ', '-', $text);
    $text = preg_replace('#\s+#', ' ', $text);
    return $text;
}

function pslm_to_plain_text($psalm) {
    $desc = [];

    foreach ($psalm['original_text'] as $part => $text) {
        $text = pslm_original_text_to_plain_text($text);

        if (preg_match('#^responsum#', $part)) {
            $desc[] = sprintf('R. %s', $text);
        } elseif (preg_match('#^verse_(\d+)#', $part, $m)) {
            $desc[] = sprintf('%s. %s', $m[1], $text);
        }
    }
    $desc = implode(' / ', $desc);
    return $desc;
}

function pslm_to_formatted_text($psalm) {
    $desc = [];

    foreach ($psalm['original_text'] as $part => $text) {
        $text = pslm_original_text_to_plain_text($text);

        if (preg_match('#^responsum#', $part)) {
            $desc[] = sprintf('<p class="responsum-text"><strong>R.</strong> %s</p>', $text);
        } elseif (preg_match('#^verse_(\d+)#', $part, $m)) {
            $desc[] = sprintf('<p class="verse-text"><strong>%s.</strong> %s</p>', $m[1], $text);
        }
    }
    $desc = implode('', $desc);
    return $desc;
}

function svg_to_data_uri($svg) {
    $svg_enc = rawurlencode($svg);
    
    // browsers tolerate these characters, and they're frequent
    $replacements = [
        '%22' => "'",
        '%20' => ' ',
        '%3D' => '=',
        '%3A' => ':',
        '%2F' => '/',
    ];
    $svg_enc = str_replace(array_keys($replacements), array_values($replacements), $svg_enc);

    return "data:image/svg+xml,$svg_enc";
}

function pslm_render_head($title, $desc) {
    ?>
    <meta charset="UTF-8">
    <?php if (!empty($title)): ?>
        <title><?= $title ?> – Olejníkův žaltář</title>
    <?php else: ?>
        <title>Olejníkův žaltář</title>
    <?php endif ?>
    <meta name="viewport" content="width=device-width, initial-scale=1">
	<link rel="stylesheet" href="css/sizes.css?ver=<?= filemtime('html/css/sizes.css') ?>" media="all">
	<link rel="stylesheet" href="css/style.css?ver=<?= filemtime('html/css/style.css') ?>" media="all">
    <link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png?ver=<?= filemtime('html/apple-touch-icon.png') ?>">
    <link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png?ver=<?= filemtime('html/favicon-32x32.png') ?>">
    <link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png?ver=<?= filemtime('html/favicon-16x16.png') ?>">
    <link rel="manifest" href="/site.webmanifest?ver=<?= filemtime('html/site.webmanifest') ?>">
    <link rel="mask-icon" href="/safari-pinned-tab.svg?ver=<?= filemtime('html/safari-pinned-tab.svg') ?>" color="#be1622">
    <meta name="msapplication-TileColor" content="#be1622">
    <!--<meta name="theme-color" content="#ffffff">-->
    <meta name="description" content="<?= $desc ?>">
    <script src="js/theme.js?ver=<?= filemtime('html/js/theme.js') ?>"></script>
    <?php
}

function pslm_render_psalm_html($id) {
    global $PSLM_SOURCES;
    $psalm = pslm_engrave($id, 'html/svg');

    $opts = $psalm['opts'];

    if (!is_array($opts['occasion'])) {
        $occasions = [$opts['occasion']];
    } else {
        $occasions = $opts['occasion'];
    }
    $source = isset($opts['source']) && isset($PSLM_SOURCES[$opts['source']]) ? $PSLM_SOURCES[$opts['source']]['reference'] : '';

    $title = pslm_psalm_title($id, $psalm);
    $plain_text = pslm_to_plain_text($psalm);
    $desc = "Noty k žalmu $title. Text žalmu: $plain_text";
    
    $formatted_text = pslm_to_formatted_text($psalm);

    ob_start();
?><!DOCTYPE html>
<html lang="cs" prefix="og: http://ogp.me/ns#" class="os-default">
<head>
    <?php pslm_render_head($title, $desc) ?>
    <script>let pslm_svg_sizes = [<?= implode(', ', PSLM_SVG_SIZES) ?>];</script>
    <script src="js/zoom.js?ver=<?= filemtime('html/js/zoom.js') ?>"></script>
    <style><?= preg_replace('#\s+#', ' ', file_get_contents("html/css/$id.css")) ?></style>
</head>
<body class="zoom-0 psalm">
    <div class="main">
        <a onclick="history.back(); return false;" href="./" class="back-button"><svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="chevron-left" class="svg-inline--fa fa-chevron-left fa-w-10" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512"><path fill="currentColor" d="M34.52 239.03L228.87 44.69c9.37-9.37 24.57-9.37 33.94 0l22.67 22.67c9.36 9.36 9.37 24.52.04 33.9L131.49 256l154.02 154.75c9.34 9.38 9.32 24.54-.04 33.9l-22.67 22.67c-9.37 9.37-24.57 9.37-33.94 0L34.52 272.97c-9.37-9.37-9.37-24.57 0-33.94z"></path></svg></a>
        <h1><?= $id ?></h1>
        <div class="occasions">
        <?php foreach($occasions as $occasion): ?>
            <h3><?= $occasion ?></h3>
        <?php endforeach ?>
            <h3><?= pslm_psalm_number($psalm) ?></h3>
        </div>
        <p><a href="<?= "pdf/$id.pdf" ?>">Stáhnout varhanní noty</a></p>
        <p class="audio"><span><a href="https://cs.wikipedia.org/wiki/Musical_Instrument_Digital_Interface">MIDI</a> výstup:</span><audio controls src="mp3/<?= $id ?>.mp3?ver=<?= filemtime("html/mp3/$id.mp3") ?>"></audio></p>
        <p><a href="#" id="zoom-in-button">Zvětšit</a> – <a href="#" id="zoom-out-button">Zmenšit</a> – <a href="#" id="zoom-reset-button">Resetovat</a></p>

        <img class="score" alt="Noty k žalmu <?= pslm_psalm_title($id, $psalm) ?>" src="<?= "svg/$id.svg" ?>?ver=<?= filemtime("html/svg/$id.svg") ?>" />

        <?php if (false): ?>
        <div class="score">
        <?php foreach (PSLM_SVG_SIZES as $size): ?>
            <img class="size-<?= $size ?>" alt="Noty k žalmu <?= pslm_psalm_title($id, $psalm) ?>" src="<?= svg_to_data_uri(file_get_contents("html/svg/$id-$size.svg")) ?>" />
        <?php endforeach ?>
        </div>
        <?php endif ?>

        <?= $formatted_text ?>
        
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
        <?php pslm_footer() ?>
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
            $css .= "@media (min-width: {$min_width}px) {\n";
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
        $css .= sprintf("%s {\n    color: var(--link-color);\n}\n", implode(",\n", $zoom_enable_selectors));
        $css .= sprintf("%s {\n    color: grey;\n}\n", implode(",\n", $zoom_disable_selectors));
        if ($i > 0) {
            $css .= "}\n";
        }
        $min_width += PSLM_PX_PER_STEP;
    }
    $css = preg_replace('#\s+#', ' ', $css);
    file_put_contents('html/css/sizes.css', $css);
}
