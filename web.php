<?php

use Symfony\Component\Yaml\Yaml;

require_once dirname(__FILE__).'/pslm.php';

define('PSLM_SVG_SIZES', [7, 8, 9, 10, 11, 12, 13, 14, 15]);
//define('PSLM_SVG_SIZES', [15]);
//define('PSLM_SVG_SIZES', [7, 9, 11, 13, 15]);
define('PSLM_CACHE', true);

define('PSLM_PX_PER_STEP', 50);
define('PSLM_MAX_WIDTH', 732);

$PSLM_AUTHORS = null;
$PSLM_SOURCES = null;

pslm_render_sizes_css();
pslm_render_index();

if (file_exists('upload.sh')) {
    system('./upload.sh');
}

function pslm_lyrics_to_text($lyrics) {
    $text = implode(' ', $lyrics);
    $text = preg_replace('#\s+--\s+#', '', $text);
    $text = preg_replace('#\s*\\\\set stanza = ("[^"]*"|\\\\responsum)\s*#', '', $text);
    $text = str_replace('"', '', $text);
    return $text;
}

function pslm_psalm_title($id, $psalm) {
    return sprintf(
        '%s – %s – %s',
        $id,
        pslm_lyrics_to_text($psalm['text']['responsum']),
        $psalm['opts']['verse_reference']
    );
}

function pslm_render_index() {
    $cal = Yaml::parseFile('db/calendar.yml');

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
    $psalms = [];

    $html = '';
    foreach ($cal as $year) {
        $html .= sprintf('<h2>%s</h2>', $year['year']);
        foreach ($year['months'] as $month) {
            $html .= sprintf('<h3>%s</h3><ul>', $month_to_text[$month['month']]);
            foreach ($month['days'] as $day) {
                $day_html = [];
                $day_html[] = sprintf('<strong>%s. %s.</strong> – %s', $day['day'], $month['month'], $day['name']);
                foreach ($day['psalms'] as $id) {
                    if (!isset($psalms[$id])) {
                        $psalms[$id] = pslm_render_psalm_html($id);
                    }
                    $day_html[] = sprintf(
                        '<a href="%s.html">%s</a>',
                        $id,
                        pslm_psalm_title($id, $psalms[$id])
                    );
                }
                $html .= sprintf('<li>%s</li>', implode('<br />', $day_html));
            }
            $html .= '</ul>';
        }
    }
    ob_start();
?><!DOCTYPE html>
<html lang="cs" prefix="og: http://ogp.me/ns#">
<head>
	<meta charset="UTF-8">
	<title>Žaltář</title>
    <meta name="viewport" content="width=device-width, initial-scale=1" />
	<link rel="stylesheet" href="css/style.css?ver=<?= time() ?>" media="all" />
</head>
<body>
    <div class="main">
        <h1>Žaltář</h1>
        <p><i>„Zpěvem se Boží slovo ukládá do srdce, aby se nám vynořilo v pravý
čas, kdy budeme plni radosti, bolesti, starosti, úzkosti nebo vděčnosti. Tak se zpívané Boží slovo žalmů stane útěchou, posilou a
světlem v našem putování do věčného domova.“</i> P. Josef Olejník</p>
        <p><a href="o-projektu.html">O projektu</a> | <a href="https://github.com/jirihon/pslm">GitHub</a></p>
        <?= $html ?>
    </div>
</body>
</html>
<?php
    $html = ob_get_clean();
    file_put_contents("html/index.html", $html);
}

function pslm_render_psalm_html($id) {
    global $PSLM_AUTHORS, $PSLM_SOURCES;
    if ($PSLM_AUTHORS === null) {
        $PSLM_AUTHORS = Yaml::parseFile(dirname(__FILE__).'/db/authors.yml');
    }
    if ($PSLM_SOURCES === null) {
        $PSLM_SOURCES = Yaml::parseFile(dirname(__FILE__).'/db/sources.yml');
    }
    $psalm = pslm_engrave($id, 'svg');
    $opts = $psalm['opts'];

    if (!is_array($opts['occasion'])) {
        $occasions = [$opts['occasion']];
    } else {
        $occasions = $opts['occasion'];
    }
    $source = isset($opts['source']) && isset($PSLM_SOURCES[$opts['source']]) ? $PSLM_SOURCES[$opts['source']] : '';

    ob_start();
?><!DOCTYPE html>
<html lang="cs" prefix="og: http://ogp.me/ns#">
<head>
	<meta charset="UTF-8">
	<title><?= $id ?></title>
    <meta name="viewport" content="width=device-width, initial-scale=1" />
	<link rel="stylesheet" href="css/sizes.css?ver=<?= time() ?>" media="all" />
	<link rel="stylesheet" href="css/style.css?ver=<?= time() ?>" media="all" />
    <script>
        let pslm_svg_sizes = [<?= implode(', ', PSLM_SVG_SIZES) ?>];
    </script>
    <script src="js/zoom.js?ver=<?= time() ?>"></script>
</head>
<body class="zoom-0 psalm">
    <div class="main">
        <a href="./" class="back-button"><svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="chevron-left" class="svg-inline--fa fa-chevron-left fa-w-10" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512"><path fill="currentColor" d="M34.52 239.03L228.87 44.69c9.37-9.37 24.57-9.37 33.94 0l22.67 22.67c9.36 9.36 9.37 24.52.04 33.9L131.49 256l154.02 154.75c9.34 9.38 9.32 24.54-.04 33.9l-22.67 22.67c-9.37 9.37-24.57 9.37-33.94 0L34.52 272.97c-9.37-9.37-9.37-24.57 0-33.94z"></path></svg></a>
        <h1><?= $id ?></h1>
        <?php foreach($occasions as $occasion): ?>
        <h3><?= $occasion ?></h3>
        <?php endforeach ?>
        
        <p>Odpověď: <?= $opts['responsum_reference'] ?></p>
        <p>Verše: <?= $opts['verse_reference'] ?></p>

        <p><audio controls src="mp3/<?= $id ?>.mp3"></audio></p>
        <p><a href="#" id="zoom-in-button">Zvětšit</a> – <a href="#" id="zoom-out-button">Zmenšit</a> – <a href="#" id="zoom-reset-button">Resetovat</a></p>

        <div class="score">
        <?php foreach (PSLM_SVG_SIZES as $size): ?>
            <div class="size-<?= $size ?>"><?= file_get_contents("svg/$id-$size.svg") ?></div>
        <?php endforeach ?>
        </div>
        
        <?php if (!empty($source)): ?>
        <p><?= $source ?></p>
        <p>Zdrojový kód: <a href="pslm/<?= $id ?>.pslm">PSLM</a>,
        <?php foreach (PSLM_SVG_SIZES as $size): ?>
        <a class="size-<?= $size ?>" href="ly/<?= $id ?>-<?= $size ?>.ly">LilyPond</a>
        <?php endforeach ?></p>
        <?php endif ?>
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
