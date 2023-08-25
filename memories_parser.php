<?php
/**
 * Parse text lines from PDF based on XML output of pdfminer.six
 * 
 * The XML was generated using the following command:
 * 
 * $ pdf2txt.py --output_type xml Svatky_a_pamatky.pdf > svatky_a_pamatky.xml
 */
use Symfony\Component\Yaml\Yaml;

require_once 'vendor/autoload.php';

define('PSLM_LINE_MARGIN', 2);
//define('PSLM_ASTERISK_LINE_MARGIN', 10);
define('PSLM_WORD_MARGIN', 2);

// Really want to override? This is supposed to be one-time operation!
pslm_save_memorials_to_psalms('db/svatky_a_pamatky.xml.gz', 'r');

function pslm_save_psalm_lines($psalm_lines, $filename) {
    file_put_contents($filename, Yaml::dump($psalm_lines));
}

function pslm_get_line_key($text_lines, $top, $margin = PSLM_LINE_MARGIN) {
    for ($d = 0; $d <= $margin; ++$d) {
        $key_1 = $top + $d;
        $key_2 = $top - $d;
        if (isset($text_lines[$key_1])) {
            return $key_1;
        } elseif (isset($text_lines[$key_2])) {
            return $key_2;
        }
    }
    return false;
}

function pslm_text_lines_to_psalm_lines($text_lines) {
    $psalm_lines = [];

    // reassign asterisks to lines with extended line margin
    /*
    foreach ($text_lines as $top => $texts) {
        foreach ($texts as $left => $text) {
            if ($text[1] === '*') {
                unset($text_lines[$top][$left]);
                if (empty($text_lines[$top])) {
                    unset($text_lines[$top]);
                }
                $key = pslm_get_line_key($text_lines, $top, PSLM_ASTERISK_LINE_MARGIN);
                if ($key === false) {
                    $key = $top;
                    $text_lines[$key] = [];
                }
                $text_lines[$key][$left] = $text;
            }
        }
    }
    */
    krsort($text_lines);

    foreach ($text_lines as $top => $texts) {
        $psalm_line = '';

        ksort($texts);
        $left = array_keys($texts);
        $text = array_values($texts);
        
        // build up an array of right coordinates to compare with left coordinate of next text
        $right = [$text[0][0]]; 
        $psalm_line .= $text[0][1];

        for ($i = 1; $i < count($texts); ++$i) {
            $right[$i] = $text[$i][0];
            if ($left[$i] - $right[$i-1] > PSLM_WORD_MARGIN) {
                $psalm_line .= " ";
            }
            $psalm_line .= $text[$i][1];
        }
        //$psalm_line = preg_replace('#[^ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮĚÓa-zA-Z0-9,\.\-\?\!„“":;\(\)\[\]\*\+/ ]+#u', '', $psalm_line);
        $psalm_line = preg_replace('#\s+#u', ' ', $psalm_line);
        $psalm_line = trim($psalm_line);
        //$psalm_line = preg_replace('#^[ AQRSE]*(\d+)[ AQRSE.]* #u', '\1. ', $psalm_line);

        $is_garbage = preg_match('#^[ AQRSE\.,\(\)]*$#u', $psalm_line);
        if (!$is_garbage) {
            $psalm_lines[] = $psalm_line;
        }
    }
    return $psalm_lines;
}

function pslm_save_memorials_to_psalms($filename) {
    $fp = gzopen($filename, 'r');
    //$page = 0;
    $text_lines = [];
    $psalm_lines = [];
    while (($xml_line = fgets($fp, 4096)) !== false) {
        preg_match('#<page id="(\d+)"#', $xml_line, $page_m);
        if ($page_m) {
            if (!empty($text_lines)) {
                $psalm_lines = array_merge($psalm_lines, pslm_text_lines_to_psalm_lines($text_lines));
            }
            //$page = $page_m[1];
            $text_lines = [];
        }
        preg_match('#<text [^>]*bbox="([\d\.]+),([\d\.]+),([\d\.]+),([\d\.]+)"[^>]*>([^<]+)</text>#', $xml_line, $text_m);
        if ($text_m) {
            $left = intval($text_m[1]);
            $top = intval($text_m[2]);
            $right = intval($text_m[3]);
            $text = $text_m[5];
            $key = pslm_get_line_key($text_lines, $top);
            if ($key === false) {
                $key = $top;
                $text_lines[$key] = [];
            }
            $text_lines[$key][$left] = [$right, $text];
        }
    }
    fclose($fp);
    $psalm_lines = array_merge($psalm_lines, pslm_text_lines_to_psalm_lines($text_lines));

    $regex = '#^(\d+. \d+\.) (.*?) (\(?(Památka|Svátek|Slavnost)\)? )?(OL\d+)#';
    $memorials = [];

    $fp = fopen('db/svatky_a_pamatky.csv', 'w');
    fputcsv($fp, ['date', 'occasion', 'psalm']);

    for ($i = 0; $i < count($psalm_lines) - 1; $i++) {
        $line = $psalm_lines[$i];
        preg_match($regex, $line, $m);
        if (!$m) {
            preg_match($regex, sprintf('%s %s', $line, $psalm_lines[$i+1]), $m);
            if ($m) {
                $i++; // skip the next line
            }
        }
        if ($m) {
            $date = $m[1];
            $occasion = $m[2];
            $id = $m[5];
            $row = [$date, $occasion, $id];
            $memorials[] = $row;
            fputcsv($fp, $row);
        }
    }
    fclose($fp);

    $number_to_month = [
        '1' => 'ledna',
        '2' => 'února',
        '3' => 'března',
        '4' => 'dubna',
        '5' => 'května',
        '6' => 'června',
        '7' => 'července',
        '8' => 'srpna',
        '9' => 'září',
        '10' => 'října',
        '11' => 'listopadu',
        '12' => 'prosince',
    ];

    $solved_psalm_dates = [];
    $psalm_occasions = json_decode(file_get_contents('db/psalm_occasions.json'));

    foreach ($psalm_occasions as $id => $occasions) {
        $solved_psalm_dates[$id] = [];
        foreach ($occasions as $occasion) {
            preg_match('#\((\d+\. [^ ,\)]+)#', $occasion, $m);
            if ($m) {
                $date = $m[1];
                if (!isset($solved_psalm_dates[$id][$date])) {
                    $solved_psalm_dates[$id][$date] = true;
                } else {
                    echo "WARNING: More than one occasion for $date in psalm $id\n";
                }
            }
        }
    }

    $psalm_occasions_to_save = [];

    foreach ($memorials as $memorial) {
        $date = $memorial[0];
        $occasion = $memorial[1];
        $id = $memorial[2];
        preg_match('#^(\d+)\. (\d+)\.$#', $date, $m);
        if (!$m) {
            echo "ERROR: No date match\n";
        }
        $long_date = sprintf('%s. %s', $m[1], $number_to_month[$m[2]]);
        
        if (isset($solved_psalm_dates[$id][$long_date])) {
            echo "Skipping $date $occasion $id\n";
            continue;
        }
        if (!isset($psalm_occasions_to_save[$id])) {
            $psalm_occasions_to_save[$id] = [];
        }
        $psalm_occasions_to_save[$id][] = sprintf('%%%% occasion: \'%s (%s)\'', $occasion, $long_date);
    }

    foreach ($psalm_occasions_to_save as $id => $occasions) {
        $psalm_path = sprintf('pslm/%s.pslm', $id);
        $psalm = file_get_contents($psalm_path);
        $new_psalm = str_replace("%% responsum_reference", sprintf("%s\n%%%% responsum_reference", implode("\n", $occasions)), $psalm);
        file_put_contents($psalm_path, $new_psalm);
    }
    return $psalm_lines;
}
