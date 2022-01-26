<?php
/**
 * Parse text lines from PDFs of Nedělení žaltář and Všední žaltář based on XML output of pdfminer.six
 * 
 * The XML was generated using the following command:
 * 
 * $ pdf2txt.py --output_type xml Zaltar.pdf > zaltar.xml
 */
use Symfony\Component\Yaml\Yaml;

require_once 'vendor/autoload.php';

define('PSLM_LINE_MARGIN', 5);
define('PSLM_ASTERISK_LINE_MARGIN', 10);
define('PSLM_WORD_MARGIN', 2);

/* // Really want to override?
$psalm_lines = pslm_pdf_to_psalm_lines('db/nedelni_zaltar.xml.gz', 'r');
pslm_save_psalm_lines($psalm_lines, 'db/nedelni_zaltar.yml');

$psalm_lines = pslm_pdf_to_psalm_lines('db/vsedni_zaltar.xml.gz', 'r');
pslm_save_psalm_lines($psalm_lines, 'db/vsedni_zaltar.yml');
*/

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
        $psalm_line = preg_replace('#[^ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮĚÓa-zA-Z0-9,\.\-\?\!„“":;\(\)\[\]\*\+/ ]+#u', '', $psalm_line);
        $psalm_line = preg_replace('#\s+#u', ' ', $psalm_line);
        $psalm_line = trim($psalm_line);
        $psalm_line = preg_replace('#^[ AQRSE]*(\d+)[ AQRSE.]* #u', '\1. ', $psalm_line);

        $is_garbage = preg_match('#^[ AQRSE\.,\(\)]*$#u', $psalm_line);
        if (!$is_garbage) {
            $psalm_lines[] = $psalm_line;
        }
    }
    return $psalm_lines;
}

function pslm_pdf_to_psalm_lines($filename) {
    $fp = gzopen($filename, 'r');
    $page = 0;
    $text_lines = [];
    $psalm_lines = [];
    while (($xml_line = fgets($fp, 4096)) !== false) {
        preg_match('#<page id="(\d+)"#', $xml_line, $page_m);
        if ($page_m) {
            if (!empty($text_lines)) {
                $psalm_lines[$page] = pslm_text_lines_to_psalm_lines($text_lines);
            }
            $page = $page_m[1];
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
    $psalm_lines[$page] = pslm_text_lines_to_psalm_lines($text_lines);
    fclose($fp);
    return $psalm_lines;
}
