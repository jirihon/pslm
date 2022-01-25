<?php
/**
 * Find text lines in PDFs of Nedělení žaltář and Všední žaltář based on XML output of pdfminer.six
 */

$fp = fopen(dirname(__FILE__).'/db/nedelni_zaltar.xml', 'r');


function pslm_get_line_key($lines, $top) {
    for ($d = 0; $d <= 5; ++$d) {
        $key_1 = $top + $d;
        $key_2 = $top - $d;
        if (isset($lines[$key_1])) {
            return $key_1;
        } elseif (isset($lines[$key_2])) {
            return $key_2;
        }
    }
    return false;
}

function pslm_print_page($page, $lines) {
    echo "Page: $page\n";
    krsort($lines);
    foreach ($lines as $words) {
        $line = '';

        if (!empty($words)) {
            ksort($words);
            $left = array_keys($words);
            $text = array_values($words);
            
            // build up an array of right coordinates to compare with left coordinate of next text
            $right = [$text[0][0]]; 
            $line .= $text[0][1];

            for ($i = 1; $i < count($words); ++$i) {
                $right[$i] = $text[$i][0];
                if ($left[$i] - $right[$i-1] > 2) {
                    $line .= " ";
                }
                $line .= $text[$i][1];
            }
        }
        //echo $top . ': '. implode(' ', $words) . "\n";
        $line = preg_replace('#[^ěščřžýáíéóúůďťňĎŇŤŠČŘŽÝÁÍÉÚŮĚÓa-zA-Z0-9,\.\-\?\!„“":;\(\)\[\]\*\+/ ]+#u', '', $line);
        $line = preg_replace('#\s*-\s*#u', '', $line);
        $line = preg_replace('#\s+#u', ' ', $line);

        echo "$line\n";
    }
}

$page = 0;
$lines = [];

while (($line = fgets($fp, 4096)) !== false) {
    ++$n;

    //preg_match('#<page number="(\d+)"#', $line, $page_m);
    preg_match('#<page id="(\d+)"#', $line, $page_m);
    if ($page_m) {
        $page = $page_m[1];
        pslm_print_page($page, $lines);
        $lines = [];
    }

    //preg_match('#<text top="(\d+)" left="(\d+)" width="(\d+)" height="(\d+)" font="(\d+)">([^<]+)</text>#', $line, $text_m);
    preg_match('#<text [^>]*bbox="([\d\.]+),([\d\.]+),([\d\.]+),([\d\.]+)"[^>]*>([^<]+)</text>#', $line, $text_m);

    if ($text_m) {
        /*$top = intval($text_m[1]);
        $left = intval($text_m[2]);
        $text = pslm_normalize($text_m[6]);*/
        $top = intval($text_m[2]);
        $left = intval($text_m[1]);
        $right = intval($text_m[3]);
        $text = $text_m[5];
        $key = pslm_get_line_key($lines, $top);
        if ($key === false) {
            $key = $top;
            $lines[$key] = [];
        }
        $lines[$key][$left] = [$right, $text];
    }

    if ($n >= 1010) {
        //break;
    }
}
pslm_print_page($page, $lines);

/*if (!feof($fp)) {
    echo "ERROR: unexpected fgets() fail\n";
}*/
fclose($fp);
