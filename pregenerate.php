<?php
/**
 * Pregenerate PSLM files based on output from PDF parser
 */
use Symfony\Component\Yaml\Yaml;

require_once 'vendor/autoload.php';

pslm_pregenerate('db/nedelni_zaltar.yml', 'nedelni_zaltar');
pslm_pregenerate('db/vsedni_zaltar.yml', 'vsedni_zaltar');

function pslm_pregenerate($filename, $source) {
    $psalms = Yaml::parseFile($filename);

    foreach ($psalms as $page => $psalm) {
        $id = '';
        $verses = [];
        $occasions = [];
        $responsum = '';
        $responsum_reference = '';
        $verse_reference = '';
        $verse_number = false;

        foreach ($psalm as $i => $psalm_line) {
            if (preg_match('#^OL\d+[a-z]*#u', $psalm_line, $id_m)) {
                $id = $id_m[0];
                $psalm[$i] = preg_replace('#^OL\d+\s*#u', '', $psalm_line);
                if (empty($psalm[$i])) {
                    unset($psalm[$i]);
                }
                break;
            }
        }
        $psalm = array_values($psalm); // reindex array

        if (empty($id) || $id === 'OL107' || $id === 'OL491' || $id === 'OL486' || $id == 'OL453') {
            continue;
        }
        foreach ($psalm as $i => $psalm_line) {
            if ($psalm_line[0] == '[') {
                $occasions = array_slice($psalm, 0, $i - 1);
                
                $responsum = $psalm[$i-1];
                if (preg_match('#\]#u', $psalm_line)) {
                    $responsum_reference = $psalm_line;
                    $verse_reference = $psalm[$i+1];
                } else {
                    $responsum_reference = $psalm_line . ' ' . $psalm[$i+1];
                    $verse_reference = $psalm[$i+2];
                }
            } elseif (!empty($responsum) && preg_match('#^(\d+)\.\s*(.*)$#u', $psalm_line, $verse_m)) {
                // verse beginning
                $verse_number = intval($verse_m[1]);
                $verses[$verse_number] = $verse_m[2];
            } elseif ($verse_number) {
                $verses[$verse_number] .= ' ' . $psalm_line;
            }

        }
        $pslm = [];
        foreach ($occasions as $occasion) {
            $pslm[] = "%% occasion: '$occasion'";
        }
        $responsum_reference = preg_replace('#[\[\]]#u', '', $responsum_reference);
        $responsum_reference = trim($responsum_reference);
        $pslm[] = "%% responsum_reference: '$responsum_reference'";
        $pslm[] = "%% verse_reference: '$verse_reference'";
        $pslm[] = '%% author: olejnik';
        $pslm[] = "%% source: $source";
        $pslm[] = "%% page: $page";
        $pslm[] = '';
        $pslm[] = 'm: \key c \major';
        $pslm[] = '';
        $pslm[] = '%% part: responsum';
        $pslm[] = '';
        $pslm[] = 'm: bB ||';
        $responsum = preg_replace('#\s*-\s*#u', '', $responsum);
        $pslm[] = "t: $responsum";
        $pslm[] = '';

        foreach ($verses as $verse) {
            $pslm[] = '%% part: verse';
            $pslm[] = '';

            $repeat_responsum = preg_match('#R\.$#u', $verse);
            $verse = preg_replace('#\s*R\.$#u', '', $verse);
            $verse = preg_replace('#\s*-\s*#u', '', $verse);

            $verse_parts = preg_split('#\s*\*\s*#u', $verse);
            foreach ($verse_parts as $i => $verse_part) {
                if ($i < count($verse_parts) - 1) {
                    $pslm[] = 'm: aB |';
                } else {
                    $pslm[] = 'm: bB ||';
                }
                if ($i > 0) {
                    $text = '* ';
                } else {
                    $text = '';
                }
                $text .= $verse_part;
                $pslm[] = "t: $text";
                $pslm[] = '';
            }
            if ($repeat_responsum) {
                $pslm[] = '%% use: responsum';
                $pslm[] = '';
            }
        }
        $pslm[] = '';
        $pslm = implode("\n", $pslm);
        file_put_contents("pslm/pregenerated/$id.pslm", $pslm);
    }
}   
