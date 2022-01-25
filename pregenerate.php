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

    foreach ($psalms as $psalm) {
        $id = '';
        $verses = [];

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
        if (empty($id) || $id === 'OL107' || $id === 'OL491' || $id === 'OL486' || $id == 'OL453') {
            continue;
        }
        foreach ($psalm as $i => $psalm_line) {
            $verse_number = 0;
            $verse_text = '';

            if ($psalm_line[0] == '[') {
                $occasions = array_slice($psalm, 0, $i - 2);            
                $responsum = $psalm[$i-1];
                if (preg_match('#\]#u', $psalm_line)) {
                    $responsum_reference = $psalm_line;
                    $verse_reference = $psalm[$i+1];
                } else {
                    $responsum_reference = $psalm_line . ' ' . $psalm[$i+1];
                    $verse_reference = $psalm[$i+2];
                }
            } elseif (preg_match('#^(\d+)\.\s*(.*)$#u', $psalm_line, $verse_m)) {
                if (!empty($verse_text)) {
                    $verses[$verse_number] = $verse_text;
                }
                $verse_number = $verse_m[1];
                $verses[$verse_number] = $verse_m[2];
            } elseif ($verse_number) {
                $verses[$verse_number] .= $psalm_line;
            }
        }
        $pslm = [];
        foreach ($occasions as $occasion) {
            $pslm[] = "%% ocassion: '$occasion'";
        }
        $responsum_reference = preg_replace('#[\[\]]#u', '', $responsum_reference);
        $responsum_reference = trim($responsum_reference);
        $pslm[] = "%% responsum_reference: '$responsum_reference'";
        $pslm[] = "%% verse_reference: '$verse_reference'";
        $pslm[] = '%% author: olejnik';
        $pslm[] = "%% source: $source";
        $pslm[] = '';
        $pslm[] = 'm: \key c \major';
        $pslm[] = '';
        $pslm[] = '%% part: responsum';
        $pslm[] = '';
        $pslm[] = 'm: b\breve \bar "||"';
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
                    $pslm[] = 'm: b\breve \bar "|"';
                } else {
                    $pslm[] = 'm: b\breve \bar "||"';
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