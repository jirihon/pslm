#!/usr/bin/env php
<?php
require_once dirname(__FILE__).'/pslm.php';

function pslm_preprocessor($psalm) {
    preg_match_all('#^%%\s*([a-z]):\s*"([^\n"]*)"\s*$#um', $psalm, $var_matches, PREG_SET_ORDER);
    
    foreach ($var_matches as $var_match) {
        list(, $var, $music) = $var_match;
        
        preg_match_all("#m: ${var}B (\|+)\nt: (.*)#u", $psalm, $snippet_matches, PREG_SET_ORDER);

        foreach ($snippet_matches as $snippet_match) {
            list($snippet, $bar, $text) = $snippet_match;

            $lyrics = pslm_text_to_lyrics($text);
            $is_last_long = preg_match('# [^ ]*([áéíóúůý]|ou)[^ ]*$#u', $lyrics, $long_m);

            $lyrics = preg_replace('#(^| )(o|u|na|ke|po|od|do|se|za|ze|ve|nad|pod|před|přes|při) (?!--)#u', ' \2 -- ', $lyrics);
            $lyrics = preg_replace('#(?<!--) ([^ ]+)$#u', ' -- \1', $lyrics);
            $lyrics = str_replace(' -- ', '--', $lyrics);

            $words = preg_split('#\s+#u', $lyrics);
            $last_word_i = count($words) - 1;
            $syllable_counts = [];
            foreach ($words as $word) {
                $syllable_counts[] = count(preg_split('#--#u', $word));
            }
            
            $notes = preg_split('#\s+#', $music);
            $last_note_i = count($notes) - 1;

            $notes_after_accent = 0;
            $accent_note_i = 0;
            for ($i = count($notes) - 1; $i >= 0; --$i) {
                if (strpos($notes[$i], '_') !== false) {
                    $notes_after_accent = count($notes) - $i;
                    $accent_note_i = $i;
                    break;
                }
            }
            if ($syllable_counts[$last_word_i] < $notes_after_accent) {
                $slur_len = $notes_after_accent - $syllable_counts[$last_word_i];
                for ($i = 0; $i <= $slur_len; ++$i) {
                    if ($i == 0) {
                        $notes[$accent_note_i + $i] .= '(';
                    }
                    if ($i == $slur_len) {
                        $notes[$accent_note_i + $i] .= ')';
                    }
                }
            } elseif ($syllable_counts[$last_word_i] == $notes_after_accent + 1) {
                $notes[$last_note_i - 1] .= ' '.$notes[$last_note_i - 1];
            }

            if (!$is_last_long) {
                $notes[$last_note_i] = preg_replace('#2(\)?)#u', '4\1 r', $notes[$last_note_i]);
            }
            $new_music = implode(' ', $notes);
            $psalm = str_replace($snippet, "m: $new_music $bar\nt: $text", $psalm);
        }
    }
    return $psalm;
}

$pslm_f = $argv[count($argv)-1];

if (preg_match('#\.pslm$#', $pslm_f)) {
    // convert .pslm to .ly before running lilypond
    $psalm = file_get_contents($pslm_f);
    $psalm = pslm_preprocessor($psalm);
    file_put_contents($pslm_f, $psalm);
    $psalm = pslm_parse_psalm($psalm);
    $lily = pslm_lilypond($psalm, 10, false);

    $lily_f = preg_replace('#\.pslm$#', '.ly', $pslm_f);
    file_put_contents($lily_f, $lily);

    $passed_args = array_slice($argv, 1, -1);
    $passed_args[] = $lily_f;
} else {
    // fallback to basic lilypond
    $passed_args = array_slice($argv, 1);
}
$passed_args = array_map(function($arg) { return escapeshellarg($arg); }, $passed_args);
$passed_args = implode(' ', $passed_args);
$cmd = "lilypond $passed_args";
system($cmd, $retcode);
exit($retcode);
