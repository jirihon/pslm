#!/usr/bin/env php
<?php
require_once dirname(__FILE__).'/pslm.php';

function pslm_is_long_syllable($syl) {
    return preg_match('#([áéíóúůý]|ou)#u', $syl);
}

function pslm_get_note($music) {
    if (preg_match('#^[abcdefgis]+#', $music, $m)) {
        return $m[0];
    } else {
        return false;
    }
}

function pslm_preprocessor($psalm) {
    preg_match_all('#^%%\s*([a-z]):\s*"([^\n"]*)"\s*$#um', $psalm, $var_matches, PREG_SET_ORDER);
    
    foreach ($var_matches as $var_match) {
        list(, $var, $music) = $var_match;
        
        preg_match_all("#m: ${var}B (\|+)\nt: (.*)#u", $psalm, $snippet_matches, PREG_SET_ORDER);

        foreach ($snippet_matches as $snippet_match) {
            list($snippet, $bar, $text) = $snippet_match;

            $lyrics = pslm_text_to_lyrics($text);
            $lyrics_tokens = pslm_parse_lyrics($lyrics);

            $lyrics = preg_replace('#"([^" ]*) ([^" ]*)"#u', '$1$2', $lyrics);
            $lyrics = preg_replace('#(?<!--)(^| )„?(o|u|na|ke|po|od|do|za|ze|ve|bez|nad|pod|před|přes|při|pro) (?!--)#ui', '$1$2 -- ', $lyrics);
            $lyrics = preg_replace('#(?<!--) ([^ ]+)$#u', ' -- $1', $lyrics);
            $lyrics = str_replace(' -- ', '--', $lyrics);
            $lyrics = trim($lyrics);

            $words = preg_split('#\s+#u', $lyrics);
            $last_word_i = count($words) - 1;
            $sc = [];
            foreach ($words as $word) {
                $sc[] = count(preg_split('#--#u', $word));
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
            if ($sc[$last_word_i] < $notes_after_accent) {
                $slur_len = $notes_after_accent - $sc[$last_word_i];
                $note = pslm_get_note($notes[$accent_note_i]);
                $same_notes = true;

                $last_word_syls = preg_split('#--#', $words[$last_word_i]);
                if ($sc[$last_word_i] > 2 && !pslm_is_long_syllable($last_word_syls[0]) && pslm_is_long_syllable($last_word_syls[1])) {
                    $slur_offset = 1;
                } else {
                    $slur_offset = 0;
                }
                for ($i = 0; $i <= $slur_len; ++$i) {
                    $note_i = $accent_note_i + $slur_offset + $i;
                    if ($note != pslm_get_note($notes[$accent_note_i + $i])) {
                        $same_notes = false;
                    }
                    if ($i == 0) {
                        $notes[$note_i] .= '(';
                    }
                    if ($i == $slur_len) {
                        $notes[$note_i] .= ')';
                    }
                }
                
                if ($same_notes && $last_note_i - $accent_note_i == 2 && $sc[$last_word_i] == 2) {
                    $syl = preg_split('#--#u', $words[$last_word_i]);
                    $long_first = pslm_is_long_syllable($syl[0]);
                    $long_second = pslm_is_long_syllable($syl[1]);

                    if (!$long_first && !$long_second) {
                        $notes[$accent_note_i] = preg_replace('#8?(_?)\($#', '8$1', $notes[$accent_note_i]);
                        $notes[$accent_note_i+1] = preg_replace('#8?\)$#', '', $notes[$accent_note_i+1]);
                        $notes[$last_note_i] = 'r4';
                    } elseif ($long_first && !$long_second) {
                        $notes[$accent_note_i] = preg_replace('#8?(_?)\($#', '4$1', $notes[$accent_note_i]);
                        $notes[$accent_note_i+1] = preg_replace('#8?\)$#', '', $notes[$accent_note_i+1]);
                        $notes[$last_note_i] = 'r';
                    } elseif (!$long_first && $long_second) {
                        $notes[$accent_note_i] = preg_replace('#8?(_?)\($#', '8$1', $notes[$accent_note_i]);
                        $notes[$accent_note_i+1] = preg_replace('#8?\)$#', '4.', $notes[$accent_note_i+1]);
                        $notes[$last_note_i] = 'r4';
                    } else {
                        $notes[$accent_note_i] = preg_replace('#8?(_?)\($#', '4$1', $notes[$accent_note_i]);
                        unset($notes[$accent_note_i+1]);
                    }
                }
            } elseif ($sc[$last_word_i] == $notes_after_accent + 1) {
                $notes[$last_note_i - 1] .= ' '.preg_replace('#[,\']+#', '', $notes[$last_note_i - 1]);
            }

            if (!pslm_is_long_syllable($lyrics_tokens[count($lyrics_tokens) - 1][1])) {
                $notes[$last_note_i] = preg_replace('#2(\)?)#u', '4$1 r', $notes[$last_note_i]);
            }

            $first_word_i = $words[0] == '*' ? 1 : 0;
            if (!preg_match('#^(vždy|v--tvém)$#ui', $words[$first_word_i]) && (
                 ($sc[$first_word_i] == 1 && $sc[$first_word_i+1] > 1) ||
                 ($sc[$first_word_i] == 1 && $sc[$first_word_i+1] == 1 && $sc[$first_word_i+2] == 1 && $sc[$first_word_i+3] > 1)
                )) {
                // add eight note iff the music starts with breve
                $notes[0] = preg_replace('#([abcdefgis]+)([,\']*)B#', '$1${2}8 $1B', $notes[0]);
                // always add at least eight rest
                $notes[0] = 'r8 '.$notes[0];
            }
            $new_music = implode(' ', $notes);

            $music_tokens = pslm_parse_music($new_music);
            $note_syllables = pslm_note_syllables($music_tokens);
            $n_note_syllables = count($note_syllables);

            $lyrics_syllables = pslm_text_syllables($lyrics_tokens);
            $n_lyrics_syllables = count($lyrics_syllables);

            if ($n_note_syllables > $n_lyrics_syllables) {
                // replace breve by eight note
                $new_music = preg_replace('#([abcdefgis]+[,\']*)B #', '${1}8 ', $new_music);
                // join first two notes by slur
                $new_music = preg_replace('#^([abcdefgis]+[,\']*8?) ([abcdefgis]+[,\']*8?) #', '${1}( ${2}) ', $new_music);
            } elseif ($n_note_syllables == $n_lyrics_syllables) {
                // replace starting breve by eight rest and eight note
                $new_music = preg_replace('#^([abcdefgis]+[,\']*)B #', 'r8 ${1} ', $new_music);
                // replace non-starting breve by eight note
                $new_music = preg_replace('#([abcdefgis]+[,\']*)B #', '${1}8 ', $new_music);
            }
            $psalm = str_replace($snippet, "m: $new_music $bar\nt: $text", $psalm);
        }
    }
    // shorten previous syllable or cancel quarter rest if there is a pick up rest in the next phrase
    $psalm = preg_replace("#([abcdefgis]+[,']*)(2|4 r4?)\s+(\|+)((\n|t:.*|%% part:.*)*m: r8)#u", '${1}4 $3$4', $psalm);

    return $psalm;
}

$pslm_f = $argv[count($argv)-1];

if (preg_match('#\.pslm$#', $pslm_f)) {
    // convert .pslm to .ly before running lilypond
    $psalm = file_get_contents($pslm_f);
    $psalm = pslm_preprocessor($psalm);
    //exit;
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
