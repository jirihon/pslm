<?php
use Symfony\Component\Yaml\Yaml;
use Vanderlee\Syllable\Syllable;

define('PSLM_SVG_SIZES', [7, 8, 9, 10, 11, 12, 13, 14, 15]);

define('PSLM_PX_PER_STEP', 50);
define('PSLM_MAX_WIDTH', 732);

require_once dirname(__FILE__).'/vendor/autoload.php';

$i = 0;
define('PSLM_TOKEN_SYLLABLE', $i++);
define('PSLM_TOKEN_HYPHEN', $i++);
define('PSLM_TOKEN_STAR', $i++);
define('PSLM_TOKEN_BREATH', $i++);
define('PSLM_TOKEN_OPTIONAL_BREATH', $i++);
define('PSLM_TOKEN_KEY', $i++);
define('PSLM_TOKEN_NOTE_SYLLABLE', $i++);
define('PSLM_TOKEN_NOTE', $i++);
define('PSLM_TOKEN_REST', $i++);
define('PSLM_TOKEN_OTHER', $i++);
define('PSLM_TOKEN_BAR', $i++);
define('PSLM_STATE_INIT', $i++);
define('PSLM_STATE_STR', $i++);
define('PSLM_STATE_MUSIC', $i++);
define('PSLM_STATE_TEXT', $i++);
define('PSLM_STATE_SLUR', $i++);
define('PSLM_STATE_LIGATURE', $i++);

define('PSLM_CRESC_MAX_DIST', 10);

$PSLM_SYLLABLE = null;
$PSLM_HYPH_EXCEPTIONS = null;

$LILYPOND = '/home/xhonji01/Programy/lilypond-2.24.0/bin/lilypond';

/* TODO
find all psalms without []

hiddenBreve =
#(define-music-function (n) (number?)
  #{ \hideNotes \repeat unfold #n { \breve*1/16 \bar "" } \unHideNotes #})
*/

function xml_node_content($node) {
    $content = '';
    foreach ($node->children() as $child) {
        $content .= $child->asXML();
    }
    return $content;
}


function pslm_render_pslm_css($id, $offsets, $aspects, $width, $height) {
    $css = '';
    $n_sizes = count(PSLM_SVG_SIZES);
    $min_width = PSLM_MAX_WIDTH - ($n_sizes) * PSLM_PX_PER_STEP;
    
    for ($i = 0; $i < $n_sizes; ++$i) {
        if ($i > 0) {
            $css .= "@media (min-width: {$min_width}px) {\n";
        }
        for ($z = -$n_sizes + 1; $z < $n_sizes; ++$z) {
            $k = $i - $z; // move size by zoom
            if ($k >= $n_sizes) {
                $k = $n_sizes - 1;
            } elseif ($k < 0) {
                $k = 0;
            }
            $size = PSLM_SVG_SIZES[$k];
            $aspect = $aspects[$size];
            $offset = $offsets[$size];
            
            $min = ($width / $aspect) / 2;
            $max = $height - $min;
            $delta = $max - $min;
            $position = ($offset - $min) / $delta * 100;

            $css .= sprintf(
                ".zoom-%d .score {\n  aspect-ratio: %s;\n  object-position: 50%% %s%%; z-index: %s}\n",
                $z, round($aspect, 4), round($position, 2), $size
            );
        }
        if ($i > 0) {
            $css .= "}\n";
        }
        $min_width += PSLM_PX_PER_STEP;
    }
    file_put_contents("html/css/$id.css", $css);
}

function pslm_join_svgs($id, $svg_d) {
    $defs = [];
    $counter = 0;
    $svg_contents = [];
    $offsets = [];
    $aspects = [];

    $offset_sum = 0;
    $width = 0;

    foreach (PSLM_SVG_SIZES as $size) {
        $symbol_id_map = [];

        $svg_name = "$svg_d/$id-$size.svg";
        $xml = simplexml_load_file($svg_name);
        $svg_content = '';

        $view_box = (string) $xml['viewBox'];
        preg_match('#(\d+) (\d+) (\d+) (\d+)#', $view_box, $m);

        $curr_height = intval($m[4]);
        $curr_width = intval($m[3]);
        
        if ($width === 0) {
            $width = $curr_width;
            $scale = 1;
        } else {
            $scale = $width / $curr_width;
            $curr_height *= $scale;
        }
        $transform = "translate(0, $offset_sum) scale($scale)";
        $offsets[$size] = $offset_sum + $curr_height / 2;
        $aspects[$size] = $width / $curr_height;
        
        foreach ($xml->children() as $node) {
            if ($node->getName() === 'defs') {
                foreach ($node->children() as $symbol) {
                    $symbol_content = xml_node_content($symbol);
                    if (!isset($defs[$symbol_content])) {
                        $symbol_id = "s$counter";
                        $defs[$symbol_content] = $symbol_id;
                        ++$counter;
                    }
                    $symbol_id_map[(string) $symbol['id']] = $defs[$symbol_content];
                }
            } else {
                $svg_content .= $node->asXML() ."\n";
            }
        };
        preg_match_all('/(<use [^>]*)xlink:href="#([^"]*)"([^>]*\/>)/', $svg_content, $m);
        /** @var string[][] $m */
        foreach ($m[2] as $i => $old_symbol_id) {
            if (isset($symbol_id_map[$old_symbol_id])) {
                $new_symbol_id = $symbol_id_map[$old_symbol_id];
                $svg_content = str_replace($m[0][$i], $m[1][$i]."xlink:href=\"#$new_symbol_id\"".$m[3][$i], $svg_content);
            } else {
                // remove uses of undefined symbols
                $svg_content = str_replace($m[0][$i], '', $svg_content);
            }
        }
        $svg_contents[$size] = "<g transform=\"$transform\">$svg_content</g>";
        $offset_sum += $curr_height;
    }
    $defs_xml = '';
    foreach ($defs as $xml => $symbol_id) {
        $defs_xml .= "<symbol overflow=\"visible\" id=\"$symbol_id\">\n$xml\n</symbol>\n";
    }
    $joined_content = implode("\n", $svg_contents);
    $offset_sum = ceil($offset_sum);

    $svg = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"$width\" height=\"$offset_sum\" viewBox=\"0 0 $width $offset_sum\" version=\"1.1\">\n<defs>\n$defs_xml</defs>\n$joined_content\n</svg>";

    $svg_f = "$svg_d/$id.svg";
    file_put_contents($svg_f, $svg);
    
    $cmd = sprintf('./node_modules/svgo/bin/svgo -i %s', $svg_f);
    system($cmd);

    pslm_render_pslm_css($id, $offsets, $aspects, $width, $offset_sum);
}

function run_cmd($cmd, $id) {
    file_put_contents("sh/$id.sh", $cmd ."\n", FILE_APPEND);
}

function pslm_engrave($id, $svg_d) {
    global $LILYPOND;
    $pslm_f = "pslm/$id.pslm";

    file_put_contents("sh/$id.sh", '');

    $psalm = file_get_contents($pslm_f);
    $psalm = pslm_parse_psalm($psalm);
    $skipped = false;

    foreach (PSLM_SVG_SIZES as $size) {
        $lily = pslm_lilypond($psalm, $size);

        $lily_f = "ly/$id-$size.ly";
        if (PSLM_CACHE && file_exists($lily_f) && $lily === file_get_contents($lily_f)) {
            echo "Skipping SVG engraving for $id, lilypond for size $size is the same.\n";
            $skipped = true;
            break;
        }
        file_put_contents($lily_f, $lily);

        $svg_name = "$svg_d/$id-$size";
        $cmd = "$LILYPOND --svg -dbackend=cairo -dno-point-and-click -o $svg_name ly/$id-$size.ly";
        run_cmd($cmd, $id);

        $cmd = sprintf('./node_modules/svgo/bin/svgo -i %s', "$svg_name.svg");
        run_cmd($cmd, $id);
    }
    if ($skipped && file_exists("$svg_d/$id.svg")) {
        echo "Skipping SVG joining for $id.\n";
    } else {
        $cmd = "php join.php $id";
        run_cmd($cmd, $id);
        // pslm_join_svgs($id, $svg_d);
    }
    $midi_f = "midi/$id.midi";

    $lily = pslm_midi($psalm);
    $lily_f = "midi/$id.ly";
    
    if (PSLM_CACHE && file_exists($lily_f) && $lily === file_get_contents($lily_f)) {
        echo "Skipping MP3 engraving for $id, lilypond is the same.\n";
    } else {
        file_put_contents($lily_f, $lily);
        $cmd = "$LILYPOND -o midi/$id $lily_f";
        run_cmd($cmd, $id);
        // TODO: loudnorm filter does something different than audio normalization, use volume filter instead with fixed amount of gain
        $cmd = "timidity --quiet -T 150 --output-24bit -Ow -o - $midi_f | ffmpeg -hide_banner -loglevel error -y -i - -filter:a loudnorm -acodec libmp3lame -qscale:a 3 html/mp3/$id.mp3";
        run_cmd($cmd, $id);
    }
    return $psalm;
}

function pslm_tokens_to_music($tokens) {
    return implode(' ', array_map(function($token) {
        return $token[1];
    }, $tokens));
}

function pslm_token_is_note($token) {
    return $token[0] == PSLM_TOKEN_NOTE || $token[0] == PSLM_TOKEN_NOTE_SYLLABLE;
}

function pslm_token_is_visible_bar($token) {
    $is_visible_bar = $token[0] == PSLM_TOKEN_BAR &&
           ($token[1] == '\bar "|"' || $token[1] == '\bar "||"' || $token[1] == '\bar "|."');
    return $is_visible_bar;
}

function pslm_process_music_part($music) {
    $music = trim(implode(' ', $music));
    
    while (preg_match('#\\\\verseAccent#', $music)) {
        $tokens = pslm_parse_music($music);
        $accent_i = 0;

        for ($i = 0; $i < count($tokens); ++$i) {
            if (preg_match('#\\\\verseAccent#', $tokens[$i][1])) {
                $accent_i = $i;
                $tokens[$i][1] = str_replace('\verseAccent', '\accent', $tokens[$i][1]);
                break;
            }
        }
        $i = $accent_i;
        $cresc_start = $i;
        $dist = 0;
        while ($i >= 0 &&
               $dist <= PSLM_CRESC_MAX_DIST &&
               !pslm_token_is_visible_bar($tokens[$i])) {
            if (pslm_token_is_note($tokens[$i])) {
                ++$dist;
                $cresc_start = $i;
            }
            --$i;
        }
        if ($cresc_start != $accent_i) {
            $tokens[$cresc_start][1] .= '\<';
        }
        $i = $accent_i;
        $decresc_end = $i;
        while ($i < count($tokens) &&
               !pslm_token_is_visible_bar($tokens[$i]) &&
               $tokens[$i][0] !== PSLM_TOKEN_REST) {
            if (pslm_token_is_note($tokens[$i])) {
                $decresc_end = $i;
            }
            ++$i;
        }
        if ($decresc_end != $accent_i) {
            $tokens[$accent_i][1] .= '\>';
            $tokens[$decresc_end][1] .= '\!';
        }
        $music = pslm_tokens_to_music($tokens);
    }
    if (pslm_contains_note($music)) {
        $music = "\\relative { $music }";
    }
    return $music;
}

function pslm_music_implode($parts) {
    $ret = '';
    foreach ($parts as $key => $music) {
        $music = pslm_process_music_part($music);
        $ret .= "$music\n";
    }
    return $ret;
}

function pslm_text_implode($texts) {
    $ret = '';
    foreach ($texts as $text) {
        $ret .= implode("\n", $text) . "\n";
    }
    return $ret;
}


function pslm_midi($psalm) {
    if (isset($psalm['opts']['midi'])) {
        $parts = $psalm['opts']['midi'];
    } else {
        $parts = ['responsum', 'verse_1'];
    }
    $music = [];
    foreach ($parts as $part) {
        $music[$part] = $psalm['music'][$part];
    }
    $music = pslm_music_implode($music);

    // fold breves back into single half note
    $music = str_replace(['\>', '\<', '\!', '\accent', '\cadenzaMeasure'], '', $music);
    $music = preg_replace('#\\\\breve\*1/16 \\\\hideNotes( \\\\breve\*1/16( \\\\bar "")?)+ \\\\unHideNotes#', '2', $music);

    $lily = sprintf('\version "2.22.1" \score { { %s } \midi {} }', $music);
    return $lily;
}


function pslm_lilypond($psalm, $size, $multiscore = false) {
    foreach ($psalm['music'] as $key => $music) {
        if (preg_match('#^verse#', $key)) {
            preg_match_all('#\\\\accent#', implode(' ', $psalm['text'][$key]), $m);
            if (count($m[0]) < 2) {
                echo "WARNING: $key does not have at least two accents\n";
            }
        }
    }
    $lily = file_get_contents(dirname(__FILE__).'/template.ly');

    if (isset($psalm['music'][0])) {
        $music = implode(' ', $psalm['music'][0]);
        if (preg_match('#\\\\key #', $music)) {
            $global = $music;
            array_shift($psalm['music']);
            array_shift($psalm['text']);
        }
    } else {
        $global = '';
    }

    $scores = [];
    $musics = [];
    $texts = [];
    $parts = array_keys($psalm['music']);

    for ($i = 0; $i < count($parts); ++$i) {
        $part = $parts[$i];

        $music = pslm_process_music_part($psalm['music'][$part]);
        $text = implode("\n", $psalm['text'][$part]);

        $text = preg_replace('#("[^"]+"|[^ ]+)[ \n]\*#', '\star $1', $text);
        $text = preg_replace('#\\\\markup +\\\\accent +\\\\star #', '\starAccent ', $text);
        $text = preg_replace('#("[^"]+"|[^ ]+)[ \n]\+#', '\breath $1', $text);
        $text = preg_replace('#("[^"]+"|[^ ]+)[ \n]\(\+\)#', '\optionalBreath $1', $text);

        if ($i == count($parts) - 1) {
            $music .= ' \bar "|."';
        }
        if ($multiscore) {
            $scores[] = sprintf('\score {
    <<
        \new Voice = "melody" { \cadenzaOn %s %s }
        \new Lyrics \lyricsto "melody" { \lyricmode { %s } }
    >>
    \layout {}
}', $global, $music, $text);
        } else {
            $musics[] = $music;
            $texts[] = $text;
        }
    }
    if (count($scores) > 0) {
        $scores = implode("\n\n", $scores);
    } else {
        $scores = sprintf('\score {
    <<
        \new Voice = "melody" { \cadenzaOn %s %s }
        \new Lyrics \lyricsto "melody" { \lyricmode { %s } }
    >>
    \layout {}
}', $global, implode("\n", $musics), implode("\n", $texts));
    }

    $lily = str_replace([
        '\paperWidth',
        '\raggedLast',
        '\scores',
    ], [
        $size,
        $multiscore ? '##t' : '##f',
        $scores,
    ], $lily);

    return $lily;
}


function pslm_parse_psalm($psalm) {
    $lines = explode("\n", $psalm);
    
    $lines[] = 'm:';

    $state = PSLM_STATE_INIT;
    $music = [];
    $text = [];
    $line_buffer = [];
    $psalm = [
        'music' => [],
        'text' => [],
    ];
    $verse_n = 1;
    $use_n = 1;
    $opts = [];
    $part = '';

    foreach ($lines as $line) {
        $line = trim($line);
        if ($line === '') {
            continue; // skip empty lines
        }
        $cmd = substr($line, 0, 2);

        if (in_array($cmd, ['%%', 'm:', 't:'])) {
            $line = trim(substr($line, 2));
        }
        if ($cmd == '%%' || $cmd == 'm:') {
            if ($state == PSLM_STATE_TEXT) {
                $text = $line_buffer;
            } elseif ($state == PSLM_STATE_MUSIC) {
                $music = $line_buffer;
            }
            $line_buffer = [];

            if (!empty($music)) {
                $music = implode(' ', $music);
                $text = implode(' ', $text);
                $original_music = $music;
                $original_text = $text;
                list($music, $text) = pslm_process_snippet($music, $text);
                
                if (!empty($part)) {
                    $psalm['music'][$part][] = $music;
                    $psalm['original_music'][$part][] = $original_music;
                    $psalm['text'][$part][] = $text;
                    $psalm['original_text'][$part][] = $original_text;
                } else {
                    $psalm['music'][] = [$music];
                    $psalm['original_music'][] = [$original_music];
                    $psalm['text'][] = [$text];
                    $psalm['original_text'][] = [$original_text];
                }
                $music = [];
                $text = [];
            }
            if ($cmd == '%%') {
                $line_opts = Yaml::parse("{ $line }");

                if (isset($line_opts['part'])) {
                    $part = $line_opts['part'];
                    
                    if ($part == 'verse') {
                        $part = "verse_$verse_n";

                        if (isset($line_opts['stanza'])) {
                            $psalm['text'][$part][] = "\set stanza = \"$line_opts[stanza]\"";
                        } else {
                            $psalm['text'][$part][] = "\set stanza = \"$verse_n.\"";
                        }
                        ++$verse_n;
                    } elseif (preg_match('#^responsum#ui', $part)) {
                        $psalm['text'][$part][] = '\set stanza = \responsum';
                    }
                } elseif (isset($line_opts['use'])) {
                    $key = $line_opts['use'];
                    $store_key = sprintf('%s_%s', $key, $use_n);
                    $psalm['music'][$store_key] = $psalm['music'][$key];
                    $psalm['text'][$store_key] = $psalm['text'][$key];
                    $psalm['original_text'][$store_key] = $psalm['original_text'][$key];
                    $part = '';
                    ++$use_n;
                } else {
                    foreach ($line_opts as $key => $value) {
                        if (isset($opts[$key])) {
                            if (is_array($opts[$key])) {
                                $opts[$key][] = $value;
                            } else {
                                $opts[$key] = [$opts[$key], $value];
                            }
                        } else {
                            $opts[$key] = $value;
                        }
                    }
                    //$opts = array_merge($opts, $line_opts);
                }
                continue; // move to the next line
            } elseif ($cmd == 'm:') {    
                $state = PSLM_STATE_MUSIC;
            }
        } elseif ($cmd == 't:') {
            $state = PSLM_STATE_TEXT;
            $music = $line_buffer;
            $line_buffer = [];
        } elseif ($cmd[0] == '%') {
            // skip comment
            continue;
        }
        $line_buffer[] = $line;
    }
    $psalm['opts'] = $opts;
    return $psalm;
}

function pslm_extra_breves($n) {
    if ($n === 0) {
        return '';
    } elseif ($n === 1) {
        return '\breve*1/16 \bar "" ';
    } else {
        return str_repeat('\breve*1/16 \bar "" ', $n - 2) . '\breve*1/16 \breve*1/16 \bar "" ';
    }
}

function pslm_process_snippet($music, $text) {
    $music_tokens = pslm_parse_music($music);

    $note_syllables = pslm_note_syllables($music_tokens);
    $n_note_syllables = count($note_syllables);

    foreach ($note_syllables as $token) {
        if ($token[0] == PSLM_TOKEN_NOTE_SYLLABLE) {
            if (preg_match('#\\\\breve\*(\d+)#', $token[1], $m)) {
                $n_note_syllables += intval($m[1]) - 1;
            }
        }
    }
    $text = pslm_text_to_lyrics($text);
    $text_tokens = pslm_parse_lyrics($text);

    $text_syllables = pslm_text_syllables($text_tokens);
    $n_text_syllables = count($text_syllables);

    for ($i = 0; $i < count($text_tokens); ++$i) {
        if ($text_tokens[$i][0] == PSLM_TOKEN_SYLLABLE && strpos($text_tokens[$i][1], ' ') !== false) {
            // should be escaped
            $text_tokens[$i][1] = sprintf('"%s"', $text_tokens[$i][1]);
        }
    }

    $duration = '4';
    foreach ($music_tokens as &$token) {
        preg_match('#(1|2|4|8|16)#', $token[1], $m);
        if ($m) {
            $duration = $m[1];
        }
        if ($duration == '8') {
            $token[1] = preg_replace(['#\(#', '#\)#'], ['[(', ')]'], $token[1]);
        }
    }
    $music = implode(' ', array_map(function($token) { return $token[1]; }, $music_tokens));
    
    $accent_syllable_i = -1;
    $breve_syllables = [];
    foreach ($note_syllables as $i => $token) {
        if (strpos($token[1], '_') !== false) {
            $accent_syllable_i = $i;
        } else if (preg_match('#\\\\breve\*(\d+)#', $token[1], $m)) {
            $breve_syllables[] = [$i, intval($m[1])];
        } else if (strpos($token[1], '\breve') !== false) {
            $breve_syllables[] = [$i, 0];
        }
    }
    if ($accent_syllable_i > -1) {
        $n_syllables_from_end = count($note_syllables) - $accent_syllable_i;
        $k = 0;
        for ($i = count($text_tokens) - 1; $i >= 0; --$i) {
            if ($text_tokens[$i][0] == PSLM_TOKEN_SYLLABLE) {
                ++$k;
                if ($k == $n_syllables_from_end) {
                    $text_tokens[$i][1] = sprintf('\markup \accent %s', $text_tokens[$i][1]);
                }
            }
        }
    }
    foreach ($breve_syllables as $breve_i => [$breve_note_i, $breve_len]) {
        if ($breve_i === 0) {
            if ($breve_len !== 0) {
                echo "ERROR: variable breve must be the first one\n";
                die;
            }
            $breve_text_start = $breve_note_i;
            $breve_text_end = $n_text_syllables - $n_note_syllables + $breve_note_i + 1;
        } else {
            $breve_text_start = $n_text_syllables - $n_note_syllables + $breve_note_i;
            $breve_text_end = $breve_text_start + $breve_len;
        }
        if ($breve_text_end - $breve_text_start < 2) {
            echo "WARNING: less than two syllables on a breve\n";
            echo implode(' ', array_map(function($token) {
                return $token[1];
            }, $text_tokens));
        } else {
            $k = 0;
            for ($i = 0; $i < count($text_tokens); ++$i) {
                if ($text_tokens[$i][0] == PSLM_TOKEN_SYLLABLE) {
                    if ($k === $breve_text_start) {
                        $text_tokens[$i][1] = sprintf('\left %s', $text_tokens[$i][1]);
                    } else if ($k === $breve_text_start + 1) {
                        $text_tokens[$i][1] = sprintf('\squash %s', $text_tokens[$i][1]);
                    } else if ($k === $breve_text_end) {
                        $text_tokens[$i][1] = sprintf('\unLeft \unSquash %s', $text_tokens[$i][1]);
                    }
                    ++$k;
                }
            }
        }
    }
    $text = implode(' ', array_map(function($token) {
        return $token[1];
    }, $text_tokens));

    // $music = str_replace('_', '', $music);
    $music = preg_replace('#([^ ]*)_#', '\bar "" $1', $music);

    $n_notes_to_add = $n_text_syllables - $n_note_syllables;

    preg_match_all('#([^\s]+)\\\\breve\*(\d+)#', $music, $matches, PREG_SET_ORDER);
    foreach ($matches as $m) {
        $extra_breves = pslm_extra_breves(intval($m[2]) - 1);
        $music = str_replace(
            $m[0],
            sprintf('%s\breve*1/16 \hideNotes %s\unHideNotes', $m[1], $extra_breves),
            $music
        );
    }

    preg_match_all('#\\\\breve(?!\*\d+)#', $music, $m, PREG_SET_ORDER);
    if (count($m) > 1) {
        echo "ERROR: More than one automatic breve in a piece of music.\n";
    }
    if ($n_notes_to_add > 0) {
        $extra_breves = pslm_extra_breves($n_notes_to_add);
        $music = preg_replace(
            '#([^\s]+)\\\\breve(?!\*\d+)#',
            sprintf('\1\breve*1/16 \hideNotes %s\unHideNotes', $extra_breves),
            $music
        );
    } elseif ($n_notes_to_add < 0) {
        $n_syllabels_to_add = -$n_notes_to_add;
        $text .= sprintf(' \repeat unfold %d { \skip 1 }', $n_syllabels_to_add);
    }
    /*
    if ($double_breve && preg_match('#\\\\unHideNotes +([abcdefgis]+)[,\']*8( +\1)+[24]?#', $music, $m)) {
        $n_breves = count(preg_split('#\s+#', $m[0])) - 1;
        $breve = '\breve*1/16';
        $extra_breves = str_repeat(sprintf('%s \bar "" ', $breve), $n_breves - 1);
        $music = str_replace(
            $m[0],
            sprintf('\unHideNotes %s%s \hideNotes %s\unHideNotes <>8', $m[1], $breve, $extra_breves),
            $music
        );
    }
    */
    $music = str_replace('\bar "||"', '\bar "||" \break', $music);
    return [$music, $text];
}


function pslm_text_to_lyrics($text) {
    global $PSLM_SYLLABLE, $PSLM_HYPH_EXCEPTIONS;
    if ($PSLM_SYLLABLE === null) {
        Syllable::setCacheDir(dirname(__FILE__) . '/cache');
        Syllable::setLanguageDir(dirname(__FILE__) . '/lang');
        $PSLM_SYLLABLE = new Syllable('cssk', ' -- ');
    }
    $htext = $PSLM_SYLLABLE->hyphenateText($text);
    
    if ($PSLM_HYPH_EXCEPTIONS === null) {
        $hyph = file_get_contents(dirname(__FILE__) . '/db/hyphenation.txt');
        $hyph = preg_split("#\n+#", trim($hyph));
        
        $search = str_replace('-', '', $hyph);
        for ($i = 0; $i < count($search); ++$i) {
            $search[$i] = '#'.$search[$i].'#ui'; // case-insensitive
        }
        $replace = str_replace('-', ' -- ', $hyph);
        $search[] = '#(?<=[aáeéěiíoóuúůyý])(?=([bdďcčfghjklmnňpqrřsštťvwxzž]|ch|ct|chr|sk|[hkmst]l|[bfkt]r|př|tv|zř|jm|[sš]t|sv|vš|zn)[aáeéěiíoóuúůyý])#ui'; // general pattern for two vowels separated by a consonant or consonant group
        $replace[] = ' -- ';
        $PSLM_HYPH_EXCEPTIONS = [$search, $replace];
    }
    $htext = preg_replace($PSLM_HYPH_EXCEPTIONS[0], $PSLM_HYPH_EXCEPTIONS[1], $htext);
    $repl = [
        '#\s{2,}#' => ' ', // normalize white-spaces to single space
        '# -- ([bjlrř]) -- #ui' => '\1 -- ', // move some ambiguous consonants to the previous syllable if both options are possible
        '# -- ([cčdďfghkmnňpqsštťvwxzž]) -- #ui' => ' -- \1', // move other ambiguous consonants to the next syllable if both options are posible
        '# -- (st|md) -- #ui' => ' -- \1', // move other ambiguous consonants to the next syllable if both options are posible
        '# -- (sť|ls|ch|mž)\b#ui' => '\1', // move unsyllabic parts to the previous syllable
        '#\b(js|lst) -- #ui' => '\1', // move unsyllabic parts to the next syllable

        '#příz -- n#ui' => 'pří -- zn',
        
        '#\b[ksvz] [^\s]+#ui' => '"\0"', // join unsyllabic preposition to the next syllable
        '#[^\s]+ \+#ui' => '"\0"', // join + sign to the previous syllable
    ];
    $htext = preg_replace(array_keys($repl), array_values($repl), $htext);
    return $htext;
}


function pslm_parse_lyrics($text) {
    $ch = preg_split('##u', trim($text), -1, PREG_SPLIT_NO_EMPTY);
    $ch[] = ' ';
    
    $tokens = [];
    $s = '';
    $state = PSLM_STATE_INIT;

    for ($i = 0; $i < count($ch); ++$i) {
        if ($state == PSLM_STATE_INIT) {
            if ($ch[$i] === '"') {
                $state = PSLM_STATE_STR;
            } elseif ($ch[$i] === ' ') {
                if ($s !== '') {
                    if ($s === '*') {
                        $tokens[] = [PSLM_TOKEN_STAR, $s];
                    } else if ($s === '+') {
                        $tokens[] = [PSLM_TOKEN_BREATH, $s];
                    } else if ($s === '(+)') {
                        $tokens[] = [PSLM_TOKEN_OPTIONAL_BREATH, $s];
                    } else {
                        $tokens[] = [PSLM_TOKEN_SYLLABLE, $s];
                    }
                    $s = '';
                }
            } elseif ($ch[$i] === '-' && $ch[$i + 1] === '-') {
                $tokens[] = [PSLM_TOKEN_HYPHEN, '--'];
                ++$i;
            } else {
                $s .= $ch[$i];
            }
        } elseif ($state == PSLM_STATE_STR) {
            if ($ch[$i] === '\\' && $ch[$i + 1] === '"') {
                $s .= '"';
                ++$i; // skip escaped quotes
            } elseif ($ch[$i] === '"') {
                $state = PSLM_STATE_INIT; // go back to init state
                $s = str_replace(' -- ', '', $s); // remove automatic hyphenation for the quoted string
                $tokens[] = [PSLM_TOKEN_SYLLABLE, $s];
                $s = '';
            } else {
                $s .= $ch[$i]; // store characters
            }
        }
    }
    return $tokens;
}

function pslm_print_text_tokens($tokens) {
    $s = [];
    foreach ($tokens as $token) {
        $s[] = sprintf('[%d,%s]', $token[0], $token[1]);
    }
    echo implode('', $s) . "\n";
}

function pslm_text_syllables($tokens) {
    return array_values(array_filter($tokens, function($token) { 
        return $token[0] == PSLM_TOKEN_SYLLABLE;
    }));
}

function pslm_note_syllables($tokens) {
    return array_values(array_filter($tokens, function($token) {
        return $token[0] == PSLM_TOKEN_NOTE_SYLLABLE;
    }));
}


function pslm_is_note($event) {
    return preg_match('#^([abcdefg]|\\\\breve)#', $event);
}

function pslm_contains_note($music) {
    return preg_match('#(^|\s)([abcdefg]|\\\\breve)#', $music);
}

function pslm_parse_music($music) {
    // replace shortcuts
    $shortcuts = [
        '#(?<=\s|^)\|\|(?=\s|$)#' => '\cadenzaMeasure \bar "||"',
        '#(?<=\s|^)\|(?=\s|$)#' => '\cadenzaMeasure \bar "|"',
        '#(?<=\s|^)/(?=\s|$)#' => '\bar ""',
        '#B#' => '\breve',
        //'#\(#' => '[(',
        //'#\)#' => ')]',
        //'#_#' => '\verseAccent',
    ];
    $music = preg_replace(array_keys($shortcuts), array_values($shortcuts), $music);
    $music = trim($music);
    $events = preg_split('#\s+#', $music);
    $tokens = [];
    $state = PSLM_STATE_INIT;

    for ($i = 0; $i < count($events); ++$i) {
        $event = $events[$i];

        if ($event == '\key') {
            $tokens[] = [PSLM_TOKEN_KEY, sprintf('%s %s %s', $event, $events[$i + 1], $events[$i + 2])];
            $i += 2;
        } elseif ($event == '\bar') {
            $tokens[] = [PSLM_TOKEN_BAR, sprintf('%s %s', $event, $events[$i + 1])];
            $i += 1;
        } elseif (pslm_is_note($event)) {
            if ($state == PSLM_STATE_INIT) {
                $tokens[] = [PSLM_TOKEN_NOTE_SYLLABLE, $event];

                if (preg_match('#\(#', $event)) {
                    $state = PSLM_STATE_SLUR;
                } elseif (preg_match('#~#', $event)) {
                    $state = PSLM_STATE_LIGATURE;
                }
            } else {
                $tokens[] = [PSLM_TOKEN_NOTE, $event];
            }
            if (($state == PSLM_STATE_SLUR && preg_match('#\)#', $event)) ||
                ($state == PSLM_STATE_LIGATURE && !preg_match('#~#', $event))) {
                $state = PSLM_STATE_INIT;
            }
        } elseif ($event[0] == 'r') {
            $tokens[] = [PSLM_TOKEN_REST, $event];
        } else {
            $tokens[] = [PSLM_TOKEN_OTHER, $event];
        }
    }
    return $tokens;
}
