<?php
use Symfony\Component\Yaml\Yaml;
use Vanderlee\Syllable\Syllable;

require_once 'vendor/autoload.php';

$i = 0;
define('PSLM_TOKEN_SYLLABLE', $i++);
define('PSLM_TOKEN_HYPHEN', $i++);
define('PSLM_TOKEN_KEY', $i++);
define('PSLM_TOKEN_NOTE_SYLLABLE', $i++);
define('PSLM_TOKEN_NOTE', $i++);
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


function pslm_engrave($id, $svg_d) {
    $pslm_f = "pslm/$id.pslm";
    $pslm_mtime = filemtime($pslm_f);

    $psalm = file_get_contents($pslm_f);
    $psalm = pslm_parse_psalm($psalm);

    foreach (PSLM_SVG_SIZES as $size) {
        $svg_f = "svg/$id-$size.svg";
        
        if (PSLM_CACHE && file_exists($svg_f) && filemtime($svg_f) > $pslm_mtime) {
            echo "Skipping SVG engraving for $id-$size.\n";
            continue;
        }
        $lily = pslm_lilypond($psalm, $size);
        file_put_contents("ly/$id-$size.ly", $lily);

        $svg_name = "$svg_d/$id-$size";

        $cmd = "lilypond --svg -dno-point-and-click -o $svg_name ly/$id-$size.ly";
        system($cmd);

        pslm_fix_svg("$svg_name.svg");
    }

    $midi_f = "midi/$id.midi";

    if (PSLM_CACHE && file_exists($midi_f) && filemtime($midi_f) > $pslm_mtime) {
        echo "Skipping MIDI engraving for $id.\n";
    } else {
        $lily = pslm_midi($psalm);

        file_put_contents("midi/$id.ly", $lily);
        $cmd = "lilypond -o midi/$id midi/$id.ly";
        system($cmd);
        $cmd = "timidity --quiet -T 150 --output-24bit -Ow -o - $midi_f | ffmpeg -hide_banner -loglevel error -y -i - -acodec libmp3lame -ab 64k html/mp3/$id.mp3";
        system($cmd);
    }
    return $psalm;
}

function pslm_fix_svg($svg_f) {
    $svg = file_get_contents($svg_f);

    $svg = preg_replace('#\s*</?tspan>\s*#', '', $svg);
    $svg = preg_replace('#<style.*?</style>#s', '', $svg);
    file_put_contents($svg_f, $svg);

    $cmd = sprintf('./node_modules/svgo/bin/svgo -i %s', $svg_f);
    system($cmd);
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
        while ($i >= 0 && $dist <= PSLM_CRESC_MAX_DIST && !pslm_token_is_visible_bar($tokens[$i])) {
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
        while ($i < count($tokens) && !pslm_token_is_visible_bar($tokens[$i])) {
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
    return $music;
}

function pslm_music_implode($parts) {
    $ret = '';
    foreach ($parts as $music) {
        $music = trim(implode(' ', $music));
        $music = pslm_process_music_part($music);
        if (pslm_is_note($music)) {
            $ret .= "\\relative { $music }\n";
        } else {
            $ret .= "$music\n";
        }
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
    $music = $psalm['music'];

    // generate midi only for responsum and the first verse
    $keys = array_keys($music);
    $verse_key = count($keys) - 1;
    for ($i = 0; $i < count($keys); ++$i) {
        if ($keys[$i] === 'verse_1') {
            $verse_key = $i;
            break;
        }
    }
    $music = array_slice($music, 0, $verse_key + 1);
    $music = pslm_music_implode($music);

    // fold breves back into single half note
    $music = str_replace(['\>', '\<', '\!', '\accent'], '', $music);
    $music = preg_replace('#\\\\breve\*1/16 \\\\hideNotes( \\\\breve\*1/16 \\\\bar "")+ \\\\unHideNotes#', '2', $music);

    $lily = sprintf('\version "2.22.1" \score { { %s } \midi {} }', $music);
    return $lily;
}


function pslm_lilypond($psalm, $size) {
    $music = pslm_music_implode($psalm['music']);
    $text = pslm_text_implode($psalm['text']);

    $lily = sprintf('\version "2.22.1"
\header { tagline = "" }
\paper {
    indent = 0\cm
    top-margin = 0\cm
    right-margin = 0\cm
    bottom-margin = 0\cm
    left-margin = 0\cm
    paper-width = %s\cm
    page-breaking = #ly:one-page-breaking 
}
#(define prevx 9999)
#(define (unhide-after-line-break g)
    (let* ((ss (ly:grob-object g \'staff-symbol))
        (refp (ly:grob-common-refpoint g ss X))
        (x (- (ly:grob-relative-coordinate g refp X)
                (ly:grob-relative-coordinate ss refp X)))
        (transparent (ly:grob-property g \'transparent)))
    (if (and (< x prevx) (equal? transparent #t)) (begin
        (ly:grob-set-property! g \'color (rgb-color 0.5 0.5 0.5))
        (ly:grob-set-property! g \'transparent #f)))
    (set! prevx x)))

accentMark = \markup \raise #0.5 \rotate #-20 \musicglyph "scripts.rvarcomma"
accent = #(make-dynamic-script accentMark)
star = \markup { \lower #0.65 \larger "*" }
responsum = \markup \concat { "R." \hspace #-1.7 \path #0.1 #\'((moveto 0 0.08) (lineto 0.8 0.7)) }

melody = {
    \cadenzaOn
%s
    \bar "|."
}
words = \lyricmode {
%s
}
\layout {
    \context {
        \Staff
        \remove "Time_signature_engraver"
    }
    \context {
        \Voice {
            \override NoteHead.after-line-breaking = #unhide-after-line-break
            \override NoteHead.output-attributes = #\'((class . "notehead"))
            \override Hairpin.height = #0.55
        }
    }
    \context {
        \Lyrics {
            \override StanzaNumber.output-attributes = #\'((class . "stanzanumber"))
        }
    }
}
\score {
    <<
        \new Staff <<
            \new Voice = "melody" \melody
            \new Lyrics \lyricsto "melody" \words
        >>
    >>
    \layout {}
}
    ', $size, $music, $text);
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
                list($music, $text) = pslm_process_snippet($music, $text);
                
                if (!empty($part)) {
                    $psalm['music'][$part][] = $music;
                    $psalm['text'][$part][] = $text;
                } else {
                    $psalm['music'][] = [$music];
                    $psalm['text'][] = [$text];
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
                        $psalm['text'][$part][] = "\set stanza = \"$verse_n.\"";
                        ++$verse_n;
                    } elseif ($part == 'responsum') {
                        $psalm['text'][$part][] = '\set stanza = \responsum';
                    }
                } elseif (isset($line_opts['use'])) {
                    $key = $line_opts['use'];
                    $psalm['music'][] = $psalm['music'][$key];
                    $psalm['text'][] = $psalm['text'][$key];
                    $part = '';
                } else {
                    $opts = array_merge($opts, $line_opts);
                }
                continue; // move to the next line
            } elseif ($cmd == 'm:') {    
                $state = PSLM_STATE_MUSIC;
            }
        } elseif ($cmd == 't:') {
            $state = PSLM_STATE_TEXT;
            $music = $line_buffer;
            $line_buffer = [];
        }
        $line_buffer[] = $line;
    }
    $psalm['opts'] = $opts;
    return $psalm;
}


function pslm_process_snippet($music, $text) {
    preg_match_all('#\\\\breve#', $music, $m);
    if (count($m[0]) > 1) {
        echo "ERROR: More than one breve in a piece of music.\n";
        return [];
    }
    $note_tokens = pslm_parse_music($music);
    $note_syllables = pslm_note_syllables($note_tokens);
    $n_note_syllables = count($note_syllables);

    $text = pslm_text_to_lyrics($text);
    $text_tokens = pslm_parse_lyrics($text);
    
    $text_syllables = pslm_text_syllables($text_tokens);
    $n_text_syllables = count($text_syllables);

    $n_notes_to_add = $n_text_syllables - $n_note_syllables;

    if ($n_notes_to_add > 0) {
        $breve = '\breve*1/16';
        $extra_breves = str_repeat(sprintf('%s \bar "" ', $breve), $n_notes_to_add);
        $music = str_replace(
            '\breve',
            sprintf('%s \hideNotes %s\unHideNotes', $breve, $extra_breves),
            $music
        );
    } elseif ($n_notes_to_add < 0) {
        $n_syllabels_to_add = -$n_notes_to_add;
        $text .= sprintf(' \repeat unfold %d { \skip 1 }', $n_syllabels_to_add);
    }
    $text = str_replace('*', '\set stanza = \star', $text);
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
            $search[$i] = '#'.$search[$i].'#ui';
        }
        $replace = str_replace('-', ' -- ', $hyph);
        $PSLM_HYPH_EXCEPTIONS = [$search, $replace];
    }
    $htext = preg_replace($PSLM_HYPH_EXCEPTIONS[0], $PSLM_HYPH_EXCEPTIONS[1], $htext);

    $repl = [
        '#\s{2,}#' => ' ', // normalize white-spaces to single space
        '# -- ([sšjdb]) -- #ui' => '\1 -- ', // move s, š or j to the previous syllable if both options are possible
        '# -- ([tz]) -- #ui' => ' -- \1', // move "t" to the next syllable if both options are posible
        //'# -- ([sš])([pt])#ui' => '\1 -- \2', // move s or š to the previous syllable if there is "p" or "t following
        
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
                    if ($s !== '*') {
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
    return array_filter($tokens, function($token) { 
        return $token[0] == PSLM_TOKEN_SYLLABLE;
    });
}

function pslm_note_syllables($tokens) {
    return array_filter($tokens, function($token) {
        return $token[0] == PSLM_TOKEN_NOTE_SYLLABLE;
    });
}


function pslm_is_note($event) {
    return preg_match('#^([abcdefg]|\\\\breve)#', $event);
}

function pslm_parse_music($music) {
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
        } else {
            $tokens[] = [PSLM_TOKEN_OTHER, $event];
        }
    }
    return $tokens;
}
