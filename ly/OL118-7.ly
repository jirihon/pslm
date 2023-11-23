\version "2.22.1"
\header { tagline = "" }
\paper {
  indent = 0\cm
  top-margin = 0\cm
  right-margin = 0\cm
  bottom-margin = 0\cm
  left-margin = 0\cm
  paper-width = 7\cm
  page-breaking = #ly:one-page-breaking
  system-system-spacing.basic-distance = #11
  score-system-spacing.basic-distance = #11
  ragged-last = ##f
}


%% Author: Thomas Morley
%% https://lists.gnu.org/archive/html/lilypond-user/2020-05/msg00002.html
#(define (line-position grob)
"Returns position of @var[grob} in current system:
   @code{'start}, if at first time-step
   @code{'end}, if at last time-step
   @code{'middle} otherwise
"
  (let* ((col (ly:item-get-column grob))
         (ln (ly:grob-object col 'left-neighbor))
         (rn (ly:grob-object col 'right-neighbor))
         (col-to-check-left (if (ly:grob? ln) ln col))
         (col-to-check-right (if (ly:grob? rn) rn col))
         (break-dir-left
           (and
             (ly:grob-property col-to-check-left 'non-musical #f)
             (ly:item-break-dir col-to-check-left)))
         (break-dir-right
           (and
             (ly:grob-property col-to-check-right 'non-musical #f)
             (ly:item-break-dir col-to-check-right))))
        (cond ((eqv? 1 break-dir-left) 'start)
              ((eqv? -1 break-dir-right) 'end)
              (else 'middle))))

#(define (tranparent-at-line-position vctor)
  (lambda (grob)
  "Relying on @code{line-position} select the relevant enry from @var{vctor}.
Used to determine transparency,"
    (case (line-position grob)
      ((end) (not (vector-ref vctor 0)))
      ((middle) (not (vector-ref vctor 1)))
      ((start) (not (vector-ref vctor 2))))))

noteHeadBreakVisibility =
#(define-music-function (break-visibility)(vector?)
"Makes @code{NoteHead}s transparent relying on @var{break-visibility}"
#{
  \override NoteHead.transparent =
    #(tranparent-at-line-position break-visibility)
#})

#(define delete-ledgers-for-transparent-note-heads
  (lambda (grob)
    "Reads whether a @code{NoteHead} is transparent.
If so this @code{NoteHead} is removed from @code{'note-heads} from
@var{grob}, which is supposed to be @code{LedgerLineSpanner}.
As a result ledgers are not printed for this @code{NoteHead}"
    (let* ((nhds-array (ly:grob-object grob 'note-heads))
           (nhds-list
             (if (ly:grob-array? nhds-array)
                 (ly:grob-array->list nhds-array)
                 '()))
           ;; Relies on the transparent-property being done before
           ;; Staff.LedgerLineSpanner.after-line-breaking is executed.
           ;; This is fragile ...
           (to-keep
             (remove
               (lambda (nhd)
                 (ly:grob-property nhd 'transparent #f))
               nhds-list)))
      ;; TODO find a better method to iterate over grob-arrays, similiar
      ;; to filter/remove etc for lists
      ;; For now rebuilt from scratch
      (set! (ly:grob-object grob 'note-heads)  '())
      (for-each
        (lambda (nhd)
          (ly:pointer-group-interface::add-grob grob 'note-heads nhd))
        to-keep))))

hideNotes = {
  \noteHeadBreakVisibility #begin-of-line-visible
}
unHideNotes = {
  \noteHeadBreakVisibility #all-visible
}

% work-around for resetting accidentals
% https://lilypond.org/doc/v2.23/Documentation/notation/displaying-rhythms#unmetered-music
cadenzaMeasure = {
  \cadenzaOff
  \partial 1024 s1024
  \cadenzaOn
}

#(define-markup-command (accent layout props text) (markup?)
  "Underline accented syllable"
  (interpret-markup layout props
    #{\markup \override #'(offset . 4.3) \underline { #text }#}))

responsum = \markup \concat {
  "R" \hspace #-1.05 \path #0.1 #'((moveto 0 0.07) (lineto 0.9 0.8)) \hspace #0.05 "."
}

\layout {
    \context {
        \Staff
        \remove "Time_signature_engraver"
        \override LedgerLineSpanner.after-line-breaking = #delete-ledgers-for-transparent-note-heads
    }
    \context {
        \Voice {
            \override NoteHead.output-attributes = #'((class . "notehead"))
            \override Hairpin.height = #0.55
        }
    }
    \context {
        \Lyrics {
            \override StanzaNumber.output-attributes = #'((class . "stanzanumber"))
            \override LyricSpace.minimum-distance = #0.9
            \override LyricText.font-name = #"TeX Gyre Schola"
            \override LyricText.font-size = 1
            \override StanzaNumber.font-name = #"TeX Gyre Schola Bold"
            \override StanzaNumber.font-size = 1
        }
    }
}

% magnetic-lyrics.ily
%
%   written by
%     Jean Abou Samra <jean@abou-samra.fr>
%     Werner Lemberg <wl@gnu.org>
%
%   adapted by
%     Jiri Hon <jiri.hon@gmail.com>
%
% Version 2022-Apr-15

% https://www.mail-archive.com/lilypond-user@gnu.org/msg149350.html

#(define (Left_hyphen_pointer_engraver context)
   "Collect syllable-hyphen-syllable occurrences in lyrics and store
them in properties.  This engraver only looks to the left.  For
example, if the lyrics input is @code{foo -- bar}, it does the
following.

@itemize @bullet
@item
Set the @code{text} property of the @code{LyricHyphen} grob between
@q{foo} and @q{bar} to @code{foo}.

@item
Set the @code{left-hyphen} property of the @code{LyricText} grob with
text @q{foo} to the @code{LyricHyphen} grob between @q{foo} and
@q{bar}.
@end itemize

Use this auxiliary engraver in combination with the
@code{lyric-@/text::@/apply-@/magnetic-@/offset!} hook."
   (let ((hyphen #f)
         (text #f))
     (make-engraver
      (acknowledgers
       ((lyric-syllable-interface engraver grob source-engraver)
        (set! text grob)))
      (end-acknowledgers
       ((lyric-hyphen-interface engraver grob source-engraver)
        ;(when (not (grob::has-interface grob 'lyric-space-interface))
          (set! hyphen grob)));)
      ((stop-translation-timestep engraver)
       (when (and text hyphen)
         (ly:grob-set-object! text 'left-hyphen hyphen))
       (set! text #f)
       (set! hyphen #f)))))

#(define (lyric-text::apply-magnetic-offset! grob)
   "If the space between two syllables is less than the value in
property @code{LyricText@/.details@/.squash-threshold}, move the right
syllable to the left so that it gets concatenated with the left
syllable.

Use this function as a hook for
@code{LyricText@/.after-@/line-@/breaking} if the
@code{Left_@/hyphen_@/pointer_@/engraver} is active."
   (let ((hyphen (ly:grob-object grob 'left-hyphen #f)))
     (when hyphen
       (let ((left-text (ly:spanner-bound hyphen LEFT)))
         (when (grob::has-interface left-text 'lyric-syllable-interface)
           (let* ((common (ly:grob-common-refpoint grob left-text X))
                  (this-x-ext (ly:grob-extent grob common X))
                  (left-x-ext
                   (begin
                     ;; Trigger magnetism for left-text.
                     (ly:grob-property left-text 'after-line-breaking)
                     (ly:grob-extent left-text common X)))
                  ;; `delta` is the gap width between two syllables.
                  (delta (- (interval-start this-x-ext)
                            (interval-end left-x-ext)))
                  (details (ly:grob-property grob 'details))
                  (threshold (assoc-get 'squash-threshold details 0.2)))
             (when (< delta threshold)
               (let* (;; We have to manipulate the input text so that
                      ;; ligatures crossing syllable boundaries are not
                      ;; disabled.  For languages based on the Latin
                      ;; script this is essentially a beautification.
                      ;; However, for non-Western scripts it can be a
                      ;; necessity.
                      (lt (ly:grob-property left-text 'text))
                      (rt (ly:grob-property grob 'text))
                      (is-space (grob::has-interface hyphen 'lyric-space-interface))
                      (space (if is-space " " ""))
                      (space-markup (grob-interpret-markup grob " "))
                      (space-size (interval-length (ly:stencil-extent space-markup X)))
                      (extra-delta (if is-space space-size 0))
                      ;; Append new syllable.
                      (ltrt-space (if (and (string? lt) (string? rt))
                                (string-append lt space rt)
                                (make-concat-markup (list lt space rt))))
                      ;; Right-align `ltrt` to the right side.
                      (ltrt-space-markup (grob-interpret-markup
                               grob
                               (make-translate-markup
                                (cons (interval-length this-x-ext) 0)
                                (make-right-align-markup ltrt-space)))))
                 (begin
                   ;; Don't print `left-text`.
                   (ly:grob-set-property! left-text 'stencil #f)
                   ;; Set text and stencil (which holds all collected
                   ;; syllables so far) and shift it to the left.
                   (ly:grob-set-property! grob 'text ltrt-space)
                   (ly:grob-set-property! grob 'stencil ltrt-space-markup)
                   (ly:grob-translate-axis! grob (- (- delta extra-delta)) X))))))))))


#(define (lyric-hyphen::displace-bounds-first grob)
   ;; Make very sure this callback isn't triggered too early.
   (let ((left (ly:spanner-bound grob LEFT))
         (right (ly:spanner-bound grob RIGHT)))
     (ly:grob-property left 'after-line-breaking)
     (ly:grob-property right 'after-line-breaking)
     (ly:lyric-hyphen::print grob)))

squashThreshold = #0.4

\layout {
  \context {
    \Lyrics
    \consists #Left_hyphen_pointer_engraver
    \override LyricText.after-line-breaking =
      #lyric-text::apply-magnetic-offset!
    \override LyricHyphen.stencil = #lyric-hyphen::displace-bounds-first
    \override LyricText.details.squash-threshold = \squashThreshold
    \override LyricHyphen.minimum-distance = 0
    \override LyricHyphen.minimum-length = \squashThreshold
  }
}

squash = \override LyricText.details.squash-threshold = 9999
unSquash = \override LyricText.details.squash-threshold = \squashThreshold

left = \override LyricText.self-alignment-X = #LEFT
unLeft = \revert LyricText.self-alignment-X

starOffset = #(lambda (grob) 
                (let ((x_offset (ly:self-alignment-interface::aligned-on-x-parent grob)))
                  (if (= x_offset 0) 0 (+ x_offset 1.2))))

star = #(define-music-function (syllable)(string?)
"Append star separator at the end of a syllable"
#{
  \once \override LyricText.X-offset = #starOffset
  \lyricmode { \markup {
    #syllable
    \override #'((font-name . "TeX Gyre Schola Bold")) \hspace #0.2 \lower #0.65 \larger "*"
  } }
#})

starAccent = #(define-music-function (syllable)(string?)
"Append star separator at the end of a syllable and make accent"
#{
  \once \override LyricText.X-offset = #starOffset
  \lyricmode { \markup {
    \accent #syllable
    \override #'((font-name . "TeX Gyre Schola Bold")) \hspace #0.2 \lower #0.65 \larger "*"
  } }
#})

breath = #(define-music-function (syllable)(string?)
"Append breathing indicator at the end of a syllable"
#{
  \lyricmode { \markup { #syllable "+" } }
#})

optionalBreath = #(define-music-function (syllable)(string?)
"Append optional breathing indicator at the end of a syllable"
#{
  \lyricmode { \markup { #syllable "(+)" } }
#})


\score {
    <<
        \new Voice = "melody" { \cadenzaOn \key f \major \relative { f'8 c f g a c \bar "" bes[( a)] a4 \bar "" r8 c d4 c \bar "" bes8 f \bar "" a[( g)] g2 \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes a8 g \bar "" g f f f4 r \cadenzaMeasure \bar "|" f\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" d c c2 \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes a8 g \bar "" g[( f)] f4 r \cadenzaMeasure \bar "|" f\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" d[( c)] c4 r \cadenzaMeasure \bar "||" \break }
\relative { f'8 c f g a c \bar "" bes[( a)] a4 \bar "" r8 c d4 c \bar "" bes8 f \bar "" a[( g)] g2 \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes a8 g \bar "" g f f4 r \cadenzaMeasure \bar "|" f\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" d[( c)] c4 r \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes a8 g \bar "" g[( f)] f4 r \cadenzaMeasure \bar "|" f\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" d[( c)] c4 r \cadenzaMeasure \bar "||" \break }
\relative { f'8 c f g a c \bar "" bes[( a)] a4 \bar "" r8 c d4 c \bar "" bes8 f \bar "" a[( g)] g2 \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes a8 g \bar "" g f f4 r \cadenzaMeasure \bar "|" f\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" d c c4 r \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes a8 g \bar "" g[( f)] f4 r \cadenzaMeasure \bar "|" f\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" d[( c)] c4 r \cadenzaMeasure \bar "||" \break }
\relative { f'8 c f g a c \bar "" bes[( a)] a4 \bar "" r8 c d4 c \bar "" bes8 f \bar "" a[( g)] g2 \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes a8 g \bar "" g[( f)] f4 r \cadenzaMeasure \bar "|" f\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" d[( c)] c4 r \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes a8 g \bar "" g f f f4 r \cadenzaMeasure \bar "|" f\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" d c c4 r \cadenzaMeasure \bar "||" \break }
\relative { f'8 c f g a c \bar "" bes[( a)] a4 \bar "" r8 c d4 c \bar "" bes8 f \bar "" a[( g)] g4 \cadenzaMeasure \bar "||" \break }
\relative { r8 bes' bes\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes a8 g \bar "" g f f f4 r \cadenzaMeasure \bar "|" f\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" d c c4 r \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes a8 g \bar "" g[( f)] f4 r \cadenzaMeasure \bar "|" f\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" d c c c4 r \cadenzaMeasure \bar "||" \break }
\relative { f'8 c f g a c \bar "" bes[( a)] a4 \bar "" r8 c d4 c \bar "" bes8 f \bar "" a[( g)] g2 \cadenzaMeasure \bar "||" \break } \bar "|." }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = \responsum
Se -- šli své -- ho du -- cha, Pa -- ne, a ob -- nov tvář -- nost ze -- mě.
\set stanza = "1."
\left Ve -- \squash leb, má \unLeft \unSquash du -- še, \markup \accent Ho -- spo -- di -- \star na! \left Ho -- \squash spo -- di -- ne, můj Bo -- že, jsi nad -- \unLeft \unSquash mí -- ru \markup \accent ve -- li -- ký!
\set stanza = "2."
\left Ve -- \squash leb -- no -- stí a vzne -- še -- no -- \unLeft \unSquash stí ses \markup \accent o -- \star děl, \left svě -- \squash tlem se ha -- líš \unLeft \unSquash ja -- ko \markup \accent plá -- štěm.
\set stanza = \responsum
Se -- šli své -- ho du -- cha, Pa -- ne, a ob -- nov tvář -- nost ze -- mě.
\set stanza = "3."
\left Ze -- \squash mi jsi za -- lo -- žil na \unLeft \unSquash je -- jích \markup \accent zá -- kla -- \star dech, \left ne -- \squash za -- ko -- lí -- sá na \unLeft \unSquash věč -- né \markup \accent vě -- ky.
\set stanza = "4."
\left O -- \squash ce -- á -- nem jsi ji při -- kryl \unLeft \unSquash ja -- ko \markup \accent ša -- \star tem, \left nad \squash ho -- ra -- mi sta -- \unLeft \unSquash nu -- ly \markup \accent vo -- dy.
\set stanza = \responsum
Se -- šli své -- ho du -- cha, Pa -- ne, a ob -- nov tvář -- nost ze -- mě.
\set stanza = "5."
\left Pra -- \squash me -- nům dá -- váš \unLeft \unSquash sté -- kat \markup \accent "v po" -- to -- \star ky, \left kte -- \squash ré ply -- nou \unLeft \unSquash me -- zi \markup \accent ho -- ra -- mi.
\set stanza = "6."
\left Po -- \squash dél nich hníz -- dí ne -- \unLeft \unSquash be -- ské \markup \accent ptac -- \star tvo, \left ve \squash vět -- vích švi -- to -- \unLeft \unSquash ří svou \markup \accent pí -- seň.
\set stanza = \responsum
Se -- šli své -- ho du -- cha, Pa -- ne, a ob -- nov tvář -- nost ze -- mě.
\set stanza = "7."
\left Ze \squash svých ko -- mor za -- vla -- \unLeft \unSquash žu -- ješ \markup \accent ho -- \star ry, \left ze -- \squash mě se sy -- tí plo -- dy \unLeft \unSquash tvé -- ho \markup \accent dí -- la.
\set stanza = "8."
\left Dá -- \squash váš růst \unLeft \unSquash trá -- vě \markup \accent pro do -- by -- \star tek, \left by -- \squash li -- nám u -- ži -- \unLeft \unSquash teč -- ným \markup \accent člo -- vě -- ku.
\set stanza = \responsum
Se -- šli své -- ho du -- cha, Pa -- ne, a ob -- nov tvář -- nost ze -- mě.
\set stanza = "9."
Jak \left čet -- \squash ná jsou tvá \unLeft \unSquash dí -- la, \markup \accent Ho -- spo -- di -- \star ne! \left Všech -- \squash no jsi \unLeft \unSquash moud -- ře \markup \accent u -- či -- nil,
\set stanza = "10."
\left ze -- \squash mě je pl -- ná \unLeft \unSquash tvé -- ho \markup \accent tvor -- \star stva. \left Ve -- \squash leb, má \unLeft \unSquash du -- še, \markup \accent Ho -- spo -- di -- na!
\set stanza = \responsum
Se -- šli své -- ho du -- cha, Pa -- ne, a ob -- nov tvář -- nost ze -- mě. } }
    >>
    \layout {}
}
