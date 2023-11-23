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
        \new Voice = "melody" { \cadenzaOn \key f \major \relative { c'8 d e f4 f8 \bar "" f e d \bar "" g g g4 \cadenzaMeasure \bar "|" bes8 c bes a g4 \bar "" a8[( f)] f4 \cadenzaMeasure \bar "||" \break }
\relative { r8 bes' bes\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes c\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes \bar "" a8 a a4 r \cadenzaMeasure \bar "|" g\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes a\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" f8 f4. r4 \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes c\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" \bar "" a4 a r \cadenzaMeasure \bar "|" g\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes a\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" f8 f f4 r \cadenzaMeasure \bar "||" \break }
\relative { c'8 d e f4 f8 \bar "" f e d \bar "" g g g4 \cadenzaMeasure \bar "|" bes8 c bes a g4 \bar "" a8[( f)] f4 \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes c\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" a8 a a4 r \cadenzaMeasure \bar "|" g\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes a\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes \bar "" f8 f f4 r \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes c\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes \bar "" a8 a a4 r \cadenzaMeasure \bar "|" g\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes a\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes \bar "" f8 f r4 \cadenzaMeasure \bar "||" \break }
\relative { c'8 d e f4 f8 \bar "" f e d \bar "" g g g4 \cadenzaMeasure \bar "|" bes8 c bes a g4 \bar "" a8[( f)] f4 \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes c\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" a8 a r4 \cadenzaMeasure \bar "|" g\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes a\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes \bar "" f8 f r4 \cadenzaMeasure \bar "||" \break }
\relative { r8 bes' c\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" a8 a4. r4 \cadenzaMeasure \bar "|" g\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes a\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" f8 f f4 r \cadenzaMeasure \bar "||" \break }
\relative { c'8 d e f4 f8 \bar "" f e d \bar "" g g g4 \cadenzaMeasure \bar "|" bes8 c bes a g4 \bar "" a8[( f)] f4 \cadenzaMeasure \bar "||" \break }
\relative { bes'\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes c\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" \bar "" a4 a \cadenzaMeasure \bar "|" r8 g g\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes a\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes \bar "" f8 f4 \cadenzaMeasure \bar "||" \break }
\relative { r8 bes' c\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" a8 a a4 \cadenzaMeasure \bar "|" r8 g g\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes a\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" f8 f r4 \cadenzaMeasure \bar "||" \break }
\relative { c'8 d e f4 f8 \bar "" f e d \bar "" g g g4 \cadenzaMeasure \bar "|" bes8 c bes a g4 \bar "" a8[( f)] f4 \cadenzaMeasure \bar "||" \break } \bar "|." }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = \responsum
Vel -- kou věc "s ná" -- mi u -- či -- nil Ho -- spo -- din, na -- pl -- ni -- la nás ra -- dost.
\set stanza = "1."
Když \left Ho -- \squash spo -- din při -- vá -- děl \left \unLeft \unSquash si -- \squash ón -- ské \unLeft \unSquash \markup \accent za -- jat -- \star ce, \left by -- \squash li jsme \left \unLeft \unSquash ja -- \squash ko \unLeft \unSquash \markup \accent ve snách.
\set stanza = "2."
\left Teh -- \squash dy na -- še ú -- sta by -- la \left \unLeft \unSquash pl -- \squash ná \unLeft \unSquash \markup \accent smí -- \star chu \left a \squash náš ja -- zyk \left \unLeft \unSquash pl -- \squash ný \unLeft \unSquash \markup \accent já -- so -- tu.
\set stanza = \responsum
Vel -- kou věc "s ná" -- mi u -- či -- nil Ho -- spo -- din, na -- pl -- ni -- la nás ra -- dost.
\set stanza = "3."
\left Teh -- \squash dy se ří -- ka -- lo \left \unLeft \unSquash me -- \squash zi \unLeft \unSquash \markup \accent po -- ha -- \star ny: \left „Vel -- \squash kou věc "s ni" -- mi \left \unLeft \unSquash u -- \squash či -- nil \unLeft \unSquash \markup \accent Ho -- spo -- din!“
\set stanza = "4."
\left A -- \squash no, vel -- kou věc "s ná" -- mi \left \unLeft \unSquash u -- \squash či -- nil \unLeft \unSquash \markup \accent Ho -- spo -- \star din, \left na -- \squash pl -- \left \unLeft \unSquash ni -- \squash la nás \unLeft \unSquash \markup \accent ra -- dost.
\set stanza = \responsum
Vel -- kou věc "s ná" -- mi u -- či -- nil Ho -- spo -- din, na -- pl -- ni -- la nás ra -- dost.
\set stanza = "5."
\left Ho -- \squash spo -- di -- ne, \left \unLeft \unSquash změň \squash náš \unLeft \unSquash \markup \accent o -- \star sud, \left ja -- \squash ko se mě -- ní ú -- do -- lí \left \unLeft \unSquash na \squash ji -- hu \unLeft \unSquash \markup \accent ze -- mě.
\set stanza = "6."
Kdo \left se -- \squash jí \unLeft \unSquash \markup \accent "v sl" -- \star zách, \left žnou -- \squash ti \left \unLeft \unSquash bu -- \squash dou \unLeft \unSquash \markup \accent "s já" -- so -- tem.
\set stanza = \responsum
Vel -- kou věc "s ná" -- mi u -- či -- nil Ho -- spo -- din, na -- pl -- ni -- la nás ra -- dost.
\set stanza = "7."
\left Vy -- \squash chá -- \left \unLeft \unSquash ze -- \squash jí \unLeft \unSquash \markup \accent "s plá" -- \star čem, když \left ne -- \squash sou \left \unLeft \unSquash se -- \squash me -- no \unLeft \unSquash \markup \accent "k se" -- tí:
\set stanza = "8."
při -- \left jdou \squash však \unLeft \unSquash \markup \accent "s já" -- so -- \star tem a \left při -- \squash ne -- \left \unLeft \unSquash sou \squash své \unLeft \unSquash \markup \accent sno -- py.
\set stanza = \responsum
Vel -- kou věc "s ná" -- mi u -- či -- nil Ho -- spo -- din, na -- pl -- ni -- la nás ra -- dost. } }
    >>
    \layout {}
}
