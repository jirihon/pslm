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
        \new Voice = "melody" { \cadenzaOn \key es \major \relative { g'8 g f4 es \bar "" as8 as g f2 \cadenzaMeasure \bar "|" g8 bes bes bes4 r \cadenzaMeasure \bar "||" \break }
\relative { c''\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes d8 c \bar "" c bes bes4 r \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes8 as \bar "" g f f f4 \cadenzaMeasure \bar "||" \break }
\relative { r8 c'' d8 c \bar "" c[( bes)] bes4 \cadenzaMeasure \bar "|" r8 as bes8 as \bar "" g[( f)] f4 r \cadenzaMeasure \bar "||" \break }
\relative { g'8 g f4 es \bar "" as8 as g f2 \cadenzaMeasure \bar "|" g8 bes bes bes4 r \cadenzaMeasure \bar "||" \break }
\relative { c''\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes d8 c \bar "" c[( bes)] bes2 \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes8 as \bar "" g f f4 r \cadenzaMeasure \bar "||" \break }
\relative { c''\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes d8 c \bar "" c bes bes4 r \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes8 as \bar "" g[( f)] f4 r \cadenzaMeasure \bar "||" \break }
\relative { g'8 g f4 es \bar "" as8 as g f2 \cadenzaMeasure \bar "|" g8 bes bes bes4 r \cadenzaMeasure \bar "||" \break }
\relative { c''\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes d8 c \bar "" c bes bes bes2 \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes8 as \bar "" g f f f4 r \cadenzaMeasure \bar "||" \break }
\relative { c''\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes d8 c \bar "" c[( bes)] bes4 r \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes8 as \bar "" g f f f2 \cadenzaMeasure \bar "||" \break }
\relative { g'8 g f4 es \bar "" as8 as g f2 \cadenzaMeasure \bar "|" g8 bes bes bes4 r \cadenzaMeasure \bar "||" \break }
\relative { c''\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes d8 c \bar "" c[( bes)] bes2 \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes bes8 as \bar "" g[( f)] f4 \cadenzaMeasure \bar "||" \break }
\relative { r8 c''8 c\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes d8 c \bar "" c[( bes)] bes4 r \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes8 as \bar "" g f f4 r \cadenzaMeasure \bar "||" \break }
\relative { g'8 g f4 es \bar "" as8 as g f2 \cadenzaMeasure \bar "|" g8 bes bes bes4 r \cadenzaMeasure \bar "||" \break }
\relative { r8 c''8 c\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes d8 c \bar "" c bes bes bes4 r \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes8 as \bar "" g[( f)] f4 \cadenzaMeasure \bar "||" \break }
\relative { r8 c'' d8[( c)] \bar "" c bes bes4 r \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes8 as \bar "" g f f4 r \cadenzaMeasure \bar "||" \break }
\relative { g'8 g f4 es \bar "" as8 as g f2 \cadenzaMeasure \bar "|" g8 bes bes bes4 r \cadenzaMeasure \bar "||" \break } \bar "|." }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = \responsum
Za -- chraň mě svou sli -- tov -- no -- stí, Ho -- spo -- di -- ne.
\set stanza = "1."
\left "K to" -- \squash bě se u -- tí -- kám, Ho -- spo -- di -- \optionalBreath ne, ať nej -- sem za -- \unLeft \unSquash han -- ben \markup \accent na vě -- \star ky, \left vy -- \squash svo -- boď \unLeft \unSquash mě, jsi \markup \accent spra -- ve -- dli -- vý!
\set stanza = "2."
Po -- přej mi \markup \accent slu -- \star chu, rych -- le mě \markup \accent za -- chraň!
\set stanza = \responsum
Za -- chraň mě svou sli -- tov -- no -- stí, Ho -- spo -- di -- ne.
\set stanza = "3."
\left Buď \squash mi o -- \unLeft \unSquash chran -- nou \markup \accent ská -- \star lou, \left o -- \squash pev -- ně -- nou tvr -- \unLeft \unSquash zí "k mé" \markup \accent zá -- chra -- ně,
\set stanza = "4."
\left vždyť \squash ty jsi má \unLeft \unSquash ská -- la \markup \accent a má \star tvrz, \left pro \squash své jmé -- no mě po -- ve -- deš a \unLeft \unSquash bu -- deš \markup \accent ří -- dit.
\set stanza = \responsum
Za -- chraň mě svou sli -- tov -- no -- stí, Ho -- spo -- di -- ne.
\set stanza = "5."
\left Vy -- \squash pro -- stíš mě "z léč" -- ky \unLeft \unSquash pro mě \markup \accent na -- stra -- že -- \star né, \left ne -- \squash boť ty \unLeft \unSquash jsi mé \markup \accent ú -- to -- či -- ště.
\set stanza = "6."
\left Do \squash tvých ru -- kou svě -- řu -- ji \unLeft \unSquash své -- ho \markup \accent du -- \star cha, \left Ho -- \squash spo -- di -- ne, věr -- ný Bo -- že, \unLeft \unSquash ty mě \markup \accent vy -- svo -- bo -- díš.
\set stanza = \responsum
Za -- chraň mě svou sli -- tov -- no -- stí, Ho -- spo -- di -- ne.
\set stanza = "7."
\left Já \squash však, Ho -- spo -- di -- ne, \unLeft \unSquash "v te" -- be \markup \accent dou -- \star fám, \left ří -- \squash kám: \unLeft \unSquash Ty jsi \markup \accent můj Bůh!
\set stanza = "8."
Můj \left o -- \squash sud je \unLeft \unSquash ve tvé \markup \accent ru -- \star ce, \left vy -- \squash svo -- boď mě "z mo" -- ci mých ú -- \unLeft \unSquash hlav -- ních \markup \accent ne -- přá -- tel!
\set stanza = \responsum
Za -- chraň mě svou sli -- tov -- no -- stí, Ho -- spo -- di -- ne.
\set stanza = "9."
Jak \left ne -- \squash smír -- ná je tvá dob -- ro -- \unLeft \unSquash ti -- vost, \markup \accent Ho -- spo -- di -- \star ne, \left u -- \squash cho -- val jsi ji těm, kdo \unLeft \unSquash se tě \markup \accent bo -- jí,
\set stanza = "10."
po -- přá -- \markup \accent váš ji \star těm, \left kdo \squash se "k to" -- bě u -- tí -- \unLeft \unSquash ka -- jí \markup \accent před lid -- mi.
\set stanza = \responsum
Za -- chraň mě svou sli -- tov -- no -- stí, Ho -- spo -- di -- ne. } }
    >>
    \layout {}
}