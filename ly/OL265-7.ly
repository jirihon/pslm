\version "2.24.0"
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
        \new Voice = "melody" { \cadenzaOn \key es \major \relative { g'8 f es as f r \cadenzaMeasure \bar "|" f f4 \bar "" f8 g as \cadenzaMeasure \bar "|" bes4. g4 r8 \cadenzaMeasure \bar "||" \break } }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = \responsum
Vy -- slyš mě, Bo -- že, ve své ve -- li -- ké lá -- sce! } }
    >>
    \layout {}
}

\score {
    <<
        \new Voice = "melody" { \cadenzaOn \key es \major \relative { c''\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes d\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" bes8 bes bes4 r \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes \bar "" g8 g g g2 \cadenzaMeasure \bar "||" \break } }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = "1."
\left Ví -- \squash ce než mám \left \unLeft \unSquash vla -- \squash sů \unLeft \unSquash \markup \accent na hla -- \star vě, \left je \squash těch, kte -- ří mě \left \unLeft \unSquash bez \squash dů -- vo -- du \unLeft \unSquash \markup \accent ne -- ná -- vi -- dí, } }
    >>
    \layout {}
}

\score {
    <<
        \new Voice = "melody" { \cadenzaOn \key es \major \relative { c''\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes d\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" bes8 bes r4 \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes \bar "" g8 g g g4 r \cadenzaMeasure \bar "||" \break } }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = "2."
\left sil -- \squash něj -- ší \left \unLeft \unSquash než \squash mé \unLeft \unSquash \markup \accent ko -- \star sti \left jsou \squash ti, kte -- ří mě \left \unLeft \unSquash stí -- \squash ha -- jí \unLeft \unSquash \markup \accent bez pří -- či -- ny. } }
    >>
    \layout {}
}

\score {
    <<
        \new Voice = "melody" { \cadenzaOn \key es \major \relative { g'8 f es as f r \cadenzaMeasure \bar "|" f f4 \bar "" f8 g as \cadenzaMeasure \bar "|" bes4. g4 r8 \cadenzaMeasure \bar "||" \break } }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = \responsum
Vy -- slyš mě, Bo -- že, ve své ve -- li -- ké lá -- sce! } }
    >>
    \layout {}
}

\score {
    <<
        \new Voice = "melody" { \cadenzaOn \key es \major \relative { r8 c''8 c\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes d\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes \bar "" bes8 bes bes4 \cadenzaMeasure \bar "|" r8 as8 as\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" g8 g r4 \cadenzaMeasure \bar "||" \break } }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = "3."
Nechť \left nej -- \squash sou \left \unLeft \unSquash kvů -- \squash li mně \unLeft \unSquash \markup \accent zkla -- má -- \star ni, kdo \left dou -- \squash fa -- jí \left \unLeft \unSquash "v te" -- \squash be, \unLeft \unSquash \markup \accent Pa -- ne, } }
    >>
    \layout {}
}

\score {
    <<
        \new Voice = "melody" { \cadenzaOn \key es \major \relative { r8 c''8 c\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes d\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes \bar "" bes8 bes bes4 \cadenzaMeasure \bar "|" r8 as8 as\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" g8 g g g4 \cadenzaMeasure \bar "||" \break } }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = "4."
nechť \left kvů -- \squash li mně nej -- sou \left \unLeft \unSquash zba -- \squash ve -- ni \unLeft \unSquash \markup \accent na -- dě -- \star je, kdo \left hle -- \squash da -- jí te -- be, \left \unLeft \unSquash Bo -- \squash že \unLeft \unSquash \markup \accent Iz -- ra -- e -- le! } }
    >>
    \layout {}
}

\score {
    <<
        \new Voice = "melody" { \cadenzaOn \key es \major \relative { r8 c''8 c\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes d\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" bes8 bes bes4 r \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes \bar "" g4 g2 \cadenzaMeasure \bar "||" \break } }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = "5."
Vždyť \left pro \squash te -- be jsem \left \unLeft \unSquash sná -- \squash šel \unLeft \unSquash \markup \accent po -- tu -- \star pu, \left po -- \squash ha -- na \left \unLeft \unSquash po -- \squash kry -- la \unLeft \unSquash \markup \accent mou tvář. } }
    >>
    \layout {}
}

\score {
    <<
        \new Voice = "melody" { \cadenzaOn \key es \major \relative { g'8 f es as f r \cadenzaMeasure \bar "|" f f4 \bar "" f8 g as \cadenzaMeasure \bar "|" bes4. g4 r8 \cadenzaMeasure \bar "||" \break } }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = \responsum
Vy -- slyš mě, Bo -- že, ve své ve -- li -- ké lá -- sce! } }
    >>
    \layout {}
}

\score {
    <<
        \new Voice = "melody" { \cadenzaOn \key es \major \relative { c''\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes d\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" bes8 bes bes bes2 \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes \bar "" g8 g r4 \cadenzaMeasure \bar "||" \break } }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = "6."
\left Vy -- \squash svo -- boď mě od těch, \left \unLeft \unSquash kte -- \squash ří \unLeft \unSquash \markup \accent ne -- ná -- vi -- \star dí, \left vy -- \squash táh -- ni mě \left \unLeft \unSquash "z hlu" -- \squash bo -- ké \unLeft \unSquash \markup \accent vo -- dy! } }
    >>
    \layout {}
}

\score {
    <<
        \new Voice = "melody" { \cadenzaOn \key es \major \relative { c''\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes d\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" bes4 bes r \cadenzaMeasure \bar "|" as\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes bes\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes \bar "" g4 g r \cadenzaMeasure \bar "||" \break } }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = "7."
\left Pí -- \squash sní bu -- du sla -- vit \left \unLeft \unSquash Bo -- \squash ží \unLeft \unSquash \markup \accent jmé -- \star no, \left ve -- \squash le -- bit je bu -- du \left \unLeft \unSquash zpě -- \squash vem \unLeft \unSquash \markup \accent chvá -- ly. } }
    >>
    \layout {}
}

\score {
    <<
        \new Voice = "melody" { \cadenzaOn \key es \major \relative { g'8 f es as f r \cadenzaMeasure \bar "|" f f4 \bar "" f8 g as \cadenzaMeasure \bar "|" bes4. g4 r8 \cadenzaMeasure \bar "||" \break } \bar "|." }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = \responsum
Vy -- slyš mě, Bo -- že, ve své ve -- li -- ké lá -- sce! } }
    >>
    \layout {}
}
