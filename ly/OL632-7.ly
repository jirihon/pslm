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
        \new Voice = "melody" { \cadenzaOn \key f \major \relative { c''8 d c d c4. a8 \cadenzaMeasure \bar "|" f bes a bes \bar "" c bes \bar "" a g g4 r \cadenzaMeasure \bar "||" \break }
\relative { f'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" a' g g g2 \cadenzaMeasure \bar "|" c,8 g'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes f8 g \bar "" bes a a a4 r \cadenzaMeasure \bar "||" \break }
\relative { f'\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" a' g g4 \cadenzaMeasure \bar "|" r8 c,8 g'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes f8 g \bar "" bes a a a2 \cadenzaMeasure \bar "||" \break }
\relative { c''8 d c d c4. a8 \cadenzaMeasure \bar "|" f bes a bes \bar "" c bes \bar "" a g g4 r \cadenzaMeasure \bar "||" \break }
\relative { f'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" a'[( g)] g4 \cadenzaMeasure \bar "|" r8 c,8 g'\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes f8 g \bar "" bes a a a4 r \cadenzaMeasure \bar "||" \break }
\relative { f'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" a'[( g)] g4 r \cadenzaMeasure \bar "|" c,8 g'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes f8 g \bar "" bes[( a)] a4 r \cadenzaMeasure \bar "||" \break }
\relative { c''8 d c d c4. a8 \cadenzaMeasure \bar "|" f bes a bes \bar "" c bes \bar "" a g g4 r \cadenzaMeasure \bar "||" \break }
\relative { f'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" a'[( g)] g4 \cadenzaMeasure \bar "|" r8 c,8 g'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes f8 g \bar "" bes a a2 \cadenzaMeasure \bar "||" \break }
\relative { f'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" a' g g4 \cadenzaMeasure \bar "|" r8 c,8 g'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes f8 g \bar "" bes a a a2 \cadenzaMeasure \bar "||" \break }
\relative { c''8 d c d c4. a8 \cadenzaMeasure \bar "|" f bes a bes \bar "" c bes \bar "" a g g4 r \cadenzaMeasure \bar "||" \break }
\relative { f'\breve*1/16 \hideNotes \breve*1/16 \bar "" \breve*1/16 \bar "" \breve*1/16 \breve*1/16 \bar "" \unHideNotes e8 d \bar "" a' g g4 \cadenzaMeasure \bar "|" r8 c,8 g'\breve*1/16 \hideNotes \breve*1/16 \breve*1/16 \bar "" \unHideNotes f8 g \bar "" bes a a a4 \cadenzaMeasure \bar "||" \break }
\relative { r8 f'8 f\breve*1/16 \hideNotes \breve*1/16 \bar "" \unHideNotes e8 d \bar "" a' g g g4 r \cadenzaMeasure \bar "|" c,8 g'8 f8 g \bar "" bes a a a2 \cadenzaMeasure \bar "||" \break }
\relative { c''8 d c d c4. a8 \cadenzaMeasure \bar "|" f bes a bes \bar "" c bes \bar "" a g g4 r \cadenzaMeasure \bar "||" \break } \bar "|." }
        \new Lyrics \lyricsto "melody" { \lyricmode { \set stanza = \responsum
Na vě -- ky chci zpí -- vat o Ho -- spo -- di -- no -- vých mi -- lo -- stech.
\set stanza = "1."
\left Smlou -- \squash vu jsem sjed -- nal \unLeft \unSquash se svým \markup \accent vy -- vo -- le -- \star ným, pří -- \left sa -- \squash hal jsem Da -- vi -- do -- vi, \unLeft \unSquash své -- mu \markup \accent slu -- žeb -- ní -- ku:
\set stanza = "2."
\left Tvůj \squash rod za -- \unLeft \unSquash ji -- stím \markup \accent na vě -- \star ky a \left tvůj \squash trůn zbu -- du -- ji na \unLeft \unSquash všech -- na \markup \accent po -- ko -- le -- ní.“
\set stanza = \responsum
Na vě -- ky chci zpí -- vat o Ho -- spo -- di -- no -- vých mi -- lo -- stech.
\set stanza = "3."
\left „Na \squash vě -- ky mu za -- cho -- \unLeft \unSquash vám svou \markup \accent mi -- \star lost, má \left smlou -- \squash va "s ním" \unLeft \unSquash pla -- tit \markup \accent ne -- pře -- sta -- ne.
\set stanza = "4."
\left Dám \squash věč -- né tr -- vá -- ní \unLeft \unSquash je -- ho \markup \accent ro -- \star du, je -- \left ho \squash trůn bu -- de ja -- \unLeft \unSquash ko věk \markup \accent ne -- bes.
\set stanza = \responsum
Na vě -- ky chci zpí -- vat o Ho -- spo -- di -- no -- vých mi -- lo -- stech.
\set stanza = "5."
\left Je -- \squash stli -- že je -- ho sy -- no -- vé o -- pu -- \unLeft \unSquash stí můj \markup \accent zá -- \star kon a \left ne -- \squash bu -- dou jed -- nat po -- \unLeft \unSquash dle mých \markup \accent pří -- ka -- zů,
\set stanza = "6."
\left je -- \squash stli -- že po -- skvr -- ní má \unLeft \unSquash u -- sta -- \markup \accent no -- ve -- \star ní a \left ne -- \squash za -- cho -- va -- \unLeft \unSquash jí má \markup \accent při -- ká -- zá -- ní:
\set stanza = \responsum
Na vě -- ky chci zpí -- vat o Ho -- spo -- di -- no -- vých mi -- lo -- stech.
\set stanza = "7."
\left Po -- \squash tre -- stám me -- tlou \unLeft \unSquash je -- jich \markup \accent ne -- pra -- \star vost a \left ra -- \squash na -- mi \unLeft \unSquash je -- jich \markup \accent pro -- vi -- ně -- ní.
\set stanza = "8."
Svou \left mi -- \squash lost \unLeft \unSquash mu však \markup \accent ne -- o -- dej -- \star mu a svou věr -- nost \markup \accent ne -- po -- ru -- ším.“
\set stanza = \responsum
Na vě -- ky chci zpí -- vat o Ho -- spo -- di -- no -- vých mi -- lo -- stech. } }
    >>
    \layout {}
}