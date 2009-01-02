\version "2.11.64"

\include "../config/lib.ly"

\score {
  \new Staff {
      \time 2/4
      \key c \minor
      \relative c'' {
        r8 g g g
        ees2
        r8 f f f
        d2~
        d
        \bar "||"
      }
    }
  \layout {
    \context {
      \Staff \consists "Horizontal_bracket_engraver"
    }
  }
}
\paper {
  paper-width = 9.2\cm
  paper-height = 1.5\cm
  top-margin = -.8\cm
  left-margin = .1\cm
  tagline = 0
  indent = #0
}
