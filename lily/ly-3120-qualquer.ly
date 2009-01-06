\version "2.11.64"

\include "../config/lib.ly"

\score {
  \new Staff {
    \time 4/4
    \relative c'' {
      \clef bass
      g4. a,,8
      g'2
      aes,
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
  paper-width = 5.2\cm
  paper-height = 1.5\cm
  line-width = 6.1\cm
  top-margin = -.5\cm
  left-margin = .1\cm
  tagline = 0
  indent = 0
}
