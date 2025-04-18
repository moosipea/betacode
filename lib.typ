#import "characters.typ"

#let tag(grapheme) = {
  input => {
    if input.starts-with(grapheme) {
      let parsed = input.slice(0, grapheme.len())
      let rst = input.slice(grapheme.len())
      (rst, parsed)
    } else {
      (input, "")
    }
  }
}

#let tag-nocase(grapheme) = {
  input => {
    let (new_input, parsed) = tag(lower(grapheme))(input)
    if parsed != "" {
      return (new_input, parsed) 
    }

    let (new_input, parsed) = tag(upper(grapheme))(input)
    if parsed != "" {
      return (new_input, parsed) 
    }

    (input, "")
  }
}

#let alt(parsers) = {
  input => {
    for parser in parsers {
      let (new_input, parsed) = parser(input) 
      if parsed != "" {
        return (new_input, parsed)
      }
    }
    (input, "")
  }
}

#let all(parsers) = {
  input => {
    let local_input = input
    let parsed = ()
    
    for parser in parsers {
      let (new_input, new_parsed) = parser(local_input)

      if new_parsed == "" {
        return (input, "")
      }

      local_input = new_input
      parsed.push(new_parsed)
    }

    // TODO: join parsed
    (local_input, parsed)
  }
}

#let map-res(parser, fn) = {
  input => {
    let (new_input, parsed) = parser(input)
    if parsed != "" {
      (new_input, fn(parsed))
    } else {
      (new_input, "") 
    }
  }
}

#let rep-res(parser, value) = map-res(parser, _ => value)

#let peek(parser) = {
  input => {
    let (_, parsed) = parser(input)
    (input, parsed)
  }
}

#let preceded(by, parser) = {
  input => {
    let (input, parsed) = tag(by)(input)
    if parsed == "" {
      return (input, "")
    }
    parser(input)
  }
} 

#let punctuation(input) = alt((
  tag("."),
  tag(","),
  rep-res(tag(":"), characters.interpunct),
  rep-res(tag(";"), characters.greek-question-mark),
  rep-res(tag("'"), characters.single-quote-closing),
  rep-res(tag("_"), characters.em-dash),
  rep-res(tag("#"), characters.prime),
))(input)

#let whitespace(input) = alt((
  tag(characters.space),
  tag(characters.no-break-space),
  tag(characters.ogham-space-mark),
  tag(characters.en-quad),
  tag(characters.em-quad ),
  tag(characters.en-space),
  tag(characters.em-space),
  tag(characters.three-per-em-space),
  tag(characters.four-per-em-space),
  tag(characters.six-per-em-space),
  tag(characters.figure-space),
  tag(characters.punctuation-space),
  tag(characters.thin-space),
  tag(characters.hair-space),
  tag(characters.narrow-no-break-space),
  tag(characters.medium-mathematical-space),
  tag(characters.ideographic-space),
))(input)  

#let eof(input) = {
  if input == "" {
    (input, characters.end-of-text)
  } else {
    (input, "")
  }
}

#let end-of-word(input) = alt((
  eof,
  whitespace,
  punctuation
))(input)

#let lowercase-sigma(input) = alt((
  rep-res(alt((
    all((tag-nocase("s"), peek(end-of-word))),
    tag-nocase("s2"),
    tag-nocase("j")
  )), "ς"),
  rep-res(alt((
    tag-nocase("s"),
    tag-nocase("s1")
  )), "σ"),
  rep-res(tag-nocase("s3"), "ϲ")
))(input)

#let lowercase-letter(input) = alt((
  rep-res(tag-nocase("a"), "α"),
  rep-res(tag-nocase("b"), "β"),
  rep-res(tag-nocase("g"), "γ"),
  rep-res(tag-nocase("d"), "δ"),
  rep-res(tag-nocase("e"), "ε"),
  rep-res(tag-nocase("v"), "ϝ"),
  rep-res(tag-nocase("z"), "ζ"),
  rep-res(tag-nocase("h"), "η"),
  rep-res(tag-nocase("q"), "θ"),
  rep-res(tag-nocase("i"), "ι"),
  rep-res(tag-nocase("k"), "κ"),
  rep-res(tag-nocase("l"), "λ"),
  rep-res(tag-nocase("m"), "μ"),
  rep-res(tag-nocase("n"), "ν"),
  rep-res(tag-nocase("c"), "ξ"),
  rep-res(tag-nocase("o"), "ο"),
  rep-res(tag-nocase("p"), "π"),
  rep-res(tag-nocase("r"), "ρ"),
  rep-res(tag-nocase("t"), "τ"),
  rep-res(tag-nocase("u"), "υ"),
  rep-res(tag-nocase("f"), "φ"),
  rep-res(tag-nocase("x"), "χ"),
  rep-res(tag-nocase("y"), "ψ"),
  rep-res(tag-nocase("w"), "ω"),
  lowercase-sigma
))(input)

#let breathing(input) = alt((
  rep-res(tag(")"), characters.smooth-breathing),
  rep-res(tag("("), characters.rough-breathing)
))(input)

#let diacritic(input) = alt((
  rep-res(tag("/"), characters.acute-accent),
  rep-res(tag("="), characters.circumflex-accent),
  rep-res(tag("\\"), characters.grave-accent),
  rep-res(tag("+"), characters.diaeresis),
  rep-res(tag("&"), characters.macron),
  rep-res(tag("'"), characters.breve)
))(input)

#let iota-subscript(input) = rep-res(tag("|"), characters.iota-subscript-char)(input)

#let many(parser) = {
  input => {
    let results = ()
    let (input, parsed) = parser(input)
    
    while parsed != "" {
      results.push(parsed)
      (input, parsed) = parser(input)
    }

    (input, results.join())
  }
}

#let lowercase-with-diacritics(input) = {
  let (input, letter) = lowercase-letter(input) 

  if letter == "" {
    return (input, "")  
  }

  // These are optional
  let (input, breathing) = breathing(input)
  let (input, accent) = many(diacritic)(input)
  let (input, iota-subscript) = iota-subscript(input)

  let result = letter + breathing + accent + iota-subscript
  (input, result)
}

#let uppercase-with-diacritics(input) = {
  let (input, asterisk) = tag("*")(input)

  if asterisk == "" {
    return (input, "") 
  }

  // These are optional
  let (input, breathing) = breathing(input)
  let (input, accent) = many(diacritic)(input)

  let (input, letter) = map-res(lowercase-letter, upper)(input)
  if letter == "" {
    return (input, "")
  }

  // Optional
  let (input, iota-subscript) = iota-subscript(input)

  let result = letter + breathing + accent + iota-subscript
  (input, result)
}

#let betacode(body) = text(
  many(
    alt((
      uppercase-with-diacritics,
      lowercase-with-diacritics,
      punctuation
    ))
  )(str(body.text)).at(1)
)

#let input = "MAXAI/RA&S"

Betacode: #input \
Unicode: #betacode[#input]

