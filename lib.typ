#import "characters.typ" as chars
#import "combinators.typ" as comb

#let tag(pat) = {
  input => {
    if input.starts-with(pat) {
      let parsed = input.slice(0, pat.len())
      let rst = input.slice(pat.len())
      (rst, parsed)
    } else {
      (input, "")
    }
  }
}

#let tag-lowercase(pat) = tag(lower(pat))
#let tag-uppercase(pat) = tag(upper(pat))

#let tag-nocase(pat) = {
  input => {
    let (new_input, parsed) = tag-lowercase(pat)(input)
    if parsed != "" {
      return (new_input, parsed) 
    }

    let (new_input, parsed) = tag-uppercase(pat)(input)
    if parsed != "" {
      return (new_input, parsed) 
    }

    (input, "")
  }
}

#let punctuation(input) = comb.alt((
  tag("."),
  tag(","),
  comb.rep-res(tag(":"), chars.interpunct),
  comb.rep-res(tag(";"), chars.greek-question-mark),
  comb.rep-res(tag("'"), chars.single-quote-closing),
  comb.rep-res(tag("_"), chars.em-dash),
  comb.rep-res(tag("#"), chars.prime),
  comb.rep-res(tag("-"), chars.hyphen),
))(input)

#let whitespace(input) = comb.alt((
  tag(chars.space),
  tag(chars.no-break-space),
  tag(chars.ogham-space-mark),
  tag(chars.en-quad),
  tag(chars.em-quad ),
  tag(chars.en-space),
  tag(chars.em-space),
  tag(chars.three-per-em-space),
  tag(chars.four-per-em-space),
  tag(chars.six-per-em-space),
  tag(chars.figure-space),
  tag(chars.punctuation-space),
  tag(chars.thin-space),
  tag(chars.hair-space),
  tag(chars.narrow-no-break-space),
  tag(chars.medium-mathematical-space),
  tag(chars.ideographic-space),
))(input)  

#let eof(input) = {
  if input == "" {
    (input, chars.end-of-text)
  } else {
    (input, "")
  }
}

#let end-of-word(input) = comb.alt((
  eof,
  whitespace,
  punctuation
))(input)

#let lowercase-sigma(tag-fn) = comb.alt((
  comb.rep-res(tag-fn("s3"), "ϲ"),
  comb.rep-res(comb.alt((
    tag-fn("s2"),
    tag-fn("j"),
    comb.all((tag-fn("s"), comb.peek(end-of-word)))
  )), "ς"),
  comb.rep-res(comb.alt((
    tag-fn("s1"),
    tag-fn("s")
  )), "σ")
))

#let lowercase-letter(tag-fn: tag-nocase) = comb.alt((
  comb.rep-res(tag-fn("a"), "α"),
  comb.rep-res(tag-fn("b"), "β"),
  comb.rep-res(tag-fn("g"), "γ"),
  comb.rep-res(tag-fn("d"), "δ"),
  comb.rep-res(tag-fn("e"), "ε"),
  comb.rep-res(tag-fn("v"), "ϝ"),
  comb.rep-res(tag-fn("z"), "ζ"),
  comb.rep-res(tag-fn("h"), "η"),
  comb.rep-res(tag-fn("q"), "θ"),
  comb.rep-res(tag-fn("i"), "ι"),
  comb.rep-res(tag-fn("k"), "κ"),
  comb.rep-res(tag-fn("l"), "λ"),
  comb.rep-res(tag-fn("m"), "μ"),
  comb.rep-res(tag-fn("n"), "ν"),
  comb.rep-res(tag-fn("c"), "ξ"),
  comb.rep-res(tag-fn("o"), "ο"),
  comb.rep-res(tag-fn("p"), "π"),
  comb.rep-res(tag-fn("r"), "ρ"),
  comb.rep-res(tag-fn("t"), "τ"),
  comb.rep-res(tag-fn("u"), "υ"),
  comb.rep-res(tag-fn("f"), "φ"),
  comb.rep-res(tag-fn("x"), "χ"),
  comb.rep-res(tag-fn("y"), "ψ"),
  comb.rep-res(tag-fn("w"), "ω"),
  lowercase-sigma(tag-fn)
))

// Breathing marks are also technically diacritics,
// but they're treated a bit different when parsing.
// TODO: explain why
#let breathing(input) = comb.alt((
  comb.rep-res(tag(")"), chars.smooth-breathing),
  comb.rep-res(tag("("), chars.rough-breathing)
))(input)

#let diacritic(input) = comb.alt((
  comb.rep-res(tag("/"), chars.acute-accent),
  comb.rep-res(tag("="), chars.circumflex-accent),
  comb.rep-res(tag("\\"), chars.grave-accent),
  comb.rep-res(tag("+"), chars.diaeresis),
  comb.rep-res(tag("?"), chars.combining-dot-below),
  comb.rep-res(tag("&"), chars.macron),
  comb.rep-res(tag("'"), chars.breve)
))(input)

#let iota-subscript(input) = comb.rep-res(
  tag("|"),
  chars.iota-subscript
)(input)

#let is-consonant(letter) = {
  let letter = lower(letter)
  let consonants = ("β", "γ", "δ", "ζ", "θ", "κ", "λ", "μ", "ν", "ξ", "π", "σ", "ς", "τ", "φ", "χ", "ψ")
  consonants.contains(letter)
}

#let lowercase-with-diacritics(letter-parser: lowercase-letter()) = {
  input => {
    let (input, letter) = letter-parser(input) 

    if letter == "" {
      return (input, "")  
    }

    let is-not-consonant = not is-consonant(letter)

    // These are optional
    let (input, breathing) = comb.cond(is-not-consonant, breathing)(input)
    let (input, accent) = comb.cond(is-not-consonant, comb.many(diacritic))(input)
    let (input, iota-subscript) = comb.cond(is-not-consonant, iota-subscript)(input)

    let result = letter + breathing + accent + iota-subscript
    (input, result)
  }
}

#let uppercase-with-diacritics(input) = {
  let (input, asterisk) = tag("*")(input)

  if asterisk == "" {
    return (input, "") 
  }

  // These are optional
  let (input, breathing) = breathing(input)
  let (input, accent) = comb.many(diacritic)(input)

  let (input, letter) = comb.map-res(lowercase-letter(), upper)(input)
  if letter == "" {
    return (input, "")
  }

  // Optional
  let (input, iota-subscript) = iota-subscript(input)

  let result = letter + breathing + accent + iota-subscript
  (input, result)
}

#let uppercase-with-auto-capitalization(input) = lowercase-with-diacritics(
  letter-parser: comb.map-res(lowercase-letter(tag-fn: tag-uppercase), upper)
)(input)

#let lowercase-with-auto-capitalization(input) = lowercase-with-diacritics(
  letter-parser: lowercase-letter(tag-fn: tag-lowercase)
)(input)

#let letter(auto-capitalization) = {
  input => {
    if auto-capitalization {
      comb.alt((
        uppercase-with-auto-capitalization,
        lowercase-with-auto-capitalization
      ))(input)
    } else {
      comb.alt((
        uppercase-with-diacritics,
        lowercase-with-diacritics(),
      ))(input)
    }
  }
}

#let betacode(input, auto-capitalization: false) = {
  assert.eq(type(input), str, message: "Please use #betacode with a str, not a content.")

  // Unescape input
  input = input
    .replace("\n", "\\n")
    .replace("\r", "\\r")
    .replace("\t", "\\t")

  let (input, parsed) = comb.many(
    comb.alt((
      letter(auto-capitalization),
      punctuation,
      whitespace
    ))
  )(input)

  assert.eq(input, "", message: "Error while parsing betacode, there might be an incomplete letter at the end of the input: `" + input + "`")

  text(parsed)
}
