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

#let tag-nocase(pat) = {
  input => {
    let (new_input, parsed) = tag(lower(pat))(input)
    if parsed != "" {
      return (new_input, parsed) 
    }

    let (new_input, parsed) = tag(upper(pat))(input)
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
  comb.rep-res(tag("-"), chars.hyphen)
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

#let lowercase-sigma(input) = comb.alt((
  comb.rep-res(tag-nocase("s3"), "ϲ"),
  comb.rep-res(comb.alt((
    tag-nocase("s2"),
    tag-nocase("j"),
    comb.all((tag-nocase("s"), comb.peek(end-of-word)))
  )), "ς"),
  comb.rep-res(comb.alt((
    tag-nocase("s1"),
    tag-nocase("s")
  )), "σ")
))(input)

#let lowercase-letter(input) = comb.alt((
  comb.rep-res(tag-nocase("a"), "α"),
  comb.rep-res(tag-nocase("b"), "β"),
  comb.rep-res(tag-nocase("g"), "γ"),
  comb.rep-res(tag-nocase("d"), "δ"),
  comb.rep-res(tag-nocase("e"), "ε"),
  comb.rep-res(tag-nocase("v"), "ϝ"),
  comb.rep-res(tag-nocase("z"), "ζ"),
  comb.rep-res(tag-nocase("h"), "η"),
  comb.rep-res(tag-nocase("q"), "θ"),
  comb.rep-res(tag-nocase("i"), "ι"),
  comb.rep-res(tag-nocase("k"), "κ"),
  comb.rep-res(tag-nocase("l"), "λ"),
  comb.rep-res(tag-nocase("m"), "μ"),
  comb.rep-res(tag-nocase("n"), "ν"),
  comb.rep-res(tag-nocase("c"), "ξ"),
  comb.rep-res(tag-nocase("o"), "ο"),
  comb.rep-res(tag-nocase("p"), "π"),
  comb.rep-res(tag-nocase("r"), "ρ"),
  comb.rep-res(tag-nocase("t"), "τ"),
  comb.rep-res(tag-nocase("u"), "υ"),
  comb.rep-res(tag-nocase("f"), "φ"),
  comb.rep-res(tag-nocase("x"), "χ"),
  comb.rep-res(tag-nocase("y"), "ψ"),
  comb.rep-res(tag-nocase("w"), "ω"),
  lowercase-sigma
))(input)

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
  comb.rep-res(tag("?"), chars.combining-dot-below)
))(input)

#let iota-subscript(input) = comb.rep-res(
  tag("|"),
  chars.iota-subscript-char
)(input)

#let lowercase-with-diacritics(input) = {
  let (input, letter) = lowercase-letter(input) 

  if letter == "" {
    return (input, "")  
  }

  // These are optional
  let (input, breathing) = breathing(input)
  let (input, accent) = comb.many(diacritic)(input)
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
  let (input, accent) = comb.many(diacritic)(input)

  let (input, letter) = comb.map-res(lowercase-letter, upper)(input)
  if letter == "" {
    return (input, "")
  }

  // Optional
  let (input, iota-subscript) = iota-subscript(input)

  let result = letter + breathing + accent + iota-subscript
  (input, result)
}

#let betacode(input) = {
  if type(input) != str {
    panic("Please use #betacode with a str, not a content.")
  }

  // Unescape input
  input = input
    .replace("\n", "\\n")
    .replace("\r", "\\r")
    .replace("\t", "\\t")

  let (input, parsed) = comb.many(
    comb.alt((
      uppercase-with-diacritics,
      lowercase-with-diacritics,
      punctuation,
      whitespace
    ))
  )(input)

  if input != "" {
    panic("Error while parsing betacode, there might be an incomplete letter at the end of the input: `" + input + "`")
  }

  text(parsed)
}
