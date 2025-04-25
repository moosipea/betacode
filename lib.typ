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

#let punctuation = comb.alt((
  tag("."),
  tag(","),
  comb.rep-res(tag(":"), chars.interpunct),
  comb.rep-res(tag(";"), chars.greek-question-mark),
  comb.rep-res(tag("'"), chars.single-quote-closing),
  comb.rep-res(tag("_"), chars.em-dash),
  comb.rep-res(tag("#"), chars.prime),
  comb.rep-res(tag("-"), chars.hyphen),
))

#let whitespace = comb.alt((
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
))  

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

#let lowercase-sigma = comb.alt((
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
))

// TODO: replace with dictionary lookup
#let vowel-letter = comb.alt((
  comb.rep-res(tag-nocase("a"), "α"),
  comb.rep-res(tag-nocase("e"), "ε"),
  comb.rep-res(tag-nocase("h"), "η"),
  comb.rep-res(tag-nocase("i"), "ι"),
  comb.rep-res(tag-nocase("o"), "ο"),
  comb.rep-res(tag-nocase("u"), "υ"),
  comb.rep-res(tag-nocase("w"), "ω"),
))

// TODO: replace with dictionary lookup
#let consonant-letter = comb.alt((
  comb.rep-res(tag-nocase("b"), "β"),
  comb.rep-res(tag-nocase("g"), "γ"),
  comb.rep-res(tag-nocase("d"), "δ"),
  comb.rep-res(tag-nocase("v"), "ϝ"),
  comb.rep-res(tag-nocase("z"), "ζ"),
  comb.rep-res(tag-nocase("q"), "θ"),
  comb.rep-res(tag-nocase("k"), "κ"),
  comb.rep-res(tag-nocase("l"), "λ"),
  comb.rep-res(tag-nocase("m"), "μ"),
  comb.rep-res(tag-nocase("n"), "ν"),
  comb.rep-res(tag-nocase("c"), "ξ"),
  comb.rep-res(tag-nocase("p"), "π"),
  comb.rep-res(tag-nocase("r"), "ρ"),
  comb.rep-res(tag-nocase("t"), "τ"),
  comb.rep-res(tag-nocase("f"), "φ"),
  comb.rep-res(tag-nocase("x"), "χ"),
  comb.rep-res(tag-nocase("y"), "ψ"),
  lowercase-sigma
))

#let breathing = comb.alt((
  comb.rep-res(tag(")"), chars.smooth-breathing),
  comb.rep-res(tag("("), chars.rough-breathing)
))

#let accent = comb.alt((
  comb.rep-res(tag("/"), chars.acute-accent),
  comb.rep-res(tag("="), chars.circumflex-accent),
  comb.rep-res(tag("\\"), chars.grave-accent),
  comb.rep-res(tag("?"), chars.combining-dot-below),
))

#let diaeresis = comb.rep-res(tag("+"), chars.diaeresis)

#let length = comb.alt((
  comb.rep-res(tag("&"), chars.macron),
  comb.rep-res(tag("'"), chars.breve)
))

#let iota-subscript = comb.rep-res(tag("|"), chars.iota-subscript)

#let lowercase-vowel-with-diacritics(input) = {
  let (input, letter) = vowel-letter(input) 

  if letter == "" {
    return (input, "")  
  }

  let (input, breathing-or-diaeresis) = comb.alt((breathing, diaeresis))(input)
  let (input, accent) = accent(input)
  let (input, iota-subscript) = iota-subscript(input)

  let result = letter + breathing-or-diaeresis + accent + iota-subscript
  (input, result)
}

#let uppercase-vowel-with-diacritics(input) = {
  let (input, asterisk) = tag("*")(input)

  if asterisk == "" {
    return (input, "") 
  }

  let (input, breathing-or-diaeresis) = comb.alt((breathing, diaeresis))(input)
  let (input, accent) = accent(input)

  let (input, letter) = comb.map-res(vowel-letter, upper)(input)
  if letter == "" {
    return (input, "")
  }

  let (input, iota-subscript) = iota-subscript(input)

  let result = letter + breathing-or-diaeresis + accent + iota-subscript
  (input, result)
}

#let vowel = comb.alt((
  uppercase-vowel-with-diacritics,
  lowercase-vowel-with-diacritics,
))

#let uppercase-consonant(input) = {
  let (input, asterisk) = tag("*")(input)

  if asterisk == "" {
    return (input, "") 
  }

  comb.map-res(consonant-letter, upper)(input)
}

#let lowercase-consonant = consonant-letter

#let consonant = comb.alt((
  uppercase-consonant,
  lowercase-consonant
))

#let letter = comb.alt((
  consonant,
  vowel
))

#let betacode-lookup = json("beta_code_to_unicode.json")

#let parse-characters = comb.many(comb.map-res(comb.alt((
  letter,
  punctuation,
  whitespace
)), char => {
    if char in  betacode-lookup{
      betacode-lookup.at(char) 
    } else {
      char 
    } 
}))

// TODO: rename to bcode?
#let betacode(input, auto-capitalization: false) = {
  assert.eq(type(input), str, message: "Please use #betacode with a str, not content.")

  let lookup = json("beta_code_to_unicode.json")

  // Unescape input
  input = input
    .replace("\n", "\\n")
    .replace("\r", "\\r")
    .replace("\t", "\\t")

  let (input, parsed) = parse-characters(input)

  assert.eq(input, "", message: "Error while parsing betacode, there might be an incomplete letter at the end of the input: `" + input + "`")

  //text(parsed)
  [#parsed]
}
