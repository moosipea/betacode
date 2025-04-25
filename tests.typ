#import "lib.typ"

// Note: Typst doesn't suppord Unicode normalization at this point
#let test-cases = (
  (
    "E)N",
    "ἐν"
  ),
  (
    "O(, OI(",
    "ὁ, οἱ"
  ),
  (
    "PRO/S",
    "πρός"
  ),
  (
    "TW=N",
    "τῶν"
  ),
  (
    "PRO\S",
    "πρὸς"
  ),
  (
    "PROI+E/NAI",
    "προϊέναι"
  ),
  (
    "TW=|",
    "τῷ"
  ),
  (
    "MAXAI/RA&S",
    "μαχαίρᾱς"
  ),
  (
    "MA/XAIRA'",
    "μάχαιρᾰ"
  ),
)

#{
  for (beta-code, expected-result) in test-cases {
    let result = lib.bcode(beta-code)
    assert.eq(result, expected-result)
  }
}
