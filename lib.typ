#let betacode-plugin = plugin("wasm.wasm")

#let bcode(input) = str(
  betacode-plugin.bcode(
    bytes(input)
  )
)
