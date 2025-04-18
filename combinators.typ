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
    (local_input, parsed.join())
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
