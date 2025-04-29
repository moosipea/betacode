use core::str;

use wasm_minimal_protocol::wasm_func;

#[cfg(target_arch = "wasm32")]
wasm_minimal_protocol::initiate_protocol!();

pub mod parser;
use parser::parse_betacode;

#[cfg_attr(target_arch = "wasm32", wasm_func)]
pub fn bcode(input: &[u8]) -> Result<Vec<u8>, String>{
    let string = str::from_utf8(input).map_err(|err| err.to_string())?;
    let betacode = parse_betacode(string).map_err(|err| err.to_string())?;
    todo!()
}
