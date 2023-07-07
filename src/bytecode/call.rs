use super::*;
use crate::{Fun, Ref};
use faerie::Link;

#[derive(Debug, Clone, Copy)]
pub struct Call<'a> {
    fun: &'a str,
    start: usize,
}

impl<'a> Call<'a> {
    pub fn from_str(input: &'a str, current_bytecode: &[u8]) -> Option<Call<'a>> {
        let input = input.trim();

        if input.starts_with("call ") {
            let fun = input[5..].trim();

            Some(Call {
                fun,
                start: current_bytecode.len() + 0x1,
            })
        } else {
            None
        }
    }

    pub fn get_ref(&self, name: &'a str) -> Ref<'a> {
        Ref {
            from: name,
            to: self.fun,
            at: self.start.try_into().unwrap(),
        }
    }

    pub fn into_bytes(&self) -> [u8; 5] {
        [0xE8, 0x0, 0x0, 0x0, 0x0]
    }
}
