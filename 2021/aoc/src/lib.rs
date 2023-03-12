pub use codegen::*;

pub mod itertools;

pub trait Solution {
    fn solve_one(&self, input: &str) -> String;
    fn solve_two(&self, input: &str) -> String;
    fn day(&self) -> u8;
}
