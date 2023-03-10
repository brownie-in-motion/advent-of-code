pub use codegen::*;

pub trait Solution {
    fn solve_one(&self, input: &str) -> String;
    fn solve_two(&self, input: &str) -> String;
    fn day(&self) -> u8;
}
