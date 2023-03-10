use std::fs;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::parse::Parse;
use syn::{Error, Ident, ItemFn, LitInt, LitStr, Result};

struct CommaSeparated<U, V>
where
    U: Parse,
    V: Parse,
{
    left: U,
    right: V,
}

impl<U, V> Parse for CommaSeparated<U, V>
where
    U: Parse,
    V: Parse,
{
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let left = input.parse()?;
        input.parse::<syn::Token![,]>()?;
        let right = input.parse()?;
        Ok(CommaSeparated { left, right })
    }
}

fn generate_day(attr: TokenStream, item: TokenStream) -> Result<TokenStream2> {
    let day = syn::parse::<LitInt>(attr)?.base10_parse::<u8>()?;

    let mut parser = syn::parse::<ItemFn>(item)?;
    parser.sig.ident = Ident::new("__parse_input", Span::call_site());

    Ok(quote! {
        #parser

        #[derive(Debug)]
        pub struct Day;

        impl aoc::Solution for Day {
            fn solve_one(&self, input: &str) -> String {
                __solve_one(input)
            }

            fn solve_two(&self, input: &str) -> String {
                __solve_two(input)
            }

            fn day(&self) -> u8 { #day }
        }
    })
}

fn generate_solve(
    attr: TokenStream,
    item: TokenStream,
) -> Result<TokenStream2> {
    let attr = syn::parse::<LitInt>(attr)?;
    let num = attr.base10_parse::<u8>()?;

    let name = Ident::new(
        match num {
            1 => "__solve_one",
            2 => "__solve_two",
            _ => {
                return Err(Error::new_spanned(
                    attr,
                    "solve attribute must be 1 or 2",
                ))
            }
        },
        Span::call_site(),
    );

    let handler = syn::parse::<ItemFn>(item)?;
    let handler_name = handler.sig.ident.clone();

    Ok(quote! {
        #handler

        fn #name(input: &str) -> String {
            std::string::ToString::to_string(
                &#handler_name(__parse_input(input))
            )
        }
    })
}

fn generate_solutions(attr: TokenStream) -> Result<TokenStream2> {
    let parsed = syn::parse::<CommaSeparated<LitStr, Ident>>(attr)?;
    let directory = parsed.left.value();
    let loader = parsed.right;

    let read_dir = match fs::read_dir(&directory) {
        Ok(dir) => dir,
        Err(_) => {
            return Err(Error::new_spanned(
                parsed.left,
                "solutions attribute must be a valid directory",
            ))
        }
    };

    let days = read_dir
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.extension() == Some("rs".as_ref()))
        .filter_map(|path| {
            path.file_stem()
                .and_then(|name| name.to_str().zip(path.to_str()))
                .map(|(name, path)| (name.to_string(), path.to_string()))
        })
        .filter(|(name, _)| name.starts_with("day_"));

    let (imports, inserts): (Vec<TokenStream2>, Vec<TokenStream2>) = days
        .map(|(name, path)| {
            (Ident::new(&name, Span::call_site()), format!("../{path}"))
        })
        .map(|(name, directory)| {
            (
                quote! { #[path = #directory] mod #name; },
                quote! {
                    let solution = #name::Day;
                    map.insert(solution.day(), Box::new(solution));
                },
            )
        })
        .unzip();

    Ok(quote! {
        use aoc::Solution;

        #(#imports)*

        fn #loader() -> std::collections::HashMap<
            u8,
            Box<dyn aoc::Solution>
        > {
            let mut map = std::collections::HashMap::<
                u8, Box<dyn aoc::Solution>
            >::new();

            #(#inserts)*

            map
        }
    })
}

fn unwrap_error(stream: Result<TokenStream2>) -> TokenStream {
    stream.unwrap_or_else(|e| e.to_compile_error()).into()
}

#[proc_macro_attribute]
pub fn day(attr: TokenStream, item: TokenStream) -> TokenStream {
    unwrap_error(generate_day(attr, item))
}

#[proc_macro_attribute]
pub fn solve(attr: TokenStream, item: TokenStream) -> TokenStream {
    unwrap_error(generate_solve(attr, item))
}

#[proc_macro]
pub fn load_solutions(input: TokenStream) -> TokenStream {
    unwrap_error(generate_solutions(input))
}
