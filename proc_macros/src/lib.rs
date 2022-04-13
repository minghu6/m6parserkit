use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Ident, Token, Visibility};


struct GenSyntaxEnum {
    visib: Visibility,
    names: Punctuated<Ident, Token![,]>,
}

impl Parse for GenSyntaxEnum {
    fn parse(input: ParseStream) -> Result<Self> {
        let visib = input.parse::<Visibility>()?;
        input.parse::<Token![|]>()?;
        let names = input.parse_terminated(Ident::parse)?;
        input.parse::<Token![|]>()?;

        Ok(Self { visib, names })
    }
}


#[proc_macro]
pub fn gen_syntax_enum(input: TokenStream) -> TokenStream {
    let GenSyntaxEnum {
        visib,
        names
    } = parse_macro_input!(input as GenSyntaxEnum);

    let mut enum_units_ts = quote! {};
    let mut match_display_ts = quote! {};

    for name in names {
        enum_units_ts.extend(quote! { #name, });

        let name_s = name.to_string();

        if name_s.chars().next().unwrap().is_uppercase() {
            // NonTerminal
            match_display_ts.extend(quote! {
                Self::#name => write!(f, "[{}]", stringify!(#name)),
            })
        } else {
            match_display_ts.extend(quote! {
                Self::#name => write!(f, "<{}>", stringify!(#name)),
            })
        }
    }

    TokenStream::from(quote! {
        #[allow(non_camel_case_types)]
        #[derive(Debug, Clone, Copy)]
        #visib enum SyntaxType {
            #enum_units_ts
        }

        impl std::fmt::Display for SyntaxType {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #match_display_ts
                }
            }
        }
    })
}
