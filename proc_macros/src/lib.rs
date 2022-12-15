use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Ident, Token, Visibility};


struct GenSyntaxEnum {
    visib: Visibility,
    enum_name: Ident,
    names: Punctuated<Ident, Token![,]>,
}

impl Parse for GenSyntaxEnum {
    fn parse(input: ParseStream) -> Result<Self> {
        let visib = input.parse::<Visibility>()?;
        let enum_name = input.parse::<Ident>()?;
        input.parse::<Token![|]>()?;
        let names = input.parse_terminated(Ident::parse)?;

        Ok(Self { visib, enum_name, names })
    }
}


#[proc_macro]
pub fn gen_syntax_enum(input: TokenStream) -> TokenStream {
    let GenSyntaxEnum {
        visib,
        enum_name,
        names
    } = parse_macro_input!(input as GenSyntaxEnum);

    let mut enum_units_ts = quote! {};
    let mut match_display_ts = quote! {};
    let mut match_name_ts = quote! {};

    for name in names {
        enum_units_ts.extend(quote! { #name, });

        let name_s = name.to_string();
        let name_s = name_s.trim_start_matches("r#");

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

        match_name_ts.extend(quote! {
            Self::#name => #name_s,
        });
    }

    TokenStream::from(quote! {
        #[allow(unused)]
        #[allow(non_camel_case_types)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #visib enum #enum_name {
            #enum_units_ts
        }

        impl std::fmt::Display for #enum_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #match_display_ts
                }
            }
        }

        impl #enum_name {
            #visib fn name(&self) -> String {
                match self {
                    #match_name_ts
                }.to_owned()
            }
        }
    })
}
