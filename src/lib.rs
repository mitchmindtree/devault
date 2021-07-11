//! A more flexible alternative to deriving `Default`.
//!
//! Deriving `Devault` behaves the same as deriving `Default`, but includes some added benefits.
//!
//! # Added Benefits
//!
//! **devault** allows for specifying specific default values for fields, even if `Default` is not
//! implemented for their respective type.
//!
//! ```rust
//! use devault::Devault;
//!
//! const C: u32 = 10;
//!
//! #[derive(Debug, Devault, PartialEq)]
//! struct Foo {
//!     a: u8,
//!     #[devault("1.0")]
//!     b: f32,
//!     #[devault("C")]
//!     c: u32,
//!     #[devault("Bar(0.5)")]
//!     d: Bar,
//! }
//!
//! #[derive(Debug, PartialEq)]
//! struct Bar(f32);
//!
//! fn main() {
//!     let foo = Foo::default();
//!     assert_eq!(foo.a, 0);
//!     assert_eq!(foo.b, 1.0);
//!     assert_eq!(foo.c, C);
//!     assert_eq!(foo.d, Bar(0.5));
//! }
//! ```
//!
//! It can be derived for enums too, with the requirement that a default value is provided.
//!
//! ```rust
//! use devault::Devault;
//!
//! #[derive(Debug, Devault, PartialEq)]
//! #[devault("Foo::B(128)")]
//! enum Foo {
//!     A,
//!     B(u8),
//! }
//!
//! fn main() {
//!     assert_eq!(Foo::default(), Foo::B(128));
//! }
//! ```
//!
//! **devault** can generate associated constants and/or functions for constructing a field's
//! default value outside of the `Default` implementation.
//!
//! ```rust
//! use devault::Devault;
//!
//! #[derive(Debug, Devault, PartialEq)]
//! struct Foo {
//!     #[devault("1.0", constant)]
//!     a: f32,
//!     #[devault("10", function)]
//!     b: u32,
//!     #[devault("0.5", constant = "INIT_C", function = "start_c")]
//!     c: f32,
//! }
//!
//! #[derive(Debug, Devault, PartialEq)]
//! #[devault("Bar::B(42)", constant)]
//! enum Bar {
//!     A,
//!     B(u8),
//! }
//!
//! fn main() {
//!     assert_eq!(Foo::DEFAULT_A, 1.0);
//!     assert_eq!(Foo::default_b(), 10);
//!     assert_eq!(Foo::INIT_C, 0.5);
//!     assert_eq!(Foo::start_c(), 0.5);
//!     assert_eq!(Bar::DEFAULT, Bar::B(42));
//! }
//! ```
//!
//! # TODO
//!
//! - Support generic types.

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Devault, attributes(devault, constant, function))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ident = &ast.ident;

    // The expression evaluating to the default value for the default impl.
    let (default_body, impl_consts, impl_fns) = match ast.data {
        syn::Data::Struct(ref data_struct) => {
            let body = struct_default_body(data_struct).unwrap();
            let consts = struct_impl_consts(&ast.ident, data_struct).unwrap();
            let fns = struct_impl_fns(&ast.ident, data_struct).unwrap();
            (body, Some(consts), Some(fns))
        }
        syn::Data::Enum(_) => {
            let body = enum_default_body(&ast).unwrap();
            let maybe_const_ = enum_impl_const(&ast).unwrap();
            (body, maybe_const_, None)
        }
        syn::Data::Union(_) => panic!("`Devault` cannot be derived for unions"),
    };

    // The full default implementation.
    let impl_default = quote! {
        impl Default for #ident {
            fn default() -> Self {
                #default_body
            }
        }
    };

    // Combine the impls together.
    let impl_consts = impl_consts.into_iter();
    let impl_fns = impl_fns.into_iter();
    let complete = quote! {
        #impl_default
        #(#impl_consts)*
        #(#impl_fns)*
    };

    complete.into()
}

// Find the `devault` attrbute in a list.
fn devault_attr(attrs: &[syn::Attribute]) -> syn::Result<Option<&syn::Attribute>> {
    let mut out = None;
    for attr in attrs {
        if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "devault" {
            if out.is_none() {
                out = Some(attr);
            } else {
                return Err(syn::Error::new_spanned(
                    attr,
                    "multiple definitions of `devault` found",
                ));
            }
        }
    }
    Ok(out)
}

// Retrieve the default expression from the given devault attribute.
fn devault_attr_expr(attr: &syn::Attribute) -> syn::Result<syn::Expr> {
    let list = match attr.parse_meta()? {
        syn::Meta::List(list) => list,
        meta => return Err(syn::Error::new_spanned(meta, "invalid `devault` attribute")),
    };

    // The expression should be the first nested argument.
    let expr_str = match list.nested.into_iter().next() {
        Some(syn::NestedMeta::Lit(syn::Lit::Str(s))) => s,
        meta => return Err(syn::Error::new_spanned(meta, "invalid `devault` attribute")),
    };

    match expr_str.parse::<syn::Expr>() {
        Ok(expr) => Ok(expr),
        Err(err) => {
            let msg = format!("invalid `devault` expression: {}", err);
            Err(syn::Error::new_spanned(expr_str, msg))
        }
    }
}

// A helper function shared between `devault_attr_const` and `devault_attr_function`.
fn devault_attr_nested(
    attr: &syn::Attribute,
    nested_attr: &str,
) -> syn::Result<Option<Option<syn::Ident>>> {
    let list = match attr.parse_meta()? {
        syn::Meta::List(list) => list,
        meta => return Err(syn::Error::new_spanned(meta, "invalid `devault` attribute")),
    };
    let elems = list.nested.into_iter().skip(1);
    let mut out = None;
    for elem in elems {
        let (path, lit) = match elem {
            syn::NestedMeta::Meta(syn::Meta::Path(ref path)) => (path, None),
            syn::NestedMeta::Meta(syn::Meta::NameValue(ref nv)) => (&nv.path, Some(&nv.lit)),
            _ => continue,
        };
        if path.segments.len() != 1 || path.segments[0].ident != nested_attr {
            continue;
        }
        if out.is_some() {
            let msg = format!("multiple definitions of `{}` found", nested_attr);
            return Err(syn::Error::new_spanned(elem, msg));
        }
        let lit_str = match lit {
            None => None,
            Some(syn::Lit::Str(lit_str)) => Some(lit_str),
            Some(lit) => {
                let msg = format!("{} identifier must be a str literal", nested_attr);
                return Err(syn::Error::new_spanned(lit, msg));
            }
        };
        out = match lit_str.map(|l| l.parse::<syn::Ident>()) {
            Some(Ok(ident)) => Some(Some(ident)),
            None => Some(None),
            Some(Err(err)) => {
                let msg = format!("invalid `{}` identifier: {}", nested_attr, err);
                return Err(syn::Error::new_spanned(lit, msg));
            }
        };
    }
    Ok(out)
}

// Retrieve the `devault` `constant` attribute if there is one.
fn devault_attr_const(attr: &syn::Attribute) -> syn::Result<Option<Option<syn::Ident>>> {
    devault_attr_nested(attr, "constant")
}

// Retrieve the `devault` `function` attribute if there is one.
fn devault_attr_fn(attr: &syn::Attribute) -> syn::Result<Option<Option<syn::Ident>>> {
    devault_attr_nested(attr, "function")
}

// Generate the default function body for any struct type.
fn struct_default_body(data: &syn::DataStruct) -> syn::Result<proc_macro2::TokenStream> {
    let ts = match data.fields {
        syn::Fields::Named(ref fields) => {
            let mut field_exprs = vec![];
            for f in &fields.named {
                let ident = &f.ident;
                let expr = match devault_attr(&f.attrs)? {
                    None => quote! { #ident: Default::default() },
                    Some(attr) => {
                        let expr = devault_attr_expr(attr)?;
                        quote! { #ident: #expr }
                    }
                };
                field_exprs.push(expr);
            }
            quote! {
                Self {
                    #(#field_exprs,)*
                }
            }
        }
        syn::Fields::Unnamed(ref fields) => {
            let mut field_exprs = vec![];
            for f in &fields.unnamed {
                let expr = match devault_attr(&f.attrs)? {
                    None => quote! { Default::default() },
                    Some(attr) => devault_attr_expr(attr)?.into_token_stream(),
                };
                field_exprs.push(expr);
            }
            quote! {
                Self(#(#field_exprs,)*)
            }
        }
        syn::Fields::Unit => {
            quote! { Self }
        }
    };
    Ok(ts)
}

// The default constant identifier for the given field in the case that one was not specified.
fn default_field_const_ident(field_ident: &syn::Ident) -> syn::Ident {
    let upper = format!("{}", field_ident).to_uppercase();
    let name = format!("DEFAULT_{}", upper);
    syn::Ident::new(&name, Span::call_site())
}

// Generate the default constants for the struct fields that have the devault `constant` attribute.
fn struct_impl_consts(
    ident: &syn::Ident,
    data: &syn::DataStruct,
) -> syn::Result<proc_macro2::TokenStream> {
    // Collect the const ident, type and expression for each field with the attribute.
    let mut nvs: Vec<(syn::Ident, syn::Type, syn::Expr)> = vec![];
    match data.fields {
        syn::Fields::Named(ref fields) => {
            for f in &fields.named {
                let attr = match devault_attr(&f.attrs)? {
                    None => continue,
                    Some(attr) => attr,
                };
                let expr = devault_attr_expr(attr)?;
                let ident = match devault_attr_const(attr)? {
                    None => continue,
                    Some(Some(ident)) => ident,
                    Some(None) => {
                        let f_ident = f.ident.as_ref().unwrap();
                        default_field_const_ident(f_ident)
                    }
                };
                let ty = f.ty.clone();
                nvs.push((ident, ty, expr));
            }
        }
        syn::Fields::Unnamed(ref fields) => {
            for (i, f) in fields.unnamed.iter().enumerate() {
                let attr = match devault_attr(&f.attrs)? {
                    None => continue,
                    Some(attr) => attr,
                };
                let expr = devault_attr_expr(attr)?;
                let ident = match devault_attr_const(attr)? {
                    None => continue,
                    Some(Some(ident)) => ident,
                    Some(None) => {
                        let name = format!("DEFAULT_{}", i);
                        syn::Ident::new(&name, Span::call_site())
                    }
                };
                let ty = f.ty.clone();
                nvs.push((ident, ty, expr));
            }
        }
        _ => (),
    };

    // Create the constant definitions.
    let field_consts = nvs.iter().map(|(ident, ty, expr)| {
        quote! {
            pub const #ident: #ty = #expr;
        }
    });

    // Wrap the constant definitions in a struct impl.
    let ts = quote! {
        impl #ident {
            #(#field_consts)*
        }
    };

    Ok(ts)
}

// Generate the default functions for the struct fields that have the devault `function` attribute.
fn struct_impl_fns(
    ident: &syn::Ident,
    data: &syn::DataStruct,
) -> syn::Result<proc_macro2::TokenStream> {
    // Collect the fn ident, type and expression for each field with the attribute.
    let mut nvs: Vec<(syn::Ident, syn::Type, syn::Expr)> = vec![];
    match data.fields {
        syn::Fields::Named(ref fields) => {
            for f in &fields.named {
                let attr = match devault_attr(&f.attrs)? {
                    None => continue,
                    Some(attr) => attr,
                };
                let expr = devault_attr_expr(attr)?;
                let ident = match devault_attr_fn(attr)? {
                    None => continue,
                    Some(Some(ident)) => ident,
                    Some(None) => {
                        let f_ident = f.ident.as_ref().unwrap();
                        let name = format!("default_{}", f_ident);
                        syn::Ident::new(&name, Span::call_site())
                    }
                };
                let ty = f.ty.clone();
                nvs.push((ident, ty, expr));
            }
        }
        syn::Fields::Unnamed(ref fields) => {
            for (i, f) in fields.unnamed.iter().enumerate() {
                let attr = match devault_attr(&f.attrs)? {
                    None => continue,
                    Some(attr) => attr,
                };
                let expr = devault_attr_expr(attr)?;
                let ident = match devault_attr_fn(attr)? {
                    None => continue,
                    Some(Some(ident)) => ident,
                    Some(None) => {
                        let name = format!("default_{}", i);
                        syn::Ident::new(&name, Span::call_site())
                    }
                };
                let ty = f.ty.clone();
                nvs.push((ident, ty, expr));
            }
        }
        _ => (),
    };

    // Create the function definitions.
    let field_fns = nvs.iter().map(|(ident, ty, expr)| {
        quote! {
            fn #ident() -> #ty {
                #expr
            }
        }
    });

    // Wrap the fn definitions in a struct impl.
    let ts = quote! {
        impl #ident {
            #(#field_fns)*
        }
    };

    Ok(ts)
}

// Generate the default function body for any non-void enum type.
fn enum_default_body(ast: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let attr = match devault_attr(&ast.attrs)? {
        Some(attr) => attr,
        None => {
            let msg = r#"deriving `Devault` for enums requires a default expr, e.g. `#[devault("Foo::A")]"#;
            return Err(syn::Error::new_spanned(ast, msg));
        }
    };
    let expr = devault_attr_expr(attr)?;
    let ts = quote! { #expr };
    Ok(ts)
}

// Generate a constant constructor for the enum's default if the attribute was specified.
fn enum_impl_const(ast: &syn::DeriveInput) -> syn::Result<Option<proc_macro2::TokenStream>> {
    let attr = match devault_attr(&ast.attrs)? {
        Some(attr) => attr,
        None => {
            let msg = r#"deriving `Devault` for enums requires a default expr, e.g. `#[devault("Foo::A")]"#;
            return Err(syn::Error::new_spanned(ast, msg));
        }
    };
    let expr = devault_attr_expr(attr)?;
    let const_ident = match devault_attr_const(attr)? {
        None => return Ok(None),
        Some(Some(ident)) => ident,
        Some(None) => syn::Ident::new("DEFAULT", Span::call_site()),
    };
    let enum_ident = &ast.ident;
    let ts = quote! {
        impl #enum_ident {
            pub const #const_ident: Self = #expr;
        }
    };
    Ok(Some(ts))
}
