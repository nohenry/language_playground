#![feature(iter_intersperse)]
use std::{collections::HashSet, str::FromStr};

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream, Parser},
    parse_macro_input,
    punctuated::Punctuated,
    Attribute, Data, DeriveInput, Fields, GenericArgument, ReturnType, Type,
};

fn has_attr<'a>(attrs: &'a Vec<Attribute>, st: &[&str]) -> Option<&'a Attribute> {
    attrs.iter().find(|a| {
        let segs = &a.meta.path().segments;
        if segs.len() != st.len() {
            return false;
        }

        for (a, b) in segs.iter().zip(st.iter()) {
            if a.ident.to_string() != *b {
                return false;
            }
        }

        true
    })
}

#[derive(Debug)]
enum ArgType {
    Ident(String),
    Type(Type),
}

impl Parse for ArgType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(syn::Token![type]) {
            let _type: syn::Token![type] = input.parse()?;
            let _eq: syn::Token![=] = input.parse()?;
            Ok(ArgType::Type(input.parse()?))
        } else {
            let ident: syn::Ident = input.parse()?;
            Ok(ArgType::Ident(ident.to_string()))
        }
    }
}

fn attr_args(attr: &Attribute) -> (HashSet<String>, HashSet<Type>) {
    match &attr.meta {
        syn::Meta::List(list) => {
            let parser = Punctuated::<ArgType, syn::Token![,]>::parse_separated_nonempty;
            let args = parser.parse(list.tokens.clone().into()).unwrap();

            let all: (HashSet<_>, HashSet<_>) = args
                .into_iter()
                .map(|f| match f {
                    ArgType::Ident(ident) => (Some(ident), None),
                    ArgType::Type(ty) => (None, Some(ty)),
                })
                .unzip();

            (
                all.0.into_iter().flatten().collect(),
                all.1.into_iter().flatten().collect(),
            )
        }
        _ => (HashSet::new(), HashSet::new()),
    }
}

fn is_optional(ty: &Type) -> bool {
    match &ty {
        syn::Type::Path(path) => {
            let segs = &path.path.segments;
            if segs.len() != 1 {
                return false;
            }
            segs.first().unwrap().ident.to_string() == "Option"
        }
        _ => false,
    }
}

fn is_box(ty: &Type) -> bool {
    match &ty {
        syn::Type::Path(path) => {
            let segs = &path.path.segments;
            if segs.len() != 1 {
                return false;
            }
            segs.first().unwrap().ident.to_string() == "Box"
        }
        _ => false,
    }
}

fn is_optional_box(ty: &Type) -> bool {
    match &ty {
        syn::Type::Path(path) => {
            let segs = &path.path.segments;
            if segs.len() != 1 {
                return false;
            }

            let seg = segs.first().unwrap();
            if seg.ident.to_string() != "Option" {
                return false;
            }

            let syn::PathArguments::AngleBracketed(angs) = &seg.arguments else {
                return false;
            };

            let Some(GenericArgument::Type(arg)) = angs.args.first() else {
                return false
            };

            is_box(arg)
        }
        _ => false,
    }
}

fn types_match(pattern: &Type, source: &Type) -> bool {
    match (pattern, source) {
        (Type::Infer(_), _) => true,
        (a, b) if a == b => {
            return true;
        }
        (Type::Tuple(tuple), Type::Tuple(source_tuple)) => {
            if tuple.elems.len() != source_tuple.elems.len() {
                return false;
            }

            for (elem, source_elem) in tuple.elems.iter().zip(source_tuple.elems.iter()) {
                if !types_match(elem, source_elem) {
                    return false;
                }
            }

            true
        }
        (Type::Path(path), Type::Path(source_path)) => {
            let path_segs = &path.path.segments;
            let source_path_segs = &source_path.path.segments;

            if path_segs.len() != source_path_segs.len() {
                return false;
            }

            for (path, source_path) in path_segs.iter().zip(source_path_segs.iter()) {
                if path.ident != source_path.ident {
                    return false;
                }

                match (&path.arguments, &source_path.arguments) {
                    (a, b) if a == b => return true,
                    (
                        syn::PathArguments::AngleBracketed(angs),
                        syn::PathArguments::AngleBracketed(source_angs),
                    ) => {
                        if angs.args.len() != source_angs.args.len() {
                            return false;
                        }

                        for (ang, source_ang) in angs.args.iter().zip(source_angs.args.iter()) {
                            match (ang, source_ang) {
                                (GenericArgument::Type(ty), GenericArgument::Type(source_ty)) => {
                                    if !types_match(ty, source_ty) {
                                        return false;
                                    }
                                }
                                (ang, source_ang) if ang != source_ang => return false,
                                _ => (),
                            }
                        }
                    }
                    (
                        syn::PathArguments::Parenthesized(angs),
                        syn::PathArguments::Parenthesized(source_angs),
                    ) => {
                        if angs.inputs.len() != source_angs.inputs.len() {
                            return false;
                        }

                        for (ty, source_ty) in angs.inputs.iter().zip(source_angs.inputs.iter()) {
                            if !types_match(ty, source_ty) {
                                return false;
                            }
                        }

                        match (&angs.output, &source_angs.output) {
                            (ReturnType::Type(_, ty), ReturnType::Type(_, source_ty)) => {
                                if !types_match(&ty, &source_ty) {
                                    return false;
                                }
                            }
                            (a, b) if a != b => return false,
                            _ => (),
                        }
                    }
                    (_, _) => return false,
                }
            }

            true
        }
        _ => false,
    }
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use proc_macro::TokenStream;
    use syn::Type;

    use crate::types_match;

    #[test]
    fn test_type_match() {
        let p = syn::parse::<Type>(TokenStream::from_str("_").unwrap()).unwrap();
        let s = syn::parse::<Type>(TokenStream::from_str("i32").unwrap()).unwrap();

        println!("{}", types_match(&p, &s));
    }
}

#[proc_macro]
pub fn mcr(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let p =
        syn::parse::<Type>(proc_macro::TokenStream::from_str("Option<Box<(_, bool)>>").unwrap())
            .unwrap();
    let s =
        syn::parse::<Type>(proc_macro::TokenStream::from_str("Option<Box<(i32, bool)>>").unwrap())
            .unwrap();

    tokens
}

#[proc_macro_derive(FormatNode, attributes(transient, ignore_item, ignore_all, keep_item,))]
pub fn derive_node(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let ident = input.ident;

    let tokens = match &input.data {
        Data::Enum(node) => {
            // let variants = node.variants.
            let mut tokens = TokenStream::new();
            let mut num_childs = TokenStream::new();
            let mut childs = TokenStream::new();
            let mut boxed_childs = TokenStream::new();

            let (ignore_all, ignore_types) = has_attr(&input.attrs, &["ignore_all"])
                .map(|attr| attr_args(attr))
                .unwrap_or((HashSet::new(), HashSet::new()));

            for variant in &node.variants {
                let name = &variant.ident;

                if ignore_all.contains(&name.to_string()) {
                    continue;
                }

                if has_attr(&variant.attrs, &["ignore_item"]).is_some() {
                    continue;
                }

                let fields_in_variant = match &variant.fields {
                    Fields::Unnamed(_) => quote::quote! { (..) },
                    Fields::Unit => quote::quote! {},
                    Fields::Named(_) => quote::quote! { {..} },
                };

                match &variant.fields {
                    Fields::Unnamed(fields) => {
                        let mut num_child_values = quote::quote!(0);
                        let mut child_values = TokenStream::new();

                        let names: String = fields
                            .unnamed
                            .iter()
                            .enumerate()
                            .map(|(i, _)| format!("_a{}", i))
                            .intersperse(",".to_string())
                            .collect();
                        let name_toks = TokenStream::from_str(&names).unwrap();

                        for (i, field) in fields.unnamed.iter().enumerate() {
                            let field_name = TokenStream::from_str(&format!("_a{}", i)).unwrap();

                            if ignore_all.contains(&field_name.to_string()) {
                                continue;
                            }

                            if ignore_types.contains(&field.ty)
                                && has_attr(&field.attrs, &["keep_item"]).is_none()
                            {
                                continue;
                            }

                            if has_attr(&field.attrs, &["ignore_item"]).is_some() {
                                continue;
                            }

                            // if has_attr(&field.attrs, &["transient"]) {
                            //     num_child_values.extend(quote::quote! {
                            //         + #field_name.len()
                            //     });
                            // } else
                            if is_optional_box(&field.ty) {
                                num_child_values.extend(quote::quote! {
                                    + addup!(#field_name)
                                });

                                child_values.extend(quote::quote!(#field_name.map_tree(),));
                            } else if is_box(&field.ty) {
                                num_child_values.extend(quote::quote! {
                                    + 1
                                });

                                child_values.extend(quote::quote!(Some(&**#field_name),));
                            } else if is_optional(&field.ty) {
                                num_child_values.extend(quote::quote! {
                                    + addup!(#field_name)
                                });

                                child_values.extend(quote::quote!(#field_name,));
                            } else {
                                num_child_values.extend(quote::quote! {
                                    + 1
                                });

                                child_values.extend(quote::quote!(Some(#field_name),));
                            }
                        }

                        num_childs.extend(quote::quote! {
                            #ident::#name (#name_toks) => {
                                #num_child_values
                            }
                        });

                        childs.extend(quote::quote! {
                            #ident::#name (#name_toks) => {
                                switchon!(index, #child_values);
                                None
                            }
                        });
                    }
                    Fields::Unit => {
                        num_childs.extend(quote::quote! {
                            #ident::#name => 0,
                        });

                        childs.extend(quote::quote! {
                            #ident::#name => None,
                        });
                    }
                    Fields::Named(fields) => {
                        let mut num_child_values = quote::quote!(0);
                        let mut child_values = TokenStream::new();

                        let names: String = fields
                            .named
                            .iter()
                            .map(|field| field.ident.as_ref().unwrap().to_string())
                            .intersperse(",".to_string())
                            .collect();
                        let name_toks = TokenStream::from_str(&names).unwrap();

                        for field in fields.named.iter() {
                            let field_name = field.ident.as_ref().unwrap();

                            if ignore_all.contains(&field_name.to_string()) {
                                continue;
                            }

                            if ignore_types.contains(&field.ty)
                                && has_attr(&field.attrs, &["keep_item"]).is_none()
                            {
                                continue;
                            }

                            if has_attr(&field.attrs, &["ignore_item"]).is_some() {
                                continue;
                            }

                            // if has_attr(&field.attrs, &["transient"]) {
                            //     num_child_values.extend(quote::quote! {
                            //         + #field_name.len()
                            //     });
                            // } else
                            if is_optional_box(&field.ty) {
                                num_child_values.extend(quote::quote! {
                                    + addup!(#field_name)
                                });

                                child_values.extend(quote::quote!(#field_name.map_tree(),));
                            } else if is_box(&field.ty) {
                                num_child_values.extend(quote::quote! {
                                    + 1
                                });

                                child_values.extend(quote::quote!(Some(&**#field_name),));
                            } else if is_optional(&field.ty) {
                                num_child_values.extend(quote::quote! {
                                    + addup!(#field_name)
                                });

                                child_values.extend(quote::quote!(#field_name,));
                            } else {
                                num_child_values.extend(quote::quote! {
                                    + 1
                                });

                                child_values.extend(quote::quote!(Some(#field_name),));
                            }
                        }

                        num_childs.extend(quote::quote! {
                            #ident::#name {#name_toks} => {
                                #num_child_values
                            }
                        });

                        childs.extend(quote::quote! {
                            #ident::#name {#name_toks} => {
                                switchon!(index, #child_values);
                                None
                            }
                        });
                    }
                };

                let string = name.to_string();

                tokens.extend(quote::quote! {
                    #ident::#name #fields_in_variant => write!(f, #string),
                })
            }

            quote::quote! {

                impl NodeDisplay for #ident {
                    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
                        match self {
                            #tokens
                            _ => Ok(())
                        }
                    }
                }

                impl TreeDisplay for #ident {
                    fn num_children(&self, _cfg: &Config) -> usize {
                        match self {
                            #num_childs
                            _ => 0
                        }
                    }

                    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
                        match self {
                            #childs
                            _ => None
                        }
                    }

                    fn child_at_bx<'b>(&'b self, index: usize, _cfg: &Config) -> Box<dyn TreeDisplay + 'b> {
                        match self {
                            #boxed_childs
                            _ => panic!("Unexpected index for enum!")
                        }
                    }
                }
            }
        }
        _ => panic!("Unexpected type"),
    };

    tokens.into_token_stream().into()
}

fn derive_ast_node_helper(
    ident: &syn::Ident,
    fields: &Fields,
    enum_field: Option<&syn::Ident>,
) -> TokenStream {
    let mut output = TokenStream::new();
    match &fields {
        Fields::Unnamed(fields) => {
            let mut generate = |left: i32,
                                mut right: i32,
                                left_name: &TokenStream,
                                stream: &TokenStream| {
                while left <= right && right >= 0 {
                    let right_field = &fields.unnamed[right as usize];
                    if has_attr(&right_field.attrs, &["skip_item"]).is_some() {
                        right -= 1;
                        continue;
                    }

                    let ignore_rights: String = (right as usize..fields.unnamed.len() - 1)
                        .map(|_| "_")
                        .intersperse(",")
                        .collect();

                    let right_name = TokenStream::from_str(&format!("_a{}", right)).unwrap();

                    let mut out = stream.clone();

                    if left != right {
                        if is_optional(&fields.unnamed[right as usize].ty) {
                            out.extend(quote::quote! { Some(#right_name) });
                        } else {
                            out.extend(right_name.clone());
                        }
                    }

                    if ignore_rights.len() > 0 {
                        if left != right {
                            out.extend(quote::quote!(,));
                        }
                        out.extend(TokenStream::from_str(&ignore_rights));
                    }

                    if let Some(enum_field) = enum_field {
                        output.extend(quote::quote! {
                            #ident::#enum_field (#out) => Range::from((&#left_name.get_range(), &#right_name.get_range())),
                        });
                    } else {
                        output.extend(quote::quote! {
                            #ident (#out) => Range::from((&#left_name.get_range(), &#right_name.get_range())),
                        });
                    }

                    if is_optional(&fields.unnamed[right as usize].ty) {
                        right -= 1
                    } else {
                        break;
                    }
                }
            };

            let mut left = 0;
            let right = fields.unnamed.len() as i32 - 1;

            while left < fields.unnamed.len() as i32 {
                let left_field = &fields.unnamed[left as usize];

                if has_attr(&left_field.attrs, &["skip_item"]).is_some() {
                    left += 1;
                    continue;
                }

                let left_name = TokenStream::from_str(&format!("_a{}", left)).unwrap();

                let mut inner_names = TokenStream::new();
                let ignore_lefts: String =
                    (0..left as usize).map(|_| "_").intersperse(",").collect();

                if ignore_lefts.len() > 0 {
                    inner_names.extend(TokenStream::from_str(&ignore_lefts));
                    inner_names.extend(quote::quote!(,));
                }

                if is_optional(&left_field.ty) {
                    inner_names.extend(quote::quote! { Some(#left_name) });
                } else {
                    inner_names.extend(left_name.clone());
                }

                inner_names.extend(quote::quote!(,..,));

                generate(left, right, &left_name, &inner_names);

                if is_optional(&left_field.ty) {
                    left += 1;
                } else {
                    break;
                }
            }
        }
        Fields::Unit => {
            if let Some(enum_field) = enum_field {
                output.extend(quote::quote! {
                    #ident::#enum_field => Range::default(),
                });
            } else {
                output.extend(quote::quote! {
                    #ident => Range::default(),
                });
            }
        }
        Fields::Named(fields) => {
            let mut generate = |left: i32,
                                mut right: i32,
                                left_name: &TokenStream,
                                stream: &TokenStream| {
                while left <= right && right >= 0 {
                    let mut out = stream.clone();

                    let right_field = &fields.named[right as usize];

                    if has_attr(&right_field.attrs, &["skip_item"]).is_some() {
                        right -= 1;
                        continue;
                    }

                    let right_name = right_field.ident.clone().unwrap().to_token_stream();

                    if left != right {
                        if is_optional(&right_field.ty) {
                            out.extend(quote::quote! { #right_name: Some(#right_name) });
                        } else {
                            out.extend(right_name.clone());
                        }
                        out.extend(quote::quote! {,});
                    }

                    if let Some(enum_field) = enum_field {
                        output.extend(quote::quote! {
                                        #ident::#enum_field { #out .. } => Range::from((&#left_name.get_range(), &#right_name.get_range())),
                                    });
                    } else {
                        output.extend(quote::quote! {
                                        #ident { #out .. } => Range::from((&#left_name.get_range(), &#right_name.get_range())),
                                    });
                    }

                    if is_optional(&right_field.ty) {
                        right -= 1
                    } else {
                        break;
                    }
                }
            };

            let mut left = 0;
            let right = fields.named.len() as i32 - 1;

            while left < fields.named.len() as i32 {
                let left_field = &fields.named[left as usize];

                if has_attr(&left_field.attrs, &["skip_item"]).is_some() {
                    left += 1;
                    continue;
                }

                let left_name = left_field.ident.clone().unwrap().to_token_stream();

                let mut inner_names = TokenStream::new();

                if is_optional(&left_field.ty) {
                    inner_names.extend(quote::quote! { #left_name: Some(#left_name) });
                } else {
                    inner_names.extend(left_name.clone());
                }

                inner_names.extend(quote::quote! {,});

                generate(left, right, &left_name, &inner_names);

                if is_optional(&left_field.ty) {
                    left += 1;
                } else {
                    break;
                }
            }
        }
    };

    output
}

#[proc_macro_derive(AstNode, attributes(skip_item))]
pub fn derive_ast_node(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let ident = input.ident;

    let tokens = match &input.data {
        Data::Struct(node) => {
            let mut num_childs = TokenStream::new();
            num_childs.extend(derive_ast_node_helper(&ident, &node.fields, None));

            quote::quote! {
                impl AstNode for #ident {
                    fn get_range(&self) -> Range {
                        match self {
                            #num_childs
                        }
                    }
                }
            }
        }

        Data::Enum(node) => {
            let mut num_childs = TokenStream::new();

            for variant in &node.variants {
                let name = &variant.ident;
                num_childs.extend(derive_ast_node_helper(&ident, &variant.fields, Some(name)));
            }

            quote::quote! {
                impl AstNode for #ident {
                    fn get_range(&self) -> Range {
                        match self {
                            #num_childs
                        }
                    }
                }
            }
        }
        _ => panic!("Unexpected type"),
    };

    // let tokens = quote::quote! {};

    tokens.into()
}
